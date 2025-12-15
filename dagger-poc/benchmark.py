#!/usr/bin/env python3
"""
Dagger pipeline for running speed-comparison benchmarks.

This pipeline uses pre-built container images from the registry.
Each image contains the language runtime + hyperfine, and benchmark
code is mounted at runtime.

Image Layers:
1. Base image (from registry): Language + hyperfine (cached, rarely changes)
2. Runtime additions: Source code + scmeta + rounds.txt (added each run)

Smart Image Detection:
- By default, tries to pull from registry first
- If image doesn't exist, automatically builds locally
- Optionally pushes newly built images to registry (AUTO_PUSH_IMAGES=1)

Usage:
    # Run all benchmarks
    dagger run python benchmark.py

    # Run specific languages
    dagger run python benchmark.py rust go python

    # Quick test with fewer iterations
    QUICK_TEST_ROUNDS=10000 dagger run python benchmark.py rust

    # Force local images only (skip registry pull attempts)
    USE_LOCAL_IMAGES=1 dagger run python benchmark.py rust

    # Auto-push built images to registry (when image was missing)
    AUTO_PUSH_IMAGES=1 dagger run python benchmark.py rust
"""

from __future__ import annotations

import asyncio
import json
import os
import sys
from pathlib import Path

import dagger

from languages import IS_ARM, LANGUAGES, Language, get_base_image_name, get_language

# =============================================================================
# Configuration
# =============================================================================

# Benchmark settings
WARMUP_RUNS = 2
BENCHMARK_RUNS = 3
TIME_UNIT = "second"

# Registry (same as build_images.py)
DEFAULT_REGISTRY = "ghcr.io/niklas-heer/speed-comparison"

# Base images for local builds
DEVBOX_IMAGE = "jetpackio/devbox:latest"
NIX_IMAGE = "nixos/nix:latest"
HYPERFINE_VERSION = "1.18.0"
MICROPYTHON_VERSION = "1.24.1"

# Paths (relative to repo root - benchmark.py lives in dagger-poc/)
REPO_ROOT = Path(__file__).parent.parent
SRC_DIR = REPO_ROOT / "src"
RESULTS_DIR = REPO_ROOT / "results"
SCMETA_SCRIPT = Path(__file__).parent / "scmeta.py"


def get_scmeta_script(client: dagger.Client) -> dagger.File:
    """Get the scmeta.py script as a Dagger File.

    Unlike the Crystal version, no build step needed - just mount the script.
    MicroPython is included in the container images.
    """
    return client.host().file(str(SCMETA_SCRIPT))


# =============================================================================
# Container Setup
# =============================================================================


def get_image_tag(registry: str, target: str, lang: Language) -> str:
    """Generate the full image tag for a language.

    Uses the base image name for languages that share a base.
    E.g., swift-simd uses the "swift" image.
    """
    base_name = get_base_image_name(target)
    version = lang.primary_version.replace("+", "-")
    return f"{registry}/{base_name}:{version}"


async def get_container_from_registry(
    client: dagger.Client,
    target: str,
    lang: Language,
    registry: str,
) -> dagger.Container:
    """Pull a pre-built image from the registry."""
    image_tag = get_image_tag(registry, target, lang)
    print(f"  Pulling: {image_tag}")
    return client.container().from_(image_tag)


async def build_local_devbox_container(
    client: dagger.Client,
    lang: Language,
) -> dagger.Container:
    """Build a Devbox container locally (for development/testing)."""
    container = client.container().from_(DEVBOX_IMAGE)

    packages = list(lang.nixpkgs) + [
        f"hyperfine@{HYPERFINE_VERSION}",
        f"micropython@{MICROPYTHON_VERSION}",
    ]
    packages_str = " ".join(packages)

    container = (
        container.with_workdir("/app")
        .with_exec(["devbox", "init"])
        .with_exec(["sh", "-c", f"devbox add {packages_str}"])
    )

    if lang.nix_setup:
        container = container.with_exec(["devbox", "run", "--", "sh", "-c", lang.nix_setup])

    return container


async def build_local_nix_container(
    client: dagger.Client,
    lang: Language,
) -> dagger.Container:
    """Build a Nix flake container locally (for development/testing)."""
    container = client.container().from_(NIX_IMAGE)

    container = container.with_exec(
        [
            "sh",
            "-c",
            "mkdir -p /etc/nix && echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf",
        ]
    )

    flake_refs = list(lang.nix_flake) + ["nixpkgs#hyperfine", "nixpkgs#micropython"]
    flake_refs_str = " ".join(flake_refs)

    install_cmd = f"nix profile install {flake_refs_str}"
    if lang.allow_insecure:
        install_cmd = f"NIXPKGS_ALLOW_INSECURE=1 nix profile install --impure {flake_refs_str}"
    container = container.with_exec(["sh", "-c", install_cmd])

    if lang.nix_setup:
        container = container.with_exec(["sh", "-c", lang.nix_setup])

    return container.with_workdir("/app")


async def get_container(
    client: dagger.Client,
    target: str,
    lang: Language,
    use_local: bool = False,
    registry: str = DEFAULT_REGISTRY,
) -> dagger.Container:
    """Get a container for the language, either from registry or built locally."""
    if use_local:
        print(f"  Building locally...")
        if lang.uses_nix_flake:
            return await build_local_nix_container(client, lang)
        else:
            return await build_local_devbox_container(client, lang)
    else:
        return await get_container_from_registry(client, target, lang, registry)


async def exec_cmd(
    container: dagger.Container,
    lang: Language,
    cmd: str,
    use_local: bool = False,
) -> dagger.Container:
    """Execute a command in the container with appropriate shell wrapper."""
    if use_local and not lang.uses_nix_flake:
        # Local Devbox build needs devbox run wrapper
        return container.with_exec(["devbox", "run", "--", "sh", "-c", cmd])
    else:
        # Registry images and Nix containers have packages in PATH
        return container.with_exec(["sh", "-c", cmd])


# =============================================================================
# Pipeline
# =============================================================================


async def run_benchmark(
    client: dagger.Client,
    target: str,
    lang: Language,
    src_dir: dagger.Directory,
    scmeta_script: dagger.File,
    quick_rounds: str | None = None,
    use_local: bool = False,
    registry: str = DEFAULT_REGISTRY,
) -> dict | None:
    """Run benchmark for a single language.

    Args:
        client: Dagger client
        target: Target name (e.g., "rust", "go")
        lang: Language configuration
        src_dir: Host directory containing source files
        scmeta_script: scmeta.py script (run with micropython)
        quick_rounds: Override rounds for quick testing
        use_local: Build images locally instead of pulling from registry
        registry: Container registry to pull from

    Returns:
        Parsed JSON result or None on failure
    """
    print(f"\n{'=' * 60}")
    print(f"Benchmarking: {lang.name} ({target})")
    print(f"{'=' * 60}")

    try:
        # Get container (from registry or build locally)
        container = await get_container(client, target, lang, use_local, registry)

        # Setup working directory
        container = container.with_workdir("/app")

        # Copy source file
        source_file = src_dir.file(lang.file)
        container = container.with_file(f"/app/{lang.file}", source_file)

        # Copy rounds.txt (or override for quick testing)
        if quick_rounds:
            container = container.with_new_file("/app/rounds.txt", quick_rounds)
        else:
            container = container.with_file("/app/rounds.txt", src_dir.file("rounds.txt"))

        # Copy scmeta.py script (runs with micropython)
        container = container.with_file("/app/scmeta.py", scmeta_script)

        # Compile if needed
        if lang.compile:
            print(f"  Compiling: {lang.compile}")
            container = await exec_cmd(container, lang, lang.compile, use_local)

        # Get version
        version_cmd = lang.version_cmd or "echo unknown"
        version_result = await (await exec_cmd(container, lang, version_cmd, use_local)).stdout()
        version = version_result.strip().split("\n")[0]
        print(f"  Version: {version}")

        # Run benchmark with hyperfine
        print(f"  Running: {lang.run}")
        hyperfine_cmd = (
            f"hyperfine '{lang.run}' "
            f"--warmup {WARMUP_RUNS} "
            f"--runs {BENCHMARK_RUNS} "
            f"--time-unit {TIME_UNIT} "
            f"--export-json hyperfine.json "
            f"&& {lang.run} > pi.txt"
        )
        container = await exec_cmd(container, lang, hyperfine_cmd, use_local)

        # Run scmeta.py with micropython to generate result JSON
        scmeta_cmd = (
            f"micropython scmeta.py "
            f'--lang-name="{lang.name}" '
            f'--target-name="{target}" '
            f'--lang-version="{version}" '
            f"--hyperfine=hyperfine.json "
            f"--pi=pi.txt "
            f"--output=result.json"
        )
        container = await exec_cmd(container, lang, scmeta_cmd, use_local)

        # Extract result
        result_content = await container.file("/app/result.json").contents()
        result = json.loads(result_content)

        print(f"  Result: {result.get('Min', 'N/A')} (min)")
        print(f"  Accuracy: {result.get('Accuracy', 'N/A')}")

        return result

    except Exception as e:
        print(f"  ERROR: {e}")
        return None


async def main(targets: list[str] | None = None) -> int:
    """Run benchmarks for specified targets (or all if none specified)."""

    # Configuration from environment
    quick_rounds = os.environ.get("QUICK_TEST_ROUNDS")
    use_local = os.environ.get("USE_LOCAL_IMAGES", "").lower() in ("1", "true", "yes")
    registry = os.environ.get("REGISTRY", DEFAULT_REGISTRY)

    # Determine which languages to benchmark
    if targets:
        invalid = [t for t in targets if t not in LANGUAGES]
        if invalid:
            print(f"Unknown targets: {invalid}")
            print(f"Available: {list(LANGUAGES.keys())}")
            return 1
        languages_to_run = {t: get_language(t) for t in targets}
    else:
        languages_to_run = LANGUAGES

    if quick_rounds:
        print(f"Quick test mode: {quick_rounds} rounds")
    if use_local:
        print("Using local image builds (not pulling from registry)")
    else:
        print(f"Registry: {registry}")

    # Ensure results directory exists
    RESULTS_DIR.mkdir(exist_ok=True)

    results: dict[str, dict] = {}

    config = dagger.Config(log_output=sys.stderr)

    async with dagger.Connection(config) as client:
        # Get scmeta.py script (no build needed - runs with micropython)
        scmeta_script = get_scmeta_script(client)

        # Get source directory from host
        src_dir = client.host().directory(str(SRC_DIR))

        for target, lang in languages_to_run.items():
            # Skip x86_64-only languages on ARM when building locally
            if use_local and IS_ARM and lang.x86_64_only:
                print(f"\n{'=' * 60}")
                print(f"Skipping: {lang.name} ({target}) - x86_64 only")
                print(f"{'=' * 60}")
                continue

            result = await run_benchmark(
                client, target, lang, src_dir, scmeta_script, quick_rounds, use_local, registry
            )
            if result:
                results[target] = result

                # Save individual result file
                result_path = RESULTS_DIR / f"{target}.json"
                result_path.write_text(json.dumps(result, indent=2))
                print(f"  Saved: {result_path}")

    # Summary
    print(f"\n{'=' * 60}")
    print("SUMMARY")
    print(f"{'=' * 60}")
    print(f"Completed: {len(results)}/{len(languages_to_run)}")

    if results:
        sorted_results = sorted(
            results.items(), key=lambda x: float(x[1].get("Min", "999").rstrip("s"))
        )
        print("\nRanking (by min time):")
        for i, (target, result) in enumerate(sorted_results, 1):
            min_time = result.get("Min", "N/A")
            print(f"  {i:2}. {target:15} {min_time}")

    return 0 if len(results) == len(languages_to_run) else 1


if __name__ == "__main__":
    targets = sys.argv[1:] if len(sys.argv) > 1 else None
    exit_code = asyncio.run(main(targets))
    sys.exit(exit_code)
