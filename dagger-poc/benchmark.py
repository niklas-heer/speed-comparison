#!/usr/bin/env python3
"""
Dagger pipeline for running speed-comparison benchmarks.

This pipeline uses pre-built container images from the registry.
Each image contains the language runtime + hyperfine, and benchmark
code is mounted at runtime.

Image Layers:
1. Base image (from registry): Language + hyperfine (cached, rarely changes)
2. Runtime additions: Source code + scmeta + rounds.txt (added each run)

Image Source:
- Default: pull pre-built image from registry
- Optional: build locally with USE_LOCAL_IMAGES=1

Usage:
    # Run all benchmarks
    dagger run python benchmark.py

    # Run specific languages
    dagger run python benchmark.py rust go python

    # Quick test with fewer iterations
    QUICK_TEST_ROUNDS=10000 dagger run python benchmark.py rust

    # Force local images only (skip registry pull attempts)
    USE_LOCAL_IMAGES=1 dagger run python benchmark.py rust

"""

from __future__ import annotations

import asyncio
import json
import os
import platform
import sys
from pathlib import Path

import dagger

from languages import (
    HYPERFINE_VERSION,
    MICROPYTHON_VERSION,
    LANGUAGES,
    Language,
    get_base_image_name,
    get_devbox_image,
    get_language,
    language_image_fingerprint,
    language_image_version_tag,
)

# =============================================================================
# Configuration
# =============================================================================

# Benchmark settings
WARMUP_RUNS = 2
BENCHMARK_RUNS = 3
TIME_UNIT = "second"
# Keep native optimization flags enabled by default on x86_64 for parity with legacy benchmarks.
HOST_ARCH = platform.machine().lower()
DEFAULT_ALLOW_NATIVE_FLAGS = HOST_ARCH in ("x86_64", "amd64")
ALLOW_NATIVE_FLAGS = os.environ.get(
    "ALLOW_NATIVE_FLAGS", "1" if DEFAULT_ALLOW_NATIVE_FLAGS else "0"
).lower() in ("1", "true", "yes")

# Registry (same as build_images.py)
DEFAULT_REGISTRY = "ghcr.io/niklas-heer/speed-comparison"

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
    version = language_image_version_tag(
        lang,
        devbox_image=get_devbox_image(),
        hyperfine_version=HYPERFINE_VERSION,
        micropython_version=MICROPYTHON_VERSION,
    )
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
    container = client.container().from_(get_devbox_image())

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
        return await build_local_devbox_container(client, lang)
    else:
        return await get_container_from_registry(client, target, lang, registry)


async def exec_cmd(
    container: dagger.Container,
    lang: Language,
    cmd: str,
) -> dagger.Container:
    """Execute a command in the container with devbox environment.

    All containers (registry or local) use Devbox, so we always need
    to run commands through 'devbox run' to get packages in PATH.
    """
    wrapped_cmd = f"unset NIX_ENFORCE_NO_NATIVE; {cmd}" if ALLOW_NATIVE_FLAGS else cmd
    return container.with_exec(["devbox", "run", "--", "sh", "-c", wrapped_cmd])


def ensure_app_writable(container: dagger.Container) -> dagger.Container:
    """Ensure /app is writable for the devbox user in registry images."""
    return (
        container.with_user("root")
        .with_exec(["sh", "-c", "chown -R devbox:devbox /app"])
        .with_user("devbox")
    )


async def collect_environment(container: dagger.Container) -> dict[str, str]:
    """Collect host environment details from inside the container."""
    env_cmd = (
        "cpu_model=$(awk -F': ' '/model name/{print $2; exit}' /proc/cpuinfo); "
        "cpu_flags=$(awk -F': ' '/flags/{print $2; exit}' /proc/cpuinfo); "
        "cpu_cores=$(awk -F': ' '/cpu cores/{print $2; exit}' /proc/cpuinfo); "
        "cpu_threads=$(grep -c '^processor' /proc/cpuinfo 2>/dev/null || nproc); "
        "arch=$(uname -m); "
        "kernel=$(uname -sr); "
        'os_release=$(awk -F= \'/^PRETTY_NAME=/{gsub(/"/,"",$2); print $2; exit}\' /etc/os-release); '
        "libc=$(getconf GNU_LIBC_VERSION 2>/dev/null || ldd --version 2>&1 | head -n1); "
        'echo "cpu_model=$cpu_model"; '
        'echo "cpu_cores=$cpu_cores"; '
        'echo "cpu_threads=$cpu_threads"; '
        'echo "cpu_flags=$cpu_flags"; '
        'echo "arch=$arch"; '
        'echo "kernel=$kernel"; '
        'echo "os_release=$os_release"; '
        'echo "libc=$libc"'
    )
    try:
        output = await container.with_exec(["sh", "-c", env_cmd]).stdout()
    except Exception:
        return {}
    info: dict[str, str] = {}
    for line in output.splitlines():
        if "=" not in line:
            continue
        key, value = line.split("=", 1)
        info[key.strip()] = value.strip()
    return info


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

        # Copy source file(s)
        # For directory-based sources (e.g., "fs/Program.fs"), copy the entire directory
        if "/" in lang.file:
            source_dir_name = lang.file.split("/")[0]
            source_subdir = src_dir.directory(source_dir_name)
            container = container.with_directory(f"/app/{source_dir_name}", source_subdir)
        else:
            source_file = src_dir.file(lang.file)
            container = container.with_file(f"/app/{lang.file}", source_file)

        # Copy any extra files needed by the language
        for extra_file in lang.extra_files:
            container = container.with_file(f"/app/{extra_file}", src_dir.file(extra_file))

        # Copy rounds.txt (or override for quick testing)
        if quick_rounds:
            container = container.with_new_file("/app/rounds.txt", quick_rounds)
        else:
            container = container.with_file("/app/rounds.txt", src_dir.file("rounds.txt"))

        # Copy scmeta.py script (runs with micropython)
        container = container.with_file("/app/scmeta.py", scmeta_script)

        # Ensure the devbox user can write to /app (CI uses non-root user)
        container = ensure_app_writable(container)
        env_info = await collect_environment(container)

        # Compile if needed
        if lang.compile:
            print(f"  Compiling: {lang.compile}")
            container = await exec_cmd(container, lang, lang.compile)

        # Get version
        version_cmd = lang.version_cmd or "echo unknown"
        version_result = await (await exec_cmd(container, lang, f"{version_cmd} 2>&1")).stdout()
        version_output = version_result.strip()
        version = lang.extract_version(version_output) if version_output else "unknown"
        if not version:
            version = "unknown"
        # Escape special characters for shell safety
        version_escaped = (
            version.replace("\\", "\\\\")
            .replace('"', '\\"')
            .replace("$", "\\$")
            .replace("`", "\\`")
        )
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
        container = await exec_cmd(container, lang, hyperfine_cmd)

        # Run scmeta.py with micropython to generate result JSON
        scmeta_cmd = (
            f"micropython scmeta.py "
            f'--lang-name="{lang.name}" '
            f'--target-name="{target}" '
            f'--lang-version="{version_escaped}" '
            f"--hyperfine=hyperfine.json "
            f"--pi=pi.txt "
            f"--output=result.json"
        )
        container = await exec_cmd(container, lang, scmeta_cmd)

        # Extract result
        result_content = await container.file("/app/result.json").contents()
        result = json.loads(result_content)
        result["Environment"] = env_info
        result["Compile"] = lang.compile or ""
        result["Run"] = lang.run
        result["Nixpkgs"] = list(lang.nixpkgs)
        result["NixFlakes"] = list(lang.nix_flakes)
        result["Category"] = lang.category
        result["ImageTag"] = get_image_tag(registry, target, lang)
        result["ImageFingerprint"] = language_image_fingerprint(
            lang,
            devbox_image=get_devbox_image(),
            hyperfine_version=HYPERFINE_VERSION,
            micropython_version=MICROPYTHON_VERSION,
        )
        result["DevboxImage"] = get_devbox_image()
        result["BuildSource"] = "local" if use_local else "registry"
        result["AllowNativeFlags"] = ALLOW_NATIVE_FLAGS

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
    print(f"Allow native flags: {ALLOW_NATIVE_FLAGS}")
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
