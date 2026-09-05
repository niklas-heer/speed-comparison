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

import argparse
import asyncio
import hashlib
import json
import os
import platform
import re
import shlex
import sys
import uuid
from dataclasses import dataclass
from pathlib import Path

import dagger

from languages import (
    HYPERFINE_VERSION,
    LANGUAGES,
    MICROPYTHON_VERSION,
    Language,
    get_base_image_name,
    get_devbox_image,
    language_image_fingerprint,
    language_image_version_tag,
)
from measurement import measurement_command, measurement_metadata
from result_metadata import enrich_result
from suite import run_phases

# =============================================================================
# Configuration
# =============================================================================

# Benchmark settings
HYPERFINE_SHOW_OUTPUT = os.environ.get("HYPERFINE_SHOW_OUTPUT", "0").lower() in (
    "1",
    "true",
    "yes",
)
# Keep native optimization flags enabled by default on x86_64 for parity with legacy benchmarks.
HOST_ARCH = platform.machine().lower()
DEFAULT_ALLOW_NATIVE_FLAGS = HOST_ARCH in ("x86_64", "amd64")
ALLOW_NATIVE_FLAGS = os.environ.get(
    "ALLOW_NATIVE_FLAGS", "1" if DEFAULT_ALLOW_NATIVE_FLAGS else "0"
).lower() in ("1", "true", "yes")
# Isolate runtime caches for benchmark reproducibility.
BENCH_XDG_CACHE_HOME = "/tmp/bench-xdg-cache"
BENCH_XDG_CONFIG_HOME = "/tmp/bench-xdg-config"
BENCH_JULIA_DEPOT_PATH = "/tmp/bench-julia-depot"

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


def default_tooling() -> dict[str, str]:
    return {
        "devbox_image": get_devbox_image(),
        "hyperfine": HYPERFINE_VERSION,
        "micropython": MICROPYTHON_VERSION,
    }


def get_image_tag(registry: str, target: str, lang: Language, tooling=None) -> str:
    """Generate the full image tag for a language.

    Uses the base image name for languages that share a base.
    E.g., swift-simd uses the "swift" image.
    """
    tooling = tooling or default_tooling()
    base_name = get_base_image_name(target, lang)
    version = language_image_version_tag(
        lang,
        devbox_image=tooling["devbox_image"],
        hyperfine_version=tooling["hyperfine"],
        micropython_version=tooling["micropython"],
    )
    return f"{registry}/{base_name}:{version}"


async def get_container_from_registry(
    client: dagger.Client,
    target: str,
    lang: Language,
    registry: str,
    tooling=None,
) -> dagger.Container:
    """Pull a pre-built image from the registry."""
    image_tag = get_image_tag(registry, target, lang, tooling)
    print(f"  Pulling: {image_tag}")
    return client.container().from_(image_tag)


async def build_local_devbox_container(
    client: dagger.Client,
    lang: Language,
    tooling=None,
) -> dagger.Container:
    """Build a Devbox container locally (for development/testing)."""
    tooling = tooling or default_tooling()
    container = client.container().from_(tooling["devbox_image"])

    if lang.allow_insecure:
        container = container.with_env_variable("NIXPKGS_ALLOW_INSECURE", "1")
        insecure_list = " ".join(lang.allow_insecure)
        container = container.with_env_variable(
            "NIX_CONFIG", f"extra-allowed-insecure-packages = {insecure_list}"
        )

    packages = list(lang.nixpkgs) + [
        f"hyperfine@{tooling['hyperfine']}",
        f"micropython@{tooling['micropython']}",
    ]
    container = container.with_workdir("/app").with_exec(["devbox", "init"])

    if packages:
        packages_str = " ".join(packages)
        if lang.allow_insecure:
            insecure_flags = " ".join(f"--allow-insecure={pkg}" for pkg in lang.allow_insecure)
            container = container.with_exec(
                ["sh", "-c", f"devbox add {packages_str} {insecure_flags}"]
            )
        else:
            container = container.with_exec(["sh", "-c", f"devbox add {packages_str}"])

    for flake_ref in lang.nix_flakes:
        container = container.with_exec(["devbox", "add", flake_ref])

    if lang.nix_setup:
        container = container.with_new_file(
            "/app/.benchmark-setup.sh", contents=lang.nix_setup
        ).with_exec(["devbox", "run", "--", "sh", "-e", "/app/.benchmark-setup.sh"])

    return container


async def get_container(
    client: dagger.Client,
    target: str,
    lang: Language,
    use_local: bool = False,
    registry: str = DEFAULT_REGISTRY,
    tooling=None,
) -> dagger.Container:
    """Get a container for the language, either from registry or built locally."""
    if use_local:
        print("  Building locally...")
        return await build_local_devbox_container(client, lang, tooling)
    else:
        return await get_container_from_registry(client, target, lang, registry, tooling)


async def exec_cmd(
    container: dagger.Container,
    lang: Language,
    cmd: str,
) -> dagger.Container:
    """Execute a command in the container with devbox environment.

    All containers (registry or local) use Devbox, so we always need
    to run commands through 'devbox run' to get packages in PATH.
    """
    env_prefix = (
        "export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1; "
        f"export XDG_CACHE_HOME={BENCH_XDG_CACHE_HOME}; "
        f"export XDG_CONFIG_HOME={BENCH_XDG_CONFIG_HOME}; "
        f"export JULIA_DEPOT_PATH={BENCH_JULIA_DEPOT_PATH}; "
    )
    native_prefix = "unset NIX_ENFORCE_NO_NATIVE; " if ALLOW_NATIVE_FLAGS else ""
    wrapped_cmd = f"{env_prefix}{native_prefix}{cmd}"
    return container.with_new_file("/app/.benchmark-command.sh", contents=wrapped_cmd).with_exec(
        ["devbox", "run", "--", "sh", "-e", "/app/.benchmark-command.sh"]
    )


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
        'os_release=$(awk -F= \'/^PRETTY_NAME=/{gsub(/"/,"",$2); '
        "print $2; exit}' /etc/os-release); "
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


@dataclass
class PreparedBenchmark:
    container: dagger.Container
    target: str
    lang: Language
    rounds: int
    version: str
    tooling: dict[str, str]
    use_local: bool
    registry: str


async def prepare_benchmark(
    client,
    target,
    lang,
    src_dir,
    scmeta_script,
    quick_rounds=None,
    use_local=False,
    registry=DEFAULT_REGISTRY,
    tooling=None,
) -> PreparedBenchmark:
    """Finish environment setup and compilation before the measurement barrier."""
    tooling = dict(tooling or default_tooling())
    # Get container (from registry or build locally)
    container = await get_container(client, target, lang, use_local, registry, tooling)

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

    rounds_text = (
        quick_rounds if quick_rounds is not None else await src_dir.file("rounds.txt").contents()
    )
    rounds = int(rounds_text.strip())
    if rounds <= 0:
        raise ValueError("Rounds must be positive")

    # Copy rounds.txt (or override for quick testing)
    if quick_rounds:
        container = container.with_new_file("/app/rounds.txt", quick_rounds)
    else:
        container = container.with_file("/app/rounds.txt", src_dir.file("rounds.txt"))

    # Copy scmeta.py script (runs with micropython)
    container = container.with_file("/app/scmeta.py", scmeta_script)

    # Ensure the devbox user can write to /app (CI uses non-root user)
    container = ensure_app_writable(container)
    container = await exec_cmd(
        container,
        lang,
        "rm -rf "
        f"{BENCH_XDG_CACHE_HOME} {BENCH_XDG_CONFIG_HOME} {BENCH_JULIA_DEPOT_PATH} "
        "&& mkdir -p "
        f"{BENCH_XDG_CACHE_HOME} {BENCH_XDG_CONFIG_HOME} {BENCH_JULIA_DEPOT_PATH}",
    )

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
    print(f"  Version: {version}")

    # Awaiting a container graph, not just constructing it, enforces the barrier.
    await container.sync()
    return PreparedBenchmark(container, target, lang, rounds, version, tooling, use_local, registry)


async def measure_benchmark(prepared: PreparedBenchmark, *, timeout_seconds=None) -> dict:
    """Measure one materialized build, always beyond the fresh-cache boundary."""
    container = prepared.container
    target, lang, rounds = prepared.target, prepared.lang, prepared.rounds
    tooling = prepared.tooling
    # Run benchmark with hyperfine
    print(f"  Running: {lang.run}")
    # Keep toolchain and compilation caching, but never reuse timing results.
    measurement_id = uuid.uuid4().hex
    container = container.with_env_variable("BENCHMARK_MEASUREMENT_ID", measurement_id)
    env_info = await collect_environment(container)
    hyperfine_cmd = measurement_command(lang.run, show_output=HYPERFINE_SHOW_OUTPUT)
    if timeout_seconds is not None:
        if timeout_seconds <= 0:
            raise ValueError("Measurement timeout must be positive")
        hyperfine_cmd = shlex.join(
            ["timeout", "--kill-after=5s", str(timeout_seconds), "sh", "-ec", hyperfine_cmd]
        )
    container = await exec_cmd(container, lang, hyperfine_cmd)

    # Run scmeta.py with micropython to generate result JSON
    scmeta_cmd = shlex.join(
        [
            "micropython",
            "scmeta.py",
            f"--lang-name={lang.name}",
            f"--target-name={target}",
            f"--lang-version={prepared.version}",
            "--hyperfine=hyperfine.json",
            "--pi=pi.txt",
            "--output=result.json",
        ]
    )
    container = await exec_cmd(container, lang, scmeta_cmd)

    # Extract result
    result_content = await container.file("/app/result.json").contents()
    result = json.loads(result_content)
    enrich_result(result, target, lang, rounds)
    result.update(measurement_metadata(), MeasurementID=measurement_id)
    result["Environment"] = env_info
    result["Compile"] = lang.compile or ""
    result["Run"] = lang.run
    result["Nixpkgs"] = list(lang.nixpkgs)
    result["NixFlakes"] = list(lang.nix_flakes)
    result["Category"] = lang.category
    result["ImageTag"] = (
        get_image_tag(prepared.registry, target, lang, tooling) if not prepared.use_local else None
    )
    result["ImageFingerprint"] = language_image_fingerprint(
        lang,
        devbox_image=tooling["devbox_image"],
        hyperfine_version=tooling["hyperfine"],
        micropython_version=tooling["micropython"],
    )
    result["DevboxImage"] = tooling["devbox_image"]
    result["BuildSource"] = "local" if prepared.use_local else "registry"
    result["AllowNativeFlags"] = ALLOW_NATIVE_FLAGS
    result["DevboxLock"] = json.loads(await container.file("/app/devbox.lock").contents())
    result["DevboxConfig"] = json.loads(await container.file("/app/devbox.json").contents())

    print(f"  Result: {result.get('Min', 'N/A')} (min)")
    print(f"  Accuracy: {result.get('Accuracy', 'N/A')}")

    return result


async def run_benchmark(
    client,
    target,
    lang,
    src_dir,
    scmeta_script,
    quick_rounds=None,
    use_local=False,
    registry=DEFAULT_REGISTRY,
    tooling=None,
) -> dict | None:
    """Compatibility wrapper for callers running one target."""
    try:
        prepared = await prepare_benchmark(
            client, target, lang, src_dir, scmeta_script, quick_rounds, use_local, registry, tooling
        )
        return await measure_benchmark(prepared)
    except Exception as error:
        print(f"  ERROR ({target}): {error}")
        return None


async def main(
    targets=None, *, revision=None, base=None, output=None, prepare_jobs=2, measurement_timeout=None
) -> int:
    """Run one selected suite in one engine session and retain a separate evidence bundle.

    Revision mode is a manual entry point for an already authorized commit. It
    fetches catalog and sources from that same immutable Git tree; the driver and
    scmeta remain trusted. It does not authorize forks or dispatch GitHub events.
    """
    if revision is not None and not re.fullmatch(r"[0-9a-f]{40}", revision):
        raise ValueError("Revision must be an explicitly authorized full commit SHA")
    if base is not None and revision is None:
        raise ValueError("--base requires --revision")
    if base is not None and targets:
        raise ValueError("Choose explicit targets or --base selection, not both")
    if not 1 <= prepare_jobs <= 8:
        raise ValueError("Preparation concurrency must be between 1 and 8")
    if measurement_timeout is not None and measurement_timeout <= 0:
        raise ValueError("Measurement timeout must be positive")
    quick_rounds = os.environ.get("QUICK_TEST_ROUNDS") or None
    if quick_rounds is not None and int(quick_rounds) <= 0:
        raise ValueError("Rounds must be positive")
    # Revision-bound execution builds from the resolved tooling. Registry naming
    # still belongs to the trusted local catalog and is not used for proposed entries.
    use_local = revision is not None or os.environ.get("USE_LOCAL_IMAGES", "").lower() in (
        "1",
        "true",
        "yes",
    )
    registry = os.environ.get("REGISTRY", DEFAULT_REGISTRY)
    run_id = uuid.uuid4().hex
    output = Path(output) if output else RESULTS_DIR / run_id
    output.mkdir(parents=True, exist_ok=False)
    record = {
        "schema_version": 1,
        "run_id": run_id,
        "status": "preparing",
        "source_revision": revision,
        "source_kind": "git" if revision else "working-tree",
        "publication_eligible": False,
        "scope": "benchmark-only",
        "prepare_jobs": prepare_jobs,
        "measurement_timeout_seconds": measurement_timeout,
        "rounds_override": int(quick_rounds) if quick_rounds is not None else None,
        "driver_sha256": {
            name: hashlib.sha256(Path(__file__).with_name(name).read_bytes()).hexdigest()
            for name in (
                "benchmark.py",
                "suite.py",
                "measurement.py",
                "scmeta.py",
                "result_metadata.py",
                "languages.py",
                "catalog_manifest.py",
                "catalog_resolver.py",
                "catalog_export.py",
            )
        },
    }

    def save_record():
        temporary = output / "run.json.tmp"
        temporary.write_text(json.dumps(record, indent=2) + "\n")
        temporary.replace(output / "run.json")

    save_record()
    print(f"Evidence bundle: {output}")
    try:
        async with dagger.Connection(dagger.Config(log_output=sys.stderr)) as client:
            if revision:
                from catalog_resolver import encode_manifest, resolve_catalog

                source = (
                    client.git("https://github.com/niklas-heer/speed-comparison.git")
                    .commit(revision)
                    .tree()
                )
                manifest = await resolve_catalog(
                    client, source.file("dagger-poc/languages.py"), source_revision=revision
                )
                languages, tooling = manifest.languages, manifest.tooling
                src_dir = source.directory("src")
                (output / "catalog.json").write_text(encode_manifest(manifest))
                record["catalog_sha256"] = manifest.catalog_sha256
            else:
                languages, tooling = LANGUAGES, default_tooling()
                src_dir = client.host().directory(str(SRC_DIR))
            if base is not None:
                sys.path.insert(0, str(REPO_ROOT / "scripts"))
                from affected_targets import plan_revisions

                plan = plan_revisions(base, revision)
                (output / "plan.json").write_text(json.dumps(plan, indent=2) + "\n")
                targets = plan["targets"]
            elif not targets or targets == ["all"]:
                targets = list(languages)
            if len(set(targets)) != len(targets) or set(targets) - set(languages):
                raise ValueError("Specify unique known targets from the resolved catalog")
            rounds_text = quick_rounds or await src_dir.file("rounds.txt").contents()
            rounds = int(rounds_text.strip())
            if rounds <= 0:
                raise ValueError("Rounds must be positive")
            target_output = output / "targets"
            target_output.mkdir()
            (target_output / "rounds.txt").write_text(str(rounds) + "\n")
            if revision:
                (target_output / "source-revision.txt").write_text(revision + "\n")
            record.update(targets=targets, tooling=tooling, rounds=rounds)
            save_record()
            scmeta = get_scmeta_script(client)

            async def prepare(target):
                return await prepare_benchmark(
                    client,
                    target,
                    languages[target],
                    src_dir,
                    scmeta,
                    str(rounds),
                    use_local,
                    registry,
                    tooling,
                )

            async def measure(prepared):
                record["status"] = "measuring"
                save_record()
                result = await measure_benchmark(prepared, timeout_seconds=measurement_timeout)
                result.update(
                    SourceRevision=revision,
                    CatalogSHA256=record.get("catalog_sha256"),
                    SuiteRunID=run_id,
                )
                return result

            def save(target, result):
                (target_output / f"{target}.json").write_text(json.dumps(result, indent=2) + "\n")

            summary = await run_phases(targets, prepare, measure, save, concurrency=prepare_jobs)
            record.update({key: value for key, value in summary.items() if key != "results"})
            record["completed_targets"] = list(summary["results"])
            record["status"] = "failed" if summary["errors"] else "succeeded"
            save_record()
            print(
                f"Completed {len(summary['results'])}/{len(targets)}; "
                f"prepare {summary['preparation_wall_seconds']:.2f}s; "
                f"total {summary['total_wall_seconds']:.2f}s"
            )
            return 1 if summary["errors"] else 0
    except Exception as error:
        record.update(status="failed", error=str(error))
        save_record()
        raise


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("targets", nargs="*")
    parser.add_argument("--revision", help="Already authorized full source commit SHA")
    parser.add_argument("--base", help="Recompute affected targets against this Git base")
    parser.add_argument("--output", type=Path, help="New directory for this run's evidence")
    parser.add_argument("--prepare-jobs", type=int, default=2)
    parser.add_argument(
        "--measurement-timeout", type=int, help="Total seconds per target's warmups/samples"
    )
    args = parser.parse_args()
    sys.exit(
        asyncio.run(
            main(
                args.targets,
                revision=args.revision,
                base=args.base,
                output=args.output,
                prepare_jobs=args.prepare_jobs,
                measurement_timeout=args.measurement_timeout,
            )
        )
    )
