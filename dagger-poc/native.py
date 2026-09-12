#!/usr/bin/env python3
"""Run one benchmark in a disposable Devbox container (for Argo Workflows).

Uses the same language definitions, hyperfine and scmeta as the Dagger adapter.
The container must provide a writable /app and a Python interpreter. No Docker
socket, registry credentials, or privileged container is needed.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import platform
import shlex
import shutil
import subprocess

from languages import LANGUAGES, get_devbox_image, get_tool_packages
from result_metadata import enrich_result
from measurement import measurement_command, measurement_metadata


def execute(command: list[str], cwd: Path, *, capture: bool = False) -> str:
    result = subprocess.run(
        command, cwd=cwd, check=True, text=True, stdout=subprocess.PIPE if capture else None
    )
    return result.stdout or ""


def run(target: str, source: Path, workspace: Path, output: Path, rounds: int) -> None:
    lang = LANGUAGES[target]
    workspace.mkdir(parents=True, exist_ok=True)
    # One target per disposable container: setup commands can use /app directly.
    packages = list(
        dict.fromkeys(
            (
                *lang.nixpkgs,
                *lang.nix_flakes,
                *get_tool_packages(lang),
            )
        )
    )
    execute(["devbox", "init"], workspace)
    execute(
        ["devbox", "add", *packages, *(f"--allow-insecure={p}" for p in lang.allow_insecure)],
        workspace,
    )

    # Do not inherit a bootstrap Python package's module search path.
    os.environ.pop("PYTHONPATH", None)
    os.environ.update(
        OMP_NUM_THREADS="1",
        OPENBLAS_NUM_THREADS="1",
        MKL_NUM_THREADS="1",
        JULIA_DEPOT_PATH="/tmp/bench-julia-depot",
    )
    allow_native = platform.machine().lower() in {"amd64", "x86_64"}
    prefix = "unset NIX_ENFORCE_NO_NATIVE; " if allow_native else ""

    def devbox(command: str, capture: bool = False) -> str:
        # Devbox interpolates inline shell arguments. A file preserves command
        # substitutions and variables until they run inside the target shell.
        script = workspace / ".benchmark-command.sh"
        script.write_text(prefix + command + "\n")
        return execute(["devbox", "run", "--", "sh", "-e", str(script)], workspace, capture=capture)

    if lang.nix_setup:
        devbox(lang.nix_setup)
    if "/" in lang.file:
        directory = lang.file.split("/")[0]
        shutil.copytree(source / directory, workspace / directory, dirs_exist_ok=True)
    else:
        shutil.copy2(source / lang.file, workspace / lang.file)
    for extra in lang.extra_files:
        destination = workspace / extra
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source / extra, destination)
    (workspace / "rounds.txt").write_text(str(rounds) + "\n")
    shutil.copy2(Path(__file__).with_name("scmeta.py"), workspace / "scmeta.py")
    if lang.compile:
        devbox(lang.compile)
    version = lang.extract_version(devbox(f"{lang.version_cmd or 'echo unknown'} 2>&1", True))
    devbox(measurement_command(lang.run, show_output=True))
    args = [
        "micropython",
        "scmeta.py",
        f"--lang-name={lang.name}",
        f"--target-name={target}",
        f"--lang-version={version}",
        "--hyperfine=hyperfine.json",
        "--pi=pi.txt",
        "--output=result.json",
    ]
    devbox(shlex.join(args))
    result = json.loads((workspace / "result.json").read_text())
    cpuinfo = Path("/proc/cpuinfo").read_text() if Path("/proc/cpuinfo").exists() else ""
    cpu = dict(line.split(":", 1) for line in cpuinfo.splitlines() if ":" in line)
    cpu = {k.strip(): v.strip() for k, v in cpu.items()}
    environment = {
        "arch": platform.machine(),
        "kernel": platform.release(),
        "cpu_model": cpu.get("model name", ""),
        "cpu_flags": cpu.get("flags", ""),
        "cpu_threads": str(os.cpu_count()),
        "runner": "homelab-argo",
    }
    result.update(
        Environment=environment,
        BuildSource="native-devbox",
        DevboxImage=get_devbox_image(),
        AllowNativeFlags=allow_native,
    )
    enrich_result(result, target, lang, rounds)
    result.update(measurement_metadata())
    # Preserve the actual resolved transitive Nix inputs for reproducing the run.
    lock = workspace / "devbox.lock"
    if lock.exists():
        result["DevboxLock"] = json.loads(lock.read_text())
        result["DevboxConfig"] = json.loads((workspace / "devbox.json").read_text())
    output.mkdir(parents=True, exist_ok=True)
    (output / f"{target}.json").write_text(json.dumps(result, indent=2, allow_nan=False))
    print(f"Completed {target}: {result['Min']}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("target", choices=sorted(LANGUAGES))
    parser.add_argument("--source", type=Path, default=Path(__file__).resolve().parents[1] / "src")
    parser.add_argument("--workspace", type=Path, default=Path("/app"))
    parser.add_argument("--output", type=Path, default=Path("/workspace/results"))
    parser.add_argument("--rounds", type=int, default=10000)
    args = parser.parse_args()
    if not 1 <= args.rounds <= 1_000_000_000:
        parser.error("rounds must be between 1 and 1,000,000,000")
    run(
        args.target,
        args.source.resolve(),
        args.workspace.resolve(),
        args.output.resolve(),
        args.rounds,
    )


if __name__ == "__main__":
    main()
