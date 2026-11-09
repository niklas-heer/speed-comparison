#!/usr/bin/env python3
"""Build and run one target in the pinned Devbox image, as homelab Argo does.

This is a functional smoke check for pull requests: it installs the declared
toolchain, compiles outside timing, runs the program on short workloads and
applies the same convergence check as a published run. The timings are on a
shared CI runner and are not benchmark results.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "pipeline"))

from languages import DEFAULT_DEVBOX_IMAGE, LANGUAGES  # noqa: E402

# Mirrors the benchmark step of the homelab WorkflowTemplate: bootstrap Python,
# then launch native.py outside that Devbox shell so the target gets only its
# own toolchain.
CONTAINER_SCRIPT = r"""
mkdir -p /tmp/bootstrap
cd /tmp/bootstrap
devbox init
devbox add python3@3.12.8
devbox run -- sh -c 'command -v python3' | tail -n1 > /tmp/python-path
for rounds in $ROUNDS; do
  echo "::group::$TARGET with $rounds rounds"
  "$(cat /tmp/python-path)" /workspace/repo/pipeline/native.py "$TARGET" \
    --rounds "$rounds" --workspace "/tmp/app-$rounds" --output "/workspace/results/$rounds"
  echo "::endgroup::"
done
"""


def seconds(value: str) -> float:
    """Parse scmeta's durations, such as "0.0019809077s"."""
    return float(value.removesuffix("s"))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("target", choices=sorted(LANGUAGES))
    parser.add_argument("--rounds", default="7 1000000",
                        help="space-separated workloads (default: a short odd one and a million)")
    parser.add_argument("--output", type=Path, default=ROOT / "smoke-results")
    args = parser.parse_args()
    rounds = args.rounds.split()
    if not rounds or not all(r.isdigit() and 1 <= int(r) <= 1_000_000_000 for r in rounds):
        parser.error("rounds must be integers between 1 and 1,000,000,000")

    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    output.chmod(0o777)  # the image runs as its own non-root user
    command = [
        "docker", "run", "--rm",
        "-e", f"TARGET={args.target}", "-e", "ROUNDS=" + " ".join(rounds),
        "-v", f"{ROOT}:/workspace/repo:ro", "-v", f"{output}:/workspace/results",
        os.environ.get("DEVBOX_IMAGE", DEFAULT_DEVBOX_IMAGE), "sh", "-ec", CONTAINER_SCRIPT,
    ]
    subprocess.run(command, check=True)

    lines = [f"### {LANGUAGES[args.target].name} (`{args.target}`)", "",
             "| Rounds | Output | Median | User CPU | Version | Math | Algorithm |",
             "| ---: | --- | ---: | ---: | --- | --- | --- |"]
    for count in rounds:
        result = json.loads((output / count / f"{args.target}.json").read_text())
        lines.append(
            f"| {int(count):,} | `{result['CalculatedPi']}` | {seconds(result['Median']):.3f} s "
            f"| {seconds(result['UserTime']):.3f} s | {result['Version']} "
            f"| {result['MathMode']} | {result['Algorithm']} |"
        )
    lines += ["", "CI runner timings show the program ran; they are not benchmark results.", ""]
    summary = "\n".join(lines)
    print(summary)
    if path := os.environ.get("GITHUB_STEP_SUMMARY"):
        with open(path, "a") as handle:
            handle.write(summary + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
