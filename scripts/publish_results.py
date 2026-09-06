#!/usr/bin/env python3
"""Publish a complete native run from a clean checkout of its source revision."""

import argparse
from pathlib import Path
import subprocess
import sys

from validate_publish import validate

ROOT = Path(__file__).resolve().parents[1]


def git(*args: str) -> str:
    return subprocess.check_output(["git", *args], cwd=ROOT, text=True).strip()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results", type=Path)
    parser.add_argument(
        "--push", action="store_true", help="Commit and push docs/history to master"
    )
    args = parser.parse_args()
    results = args.results.resolve()
    revision = (results / "source-revision.txt").read_text().strip()
    if revision != git("rev-parse", "HEAD"):
        raise SystemExit("Checkout must match the benchmark source revision")
    if git("status", "--porcelain"):
        raise SystemExit("Use a clean checkout before publishing")
    validate(results)
    if args.push:
        git("fetch", "origin", "master")
        if revision != git("rev-parse", "origin/master"):
            raise SystemExit(
                "Master changed during the run; benchmark the new revision first"
            )
    rounds = results / "rounds.txt"
    rounds.write_text("1000000000\n")
    subprocess.run(
        [
            sys.executable,
            str(ROOT / "analyze.py"),
            "--folder",
            str(results),
            "--out",
            str(results),
            "--rounds",
            str(rounds),
        ],
        cwd=ROOT,
        check=True,
    )
    subprocess.run(
        [sys.executable, str(ROOT / "publish.py"), "--results", str(results)],
        cwd=ROOT,
        check=True,
    )
    if args.push:
        git("add", "docs/history", "README.md")
        git(
            "-c",
            "user.name=Speed comparison benchmarks",
            "-c",
            "user.email=41898282+github-actions[bot]@users.noreply.github.com",
            "commit",
            "-m",
            "docs(results): publish homelab benchmark run",
        )
        git("push", "origin", "HEAD:master")


if __name__ == "__main__":
    main()
