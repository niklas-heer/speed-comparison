#!/usr/bin/env python3
"""Recover inspectable source from a published run's immutable Git revision.

Uses AST declarations, never executes historical or proposed Python. Writes a
separate evidence supplement; original published artifacts remain unchanged.
"""

import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess

from affected_targets import catalog

ROOT = Path(__file__).resolve().parents[1]


def export(run_id):
    if not re.fullmatch(r"\d{4}-\d{2}-\d{2}T\d{6}", run_id):
        raise ValueError("Invalid published run ID")
    history = ROOT / "docs/history" / run_id
    revision = (history / "source-revision.txt").read_text().strip()
    if not re.fullmatch(r"[0-9a-f]{40}", revision):
        raise ValueError("Source must be an immutable Git revision")

    def git(*args):
        return subprocess.check_output(["git", *args], cwd=ROOT)

    catalog_bytes = git("show", f"{revision}:dagger-poc/languages.py")
    definitions, _, _ = catalog(catalog_bytes.decode())
    available = (
        git("ls-tree", "-r", "--name-only", revision, "src/").decode().splitlines()
    )
    rows = json.loads((history / "combined_results.json").read_text())
    targets, files = {}, {}
    for row in rows:
        target = row["target"]
        config = definitions[target]
        selected = sorted(
            set(config["paths"])
            | {
                p
                for p in available
                if config["prefix"] and p.startswith(config["prefix"])
            }
        )
        targets[target] = {"primary": config["paths"][0], "paths": selected}
        for path in selected:
            contents = git("show", f"{revision}:{path}")
            files[path] = {
                "content": contents.decode(),
                "sha256": hashlib.sha256(contents).hexdigest(),
            }
    destination = ROOT / "docs/report-evidence" / run_id
    destination.mkdir(parents=True, exist_ok=True)
    (destination / "sources.json").write_text(
        json.dumps(
            {
                "source_revision": revision,
                "provenance": "Recovered from the measured Git revision; not embedded in the original raw results",
                "catalog_sha256": hashlib.sha256(catalog_bytes).hexdigest(),
                "targets": targets,
                "files": files,
            },
            indent=2,
        )
        + "\n"
    )
    print(
        f"Recovered {len(files)} source files for {len(targets)} implementations at {revision}"
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("run_id")
    export(parser.parse_args().run_id)
