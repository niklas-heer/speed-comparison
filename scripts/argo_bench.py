#!/usr/bin/env python3
"""Submit a benchmark to the homelab's Argo WorkflowTemplate using kubeconfig."""

import argparse
import json
import re
import subprocess


def workflow(revision: str, targets: str, rounds: int) -> dict:
    if not re.fullmatch(r"master|codex/[A-Za-z0-9._/-]+", revision):
        raise ValueError("Use master or a reviewed codex branch")
    if not re.fullmatch(r"[a-z0-9_-]+(?:\s+[a-z0-9_-]+)*", targets):
        raise ValueError("Targets must be names separated by spaces")
    if not 1 <= rounds <= 1_000_000_000:
        raise ValueError("Rounds must be between 1 and 1,000,000,000")
    return {
        "apiVersion": "argoproj.io/v1alpha1",
        "kind": "Workflow",
        "metadata": {
            "generateName": "speed-comparison-manual-",
            "namespace": "speed-comparison",
        },
        "spec": {
            "workflowTemplateRef": {"name": "speed-comparison"},
            "arguments": {
                "parameters": [
                    {"name": "revision", "value": revision},
                    {"name": "targets", "value": targets},
                    {"name": "rounds", "value": str(rounds)},
                ]
            },
        },
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--revision", default="master")
    parser.add_argument("--targets", default="c rust go python")
    parser.add_argument("--rounds", type=int, default=10000)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    manifest = workflow(args.revision, args.targets, args.rounds)
    if args.dry_run:
        print(json.dumps(manifest, indent=2))
    else:
        subprocess.run(
            ["kubectl", "create", "-f", "-"],
            input=json.dumps(manifest),
            text=True,
            check=True,
        )


if __name__ == "__main__":
    main()
