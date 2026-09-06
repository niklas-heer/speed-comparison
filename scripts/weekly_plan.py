#!/usr/bin/env python3
"""Decide whether a weekly full run is due from a successful publication checkpoint.

This is a trusted scheduling decision, not authorization to execute a PR. Runner
identity must describe the actual environment, limits and profile used by Argo.
"""

import argparse
import hashlib
import json
from pathlib import Path
import re

from affected_targets import CATALOG, affected, git


def environment_digest(environment: dict) -> str:
    if (
        not environment
        or not environment.get("runner_id")
        or not environment.get("profile")
    ):
        raise ValueError("Runner identity requires runner_id and profile")
    return hashlib.sha256(
        json.dumps(environment, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest()


def plan(checkpoint: dict | None, head_ref: str, environment: dict) -> dict:
    head = git(
        "rev-parse", "--verify", "--end-of-options", head_ref + "^{commit}"
    ).strip()
    digest = environment_digest(environment)
    reasons = []
    base = checkpoint.get("source_revision") if checkpoint else None
    if checkpoint and (
        checkpoint.get("schema_version") != 1 or checkpoint.get("status") != "published"
    ):
        raise ValueError("Checkpoint must identify a successful published run")
    if checkpoint and (
        not isinstance(base, str) or not re.fullmatch(r"[0-9a-f]{40}", base)
    ):
        raise ValueError("Checkpoint must identify an immutable source revision")
    base_commit = (
        git(
            "rev-parse",
            "--verify",
            "--end-of-options",
            str(base) + "^{commit}",
            required=False,
        )
        if base
        else None
    )
    if base_commit:
        base = base_commit.strip()
        # Compare the actual measured trees, not their merge base. A reverted or
        # force-pushed main branch must still be compared with the published run.
        paths = [
            p
            for p in git("diff", "--name-only", "--no-renames", "-z", base, head).split(
                "\0"
            )
            if p
        ]
        changes = affected(
            git("show", f"{base}:{CATALOG}", required=False),
            git("show", f"{head}:{CATALOG}"),
            paths,
        )
        if changes["targets"] or changes["removed_targets"]:
            reasons.append(
                "Benchmark inputs changed since the published source revision"
            )
    else:
        changes = affected(None, git("show", f"{head}:{CATALOG}"), [])
        changes["report_check"] = True
        reasons.append("No available successful full-run checkpoint")
    if not checkpoint or checkpoint.get("environment_sha256") != digest:
        reasons.append("Runner environment or reporting profile changed")
    return {
        "schema_version": 1,
        "run_full_suite": bool(reasons),
        "reasons": reasons,
        "head_revision": head,
        "source_revision": base,
        "environment_sha256": digest,
        "report_check": changes["report_check"],
        "affected_targets": changes["targets"],
        "removed_targets": changes["removed_targets"],
        "publication_eligible": False,
    }


def published_checkpoint(publication_ref: str, run_id: str, environment: dict) -> dict:
    """Derive state only from an already committed, complete raw publication.

    This does not enable a schedule or publish anything. Called AFTER publication
    succeeded, against its fetched remote commit. The runner must attach the exact
    same environment digest to each measured result before it can be checkpointed.
    """
    if not re.fullmatch(r"\d{4}-\d{2}-\d{2}T\d{6}", run_id):
        raise ValueError("Invalid run ID")
    from tempfile import TemporaryDirectory
    from validate_publish import validate

    revision = git(
        "rev-parse", "--verify", "--end-of-options", publication_ref + "^{commit}"
    ).strip()
    prefix = f"docs/history/{run_id}/raw/"
    names = git("ls-tree", "-r", "--name-only", revision, "--", prefix).splitlines()
    digest = environment_digest(environment)
    with TemporaryDirectory() as temporary:
        directory = Path(temporary)
        for name in names:
            relative = name.removeprefix(prefix)
            if "/" in relative:
                raise ValueError("Unexpected nested raw artifact")
            (directory / relative).write_text(git("show", f"{revision}:{name}"))
        validate(directory)
        source = (directory / "source-revision.txt").read_text().strip()
        if not re.fullmatch("[0-9a-f]{40}", source):
            raise ValueError("Missing measured source revision")
        for file in directory.glob("*.json"):
            raw = json.loads(file.read_text())
            if (
                raw.get("RunnerEnvironmentSHA256") != digest
                or raw.get("MeasurementProfile") != environment["profile"]
            ):
                raise ValueError(
                    "Published evidence does not match the runner identity/profile"
                )
            if raw.get("SourceRevision") != source:
                raise ValueError("Mixed or missing source identity")
    return {
        "schema_version": 1,
        "status": "published",
        "run_id": run_id,
        "source_revision": source,
        "publication_revision": revision,
        "environment_sha256": digest,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--checkpoint", type=Path, required=True)
    parser.add_argument("--environment", type=Path, required=True)
    parser.add_argument("--head", default="HEAD")
    parser.add_argument("--published-revision")
    parser.add_argument("--run-id")
    args = parser.parse_args()
    environment = json.loads(args.environment.read_text())
    if args.published_revision:
        checkpoint = published_checkpoint(
            args.published_revision, args.run_id or "", environment
        )
        args.checkpoint.parent.mkdir(parents=True, exist_ok=True)
        temporary = args.checkpoint.with_suffix(".tmp")
        temporary.write_text(json.dumps(checkpoint, indent=2) + "\n")
        temporary.replace(args.checkpoint)
        print(json.dumps(checkpoint, indent=2))
    else:
        checkpoint = (
            json.loads(args.checkpoint.read_text())
            if args.checkpoint.exists()
            else None
        )
        print(json.dumps(plan(checkpoint, args.head, environment), indent=2))


if __name__ == "__main__":
    main()
