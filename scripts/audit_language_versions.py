#!/usr/bin/env python3
"""Audit Dagger language pins against the historical benchmark baseline."""

from __future__ import annotations

import argparse
import importlib.util
import json
import sys
from pathlib import Path
from typing import Any


def numeric_parts(version: str) -> tuple[int, ...]:
    """Extract numeric components of a version string."""
    parts = []
    current = ""
    for ch in version:
        if ch.isdigit():
            current += ch
        elif current:
            parts.append(int(current))
            current = ""
    if current:
        parts.append(int(current))
    return tuple(parts)


def version_status(pinned: str, baseline: str) -> str:
    """Classify version alignment."""
    if pinned == baseline:
        return "match"
    if not baseline:
        return "missing"

    pinned_parts = numeric_parts(pinned)
    baseline_parts = numeric_parts(baseline)
    if pinned_parts and baseline_parts:
        common = min(len(pinned_parts), len(baseline_parts))
        if pinned_parts[:common] == baseline_parts[:common]:
            return "compatible"
    return "mismatch"


def load_languages(path: Path) -> dict[str, Any]:
    """Load LANGUAGES from dagger-poc/languages.py."""
    spec = importlib.util.spec_from_file_location("dagger_languages", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Failed to load {path}")
    module = importlib.util.module_from_spec(spec)
    # Needed for dataclass module lookup on Python 3.14+
    sys.modules["dagger_languages"] = module
    spec.loader.exec_module(module)
    return module.LANGUAGES


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Compare language package pins in dagger-poc/languages.py against docs/history/latest"
    )
    parser.add_argument(
        "--baseline",
        default="docs/history/latest/combined_results.json",
        help="Path to baseline combined_results.json",
    )
    parser.add_argument(
        "--languages",
        default="dagger-poc/languages.py",
        help="Path to languages.py",
    )
    parser.add_argument(
        "--only-mismatch",
        action="store_true",
        help="Show only mismatched or missing languages",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="Emit machine-readable JSON",
    )
    args = parser.parse_args()

    baseline_path = Path(args.baseline)
    languages_path = Path(args.languages)

    baseline = json.loads(baseline_path.read_text())
    by_target = {str(r.get("target", "")).strip(): r for r in baseline if r.get("target")}
    by_name = {str(r.get("name", "")).strip(): r for r in baseline if r.get("name")}

    languages = load_languages(languages_path)

    rows: list[dict[str, Any]] = []
    for target, lang in languages.items():
        baseline_row = by_target.get(target)
        matched_by = "target"
        if baseline_row is None:
            baseline_row = by_name.get(lang.name)
            matched_by = "name"

        baseline_version = ""
        status = "missing"
        if baseline_row is not None:
            baseline_version = str(baseline_row.get("version", "")).strip()
            status = version_status(lang.primary_version, baseline_version)

        rows.append(
            {
                "target": target,
                "name": lang.name,
                "package": lang.primary_package,
                "pinned_version": lang.primary_version,
                "baseline_version": baseline_version,
                "status": status,
                "matched_by": matched_by if baseline_row is not None else "",
            }
        )

    rows.sort(key=lambda r: (r["status"], r["target"]))
    total_counts = {
        "match": sum(1 for r in rows if r["status"] == "match"),
        "compatible": sum(1 for r in rows if r["status"] == "compatible"),
        "mismatch": sum(1 for r in rows if r["status"] == "mismatch"),
        "missing": sum(1 for r in rows if r["status"] == "missing"),
    }
    if args.only_mismatch:
        rows = [r for r in rows if r["status"] not in ("match", "compatible")]

    counts = {
        "match": sum(1 for r in rows if r["status"] == "match"),
        "compatible": sum(1 for r in rows if r["status"] == "compatible"),
        "mismatch": sum(1 for r in rows if r["status"] == "mismatch"),
        "missing": sum(1 for r in rows if r["status"] == "missing"),
    }

    if args.json:
        print(json.dumps({"counts": counts, "rows": rows}, indent=2))
        return 0

    print(
        f"{'Target':16} {'Package':18} {'Pinned':12} {'Baseline':12} "
        f"{'Status':10} {'Match By'}"
    )
    print("-" * 86)
    for row in rows:
        print(
            f"{row['target'][:16]:16} {row['package'][:18]:18} "
            f"{row['pinned_version'][:12]:12} {row['baseline_version'][:12]:12} "
            f"{row['status'][:10]:10} {row['matched_by']}"
        )

    print()
    if args.only_mismatch:
        print(
            "Filtered summary: "
            f"match={counts['match']} compatible={counts['compatible']} "
            f"mismatch={counts['mismatch']} missing={counts['missing']}"
        )
        print(
            "Total summary: "
            f"match={total_counts['match']} compatible={total_counts['compatible']} "
            f"mismatch={total_counts['mismatch']} missing={total_counts['missing']}"
        )
    else:
        print(
            "Summary: "
            f"match={counts['match']} compatible={counts['compatible']} "
            f"mismatch={counts['mismatch']} missing={counts['missing']}"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
