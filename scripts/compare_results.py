#!/usr/bin/env python3
"""Compare two benchmark result sets and report drift.

Supports:
- raw per-language JSON files (results/*.json)
- combined_results.json (list with lower-case keys)
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any


def parse_time_ms(value: Any, *, source: str) -> float:
    """Parse time value to milliseconds."""
    if value is None:
        return float("nan")

    if isinstance(value, (int, float)):
        # combined_results.json uses milliseconds already
        return float(value) if source == "combined" else float(value) * 1000.0

    text = str(value).strip().lower()
    if text.endswith("ms"):
        return float(text[:-2])
    if text.endswith("s"):
        return float(text[:-1]) * 1000.0
    return float(text)


def normalize_record(data: dict[str, Any], *, source: str, fallback_target: str) -> dict[str, Any]:
    """Normalize a result entry to a common schema."""
    if source == "combined":
        name = str(data.get("name", "")).strip()
        target = str(data.get("target", "")).strip() or fallback_target
        min_ms = parse_time_ms(data.get("min"), source=source)
        median_ms = parse_time_ms(data.get("median"), source=source)
        version = str(data.get("version", "")).strip()
    else:
        name = str(data.get("Language", "")).strip()
        target = str(data.get("Target", "")).strip() or fallback_target
        min_ms = parse_time_ms(data.get("Min"), source=source)
        median_ms = parse_time_ms(data.get("Median"), source=source)
        version = str(data.get("Version", "")).strip()

    return {
        "name": name,
        "target": target,
        "min_ms": min_ms,
        "median_ms": median_ms,
        "version": version,
    }


def load_result_set(path: Path) -> list[dict[str, Any]]:
    """Load results from a directory or combined_results.json file."""
    if path.is_file():
        data = json.loads(path.read_text())
        if not isinstance(data, list):
            raise ValueError(f"Expected list in {path}")
        return [normalize_record(x, source="combined", fallback_target="") for x in data]

    if not path.is_dir():
        raise FileNotFoundError(path)

    records: list[dict[str, Any]] = []
    skip_files = {
        "combined_results.json",
        "run_metadata.json",
        "parity_report.json",
    }
    for file in sorted(path.rglob("*.json")):
        # Skip generated aggregate files if present in directory mode
        if file.name in skip_files:
            continue
        try:
            data = json.loads(file.read_text())
        except json.JSONDecodeError:
            continue
        if isinstance(data, dict):
            records.append(
                normalize_record(data, source="raw", fallback_target=file.stem)
            )
            continue
        if isinstance(data, list):
            for entry in data:
                if not isinstance(entry, dict):
                    continue
                records.append(
                    normalize_record(entry, source="combined", fallback_target=file.stem)
                )
    return records


def build_index(records: list[dict[str, Any]], key: str) -> dict[str, dict[str, Any]]:
    indexed: dict[str, dict[str, Any]] = {}
    for row in records:
        k = row.get(key, "")
        if not k:
            continue
        indexed[str(k)] = row
    return indexed


def rank_map(records: list[dict[str, Any]], key: str) -> dict[str, int]:
    sorted_rows = sorted(records, key=lambda r: r["min_ms"])
    return {str(row[key]): idx + 1 for idx, row in enumerate(sorted_rows)}


def fmt_pct(value: float) -> str:
    sign = "+" if value >= 0 else ""
    return f"{sign}{value:.2f}%"


def print_missing(
    *,
    baseline_only: list[str],
    candidate_only: list[str],
    limit: int,
) -> None:
    """Print keys that exist only on one side."""
    if baseline_only:
        print(f"\nMissing from candidate: {len(baseline_only)}")
        for key in baseline_only[:limit]:
            print(f"  - {key}")
        if len(baseline_only) > limit:
            print(f"  ... ({len(baseline_only) - limit} more)")
    if candidate_only:
        print(f"\nOnly in candidate: {len(candidate_only)}")
        for key in candidate_only[:limit]:
            print(f"  - {key}")
        if len(candidate_only) > limit:
            print(f"  ... ({len(candidate_only) - limit} more)")


def main() -> int:
    parser = argparse.ArgumentParser(description="Compare two benchmark result sets")
    parser.add_argument("--baseline", required=True, help="Baseline dir or combined_results.json")
    parser.add_argument("--candidate", required=True, help="Candidate dir or combined_results.json")
    parser.add_argument(
        "--key",
        choices=["name", "target"],
        default="name",
        help="Matching key between datasets",
    )
    parser.add_argument(
        "--top",
        type=int,
        default=20,
        help="How many largest deltas to print",
    )
    parser.add_argument(
        "--show-missing",
        action="store_true",
        help="Show keys missing from one side",
    )
    parser.add_argument(
        "--missing-limit",
        type=int,
        default=30,
        help="Max keys to print per missing list",
    )
    parser.add_argument(
        "--require-min-overlap",
        type=int,
        default=1,
        help="Fail if overlapping entries are below this count",
    )
    parser.add_argument(
        "--max-avg-abs-pct",
        type=float,
        default=None,
        help="Fail if average absolute min delta exceeds this threshold",
    )
    parser.add_argument(
        "--max-rank-changes",
        type=int,
        default=None,
        help="Fail if number of rank changes exceeds this threshold",
    )
    parser.add_argument(
        "--summary-json",
        default=None,
        help="Optional path to write machine-readable summary JSON",
    )
    args = parser.parse_args()

    baseline_records = load_result_set(Path(args.baseline))
    candidate_records = load_result_set(Path(args.candidate))

    baseline_idx = build_index(baseline_records, args.key)
    candidate_idx = build_index(candidate_records, args.key)

    baseline_keys = set(baseline_idx)
    candidate_keys = set(candidate_idx)
    common_keys = sorted(baseline_keys & candidate_keys)
    baseline_only = sorted(baseline_keys - candidate_keys)
    candidate_only = sorted(candidate_keys - baseline_keys)

    if len(common_keys) < args.require_min_overlap:
        print(
            f"Insufficient overlap: {len(common_keys)} entries "
            f"(required >= {args.require_min_overlap})"
        )
        if args.show_missing:
            print_missing(
                baseline_only=baseline_only,
                candidate_only=candidate_only,
                limit=args.missing_limit,
            )
        return 1

    baseline_ranks = rank_map([baseline_idx[k] for k in common_keys], args.key)
    candidate_ranks = rank_map([candidate_idx[k] for k in common_keys], args.key)

    rows = []
    for key in common_keys:
        b = baseline_idx[key]
        c = candidate_idx[key]
        if b["min_ms"] <= 0:
            continue
        min_delta_pct = ((c["min_ms"] - b["min_ms"]) / b["min_ms"]) * 100.0
        median_delta_pct = ((c["median_ms"] - b["median_ms"]) / b["median_ms"]) * 100.0
        rows.append(
            {
                "key": key,
                "baseline_min": b["min_ms"],
                "candidate_min": c["min_ms"],
                "min_delta_pct": min_delta_pct,
                "median_delta_pct": median_delta_pct,
                "rank_delta": baseline_ranks[key] - candidate_ranks[key],
                "baseline_ver": b["version"],
                "candidate_ver": c["version"],
            }
        )

    rows.sort(key=lambda r: abs(r["min_delta_pct"]), reverse=True)
    top = rows[: args.top]

    print(
        f"Compared {len(common_keys)} overlapping entries "
        f"(baseline={len(baseline_records)}, candidate={len(candidate_records)})"
    )
    coverage = (len(common_keys) / len(baseline_idx) * 100.0) if baseline_idx else 0.0
    print(f"Coverage vs baseline: {coverage:.2f}%")
    print(
        f"{'Key':24} {'Min Δ%':>10} {'Median Δ%':>10} {'Rank Δ':>8} "
        f"{'Baseline(ms)':>12} {'Candidate(ms)':>14}"
    )
    for row in top:
        print(
            f"{row['key'][:24]:24} "
            f"{fmt_pct(row['min_delta_pct']):>10} "
            f"{fmt_pct(row['median_delta_pct']):>10} "
            f"{row['rank_delta']:>8} "
            f"{row['baseline_min']:>12.2f} "
            f"{row['candidate_min']:>14.2f}"
        )

    avg_abs = sum(abs(r["min_delta_pct"]) for r in rows) / len(rows) if rows else 0.0
    moved = sum(1 for r in rows if r["rank_delta"] != 0)
    print(
        f"\nSummary: avg |min delta| = {avg_abs:.2f}% | rank changes = {moved}/{len(rows)}"
    )

    if args.show_missing:
        print_missing(
            baseline_only=baseline_only,
            candidate_only=candidate_only,
            limit=args.missing_limit,
        )

    violations: list[str] = []
    if args.max_avg_abs_pct is not None and avg_abs > args.max_avg_abs_pct:
        violations.append(
            f"avg |min delta| {avg_abs:.2f}% > threshold {args.max_avg_abs_pct:.2f}%"
        )
    if args.max_rank_changes is not None and moved > args.max_rank_changes:
        violations.append(
            f"rank changes {moved} > threshold {args.max_rank_changes}"
        )

    if args.summary_json:
        payload = {
            "overlap": len(common_keys),
            "baseline_records": len(baseline_records),
            "candidate_records": len(candidate_records),
            "coverage_pct": coverage,
            "avg_abs_min_delta_pct": avg_abs,
            "rank_changes": moved,
            "rows": rows,
            "baseline_only": baseline_only,
            "candidate_only": candidate_only,
            "violations": violations,
        }
        Path(args.summary_json).write_text(json.dumps(payload, indent=2))

    if violations:
        print("\nParity thresholds failed:")
        for violation in violations:
            print(f"  - {violation}")
        return 2

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
