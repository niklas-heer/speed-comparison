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
    for file in sorted(path.rglob("*.json")):
        # Skip generated aggregate files if present in directory mode
        if file.name in {"combined_results.json", "run_metadata.json"}:
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
    args = parser.parse_args()

    baseline_records = load_result_set(Path(args.baseline))
    candidate_records = load_result_set(Path(args.candidate))

    baseline_idx = build_index(baseline_records, args.key)
    candidate_idx = build_index(candidate_records, args.key)

    common_keys = sorted(set(baseline_idx) & set(candidate_idx))
    if not common_keys:
        print("No overlapping entries found.")
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

    avg_abs = sum(abs(r["min_delta_pct"]) for r in rows) / len(rows)
    moved = sum(1 for r in rows if r["rank_delta"] != 0)
    print(
        f"\nSummary: avg |min delta| = {avg_abs:.2f}% | rank changes = {moved}/{len(rows)}"
    )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
