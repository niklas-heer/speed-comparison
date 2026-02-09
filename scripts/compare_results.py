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


def rank_key_order(records: list[dict[str, Any]], key: str) -> list[str]:
    """Return keys ordered by ascending min time."""
    return [str(row[key]) for row in sorted(records, key=lambda r: r["min_ms"])]


def pair_relation(a_ms: float, b_ms: float, tie_epsilon_pct: float) -> int:
    """Compare two timings with a tolerance band.

    Returns:
      -1 if a faster than b (materially)
       0 if within tie band
      +1 if a slower than b (materially)
    """
    if a_ms <= 0 or b_ms <= 0:
        return 0
    gap_pct = abs(a_ms - b_ms) / min(a_ms, b_ms) * 100.0
    if gap_pct <= tie_epsilon_pct:
        return 0
    return -1 if a_ms < b_ms else 1


def material_rank_map(
    keys: list[str],
    times: dict[str, float],
    tie_epsilon_pct: float,
) -> dict[str, int]:
    """Rank based only on materially faster competitors (tie-aware)."""
    ranks: dict[str, int] = {}
    for key in keys:
        faster = 0
        for other in keys:
            if other == key:
                continue
            # other materially faster than key
            if pair_relation(times[other], times[key], tie_epsilon_pct) == -1:
                faster += 1
        ranks[key] = faster + 1
    return ranks


def rank_parity_metrics(
    *,
    keys_in_baseline_order: list[str],
    baseline_times: dict[str, float],
    candidate_times: dict[str, float],
    candidate_ranks: dict[str, int],
    baseline_material_ranks: dict[str, int],
    candidate_material_ranks: dict[str, int],
    top_k: int,
    tie_epsilon_pct: float,
) -> dict[str, float | int]:
    """Compute rank-parity metrics from baseline ordering and candidate ranks."""
    n = len(keys_in_baseline_order)
    total_pairs = n * (n - 1) // 2
    decisive_pairs = 0
    tie_ignored_pairs = 0
    inversions = 0
    for i in range(n):
        a = keys_in_baseline_order[i]
        for j in range(i + 1, n):
            b = keys_in_baseline_order[j]
            baseline_rel = pair_relation(
                baseline_times[a], baseline_times[b], tie_epsilon_pct
            )
            candidate_rel = pair_relation(
                candidate_times[a], candidate_times[b], tie_epsilon_pct
            )
            if baseline_rel == 0 or candidate_rel == 0:
                tie_ignored_pairs += 1
                continue
            decisive_pairs += 1
            if baseline_rel != candidate_rel:
                inversions += 1

    non_inversions = decisive_pairs - inversions
    inversion_rate_pct = (inversions / decisive_pairs * 100.0) if decisive_pairs else 0.0
    kendall_tau = (
        (non_inversions - inversions) / decisive_pairs if decisive_pairs else 1.0
    )

    k = max(1, min(top_k, n)) if n else 0
    top_keys = keys_in_baseline_order[:k]
    top_disp: list[int] = []
    for idx, key in enumerate(top_keys, 1):
        top_disp.append(abs(idx - candidate_ranks[key]))

    top_mean_displacement = (
        sum(top_disp) / len(top_disp) if top_disp else 0.0
    )
    top_max_displacement = max(top_disp) if top_disp else 0

    candidate_top_set = {name for name, rank in candidate_ranks.items() if rank <= k}
    baseline_top_set = set(top_keys)
    top_overlap = len(baseline_top_set & candidate_top_set)
    top_overlap_pct = (top_overlap / k * 100.0) if k else 0.0

    material_rank_changes = 0
    for key in keys_in_baseline_order:
        if baseline_material_ranks.get(key) != candidate_material_ranks.get(key):
            material_rank_changes += 1

    return {
        "n": n,
        "pair_count": total_pairs,
        "decisive_pair_count": decisive_pairs,
        "tie_ignored_pair_count": tie_ignored_pairs,
        "inversions": inversions,
        "inversion_rate_pct": inversion_rate_pct,
        "kendall_tau": kendall_tau,
        "top_k": k,
        "top_k_mean_displacement": top_mean_displacement,
        "top_k_max_displacement": top_max_displacement,
        "top_k_overlap": top_overlap,
        "top_k_overlap_pct": top_overlap_pct,
        "material_rank_changes": material_rank_changes,
    }


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
        "--tie-epsilon-pct",
        type=float,
        default=5.0,
        help="Treat pairwise differences <= this percent as same performance cluster",
    )
    parser.add_argument(
        "--top-k",
        type=int,
        default=10,
        help="Top-K slice used for rank displacement/overlap metrics",
    )
    parser.add_argument(
        "--max-inversion-rate-pct",
        type=float,
        default=None,
        help="Fail if pairwise inversion rate exceeds this threshold",
    )
    parser.add_argument(
        "--min-kendall-tau",
        type=float,
        default=None,
        help="Fail if Kendall tau drops below this threshold",
    )
    parser.add_argument(
        "--max-topk-mean-displacement",
        type=float,
        default=None,
        help="Fail if top-K mean rank displacement exceeds this threshold",
    )
    parser.add_argument(
        "--min-topk-overlap-pct",
        type=float,
        default=None,
        help="Fail if top-K overlap percentage is below this threshold",
    )
    parser.add_argument(
        "--max-tolerant-rank-changes",
        type=int,
        default=None,
        help="Fail if cluster-aware rank changes exceed this threshold",
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
    baseline_order = rank_key_order([baseline_idx[k] for k in common_keys], args.key)
    baseline_times = {k: baseline_idx[k]["min_ms"] for k in common_keys}
    candidate_times = {k: candidate_idx[k]["min_ms"] for k in common_keys}
    baseline_material_ranks = material_rank_map(
        baseline_order,
        baseline_times,
        args.tie_epsilon_pct,
    )
    candidate_material_ranks = material_rank_map(
        baseline_order,
        candidate_times,
        args.tie_epsilon_pct,
    )

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
    rank_metrics = rank_parity_metrics(
        keys_in_baseline_order=baseline_order,
        baseline_times=baseline_times,
        candidate_times=candidate_times,
        candidate_ranks=candidate_ranks,
        baseline_material_ranks=baseline_material_ranks,
        candidate_material_ranks=candidate_material_ranks,
        top_k=args.top_k,
        tie_epsilon_pct=args.tie_epsilon_pct,
    )
    print(
        f"\nSummary: avg |min delta| = {avg_abs:.2f}% | rank changes = {moved}/{len(rows)}"
    )
    print(
        f"Cluster-aware (<= {args.tie_epsilon_pct:.2f}% treated as tie): "
        f"rank changes = {rank_metrics['material_rank_changes']}/{len(rows)}"
    )
    print(
        "Rank parity: "
        f"inversions={rank_metrics['inversions']}/"
        f"{rank_metrics['decisive_pair_count']} "
        f"({rank_metrics['inversion_rate_pct']:.2f}%) | "
        f"tie-ignored pairs={rank_metrics['tie_ignored_pair_count']} | "
        f"kendall_tau={rank_metrics['kendall_tau']:.4f} | "
        f"top{rank_metrics['top_k']} mean displacement="
        f"{rank_metrics['top_k_mean_displacement']:.2f} (max "
        f"{rank_metrics['top_k_max_displacement']}) | "
        f"top{rank_metrics['top_k']} overlap={rank_metrics['top_k_overlap']}/"
        f"{rank_metrics['top_k']} ({rank_metrics['top_k_overlap_pct']:.1f}%)"
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
    if (
        args.max_tolerant_rank_changes is not None
        and rank_metrics["material_rank_changes"] > args.max_tolerant_rank_changes
    ):
        violations.append(
            "cluster-aware rank changes "
            f"{rank_metrics['material_rank_changes']} > threshold "
            f"{args.max_tolerant_rank_changes}"
        )
    if (
        args.max_inversion_rate_pct is not None
        and rank_metrics["inversion_rate_pct"] > args.max_inversion_rate_pct
    ):
        violations.append(
            "inversion rate "
            f"{rank_metrics['inversion_rate_pct']:.2f}% > threshold "
            f"{args.max_inversion_rate_pct:.2f}%"
        )
    if (
        args.min_kendall_tau is not None
        and rank_metrics["kendall_tau"] < args.min_kendall_tau
    ):
        violations.append(
            f"kendall_tau {rank_metrics['kendall_tau']:.4f} < threshold "
            f"{args.min_kendall_tau:.4f}"
        )
    if (
        args.max_topk_mean_displacement is not None
        and rank_metrics["top_k_mean_displacement"] > args.max_topk_mean_displacement
    ):
        violations.append(
            "top-K mean displacement "
            f"{rank_metrics['top_k_mean_displacement']:.2f} > threshold "
            f"{args.max_topk_mean_displacement:.2f}"
        )
    if (
        args.min_topk_overlap_pct is not None
        and rank_metrics["top_k_overlap_pct"] < args.min_topk_overlap_pct
    ):
        violations.append(
            "top-K overlap "
            f"{rank_metrics['top_k_overlap_pct']:.1f}% < threshold "
            f"{args.min_topk_overlap_pct:.1f}%"
        )

    if args.summary_json:
        payload = {
            "overlap": len(common_keys),
            "baseline_records": len(baseline_records),
            "candidate_records": len(candidate_records),
            "coverage_pct": coverage,
            "avg_abs_min_delta_pct": avg_abs,
            "rank_changes": moved,
            "rank_parity": rank_metrics,
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
