"""Validate suite provenance before including whole-run timing in a report."""

import math


def execution_metadata(record, raw_results):
    """Bind the suite clock to these results, never sum per-target samples."""
    if record.get("status") != "succeeded":
        raise ValueError("Report timing requires a successful suite")
    targets = {r["Target"] for r in raw_results}
    if (
        not targets
        or targets != set(record.get("completed_targets", []))
        or targets != set(record.get("targets", []))
    ):
        raise ValueError("Run record does not match report targets")
    for result in raw_results:
        if not record.get("run_id") or result.get("SuiteRunID") != record["run_id"]:
            raise ValueError("Run record does not match result suite identity")
        if result.get("SourceRevision") != record.get("source_revision"):
            raise ValueError("Run record source revision mismatch")
    elapsed = record.get("elapsed_seconds")
    if (
        isinstance(elapsed, bool)
        or not isinstance(elapsed, (int, float))
        or not math.isfinite(elapsed)
        or elapsed < 0
    ):
        raise ValueError("Invalid suite elapsed time")
    return {
        key: record.get(key)
        for key in (
            "run_id",
            "status",
            "started_at",
            "finished_at",
            "elapsed_seconds",
            "elapsed_scope",
            "preparation_wall_seconds",
            "total_wall_seconds",
            "target_phases",
        )
    }
