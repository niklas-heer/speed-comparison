#!/usr/bin/env python3
"""Refresh the presentation image from published evidence, without rewriting history."""

import argparse
import json
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
import pandas as pd
from analyze import (
    build_combined_results,
    load_raw_results,
    parse_time_value,
    plot_results,
)


def render(run_id):
    if not re.fullmatch(r"\d{4}-\d{2}-\d{2}T\d{6}", run_id):
        raise ValueError("Invalid run ID")
    folder = ROOT / "docs/history" / run_id
    results = load_raw_results(str(folder / "raw"))
    if not results:
        raise ValueError("A refreshed chart needs recorded raw samples")
    frame = pd.DataFrame(build_combined_results(results))
    for field, key in (("median", "Median"), ("min", "Min"), ("max", "Max")):
        frame[field] = frame.target.map(
            {r["Target"]: parse_time_value(r[key]) * 1000 for r in results}
        )
    metadata = json.loads((folder / "run_metadata.json").read_text())
    supplement = ROOT / "docs/report-evidence" / run_id / "report.json"
    if supplement.exists():
        recovered = json.loads(supplement.read_text())
        if recovered["source_revision"] != metadata["source_revision"]:
            raise ValueError("Supplement source mismatch")
        metadata["execution"] = recovered["execution"]
    output = ROOT / "docs/report-images" / f"{run_id}.png"
    output.parent.mkdir(parents=True, exist_ok=True)
    plot_results(frame, str(results[0]["Rounds"]), str(output), metadata)
    print(output)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("run_id")
    render(parser.parse_args().run_id)
