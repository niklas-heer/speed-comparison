#!/usr/bin/env python3
"""Reject partial, quick, mixed-hardware, or invalid Nix benchmark publications."""

import argparse
import json
import math
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "dagger-poc"))
from languages import LANGUAGES
from result_metadata import enrich_result


def validate(results: Path) -> None:
    seen = set()
    hardware = set()
    for path in results.glob("*.json"):
        if path.name in {"combined_results.json", "run_metadata.json"}:
            continue
        data = json.loads(path.read_text())
        target = data.get("Target")
        if target not in LANGUAGES or target in seen:
            raise ValueError(f"Unexpected or duplicate target: {target}")
        seen.add(target)
        if data.get("Rounds") != 1_000_000_000:
            raise ValueError(f"{target}: only full-round results can be published")
        enrich_result(data, target, LANGUAGES[target], data["Rounds"])
        for key in ("Min", "Median", "Max"):
            number = float(str(data[key]).removesuffix("s"))
            if not math.isfinite(number) or number <= 0:
                raise ValueError(f"{target}: invalid {key}")
        env = data.get("Environment", {})
        if not env.get("cpu_model") or not env.get("arch"):
            raise ValueError(f"{target}: missing hardware metadata")
        hardware.add((env["cpu_model"], env["arch"], env.get("runner")))
    if seen != set(LANGUAGES):
        raise ValueError(
            "Incomplete suite; missing: " + ", ".join(sorted(set(LANGUAGES) - seen))
        )
    if len(hardware) != 1:
        raise ValueError("Results mix different hardware or runners")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results", type=Path)
    args = parser.parse_args()
    validate(args.results)
    print("Full benchmark publication validated")
