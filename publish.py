#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Publishes benchmark results to the docs folder for GitHub Pages."""

import json
import shutil
from argparse import ArgumentParser
from datetime import datetime
from pathlib import Path
import re


def update_readme(readme: Path, run_id: str, count: int, metadata: dict) -> None:
    """Keep the README's whole-run clock in sync with a new published snapshot."""
    if not readme.exists():
        return
    clock = metadata.get("execution", {})
    elapsed = clock.get("elapsed_seconds")
    if elapsed is None:
        duration = "not recorded"
    else:
        seconds = round(elapsed)
        duration = f"{seconds // 3600}h {(seconds % 3600) // 60}m {seconds % 60}s"
    text = (
        "<!-- latest-run:start -->\n"
        f"**Latest full run: {duration} · {count} implementations.**\n\n"
        f"[Inspect the run](https://speed-comparison.vercel.app/runs/{run_id}/) · "
        "[Download the image](https://speed-comparison.vercel.app/report-images/latest.png)\n\n"
        + clock.get(
            "elapsed_scope",
            "Whole-run time was not recorded; individual sample times are not a workflow clock.",
        )
        + "\n<!-- latest-run:end -->"
    )
    readme.write_text(
        re.sub(
            r"<!-- latest-run:start -->.*?<!-- latest-run:end -->",
            lambda _: text,
            readme.read_text(),
            flags=re.S,
        )
    )


def update_manifest(history_dir: Path, new_run_id: str, lang_count: int) -> None:
    """Update manifest.json with the new run."""
    manifest_path = history_dir / "manifest.json"

    if manifest_path.exists():
        with open(manifest_path) as f:
            manifest = json.load(f)
    else:
        manifest = {"runs": []}

    # Parse date from run_id (format: 2023-02-05T185235)
    date_str = new_run_id.replace("T", " ")
    date_str = f"{date_str[:10]} {date_str[11:13]}:{date_str[13:15]}:{date_str[15:17]}"

    new_run = {
        "id": new_run_id,
        "date": date_str,
        "languages": lang_count,
    }

    # Add new run at the beginning (most recent first)
    manifest["runs"].insert(0, new_run)
    manifest["generated"] = datetime.now().strftime("%Y-%m-%d")

    with open(manifest_path, "w") as f:
        json.dump(manifest, f, indent=2)


def count_languages(csv_path: Path) -> int:
    """Count the number of languages in a results CSV."""
    if not csv_path.exists():
        return 0
    with open(csv_path) as f:
        return len(f.readlines()) - 1  # minus header


def main():
    parser = ArgumentParser(description="Publish benchmark results to docs folder")
    parser.add_argument(
        "--results",
        required=True,
        help="Path to results folder containing CSV and PNG",
    )
    parser.add_argument(
        "--docs",
        default="./docs",
        help="Path to docs folder (default: ./docs)",
    )
    args = parser.parse_args()

    results_dir = Path(args.results)
    docs_dir = Path(args.docs)
    history_dir = docs_dir / "history"

    # Validate input
    csv_file = results_dir / "combined_results.csv"
    png_file = results_dir / "combined_results.png"
    json_file = results_dir / "combined_results.json"
    meta_file = results_dir / "run_metadata.json"

    if not csv_file.exists():
        print(f"Error: {csv_file} not found")
        return 1
    if not png_file.exists():
        print(f"Error: {png_file} not found")
        return 1

    # Generate run ID from current timestamp
    run_id = datetime.now().strftime("%Y-%m-%dT%H%M%S")
    run_dir = history_dir / run_id

    # Create run directory and copy files
    run_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy(csv_file, run_dir / "combined_results.csv")
    shutil.copy(png_file, run_dir / "combined_results.png")
    if json_file.exists():
        shutil.copy(json_file, run_dir / "combined_results.json")
    if meta_file.exists():
        shutil.copy(meta_file, run_dir / "run_metadata.json")

    # Also copy to 'latest' folder for stable URL reference
    latest_dir = history_dir / "latest"
    latest_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy(csv_file, latest_dir / "combined_results.csv")
    shutil.copy(png_file, latest_dir / "combined_results.png")
    if json_file.exists():
        shutil.copy(json_file, latest_dir / "combined_results.json")
    if meta_file.exists():
        shutil.copy(meta_file, latest_dir / "run_metadata.json")
    run_record = results_dir / "run.json"
    if not run_record.exists() and results_dir.name == "targets":
        run_record = results_dir.parent / "run.json"
    if run_record.exists():
        for destination in (run_dir, latest_dir):
            shutil.copy(run_record, destination / "run.json")
    else:
        (latest_dir / "run.json").unlink(missing_ok=True)

    # Keep the original per-target evidence independently of presentation/schema changes.
    raw_results = []
    for path in results_dir.glob("*.json"):
        if path.name in {"combined_results.json", "run_metadata.json", "run.json"}:
            continue
        data = json.loads(path.read_text())
        if isinstance(data, dict) and data.get("Target"):
            raw_results.append(path)
    # Latest can point to a smaller or historical report: never retain stale raw files.
    latest_raw = latest_dir / "raw"
    if latest_raw.exists():
        shutil.rmtree(latest_raw)
    if raw_results:
        for destination in (run_dir / "raw", latest_raw):
            destination.mkdir(parents=True, exist_ok=True)
            for path in raw_results:
                shutil.copy(path, destination / path.name)
            for name in ("source-revision.txt", "rounds.txt"):
                if (results_dir / name).exists():
                    shutil.copy(results_dir / name, destination / name)

    source_revision = results_dir / "source-revision.txt"
    if source_revision.exists():
        shutil.copy(source_revision, run_dir / "source-revision.txt")
        shutil.copy(source_revision, latest_dir / "source-revision.txt")

    # Update manifest
    lang_count = count_languages(csv_file)
    update_manifest(history_dir, run_id, lang_count)
    update_readme(
        docs_dir.parent / "README.md",
        run_id,
        lang_count,
        json.loads(meta_file.read_text()) if meta_file.exists() else {},
    )

    print(f"Published benchmark results:")
    print(f"  Run ID: {run_id}")
    print(f"  Languages: {lang_count}")
    print(f"  Location: {run_dir}")
    print(f"  Latest: {latest_dir}")
    print(f"  URL: https://niklas-heer.github.io/speed-comparison/")

    return 0


if __name__ == "__main__":
    exit(main())
