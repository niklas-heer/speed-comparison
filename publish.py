#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Publishes benchmark results to the docs folder for GitHub Pages."""

import json
import shutil
from argparse import ArgumentParser
from datetime import datetime
from pathlib import Path


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

    # Also copy to 'latest' folder for stable URL reference
    latest_dir = history_dir / "latest"
    latest_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy(csv_file, latest_dir / "combined_results.csv")
    shutil.copy(png_file, latest_dir / "combined_results.png")

    # Update manifest
    lang_count = count_languages(csv_file)
    update_manifest(history_dir, run_id, lang_count)

    print(f"Published benchmark results:")
    print(f"  Run ID: {run_id}")
    print(f"  Languages: {lang_count}")
    print(f"  Location: {run_dir}")
    print(f"  Latest: {latest_dir}")
    print(f"  URL: https://niklas-heer.github.io/speed-comparison/")

    return 0


if __name__ == "__main__":
    exit(main())
