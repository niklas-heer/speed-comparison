#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Analyzes benchmark JSON results and generates visualizations."""

import json
from argparse import ArgumentParser
from datetime import datetime
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.cm import ScalarMappable
from matplotlib.colors import Normalize


def load_results(folder: str) -> pd.DataFrame:
    """Load all JSON result files from a folder into a DataFrame."""
    data = {
        "name": [],
        "version": [],
        "median": [],
        "min": [],
        "max": [],
        "accuracy": [],
    }

    folder_path = Path(folder)
    for file_path in folder_path.glob("*.json"):
        with open(file_path, "r") as f:
            result = json.load(f)
            data["name"].append(result["Language"])
            data["version"].append(result["Version"])
            # Convert to milliseconds
            data["median"].append(
                round(pd.Timedelta(result["Median"]).total_seconds() * 1000, 2)
            )
            data["max"].append(
                round(pd.Timedelta(result["Max"]).total_seconds() * 1000, 2)
            )
            data["min"].append(
                round(pd.Timedelta(result["Min"]).total_seconds() * 1000, 2)
            )
            data["accuracy"].append(round(result["Accuracy"], 4))

    df = pd.DataFrame(data)
    df.sort_values(by=["min"], inplace=True, ascending=True)
    return df


def create_color_mapping(values: np.ndarray, cmap_name: str = "YlGn"):
    """Create a color mapping based on values using a colormap."""
    norm = Normalize(vmin=values.min(), vmax=values.max())
    cmap = plt.get_cmap(cmap_name)
    return [cmap(norm(v)) for v in values], norm, cmap


def format_time(ms: float) -> str:
    """Format milliseconds into a readable string."""
    if ms >= 1000:
        return f"{ms / 1000:.2f}s"
    return f"{ms:.1f}ms"


def plot_results(df: pd.DataFrame, rounds: str, output_path: str):
    """Generate the benchmark comparison chart."""
    # Calculate dynamic figure size based on number of languages
    num_languages = len(df)
    bar_height = 0.55  # Height per bar in inches
    fig_height = max(8, num_languages * bar_height + 2)  # +2 for compact title/footer
    fig_width = 16

    # Setup the figure
    fig, ax = plt.subplots(figsize=(fig_width, fig_height))

    # Dark theme - GitHub dark colors for better contrast
    plt.style.use("dark_background")
    bg_color = "#0d1117"
    fig.patch.set_facecolor(bg_color)
    ax.set_facecolor(bg_color)

    # Create display name with version
    df = df.copy()
    df["display_name"] = df["name"] + "  v" + df["version"].astype(str)

    # Create color mapping based on accuracy (higher = greener)
    colors, norm, cmap = create_color_mapping(df["accuracy"].values, "YlGn")

    # Create horizontal bar chart
    y_pos = np.arange(len(df))
    bars = ax.barh(
        y_pos,
        df["min"],
        color=colors,
        edgecolor="#30363d",
        linewidth=1,
        height=0.7,
    )

    # Use log scale for x-axis
    ax.set_xscale("log")

    # Set y-axis labels with better font size and color
    ax.set_yticks(y_pos)
    ax.set_yticklabels(
        df["display_name"],
        fontsize=11,
        fontfamily="monospace",
        color="#e6edf3",
    )

    # Add value labels outside bars for better readability
    for bar, val in zip(bars, df["min"]):
        ax.text(
            bar.get_width() * 1.08,
            bar.get_y() + bar.get_height() / 2,
            format_time(val),
            va="center",
            ha="left",
            fontsize=10,
            color="#e6edf3",
            fontfamily="monospace",
        )

    # X-axis label
    ax.set_xlabel(
        "Minimum execution time (log scale)",
        fontsize=11,
        color="#8b949e",
        labelpad=8,
    )
    ax.set_ylabel("")

    # Compact title - single line with subtitle
    ax.set_title(
        f"Speed Comparison of Programming Languages  —  Leibniz π formula, {int(rounds):,} iterations",
        fontsize=14,
        fontweight="bold",
        color="#e6edf3",
        pad=12,
        loc="left",
    )

    # Add colorbar for accuracy legend
    sm = ScalarMappable(norm=norm, cmap=cmap)
    sm.set_array([])
    cbar = fig.colorbar(sm, ax=ax, pad=0.01, aspect=40, shrink=0.8)
    cbar.set_label(
        "π Accuracy (correct decimal places)",
        fontsize=11,
        color="#e6edf3",
        labelpad=10,
    )
    cbar.ax.yaxis.set_tick_params(color="#e6edf3", labelsize=10)
    cbar.outline.set_edgecolor("#30363d")
    plt.setp(plt.getp(cbar.ax.axes, "yticklabels"), color="#e6edf3")

    # Grid for readability (only vertical lines)
    ax.xaxis.grid(True, linestyle="-", alpha=0.15, color="#8b949e")
    ax.yaxis.grid(False)
    ax.set_axisbelow(True)

    # Style spines
    for spine in ax.spines.values():
        spine.set_color("#30363d")
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    # Tick styling
    ax.tick_params(axis="x", colors="#8b949e", labelsize=10)
    ax.tick_params(axis="y", colors="#e6edf3", length=0)

    # Footer with URL and timestamp
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M")
    fig.text(
        0.98,
        0.01,
        "github.com/niklas-heer/speed-comparison",
        ha="right",
        fontsize=10,
        color="#8b949e",
        transform=fig.transFigure,
    )
    fig.text(
        0.02,
        0.01,
        f"Generated: {timestamp}",
        ha="left",
        fontsize=10,
        color="#8b949e",
        transform=fig.transFigure,
    )

    # Invert y-axis so fastest is at top
    ax.invert_yaxis()

    # Extend x-axis to make room for time labels
    x_max = df["min"].max()
    ax.set_xlim(right=x_max * 2.5)

    # Save with proper layout - minimal margins for compact look
    plt.tight_layout(rect=[0, 0.02, 1, 0.98])
    plt.savefig(
        output_path,
        dpi=150,
        bbox_inches="tight",
        facecolor=fig.get_facecolor(),
        edgecolor="none",
        pad_inches=0.3,
    )
    plt.close()


def main():
    parser = ArgumentParser(
        description="Analyze benchmark results and generate visualizations"
    )
    parser.add_argument(
        "--folder",
        required=True,
        help="Path to folder containing JSON result files",
    )
    parser.add_argument(
        "--out",
        required=True,
        help="Output directory for generated files",
    )
    parser.add_argument(
        "--rounds",
        required=True,
        help="Path to rounds.txt file",
    )
    args = parser.parse_args()

    # Load data
    df = load_results(args.folder)

    if df.empty:
        print("No JSON result files found!")
        return 1

    # Save CSV
    csv_path = Path(args.out) / "combined_results.csv"
    df.to_csv(csv_path, index=False, encoding="utf-8")

    # Read rounds
    rounds = Path(args.rounds).read_text().strip()

    # Generate visualization
    png_path = Path(args.out) / "combined_results.png"
    plot_results(df, rounds, str(png_path))

    print(f"Generated {len(df)} language results:")
    print(f"  CSV: {csv_path}")
    print(f"  PNG: {png_path}")

    return 0


if __name__ == "__main__":
    exit(main())
