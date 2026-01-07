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
from matplotlib.colors import LinearSegmentedColormap, Normalize
from matplotlib.offsetbox import AnnotationBbox, OffsetImage
from PIL import Image, ImageFilter, ImageOps

# Map benchmark language names to icon file names
ICON_MAP = {
    "C (gcc)": "c",
    "C (clang)": "c",
    "C++": "cplusplus",
    "C++ (g++)": "cplusplus",
    "C++ (clang++)": "cplusplus",
    "C++ (avx2)": "cplusplus",
    "C++ (AVX2)": "cplusplus",
    "C#": "csharp",
    "C# (SIMD)": "csharp",
    "Objective-C": "objectivec",
    "Rust": "rust",
    "Rust (nightly)": "rust",
    "Go": "go",
    "Zig": "zig",
    "Nim": "nim",
    "Nature": "nature",
    "D (GDC)": "d",
    "D (LDC)": "d",
    "Odin": "odin",
    "V": "v",
    "Java": "java",
    "Java graalvm": "java",
    "Java (GraalVM)": "java",
    "Java (Vec Ops)": "java",
    "Kotlin": "kotlin",
    "Scala": "scala",
    "Clojure": "clojure",
    "Groovy": "groovy",
    "F#": "fsharp",
    "Python (CPython)": "python",
    "Python (PyPy)": "python",
    "Python (NumPy)": "python",
    "Python (MyPyC)": "python",
    "Python (mypyc)": "python",
    "MicroPython": "python",
    "Ruby": "ruby",
    "Perl": "perl",
    "Raku": "raku",
    "PHP": "php",
    "Lua": "lua",
    "LuaJIT": "lua",
    "Javascript (nodejs)": "nodejs",
    "Javascript (bun)": "bun",
    "Deno (TypeScript)": "denojs",
    "Haskell (GHC)": "haskell",
    "OCaml": "ocaml",
    "Elixir": "elixir",
    "Erlang": "erlang",
    "Gleam": "gleam",
    "Racket": "racket",
    "Common Lisp (SBCL)": "lisp",
    "Swift": "swift",
    "Swift (relaxed)": "swift",
    "Swift (SIMD)": "swift",
    "Crystal": "crystal",
    "Julia": "julia",
    "Julia (AOT compiled)": "julia",
    "Fortran 90": "fortran",
    "Ada (gnat-gcc)": "ada",
    "Pascal (FPC)": "pascal",
    "Pony": "pony",
    "Pony(nightly)": "pony",
    "R": "r",
    "Dart (JIT)": "dart",
    "Dart": "dart",
    "Dart (AOT)": "dart",
    "Haxe (C++)": "haxe",
    "Haxe": "haxe",
    "Janet": "janet",
    "Janet (compiled)": "janet",
    "WASM (C via Wasmtime)": "wasm",
}


def add_drop_shadow(
    image: Image.Image,
    offset: tuple = (2, 2),
    shadow_color: tuple = (0, 0, 0, 180),
    blur_radius: int = 3,
) -> Image.Image:
    """Add a drop shadow to an image."""
    # Ensure image has alpha channel
    if image.mode != "RGBA":
        image = image.convert("RGBA")

    # Create a larger canvas to accommodate the shadow
    padding = blur_radius * 2 + max(abs(offset[0]), abs(offset[1]))
    new_size = (image.width + padding * 2, image.height + padding * 2)

    # Create shadow layer
    shadow = Image.new("RGBA", new_size, (0, 0, 0, 0))

    # Create shadow from alpha channel
    alpha = image.split()[3]
    shadow_img = Image.new("RGBA", image.size, shadow_color)
    shadow_img.putalpha(alpha)

    # Paste shadow with offset
    shadow.paste(shadow_img, (padding + offset[0], padding + offset[1]))

    # Blur the shadow
    shadow = shadow.filter(ImageFilter.GaussianBlur(blur_radius))

    # Paste original image on top
    shadow.paste(image, (padding, padding), image)

    return shadow


def load_icon(icon_name: str) -> Image.Image | None:
    """Load a PNG icon from the icons directory with drop shadow."""
    icon_path = Path(__file__).parent / "icons" / f"{icon_name}.png"
    if not icon_path.exists():
        return None

    try:
        img = Image.open(icon_path)
        # Add black drop shadow for visibility on bars
        img = add_drop_shadow(
            img, offset=(1, 1), shadow_color=(0, 0, 0, 200), blur_radius=2
        )
        return img
    except Exception:
        return None


def parse_time_value(value: str) -> float:
    """Parse time value like '1.74705559758s' to seconds."""
    if isinstance(value, (int, float)):
        return float(value)
    value = str(value).strip()
    if value.endswith("s"):
        return float(value[:-1])
    return float(value)


def load_raw_results(folder: str) -> list[dict]:
    """Load raw JSON results (per-language) from a folder."""
    folder_path = Path(folder)
    results = []
    for file_path in folder_path.glob("*.json"):
        if file_path.name in ("combined_results.json", "run_metadata.json"):
            continue
        with open(file_path, "r") as f:
            results.append(json.load(f))
    return results


def load_results(folder: str) -> pd.DataFrame:
    """Load all JSON result files from a folder into a DataFrame."""
    folder_path = Path(folder)

    # Check if there's a CSV file (pre-processed results)
    csv_files = list(folder_path.glob("*.csv"))
    if csv_files:
        df = pd.read_csv(csv_files[0])
        df.sort_values(by=["min"], inplace=True, ascending=True)
        return df

    # Otherwise load from JSON files
    data = {
        "name": [],
        "target": [],
        "version": [],
        "median": [],
        "min": [],
        "max": [],
        "accuracy": [],
    }

    for file_path in folder_path.glob("*.json"):
        with open(file_path, "r") as f:
            result = json.load(f)
            data["name"].append(result["Language"])
            # Use Target from JSON if available, otherwise infer from filename
            target = result.get("Target") or file_path.stem
            data["target"].append(target)
            data["version"].append(result["Version"])
            # Convert to milliseconds
            data["median"].append(round(parse_time_value(result["Median"]) * 1000, 2))
            data["max"].append(round(parse_time_value(result["Max"]) * 1000, 2))
            data["min"].append(round(parse_time_value(result["Min"]) * 1000, 2))
            data["accuracy"].append(round(result["Accuracy"], 4))

    df = pd.DataFrame(data)
    df.sort_values(by=["min"], inplace=True, ascending=True)
    return df


def create_neon_cmap():
    """Create a neon colormap (Tokyo Night style) - pink to blue (inverted for variety)."""
    colors = [
        "#f7768e",  # Tokyo Night red/pink (low accuracy)
        "#ff007c",  # Hot pink/magenta
        "#bb9af7",  # Tokyo Night purple
        "#7aa2f7",  # Tokyo Night blue
        "#7dcfff",  # Tokyo Night cyan (high accuracy)
    ]
    return LinearSegmentedColormap.from_list("neon", colors, N=256)


def create_color_mapping(values: np.ndarray):
    """Create a color mapping based on values using neon colormap."""
    norm = Normalize(vmin=values.min(), vmax=values.max())
    cmap = create_neon_cmap()
    return [cmap(norm(v)) for v in values], norm, cmap


def format_time(ms: float) -> str:
    """Format milliseconds into a readable string."""
    if ms >= 1000:
        return f"{ms / 1000:.2f}s"
    return f"{ms:.1f}ms"


def trim_text(text: str, max_len: int = 60) -> str:
    """Trim text to a max length using ASCII ellipsis."""
    if len(text) <= max_len:
        return text
    return text[: max_len - 3] + "..."


def plot_results(
    df: pd.DataFrame, rounds: str, output_path: str, env_summary: dict | None = None
):
    """Generate the benchmark comparison chart."""
    # Calculate dynamic figure size based on number of languages
    num_languages = len(df)
    bar_height = 0.30  # Height per bar in inches (compact)
    fig_height = max(6, num_languages * bar_height + 1.2)  # Space for header
    fig_width = 14

    # Setup the figure
    fig, ax = plt.subplots(figsize=(fig_width, fig_height))

    # Dark theme - Tokyo Night inspired colors
    plt.style.use("dark_background")
    bg_color = "#1a1b26"
    fig.patch.set_facecolor(bg_color)
    ax.set_facecolor(bg_color)

    # Create display name with version
    df = df.copy()
    df["display_name"] = df["name"] + "  v" + df["version"].astype(str)

    # Create color mapping based on accuracy (higher = more purple)
    colors, norm, cmap = create_color_mapping(df["accuracy"].values)

    # Create horizontal bar chart
    y_pos = np.arange(len(df))
    bars = ax.barh(
        y_pos,
        df["min"],
        color=colors,
        edgecolor="#1e1e2e",
        linewidth=0.5,
        height=0.75,
    )

    # Use log scale for x-axis
    ax.set_xscale("log")

    # Add language icons at the start of each bar
    icon_cache = {}  # Cache loaded icons
    for idx, (_, row) in enumerate(df.iterrows()):
        lang_name = row["name"]
        icon_name = ICON_MAP.get(lang_name, "default")  # Use default icon if not found

        if icon_name not in icon_cache:
            icon_cache[icon_name] = load_icon(icon_name)

        icon_img = icon_cache.get(icon_name)
        if icon_img:
            # Position icon at the left edge of the bar
            imagebox = OffsetImage(icon_img, zoom=0.4)
            imagebox.image.axes = ax

            # Place icon relative to y-axis (axes fraction 0 = left edge of plot)
            # This stays consistent regardless of data values or label lengths
            ab = AnnotationBbox(
                imagebox,
                (0, y_pos[idx]),
                xybox=(10, 0),  # Offset in points from left edge of plot area
                xycoords=("axes fraction", "data"),
                boxcoords="offset points",
                frameon=False,
                pad=0,
            )
            ax.add_artist(ab)

    # Set y-axis labels - bold and bright white for readability
    ax.set_yticks(y_pos)
    ax.set_yticklabels(
        df["display_name"],
        fontsize=9,
        fontfamily="monospace",
        fontweight="bold",
        color="#ffffff",
    )

    # Add value labels outside bars - bold and bright white
    for bar, val in zip(bars, df["min"]):
        ax.text(
            bar.get_width() * 1.08,
            bar.get_y() + bar.get_height() / 2,
            format_time(val),
            va="center",
            ha="left",
            fontsize=8,
            fontweight="bold",
            color="#ffffff",
            fontfamily="monospace",
        )

    # X-axis label
    ax.set_xlabel(
        "Minimum execution time (log scale)",
        fontsize=9,
        color="#8b949e",
        labelpad=5,
    )
    ax.set_ylabel("")

    # Centered title
    ax.set_title(
        f"Speed Comparison  —  Leibniz π, {int(rounds):,} iterations",
        fontsize=13,
        fontweight="bold",
        color="#ffffff",
        pad=8,
        loc="center",
    )

    # Language count at top right
    fig.text(
        0.99,
        0.99,
        f"{num_languages} Languages",
        ha="right",
        va="top",
        fontsize=10,
        fontweight="bold",
        color="#7aa2f7",
        fontfamily="monospace",
        transform=fig.transFigure,
    )

    # Add colorbar for accuracy legend
    sm = ScalarMappable(norm=norm, cmap=cmap)
    sm.set_array([])
    cbar = fig.colorbar(sm, ax=ax, pad=0.01, aspect=30, shrink=0.6)
    cbar.set_label(
        "π Accuracy",
        fontsize=9,
        color="#e6edf3",
        labelpad=8,
    )
    cbar.ax.yaxis.set_tick_params(color="#e6edf3", labelsize=8)
    cbar.outline.set_edgecolor("#1e1e2e")
    plt.setp(plt.getp(cbar.ax.axes, "yticklabels"), color="#e6edf3")

    # Grid for readability (only vertical lines)
    ax.xaxis.grid(True, linestyle="-", alpha=0.12, color="#4a5568")
    ax.yaxis.grid(False)
    ax.set_axisbelow(True)

    # Style spines
    for spine in ax.spines.values():
        spine.set_color("#1e1e2e")
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    # Tick styling
    ax.tick_params(axis="x", colors="#8b949e", labelsize=8)
    ax.tick_params(axis="y", colors="#e6edf3", length=0, pad=2)

    # Invert y-axis so fastest is at top
    ax.invert_yaxis()

    # Extend x-axis to make room for time labels
    x_max = df["min"].max()
    ax.set_xlim(right=x_max * 2.2)

    # Optional environment line
    if env_summary and env_summary.get("environment"):
        env = env_summary["environment"]
        parts = []
        cpu_model = env.get("cpu_model", "")
        if cpu_model:
            parts.append(trim_text(cpu_model, 48))
        cores = env.get("cpu_cores", "")
        threads = env.get("cpu_threads", "")
        if cores:
            parts.append(f"{cores}c/{threads}t" if threads else f"{cores}c")
        arch = env.get("arch", "")
        if arch:
            parts.append(arch)
        libc = env.get("libc", "")
        if libc:
            parts.append(trim_text(libc, 28))

        env_line = " | ".join(parts)
        if env_line:
            fig.text(
                0.01,
                0.02,
                f"Env: {env_line}",
                ha="left",
                va="bottom",
                fontsize=8,
                color="#4a5568",
                alpha=0.8,
                fontfamily="monospace",
                transform=fig.transFigure,
            )

    # Add watermark with generation date+time (left) and repo URL (right)
    generation_datetime = datetime.now().strftime("%Y-%m-%d %H:%M")
    fig.text(
        0.01,
        0.005,
        f"Generated: {generation_datetime}",
        ha="left",
        va="bottom",
        fontsize=8,
        color="#4a5568",
        alpha=0.7,
        fontfamily="monospace",
        transform=fig.transFigure,
    )
    fig.text(
        0.99,
        0.005,
        "github.com/niklas-heer/speed-comparison",
        ha="right",
        va="bottom",
        fontsize=8,
        color="#4a5568",
        alpha=0.7,
        fontfamily="monospace",
        transform=fig.transFigure,
    )

    # Save with proper layout - tight margins
    plt.tight_layout()
    plt.subplots_adjust(top=0.98, bottom=0.04, left=0.15, right=0.92)
    plt.savefig(
        output_path,
        dpi=150,
        bbox_inches="tight",
        facecolor=fig.get_facecolor(),
        edgecolor="none",
        pad_inches=0.1,
    )
    plt.close()


def summarize_environment(raw_results: list[dict]) -> dict:
    """Summarize environment metadata across all results."""
    envs = [r.get("Environment", {}) for r in raw_results if r.get("Environment")]
    if not envs:
        return {}

    keys = [
        "cpu_model",
        "cpu_cores",
        "cpu_threads",
        "arch",
        "kernel",
        "os_release",
        "libc",
    ]
    summary: dict[str, str] = {}
    unique_envs = set()
    for env in envs:
        unique_envs.add(tuple(env.get(k, "") for k in keys))

    for key in keys:
        values = sorted({env.get(key, "") for env in envs if env.get(key)})
        if len(values) == 1:
            summary[key] = values[0]
        elif len(values) > 1:
            summary[key] = "mixed"
        else:
            summary[key] = ""

    return {
        "environment": summary,
        "environment_variants": len(unique_envs),
        "languages": len(envs),
    }


def build_combined_results(raw_results: list[dict]) -> list[dict]:
    """Build combined results with extended metadata."""
    combined = []
    for result in raw_results:
        combined.append(
            {
                "name": result.get("Language", ""),
                "target": result.get("Target", ""),
                "version": result.get("Version", ""),
                "median": round(parse_time_value(result.get("Median", 0)) * 1000, 2),
                "min": round(parse_time_value(result.get("Min", 0)) * 1000, 2),
                "max": round(parse_time_value(result.get("Max", 0)) * 1000, 2),
                "accuracy": round(float(result.get("Accuracy", 0)), 4),
                "environment": result.get("Environment", {}),
                "compile": result.get("Compile", ""),
                "run": result.get("Run", ""),
                "nixpkgs": result.get("Nixpkgs", []),
                "nix_flakes": result.get("NixFlakes", []),
                "category": result.get("Category", ""),
            }
        )

    combined.sort(key=lambda r: float(r["min"]))
    return combined


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
    raw_results = load_raw_results(args.folder)

    if df.empty:
        print("No JSON result files found!")
        return 1

    output_dir = Path(args.out)

    # Save CSV
    csv_path = output_dir / "combined_results.csv"
    df.to_csv(csv_path, index=False, encoding="utf-8")

    # Save combined JSON and run metadata
    env_summary = summarize_environment(raw_results) if raw_results else {}
    if raw_results:
        combined_results = build_combined_results(raw_results)
        combined_json_path = output_dir / "combined_results.json"
        combined_json_path.write_text(json.dumps(combined_results, indent=2))

        if env_summary:
            env_summary["generated"] = datetime.now().strftime("%Y-%m-%d %H:%M")
            run_meta_path = output_dir / "run_metadata.json"
            run_meta_path.write_text(json.dumps(env_summary, indent=2))

    # Read rounds
    rounds = Path(args.rounds).read_text().strip()

    # Generate visualization
    png_path = output_dir / "combined_results.png"
    plot_results(df, rounds, str(png_path), env_summary=env_summary)

    print(f"Generated {len(df)} language results:")
    print(f"  CSV: {csv_path}")
    if raw_results:
        print(f"  JSON: {output_dir / 'combined_results.json'}")
        if (output_dir / "run_metadata.json").exists():
            print(f"  Metadata: {output_dir / 'run_metadata.json'}")
    print(f"  PNG: {png_path}")

    return 0


if __name__ == "__main__":
    exit(main())
