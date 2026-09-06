#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Analyzes benchmark JSON results and generates visualizations."""

import json
from argparse import ArgumentParser
from datetime import datetime
from pathlib import Path
from report_metadata import execution_metadata

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
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
        if file_path.name in ("combined_results.json", "run_metadata.json", "run.json"):
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
        if file_path.name in {"combined_results.json", "run_metadata.json", "run.json"}:
            continue
        result = json.loads(file_path.read_text())
        data["name"].append(result["Language"])
        # Retain compatibility with older per-file exports without Target.
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
    """Render an exportable median chart with sample ranges and method labels."""
    from matplotlib.ticker import FuncFormatter, LogLocator
    from matplotlib.lines import Line2D

    df = df.sort_values("median").copy()
    count = len(df)
    height = max(7, count * 0.30 + 3.3)
    bg, ink, muted = "#f5f4ed", "#203e36", "#596e64"
    colors = {"default": "#467762", "simd": "#cf682e", "relaxed": "#79669e"}
    with plt.rc_context({"font.family": "DejaVu Sans", "savefig.facecolor": bg}):
        fig, ax = plt.subplots(figsize=(15, height))
        fig.patch.set_facecolor(bg)
        ax.set_facecolor(bg)
        y = np.arange(count)
        values = df["median"].to_numpy()
        bar_colors = [
            colors["simd"]
            if row.get("explicit_simd") is True
            else colors["relaxed"]
            if row.get("math_mode") == "relaxed"
            else colors["default"]
            for _, row in df.iterrows()
        ]
        low = min(df["min"].min(), values.min()) * 0.45
        ax.barh(y, values - low, left=low, height=0.63, color=bar_colors, alpha=0.9)
        # Whiskers show observed min/max, not a confidence interval.
        ax.hlines(y, df["min"], df["max"], color=ink, linewidth=1.2)
        ax.scatter(df["min"], y, color=ink, marker="|", s=14, linewidths=0.9)
        ax.scatter(df["max"], y, color=ink, marker="|", s=14, linewidths=0.9)
        labels = []
        for _, row in df.iterrows():
            flags = (" [S]" if row.get("explicit_simd") is True else "") + (
                " [R]" if row.get("math_mode") == "relaxed" else ""
            )
            labels.append(f"{row['name']}  {row['version']}{flags}")
        ax.set_yticks(y, labels, fontsize=8.3, color=ink)
        ax.tick_params(axis="y", length=0, pad=28)
        for index, (_, row) in enumerate(df.iterrows()):
            base_name = str(row["name"]).split(" (")[0]
            fallback = {
                "Common Lisp": "lisp",
                "F#": "fsharp",
                "Python": "python",
                "Kotlin": "kotlin",
                "Rust": "rust",
                "Zig": "zig",
                "Hare": "hare",
            }.get(base_name, base_name.lower())
            icon_name = ICON_MAP.get(row["name"], fallback)
            icon = (
                load_icon(icon_name)
                if (Path(__file__).parent / "icons" / f"{icon_name}.png").exists()
                else None
            )
            if icon:
                ax.add_artist(
                    AnnotationBbox(
                        OffsetImage(icon, zoom=0.30),
                        (0, index),
                        xycoords=("axes fraction", "data"),
                        xybox=(-12, 0),
                        boxcoords="offset points",
                        frameon=False,
                        pad=0,
                    )
                )
            ax.text(
                max(row["max"], row["median"]) * 1.10,
                index,
                format_time(row["median"]),
                va="center",
                fontsize=8.3,
                color=ink,
                fontfamily="DejaVu Sans Mono",
            )
        ax.set_xscale("log")
        ax.set_xlim(low, df["max"].max() * 2.9)
        ax.invert_yaxis()
        ax.margins(y=0.005)
        ax.xaxis.set_major_locator(LogLocator(base=10))
        ax.xaxis.set_major_formatter(FuncFormatter(lambda value, _: format_time(value)))
        ax.tick_params(axis="x", labelsize=9, colors=muted)
        ax.grid(axis="x", which="major", color="#c8d0c6", linewidth=0.7)
        ax.set_axisbelow(True)
        for spine in ax.spines.values():
            spine.set_visible(False)
        ax.set_xlabel(
            "Median execution time · logarithmic axis · whiskers show observed min–max",
            color=muted,
            fontsize=10,
            labelpad=14,
        )
        top = 1 - 1.65 / height
        bottom = 1.15 / height
        fig.subplots_adjust(left=0.30, right=0.97, top=top, bottom=bottom)
        fig.text(
            0.03,
            1 - 0.25 / height,
            "SPEED COMPARISON / FIELD NOTES",
            fontsize=10,
            color=muted,
            va="top",
            fontfamily="DejaVu Sans Mono",
        )
        fig.text(
            0.03,
            1 - 0.52 / height,
            "One calculation. Many ways to get there.",
            fontsize=23,
            fontweight="bold",
            color=ink,
            va="top",
        )
        fig.text(
            0.03,
            1 - 0.94 / height,
            f"{count} implementations  ·  Leibniz π  ·  {int(rounds):,} terms per execution",
            fontsize=11,
            color=muted,
            va="top",
        )
        legend = [
            Line2D(
                [0],
                [0],
                color=colors["default"],
                lw=7,
                label="Compiler default / see details",
            ),
            Line2D([0], [0], color=colors["simd"], lw=7, label="[S] Explicit SIMD"),
            Line2D([0], [0], color=colors["relaxed"], lw=7, label="[R] Relaxed math"),
        ]
        fig.legend(
            handles=legend,
            loc="upper left",
            bbox_to_anchor=(0.028, 1 - 1.16 / height),
            ncol=3,
            frameon=False,
            fontsize=9,
            labelcolor=ink,
        )
        meta = env_summary or {}
        env = meta.get("environment", {})
        machine = (
            " · ".join(
                str(env[k]) for k in ("cpu_model", "arch", "kernel") if env.get(k)
            )
            or "Hardware not recorded"
        )
        elapsed = meta.get("execution", {}).get("elapsed_seconds")
        if elapsed is not None:
            total = round(elapsed)
            runtime = f"{total // 3600}h {(total % 3600) // 60}m {total % 60}s"
        else:
            runtime = "not recorded"
        revision = meta.get("source_revision", "not recorded")
        fig.text(0.03, 0.76 / height, trim_text(machine, 135), fontsize=9, color=muted)
        fig.text(
            0.03,
            0.53 / height,
            f"Whole run: {runtime} · source: {revision}",
            fontsize=8.5,
            color=ink,
        )
        fig.text(
            0.03,
            0.30 / height,
            "A numerical microbenchmark, not a universal language ranking. Conditions and run-clock scope on the website.",
            fontsize=8,
            color=muted,
        )
        fig.text(
            0.97,
            0.09 / height,
            "speed-comparison.vercel.app",
            ha="right",
            fontsize=9,
            color=ink,
        )
        fig.savefig(output_path, dpi=150, facecolor=bg)
        plt.close(fig)


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
                # Preserve samples in their original seconds, without presentation rounding.
                # None records missing historical metadata rather than inventing a protocol.
                "times_per_run": result.get("TimesPerRun"),
                "exit_codes_per_run": result.get("ExitCodesPerRun"),
                "calculated_pi": result.get("CalculatedPi"),
                "warmup_runs": result.get("WarmupRuns"),
                "measured_runs": result.get("MeasuredRuns"),
                "output_capture": result.get("OutputCapture"),
                "measurement_id": result.get("MeasurementID"),
                "measurement_profile": result.get("MeasurementProfile"),
                "measurement_timeout_seconds": result.get("MeasurementTimeoutSeconds"),
                "source_revision": result.get("SourceRevision"),
                "source_file": result.get("SourceFile"),
                "source_files": result.get("SourceFiles"),
                "execution_adapter": result.get("ExecutionAdapter"),
                "dagger_sdk_version": result.get("DaggerSDKVersion"),
                "dagger_engine_version": result.get("DaggerEngineVersion"),
                "tooling": result.get("Tooling"),
                "nix_setup": result.get("NixSetup"),
                "environment": result.get("Environment", {}),
                "compile": result.get("Compile", ""),
                "run": result.get("Run", ""),
                "nixpkgs": result.get("Nixpkgs", []),
                "nix_flakes": result.get("NixFlakes", []),
                "category": result.get("Category", ""),
                "rounds": result.get("Rounds"),
                "math_mode": result.get("MathMode", "unknown"),
                "explicit_simd": result.get("ExplicitSIMD"),
                "algorithm": result.get("Algorithm", "unknown"),
                "devbox_lock": result.get("DevboxLock", {}),
                "devbox_config": result.get("DevboxConfig", {}),
                "image_tag": result.get("ImageTag", ""),
                "image_fingerprint": result.get("ImageFingerprint", ""),
                "devbox_image": result.get("DevboxImage", ""),
                "build_source": result.get("BuildSource", ""),
                "allow_native_flags": result.get("AllowNativeFlags", None),
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
    output_dir.mkdir(parents=True, exist_ok=True)

    # Save CSV
    csv_path = output_dir / "combined_results.csv"
    df.to_csv(csv_path, index=False, encoding="utf-8")

    # Save combined JSON and run metadata
    env_summary = summarize_environment(raw_results) if raw_results else {}
    run_record = Path(args.folder) / "run.json"
    if not run_record.exists() and Path(args.folder).name == "targets":
        run_record = Path(args.folder).parent / "run.json"
    if run_record.exists():
        env_summary["execution"] = execution_metadata(
            json.loads(run_record.read_text()), raw_results
        )
    if raw_results:
        combined_results = build_combined_results(raw_results)
        combined_json_path = output_dir / "combined_results.json"
        combined_json_path.write_text(json.dumps(combined_results, indent=2))

        if env_summary:
            source_revision = Path(args.folder) / "source-revision.txt"
            if source_revision.exists():
                env_summary["source_revision"] = source_revision.read_text().strip()
            env_summary["generated"] = datetime.now().strftime("%Y-%m-%d %H:%M")
            run_meta_path = output_dir / "run_metadata.json"
            run_meta_path.write_text(json.dumps(env_summary, indent=2))

    # Read rounds
    rounds = Path(args.rounds).read_text().strip()

    # Use full-precision raw medians and methodology for the chart when available.
    if raw_results and all(r.get("Target") for r in raw_results):
        df = pd.DataFrame(build_combined_results(raw_results))
        for field, raw_key in (("median", "Median"), ("min", "Min"), ("max", "Max")):
            exact = {
                r["Target"]: parse_time_value(r[raw_key]) * 1000 for r in raw_results
            }
            df[field] = df["target"].map(exact)
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
