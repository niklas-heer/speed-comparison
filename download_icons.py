#!/usr/bin/env python3
"""Download programming language icons from devicon and convert to PNG.

Run with: DYLD_LIBRARY_PATH=/opt/homebrew/opt/cairo/lib uv run python download_icons.py
"""

import urllib.request
from pathlib import Path

import cairosvg

# Map benchmark language names to devicon icon names
# Format: "benchmark_name": "devicon_name"
ICON_MAP = {
    # C family
    "C (gcc)": "c",
    "C (clang)": "c",
    "C++": "cplusplus",
    "C++ (g++)": "cplusplus",
    "C++ (clang++)": "cplusplus",
    "C++ (avx2)": "cplusplus",
    "C#": "csharp",
    "Objective-C": "objectivec",
    # Systems languages
    "Rust": "rust",
    "Rust (nightly)": "rust",
    "Go": "go",
    "Zig": "zig",
    "Nim": "nim",
    "D (GDC)": "d",
    "D (LDC)": "d",
    "Odin": "odin",
    "V": "v",
    # JVM languages
    "Java": "java",
    "Java graalvm": "java",
    "Java (Vec Ops)": "java",
    "Kotlin": "kotlin",
    "Scala": "scala",
    "Clojure": "clojure",
    "Groovy": "groovy",
    # .NET
    "F#": "fsharp",
    # Scripting
    "Python (CPython)": "python",
    "Python (PyPy)": "python",
    "Python (NumPy)": "python",
    "Python (MyPyC)": "python",
    "Ruby": "ruby",
    "Perl": "perl",
    "Raku": "raku",
    "PHP": "php",
    "Lua": "lua",
    "LuaJIT": "lua",
    # JavaScript/TypeScript
    "Javascript (nodejs)": "nodejs",
    "Javascript (bun)": "bun",
    "Deno (TypeScript)": "denojs",
    # Functional
    "Haskell (GHC)": "haskell",
    "OCaml": "ocaml",
    "Elixir": "elixir",
    "Erlang": "erlang",
    "Gleam": "gleam",
    "Racket": "racket",
    "Common Lisp (SBCL)": "lisp",
    # Other compiled
    "Swift": "swift",
    "Swift (SIMD)": "swift",
    "Crystal": "crystal",
    "Julia": "julia",
    "Julia (AOT compiled)": "julia",
    "Julia (ux4)": "julia",
    "Fortran 90": "fortran",
    "Ada (gnat-gcc)": "ada",
    "Pascal (FPC)": "pascal",
    "Pony": "pony",
    "Pony(nightly)": "pony",
    # Data/Stats
    "R": "r",
    # Other
    "Dart (JIT)": "dart",
    "Dart (AOT)": "dart",
    "Haxe (C++)": "haxe",
    "Janet": "janet",
    "Janet (compiled)": "janet",
    "WASM (C via Wasmtime)": "wasm",
}

DEVICON_BASE = "https://raw.githubusercontent.com/devicons/devicon/master/icons"

# Custom icons (not in devicon) - these are created manually as SVG
CUSTOM_ICONS = {"d", "odin", "v", "lisp", "ada", "pascal", "pony", "janet", "raku"}


def download_and_convert_icon(name: str, output_dir: Path, size: int = 32):
    """Download a single SVG icon from devicon and convert to PNG."""
    png_path = output_dir / f"{name}.png"

    if name in CUSTOM_ICONS:
        # Custom icons are already in the icons folder as SVG
        svg_path = output_dir / f"{name}.svg"
        if svg_path.exists():
            try:
                cairosvg.svg2png(
                    url=str(svg_path),
                    write_to=str(png_path),
                    output_width=size,
                    output_height=size,
                )
                print(f"Converted custom: {name}")
                return True
            except Exception as e:
                print(f"Failed to convert custom {name}: {e}")
                return False
        else:
            print(f"Custom icon missing: {name}")
            return False

    # Try original first, then plain
    for variant in ["original", "plain"]:
        url = f"{DEVICON_BASE}/{name}/{name}-{variant}.svg"

        try:
            svg_path, _ = urllib.request.urlretrieve(url)
            cairosvg.svg2png(
                url=svg_path,
                write_to=str(png_path),
                output_width=size,
                output_height=size,
            )
            print(f"Downloaded & converted: {name} ({variant})")
            return True
        except Exception:
            continue

    print(f"Failed to download: {name}")
    return False


def main():
    output_dir = Path(__file__).parent / "icons"
    output_dir.mkdir(exist_ok=True)

    # Get unique icon names
    icons_to_download = set(v for v in ICON_MAP.values() if v is not None)

    print(f"Downloading and converting {len(icons_to_download)} icons to PNG...")

    for icon_name in sorted(icons_to_download):
        download_and_convert_icon(icon_name, output_dir)

    print(f"\nPNG icons saved to: {output_dir}")


if __name__ == "__main__":
    main()
