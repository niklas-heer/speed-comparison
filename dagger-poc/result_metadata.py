"""Benchmark output checks and methodology metadata shared by both adapters."""

import math

from languages import Language


EXPLICIT_SIMD = {
    "cpp-avx2",
    "csharp-simd",
    "fsharp-simd",
    "java-vecops",
    "rust-simd",
    "rust-nightly",
    "swift-simd",
    "sbcl-simd",
    "zig-simd",
    "octave-vectorised",
}
SOURCE_RELAXED_MATH = {"zig", "zig-simd", "julia", "cpython-numba", "swift-relaxed"}


def methodology(target: str, lang: Language) -> dict:
    flags = lang.compile or ""
    relaxed = target in SOURCE_RELAXED_MATH or any(
        flag in flags for flag in ("fast-math", "fassociative-math", "unsafe-fp-math", "ffast-math")
    )
    return {
        "MathMode": "relaxed" if relaxed else "compiler-default",
        "ExplicitSIMD": target in EXPLICIT_SIMD,
        "Algorithm": "paired-terms" if target == "swift" else "leibniz-series",
    }


def enrich_result(result: dict, target: str, lang: Language, rounds: int) -> None:
    value = float(result["CalculatedPi"])
    # A convergence sanity check, not an assertion of bitwise equality. Historical
    # implementations differ by a boundary term and optimized summation order.
    if not math.isfinite(value) or abs(value - math.pi) > 4 / (2 * rounds - 1) + 1e-8:
        raise ValueError(f"{target}: invalid Leibniz result {value} for {rounds} rounds")
    result.update(methodology(target, lang))
    result.update(
        Rounds=rounds,
        Compile=lang.compile or "",
        Run=lang.run,
        Nixpkgs=list(lang.nixpkgs),
        NixFlakes=list(lang.nix_flakes),
        Category=lang.category,
    )
