"""Shared measurement protocol for the native and Dagger adapters."""

import shlex


WARMUP_RUNS = 2
BENCHMARK_RUNS = 3


def measurement_command(run: str, *, show_output: bool = False) -> str:
    """Capture output during the first warmup, then measure the unchanged command.

    The first warmup is outside hyperfine so its output can be retained. This
    preserves two warmups and three measurements without a sixth execution just
    to calculate pi. Parentheses apply redirection to the entire compound command.
    """
    hyperfine = [
        "hyperfine",
        "--warmup", str(WARMUP_RUNS - 1),
        "--runs", str(BENCHMARK_RUNS),
        "--time-unit", "second",
        "--export-json", "hyperfine.json",
    ]
    if show_output:
        hyperfine.append("--show-output")
    hyperfine.append(run)
    return f"({run}) > pi.txt && {shlex.join(hyperfine)}"


def measurement_metadata() -> dict:
    return {
        "WarmupRuns": WARMUP_RUNS,
        "MeasuredRuns": BENCHMARK_RUNS,
        "OutputCapture": "first-warmup",
    }
