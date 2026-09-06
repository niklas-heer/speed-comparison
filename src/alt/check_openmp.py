"""Compile and check the standalone OpenMP example with GNU Fortran."""

import math
import os
import subprocess
import tempfile
from pathlib import Path


def main():
    source = Path(__file__).with_name("leibniz_openmp.f90").resolve()
    cases = [*range(18), 31, 32, 33, 10000, 10001, 1000001]
    expected = {
        rounds: 4
        * (1 + math.fsum((-1 if i % 2 else 1) / (2 * i + 1) for i in range(1, rounds + 1)))
        for rounds in cases
    }
    with tempfile.TemporaryDirectory(prefix="fortran-openmp-") as directory:
        work = Path(directory)
        for mode, flags in {
            "debug": ["-O0", "-g", "-fcheck=all"],
            "optimized": ["-Ofast", "-march=native", "-funroll-loops", "-flto"],
        }.items():
            binary = work / mode
            subprocess.run(
                [
                    "gfortran",
                    "-fopenmp",
                    "-Wall",
                    "-Wextra",
                    *flags,
                    str(source),
                    "-o",
                    str(binary),
                ],
                check=True,
            )
            for threads in (1, 2, 4):
                env = dict(os.environ, OMP_NUM_THREADS=str(threads), OMP_DYNAMIC="FALSE")
                for rounds in cases:
                    (work / "rounds.txt").write_text(f"{rounds}\n")
                    result = subprocess.run(
                        [str(binary)],
                        cwd=work,
                        env=env,
                        text=True,
                        capture_output=True,
                        check=True,
                        timeout=30,
                    )
                    actual = float(result.stdout.strip())
                    if not math.isclose(actual, expected[rounds], rel_tol=0, abs_tol=1e-11):
                        raise AssertionError((mode, threads, rounds, actual, expected[rounds]))
            for invalid in ("", "invalid\n", "-1\n", "9223372036854775807\n"):
                (work / "rounds.txt").write_text(invalid)
                result = subprocess.run(
                    [str(binary)],
                    cwd=work,
                    capture_output=True,
                    timeout=10,
                )
                if result.returncode == 0:
                    raise AssertionError(f"{mode}: accepted invalid input {invalid!r}")
            (work / "rounds.txt").unlink()
            result = subprocess.run([str(binary)], cwd=work, capture_output=True, timeout=10)
            if result.returncode == 0:
                raise AssertionError(f"{mode}: accepted missing rounds.txt")
            print(f"{mode}: {len(cases)} inputs × 3 thread counts and input-error checks passed")

        (work / "rounds.txt").write_text("1000000000\n")
        result = subprocess.run(
            [str(work / "optimized")],
            cwd=work,
            env=dict(os.environ, OMP_NUM_THREADS="4", OMP_DYNAMIC="FALSE"),
            text=True,
            capture_output=True,
            check=True,
            timeout=120,
        )
        actual = float(result.stdout.strip())
        if not math.isfinite(actual) or abs(actual - math.pi) > 2e-9:
            raise AssertionError(f"Billion-round convergence check failed: {actual}")
        print(f"optimized: billion-round convergence check passed ({actual:.16f})")


if __name__ == "__main__":
    main()
