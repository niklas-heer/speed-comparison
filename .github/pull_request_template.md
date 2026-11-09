<!-- Thanks for contributing! Delete what doesn't apply. -->

**What changes and why**


**Checklist for language and implementation changes**

- [ ] Single-threaded, and still the Leibniz series
- [ ] Reads `rounds.txt` and prints π with 16 decimals
- [ ] Toolchain pinned in `pipeline/languages.py` (Devbox version, flake at a full commit, or hash-pinned packages)
- [ ] Relaxed math, SIMD or a changed summation is labeled in `pipeline/result_metadata.py`, and explicit SIMD has its own target
- [ ] `uv run --locked --project pipeline --extra dev pytest pipeline -q` passes

CI builds and runs affected targets on short workloads, but only canaries when more
than 30 are affected, and it skips AVX-512 targets on runners without AVX-512. Check
anything it didn't run yourself. Timings from CI or your own machine are welcome as
context but are not benchmark results.
