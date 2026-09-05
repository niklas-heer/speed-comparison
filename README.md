[![CI](https://github.com/niklas-heer/speed-comparison/actions/workflows/ci.yml/badge.svg)](https://github.com/niklas-heer/speed-comparison/actions/workflows/ci.yml)

![plot](https://speed-comparison.vercel.app/history/latest/combined_results.png "Speed comparison of programming languages")

---

# Speed comparison of programming languages

This projects tries to compare the speed of different programming languages.
In this project we don't really care about getting a precise calculation of pi. We only want to see how fast are the programming languages doing. <br />
It uses an implementation of the [Leibniz formula for π](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) to do the comparison. <br />
Here is a video which explains how it works: [Calculating π by hand](https://www.youtube.com/watch?v=HrRMnzANHHs)

You can find the results here: https://speed-comparison.vercel.app/

## Disclaimer

I'm no expert in all these languages, so take my results with a grain of salt.<br>

This is a microbenchmark. It can certainly give you some clue about a language, but it doesn't tell you the whole picture. 

Also the findings just show how good a language is (or can be) at loops and floating-point math, which is just a small subset of a programming language.

You are also welcome to contribute and help me fix my possible horrible code in some languages. :smile:

## Rules

The benchmark measures **single-threaded computational performance**. To keep comparisons fair:

1. **No concurrency/parallelism**: Implementations must be single-threaded. No multi-threading, async, or parallel processing.

2. **SIMD is allowed but separate**: SIMD optimizations (using wider registers) are permitted but should be separate targets (e.g., `swift-simd`, `cpp-avx2`) rather than replacing the standard implementation.

3. **Standard language features**: Use idiomatic code for the language. Compiler optimizations flags are fine.

4. **Same algorithm**: All implementations must use the Leibniz formula as shown in the existing implementations.

**Why no concurrency?** Concurrency results depend heavily on core count (4-core vs 64-core gives vastly different results), making comparisons meaningless. SIMD stays single-threaded - it just processes more data per instruction.

<!-- TODO: Create a new video for hyperfine and scmeta -->
<!-- ## Adding new languages

[<img src="https://github.com/niklas-heer/speed-comparison/raw/master/assets/how-to-contribute_thumbnail.png" width="50%">](https://www.youtube.com/watch?v=ksV4WObYSiQ "Contributing to speed comparison ") -->

## Run it yourself

The benchmark toolchains are installed with **Nix through Devbox**. Language
versions, compilation flags, source files, and execution commands live in
[`dagger-poc/languages.py`](dagger-poc/languages.py). The directory name is retained
for compatibility. Hyperfine measures execution; `scmeta.py` records the output,
timings, and accuracy. Compilation is outside the timed command.

### Local testing with Dagger

Install Docker and [uv](https://docs.astral.sh/uv/), then run:

```bash
# Configuration and metadata regression tests
uv run --locked --project dagger-poc --extra dev pytest dagger-poc -q

# Quick end-to-end test (10,000 rounds)
QUICK_TEST_ROUNDS=10000 USE_LOCAL_IMAGES=1 \
  uv run --locked --project dagger-poc python dagger-poc/benchmark.py rust go python

# Full benchmark: omit QUICK_TEST_ROUNDS
USE_LOCAL_IMAGES=1 uv run --locked --project dagger-poc python dagger-poc/benchmark.py rust

# List targets and their exact commands
uv run --directory dagger-poc python -c \
  "from languages import LANGUAGES; print('\\n'.join(LANGUAGES))"
```

Each suite writes a new `results/RUN_ID/` evidence bundle (`--output` selects a new
directory). Target JSON and actual rounds are under `targets/`; run status and the
resolved catalog sit alongside it. `--revision FULL_SHA` binds declarations and
sources to an authorized commit; `--base BASE_SHA` selects affected targets.
See the [consolidation plan](docs/pipeline-consolidation.md) for execution stages.

The Dagger Python SDK provisions its CLI/engine. Docker must be running.
Some targets require x86_64 and specific CPU instructions (AVX2 or AVX-512).
Use native Linux for performance measurements; emulated runs only test functionality.

### Homelab benchmarks with Argo

Argo Workflows is the primary scheduled/manual execution path. Its native adapter
uses the same definitions and measurement tools as Dagger, inside restricted
Devbox containers. It runs one target at a time and archives raw JSON, charts,
CSV, compiler commands, hardware information, and the resolved Devbox lock.
No registry write credentials or Docker socket are needed for benchmarks.

With the cluster kubeconfig configured:

```bash
python scripts/argo_bench.py --targets "c rust go python" --rounds 10000
python scripts/argo_bench.py --targets all --rounds 1000000000
kubectl -n speed-comparison get workflows,pods
```

The manifests and operator commands are maintained in the homelab repository.
The weekly schedule stays suspended until migration validation is complete.
See [the migration tracker](docs/nix-migration.md) for rollout and issue status.

### Analyze results

```bash
uv run --locked analyze.py --folder ./results --out ./results --rounds ./src/rounds.txt
```

Use the `rounds.txt` from the corresponding run. Quick-test timings are not suitable
for ranking languages and must never replace the public full-benchmark results.

## CI/CD and versions

GitHub Actions runs configuration and metadata tests on hosted Ubuntu. Expensive
Ubicloud builds are removed from the normal path. The optional Dagger workflow is
manual; normal benchmarks run in Argo. The former `/bench` and `/dagger-bench`
comment commands are replaced by the Argo submission command above.

The weekly Nix version checker proposes explicit package updates in draft PRs.
Configuration tests validate those proposals; affected targets still need a native
smoke test before merging. Flake packages use immutable nixpkgs revisions and are
reviewed separately. The Earthfile and its manually triggered version workflow
remain as migration references.

## Hardware and interpretation

New homelab runs record their actual CPU and environment in result metadata. Older
published results were collected on Ubicloud AMD EPYC 9454P runners. Do not compare
absolute times across different hardware, compiler versions, or round counts.

Optimized variants remain allowed. Results label relaxed floating-point math,
explicit SIMD/vectorized variants, and paired-term algebraic transformations.
`compiler-default` describes the absence of explicitly requested relaxed math; it
does not promise identical IEEE evaluation order across languages. Every result
must be finite and pass a round-dependent Leibniz convergence check. This catches
gross errors while allowing existing rounding and boundary-term differences.

## FAQ

<details>
<summary><strong>Why do you also count reading a file and printing the output?</strong></summary>

Because I think this is a more realistic scenario to compare speeds.
</details>

<details>
<summary><strong>Are the compile times included in the measurements?</strong></summary>

No they are not included, because when running the program in the real world this would also be done before.
</details>

<details>
<summary><strong>Isn't this just measuring startup time for fast languages?</strong></summary>

No. The benchmark runs 1 billion iterations. Testing with Zig by timing segments inside the program:

- Startup + file read: ~0.01ms
- Computation: ~200ms
- Overhead: ~0.01%

Even at 1 million iterations, startup would only be ~4% overhead. At 1 billion, it's essentially zero.
</details>

<details>
<summary><strong>Why is C++ (AVX2) slower than regular C++?</strong></summary>

The standard C++ uses `i & 0x1` which lets the compiler auto-vectorize. With `-O3 -ffast-math -march=native`, modern compilers do this extremely well. The explicit AVX2 version has overhead from manual vector setup and horizontal sum operations. Often compiler auto-vectorization beats hand-written SIMD for simple loops.
</details>

<details>
<summary><strong>Why are Crystal/Odin/Ada so slow?</strong></summary>

All three use the `x = -x` pattern which creates a loop-carried dependency that blocks auto-vectorization. The fast implementations use the branchless `i & 0x1` trick instead, which allows the compiler to vectorize the loop.
</details>

<details>
<summary><strong>Does Zig use fast-math?</strong></summary>

Yes. Zig uses `@setFloatMode(.optimized)` which is equivalent to `-ffast-math`. This is documented in the source code.
</details>

<details>
<summary><strong>Does Julia use fast-math and SIMD?</strong></summary>

Yes. Julia uses `@fastmath @simd for` - both annotations together. The `@simd` enables vectorization hints (similar to compiler auto-vectorization), while `@fastmath` relaxes floating-point strictness.
</details>

<details>
<summary><strong>Why is Nim faster than C?</strong></summary>

Both compile to native code via gcc with similar flags. The marginal difference is likely measurement variance. Nim's code explicitly uses `cuint` to match C's unsigned int type for the loop counter.
</details>

<details>
<summary><strong>Some implementations aren't optimized / weren't written by experts</strong></summary>

Fair point. I'm not an expert in all 40+ languages. The goal was idiomatic-ish code, but some implementations could definitely be improved. That's why PRs are always welcome! For example, Swift has 3 variants (standard, relaxed, SIMD) showing different optimization levels.

This benchmark shows what performance you can expect when someone not deeply versed in a language writes the code - which is actually a useful data point.
</details>

<details>
<summary><strong>Which languages use -ffast-math or equivalent?</strong></summary>

| Language | Fast-math | Notes |
|----------|-----------|-------|
| C/C++ (gcc/clang) | `-ffast-math` | Full optimizations |
| D (GDC/LDC) | `-ffast-math` | Full optimizations |
| Zig | `@setFloatMode(.optimized)` | Equivalent to fast-math |
| Julia | `@fastmath` | Plus `@simd` hint |
| Fortran | **No** | Uses manual loop unrolling instead |
| Rust | **No** | But uses vectorizable pattern |
</details>

## Thanks

### Contributors

See all contributors on the [Contributors page](https://github.com/niklas-heer/speed-comparison/graphs/contributors).

### Special thanks

#### sharkdp

For creating [hyperfine](https://github.com/sharkdp/hyperfine) which is used for the fundamental benchmarking.

#### Thomas

This projects takes inspiration from [Thomas](https://www.thomaschristlieb.de) who did a similar comparison [on his blog](https://www.thomaschristlieb.de/performance-vergleich-zwischen-verschiedenen-programmiersprachen-und-systemen/).

## Website and result archive

The [Astro report](https://speed-comparison.vercel.app/) exposes per-implementation
measurements, math/SIMD labels, resolved Nix packages, build commands and recorded
resource limits. Historical evidence remains unchanged. The [project journal](https://speed-comparison.vercel.app/journal/)
explains the changes and remaining limitations. See [site/README.md](site/README.md)
for development and the private homelab PostgreSQL archive.
