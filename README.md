[![CI](https://github.com/niklas-heer/speed-comparison/actions/workflows/ci.yml/badge.svg)](https://github.com/niklas-heer/speed-comparison/actions/workflows/ci.yml)

![plot](https://niklas-heer.github.io/speed-comparison/history/latest/combined_results.png "Speed comparison of programming languages")

---

# Speed comparison of programming languages

This projects tries to compare the speed of different programming languages.
In this project we don't really care about getting a precise calculation of pi. We only want to see how fast are the programming languages doing. <br />
It uses an implementation of the [Leibniz formula for π](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) to do the comparison. <br />
Here is a video which explains how it works: [Calculating π by hand](https://www.youtube.com/watch?v=HrRMnzANHHs)

You can find the results here: https://niklas-heer.github.io/speed-comparison/

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

## Used hardware

The benchmarks run on Ubicloud standard-4 runners:

    CPU: 4 vCPUs (2 physical cores) on AMD EPYC 9454P processors
    RAM: 16 GB
    Storage: NVMe SSDs
    OS: Ubuntu 24.04

See [Ubicloud Runner Types](https://www.ubicloud.com/docs/github-actions-integration/runner-types) for more details.

## Run it yourself

Everything is run by a Docker container and a bash script which envokes the programs.

To measure the execution time a [python package](https://pypi.python.org/pypi/lauda/1.2.0) is used.

### Requirements
- `Docker`
- [earthly](https://earthly.dev/)

### Run everything
Earthly allows to run everything with a single command:
```bash
earthly +all
```
This will run all tasks to collect all measurements and then run the analysis.

### Collect data
To collect data for all languages run:
```bash
earthly +collect-data
```

To collect data for a single language run:
```bash
earthly +rust    # or any other language target
```

### Available language targets
Language targets are auto-discovered from the Earthfile. You can list them with:
```bash
./scripts/discover-languages.sh
```

### Analyse results
To generate the combined CSV and chart from all results:
```bash
earthly +analysis
```

### Fast check (subset)
For quick testing, run only a subset of fast languages:
```bash
earthly +fast-check   # runs: c, go, rust, cpython
```

## CI/CD

The project uses GitHub Actions with a **parallel matrix build**:

1. **Auto-discovery**: Language targets are automatically detected from the Earthfile
2. **Parallel execution**: All 43+ languages run simultaneously in separate jobs
3. **Isolation**: Each language gets a fresh runner environment
4. **Results collection**: All results are merged and analyzed together
5. **Auto-publish**: Results are published to [GitHub Pages](https://niklas-heer.github.io/speed-comparison/)

### PR Commands

Repository maintainers can trigger benchmarks on PRs using comments:

```
/bench rust go c     # Run specific languages
```

### Labels

- `enable-ci`: Trigger full benchmark suite on a PR
- `skip-ci`: Skip the fast-check on a PR

## Automated Version Updates

This project uses an AI-powered CI workflow to keep all programming languages up to date automatically.

### How It Works

1. **Weekly Check**: A scheduled workflow runs every Monday at 6 AM UTC
2. **Version Detection**: Checks for new versions via:
   - Docker Hub Registry API (for official language images)
   - GitHub Releases API (for languages like Zig, Nim, Gleam)
   - Alpine package index (for Alpine-based packages)
3. **Automated Updates**: Claude Code (via OpenRouter) updates the Earthfile with new versions
4. **Validation**: Runs a quick benchmark to verify the update compiles and runs correctly
5. **Breaking Changes**: If the build fails, Claude Code (Opus) researches and fixes breaking changes (up to 3 attempts)
6. **PR Creation**: Creates a PR for review if successful, or an issue describing the failure if not

### Manual Trigger

You can manually trigger a version check:

1. Go to **Actions** → **Version Check** → **Run workflow**
2. Optionally specify a single language name to check only that one
3. Enable "Dry run" to check versions without creating PRs

### Configuration

Version sources are defined in [`scripts/version-sources.json`](scripts/version-sources.json). Each language maps to:
- `source`: Where to check for updates (docker, github, alpine, apt)
- `image` or `repo`: The Docker image or GitHub repository
- `earthfile_pattern`: Regex to extract current version from Earthfile
- `source_file`: The source code file for this language

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
