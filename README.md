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
Also the findings just show how good the languages can handle floating point operations, which is only one aspect of a programming language.

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

## FAQ

> Why do you also count reading a file and printing the output?

Because I think this is a more realistic scenario to compare speeds.

> Are the compile times included in the measurements?

No they are not included, because when running the program in the real world this would also be done before.

## Thanks

### Contributors

See all contributors on the [Contributors page](https://github.com/niklas-heer/speed-comparison/graphs/contributors).

### Special thanks

#### sharkdp

For creating [hyperfine](https://github.com/sharkdp/hyperfine) which is used for the fundamental benchmarking.

#### Thomas

This projects takes inspiration from [Thomas](https://www.thomaschristlieb.de) who did a similar comparison [on his blog](https://www.thomaschristlieb.de/performance-vergleich-zwischen-verschiedenen-programmiersprachen-und-systemen/).
