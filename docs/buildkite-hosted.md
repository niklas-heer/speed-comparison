# Buildkite Hosted Setup

This repository can run Dagger benchmarks on Buildkite hosted Linux AMD64 agents using:

- `.buildkite/pipeline.yml`
- `.buildkite/scripts/bootstrap.sh`
- `.buildkite/scripts/dagger_bench.sh`

## Required Buildkite Pipeline Settings

1. Pipeline uses steps from repository:
Path: `.buildkite/pipeline.yml`
2. Queue assignment:
Queue key used in this repo is `default`

## Environment Variables

Registry selection defaults:

- On Buildkite hosted agents, the script defaults to Buildkite internal registry:
`$BUILDKITE_HOSTED_REGISTRY_URL/$BUILDKITE_PIPELINE_SLUG`
- Outside hosted agents, it falls back to:
`ghcr.io/niklas-heer/speed-comparison`
- Override explicitly with `REGISTRY=...` if needed.

Required only if you want to publish images (`PUSH_IMAGES=true`):

- `GHCR_USER`: GitHub username used for container registry login
- `GHCR_TOKEN`: GitHub token with package read/write access

Recommended:

- `REGISTRY_REPOSITORY`: override repository suffix used with Buildkite internal registry

## Optional Runtime Toggles

All toggles are optional and can be passed per build:

- `LANGUAGES`: space-separated list, default:
`all` (all languages from `dagger-poc/languages.py`, currently 60)
- `BUILD_ONLY`: `true` or `false` (default `false`)
- `BENCHMARK_ONLY`: `true` or `false` (default `false`)
- `PUSH_IMAGES`: `true` or `false` (default `false`)
- `QUICK_TEST_ROUNDS`: e.g. `10000`
- `DRY_RUN`: `true` or `false` (default `false`)
- `HYPERFINE_SHOW_OUTPUT`: `true` or `false` (default `false`)
- `ALLOW_LOW_MEMORY_FULL_RUN`: `true` or `false` (default `false`)
- `MIN_HEAVY_LANG_RAM_GIB`: minimum RAM guard for full `cpython-numpy`/`r` runs (default `12`)

Parity gate thresholds (all optional; defaults are enforced in pipeline):

- `PARITY_TIE_EPSILON_PCT` (default `5`)
- `PARITY_TOP_K` (default `20`)
- `PARITY_MAX_INVERSION_RATE_PCT` (default `2.5`)
- `PARITY_MIN_KENDALL_TAU` (default `0.96`)
- `PARITY_MAX_TOPK_MEAN_DISPLACEMENT` (default `4`)
- `PARITY_MIN_TOPK_OVERLAP_PCT` (default `80`)
- `PARITY_MAX_TOLERANT_RANK_CHANGES` (default `20`)

Notes:
- With `PUSH_IMAGES=false` (default), the benchmark step skips registry prebuild/push.
- If a selected target image is missing in the active registry, the step auto-falls back to `USE_LOCAL_IMAGES=1` for that target.
- If a registry benchmark fails after pull retries, the step retries that target once with local image fallback.
- Full-round `cpython-numpy` and `r` need significant memory (~7.5GiB each for vector allocations). The script now fails fast on low-memory agents unless you set `QUICK_TEST_ROUNDS` or explicitly bypass with `ALLOW_LOW_MEMORY_FULL_RUN=true`.

## Branch Test Run

To test this branch (`codex/dagger-parity-repro`) in Buildkite:

1. Create a new build for branch `codex/dagger-parity-repro`
2. Set env for a quick smoke:
`LANGUAGES="rust bun deno lua"`
`QUICK_TEST_ROUNDS=10000`
3. Run build

For benchmark-only reruns:

`BENCHMARK_ONLY=true`
`LANGUAGES="rust bun deno lua ocaml racket sbcl julia"`
