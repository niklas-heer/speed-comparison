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

Required only if you want to publish images (`PUSH_IMAGES=true`):

- `GHCR_USER`: GitHub username used for container registry login
- `GHCR_TOKEN`: GitHub token with package read/write access

Recommended:

- `REGISTRY=ghcr.io/niklas-heer/speed-comparison`

## Optional Runtime Toggles

All toggles are optional and can be passed per build:

- `LANGUAGES`: space-separated list, default:
`rust bun deno lua ocaml racket sbcl julia`
- `BUILD_ONLY`: `true` or `false` (default `false`)
- `BENCHMARK_ONLY`: `true` or `false` (default `false`)
- `PUSH_IMAGES`: `true` or `false` (default `false`)
- `QUICK_TEST_ROUNDS`: e.g. `10000`
- `DRY_RUN`: `true` or `false` (default `false`)

Notes:
- With `PUSH_IMAGES=false` (default), the benchmark step skips registry prebuild/push.
- If a selected target image is missing in GHCR, the step auto-falls back to `USE_LOCAL_IMAGES=1` for that target.

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
