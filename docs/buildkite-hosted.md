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
`rust bun deno lua ocaml racket sbcl julia`
- `BUILD_ONLY`: `true` or `false` (default `false`)
- `BENCHMARK_ONLY`: `true` or `false` (default `false`)
- `PUSH_IMAGES`: `true` or `false` (default `false`)
- `QUICK_TEST_ROUNDS`: e.g. `10000`
- `DRY_RUN`: `true` or `false` (default `false`)

Notes:
- With `PUSH_IMAGES=false` (default), the benchmark step skips registry prebuild/push.
- If a selected target image is missing in the active registry, the step auto-falls back to `USE_LOCAL_IMAGES=1` for that target.
- If a registry benchmark fails after pull retries, the step retries that target once with local image fallback.

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
