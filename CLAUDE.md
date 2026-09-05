# Project context

This repository compares single-threaded Leibniz implementations and publishes
historical results to GitHub Pages. Read `docs/nix-migration.md` for the project
map, contribution integration, and current validation gates.

## Build and execution

`dagger-poc/languages.py` is the shared source of truth for Nix/Devbox packages,
setup, compiler flags, execution commands, and variant names. Pin package
versions and flake commits. Auxiliary upstream packages must be pinned too.

The primary executor is the homelab's restricted Argo WorkflowTemplate, defined
in the homelab repository under `cluster/apps/speed-comparison/`. It runs one
target at a time on a fixed x86_64 worker using `dagger-poc/native.py`.
The optional Dagger adapter (`benchmark.py`, `build_images.py`) uses the same
definitions. `Earthfile`, Crystal `scmeta/`, Buildkite, and Fly helpers are
historical references; do not add new mandatory paid runners.

Commands from the repository root:

```bash
uv run --project dagger-poc --extra dev pytest dagger-poc -q
python scripts/argo_bench.py --targets 'c rust go python' --rounds 10000
python scripts/argo_bench.py --targets all --rounds 1000000000
```

Argo submission requires the homelab kubeconfig. Quick/subset runs are validation
artifacts, not performance publications. Review source before submitting a
`codex/` branch. Automated GitHub Actions runs only code validation and a weekly
package update check; the optional Dagger benchmark is manually dispatched.

## Contributions and methodology

Add source under `src/` and a distinct target in `languages.py`; multi-file
projects may use a source directory. No custom Docker image is required.
Execute setup/compile shell scripts as files through Devbox: inline command
arguments can expand variables before entering the target environment.

Keep computation single-threaded. Explicit SIMD variants have separate names;
auto-vectorization and relaxed math are allowed with accurate metadata.
`result_metadata.py` records math mode, SIMD, summation algorithm, and rounds.
Its convergence check allows historical boundary-term and summation-order
differences; it does not promise bitwise-identical output.

Compilation stays outside timing. Confirm source consumes `rounds.txt`, prints
the computed result, and handles short/odd inputs as well as one billion rounds.
Bound vector allocations so full runs fit the worker's memory limit.

## Analysis and publication

`analyze.py` creates CSV, JSON, chart, and hardware metadata.
`scripts/validate_publish.py` requires the complete full-round roster with
valid output/timings and consistent recorded hardware.
`scripts/publish_results.py` operates from a clean matching source checkout,
regenerates analysis, and optionally commits only `docs/history` to master.
It rejects stale master revisions and never force-pushes.

The homelab publisher alone receives a repository-scoped deploy key from
1Password. Benchmark containers must not receive repository write credentials.
Keep the weekly schedule suspended until the full validation gate passes.
