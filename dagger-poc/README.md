# Nix benchmark pipeline

`languages.py` is the source of truth for compiler packages, source files, setup,
compilation, and benchmark commands. Two adapters use those definitions:

- `native.py`: one target per restricted Devbox container, used by homelab Argo.
- `benchmark.py`: portable Dagger adapter for local Docker testing and optional
  manually dispatched GitHub benchmarks.

Both use hyperfine and the MicroPython-compatible `scmeta.py`. Compilation precedes
measurement. `result_metadata.py` validates finite, plausible Leibniz output and
adds actual rounds and math/SIMD labels. Native results retain the resolved
`devbox.lock`, including transitive Nix inputs.

Both adapters perform two warmups and three measured executions. The first warmup
also captures the output for validation, avoiding an extra execution solely for
metadata. Dagger retains setup/build caches but adds a fresh measurement ID before
warmup so requested timings cannot come from an earlier cached run.

`python scripts/affected_targets.py --base origin/master --head HEAD` (from the
repository root) selects affected targets, including shared sources, source
directories, extra files and referenced compiler settings. It parses both catalogs
without executing their Python code. Shared runner/tooling changes select the full
suite; documentation changes do not. Root reporting dependencies and report scripts
select a separate report check, which renders recorded samples and publishes only
to a temporary directory. They do not select benchmark execution. GitHub CI uploads
this plan. Submission to
the homelab must recompute it against the exact authorized revision rather than
trusting a PR-produced artifact.

```bash
# From the repository root:
uv run --locked --project dagger-poc --extra dev pytest dagger-poc -q
QUICK_TEST_ROUNDS=10000 USE_LOCAL_IMAGES=1 \\
  uv run --locked --project dagger-poc python dagger-poc/benchmark.py rust go python
python scripts/argo_bench.py --targets 'rust go python' --rounds 10000
```

The directory keeps its original name to preserve scripts and dependency update
paths. Use `just` inside this directory for convenience commands. `uv sync --extra
dev` installs pytest and the development tooling.

The optional [catalog resolver](../docs/catalog-resolution.md) preserves these
Python declarations while exporting validated data through a Dagger container. It
is a prerequisite for future authorized PR execution; it does not change the active
benchmark adapter or enable automatic dispatch.

For a new language, add a `Language` entry and its source, run the tests, then run
a native smoke test. Validate small odd/even round counts and SIMD tail handling.
Variants need distinct display names. Keep package versions explicit; flake inputs
must reference immutable commits. Capture actual compiler versions in the output.

`check_versions.py --json` queries the Devbox catalog. `update_versions.py` applies
only literal package-version changes, including shared variants, while leaving
compile/setup commands intact. Proposed upgrades require native validation.

Registry image builds remain optional (`build_images.py`). Their tags fingerprint
the package/setup/tool configuration. Registry access is not needed by Argo.

See [the project README](../README.md) and [migration tracker](../docs/nix-migration.md)
for hardware, execution commands, and remaining rollout gates.
