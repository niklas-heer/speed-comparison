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
