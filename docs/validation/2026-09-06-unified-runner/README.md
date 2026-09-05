# Unified runner integration evidence

Local Dagger 0.19.8 on an ARM development engine, using Git source
`3d0220e28f495d3feaca03f730a00b7259924848`. These are 10,000-round integration checks,
not performance rankings, native x86 validation or a complete suite.

- `first/` and `repeat/`: Go and Hare, one engine session per suite. The first suite
  spent 20.59 s preparing and 22.30 s in build/measurement phases; the repeat spent
  0.17 s preparing and 2.36 s total. Existing engine caches were present before the
  first run, so this is not an empty-cache cold-start benchmark. The times exclude
  catalog resolution, connection and teardown. Measurement IDs and samples differ.
- `final/`: final runner layout with target files in `targets/` and driver hashes
  in `run.json`. Go, Hare and Python all passed. Source revision, resolved catalog,
  tooling, rounds and successes were retained together. The earlier pair predates
  the final output-layout/provenance additions and is preserved unchanged.
- `empty-selection.json`: the same revision used as both base/head selects no
  targets and executes no preparation/measurement work. It predates the final
  target-directory layout; the selection and scheduler behavior are unchanged.
- `input-contract.json`: the committed `check_unified_runner.py` checks a new Go
  target absent from the driver's catalog, a display name containing shell syntax,
  and a supplied 17-round source differing from the host checkout. Invalid client
  tooling defaults deliberately force the runner to use the manifest's versions
  and Devbox image; the run succeeds and preserves the expected values.

The very short Hare commands include Hyperfine-corrected zero durations. These
checks establish functionality and fresh execution, not useful timing accuracy.
The published billion-round dataset is unchanged. All raw files are hashed in
`SHA256SUMS`. The final runner regression suite has 137 passing tests.

Reproduce the suite with a new output path for each attempt:

```sh
QUICK_TEST_ROUNDS=10000 uv run --locked --project dagger-poc python \
  dagger-poc/benchmark.py --revision 3d0220e28f495d3feaca03f730a00b7259924848 \
  --output /tmp/new-unified-run --measurement-timeout 60 go hare python
uv run --locked --project dagger-poc python dagger-poc/check_unified_runner.py
```

The final target directory was also read by the existing analyzer in a separate
output directory. The three-target combined JSON and generated PNG were verified;
no benchmarks were repeated for report rendering and nothing was published.
