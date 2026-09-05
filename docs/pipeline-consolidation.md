# Consolidating the benchmark pipeline

Decision and measured evidence, 2026-09-06.

## What makes it slow

The original 75-target billion-round run took about 5h18m. Summing its per-target
medians gives 28.05 minutes for one execution of every target. This sum is a workload
estimate, not measured workflow elapsed time:

| Target | Median execution | Share of summed medians |
| --- | ---: | ---: |
| Scalar Octave | 1,004.05 s | 59.7% |
| MicroPython | 109.50 s | 6.5% |
| Perl | 106.49 s | 6.3% |
| Raku | 82.74 s | 4.9% |
| Python | 76.58 s | 4.6% |
| Remaining 70 targets | 303.50 s | 18.0% |

The five slowest targets account for 82% of this execution estimate. Even with
zero setup overhead, the current five-execution protocol would project about
140 minutes at that workload, assuming similar durations. Caching cannot remove
that computation. Conversely, the native run installed environments separately
in disposable pods: C's roughly one second of executions occupied a 70-second
step, and Rust SIMD a 92-second step. Both workload and preparation need attention.
The [original samples](history/2026-09-05T193245/raw/README.md) and
[SDK comparison](dagger-sdk-comparison.md) support these separate diagnoses.
A Go rewrite would address seconds of client overhead rather than either major cost.

## One execution path

Keep Python `Language` declarations and `benchmark.py` as the execution entry point.
Argo should queue a suite and invoke that entry point against a persistent engine.
GitHub provides event/check integration; the website reads completed evidence.
The native adapter is a migration bootstrap to retire after the Dagger homelab
cutover passes. Do not expand both execution adapters independently.

The runner now supports one engine session with these stages:

1. Resolve an explicitly authorized full source commit. Evaluate its catalog inside
   Dagger, validate the manifest, and use sources from the same Git tree. Use the
   trusted driver's runner and result validator. Store the manifest with the run.
2. Select explicit targets or recompute an affected-target plan with `--base`.
   Unknown history falls back to all targets. Empty selections start no builds.
3. Prepare environments and compile with bounded concurrency (default two).
   Await materialized builds: constructing lazy graphs alone is not a barrier.
4. Once **all** preparations finish or fail, measure successful targets serially.
   A new ID invalidates measurement caching after the reusable build stages.
5. Save each success immediately under `targets/` in a new run directory. Record per-target failures,
   stage durations, exact source identity, driver hashes and resolved tooling.

No compilation from this suite overlaps its measurements. The scheduler cannot
prevent a second client or unrelated process using the same machine: the Argo
integration still needs a mutex shared by manual, scheduled and PR workflows, plus
an isolated engine/worker. A dedicated VM isolates execution from production apps;
it does not by itself guarantee stable physical CPU performance. Published cohorts
must retain the actual hardware identity and virtualization context.

The manual GitHub workflow now invokes the suite once, replacing one fresh hosted
job/engine per target. It uploads partial evidence even on failure. Hosted cache
persistence between runs is still absent; the persistent homelab engine supplies
that next benefit. A GitHub fallback is not the intended permanent benchmark host.

Registry-backed local developer calls remain compatible. Revision-bound calls build
from the resolved manifest's tooling, avoiding registry-name assumptions for new or
changed declarations. Nix image construction remains cached by Dagger. Builds
conservatively include rounds.txt because existing commands can copy it at build
time (Gleam does); cross-workload compilation reuse needs an explicit input contract.

## Workload policy

Use three kinds of work, with explicit purposes:

| Work | Trigger | Scope and intended outcome |
| --- | --- | --- |
| Contribution validation | An authorized PR revision | Only affected targets; short correctness cases, then optional same-workload before/after measurements. A few minutes warm is a goal, not a proven bound for every compiler. |
| Normal comparison | An approved reporting profile | Common round counts, stable samples and a time budget. Show coverage gaps and uncertainty; do not rank different workloads together. Tens of minutes is the goal to validate. |
| Extended comparison | Occasional/manual | Larger common-size points and the historical billion-round suite. It must not gate contributor feedback or website changes. |

The existing six-language calibration makes 100 million rounds a candidate reporting
size, not a final policy. Validate the slow tail and JIT behavior on the selected
reporting worker before adopting it. A tiny workload mostly measures startup for
fast implementations; show those measurements as validation, not rankings. The
10,000-round local checks below even contain Hyperfine-corrected zero durations,
which is evidence that they are too short for performance claims.

Longer is useful when it reveals steady-state behavior or reduces relative noise.
It is not automatically better when an already slow interpreter consumes minutes
per sample. Keep individual samples, report uncertainty, and do not claim a winner
when variability prevents a reliable distinction. Compare source revisions on the
same worker, round count and declared methodology. Optimized/math/SIMD variants stay
explicitly labelled rather than being silently mixed with scalar strict arithmetic.

`--measurement-timeout SECONDS` bounds each target's combined warmups and samples.
A timeout is a failed/incomplete target, never an invented duration or a silent
success. It is optional so historical manual reproductions retain their workload.
Profile-level publication eligibility still belongs to the publication validator;
these new execution bundles are marked ineligible by default.

## Results and website

Use the run bundle as the handoff contract first. Store immutable raw bundles in
object storage and make analysis/publication retryable without repeating execution.
The existing site can read a promoted snapshot during the transition. Only target
results go under `targets/`, with actual `rounds.txt` and `source-revision.txt`;
`run.json` and `catalog.json` stay outside the analyzer input. The current analyzer
can consume the bundle without rerunning benchmarks:

```sh
mkdir -p /path/to/local-report
uv run --locked python analyze.py --folder /path/to/run/targets \
  --rounds /path/to/run/targets/rounds.txt --out /path/to/local-report
```

This is local analysis, not authorization to promote a quick/subset report as latest.

Postgres can later index those bundles, and Astro can provide workload selectors,
methodology filters and history. Neither should become a dependency of benchmark
execution. A website change must not run benchmarks. Implement indexing and the
Astro view only after the common runner, event integration and calibrated profile
are working; the existing report/history remains available throughout.

## Current proof and next acceptance gates

Two local Go/Hare runs at 10,000 rounds used the same Git source revision and Dagger
0.19.8 engine. Preparation went from 20.59 seconds to 0.17 seconds; total
build/measurement phases went from 22.30 seconds to 2.36 seconds. Both runs produced
different measurement IDs and samples. These are a small integration experiment on
an ARM development machine with existing caches, not cold-start measurements, an
x86 performance comparison, or evidence that all 75 targets finish in seconds.
The phase durations exclude catalog resolution, connection and teardown.

The [saved evidence](validation/2026-09-06-unified-runner/README.md) retains both raw
runs. Scheduler tests cover the build barrier, bounded concurrency, serial timing,
partial failure retention, failed evidence writes and empty selections.

Remaining acceptance gates, in order:

1. Choose the isolated worker/engine boundary and persistent cache volume. Configure
   one queue/mutex, pinned engine/client versions and a private engine connection.
2. Run this same entry point through Argo twice; verify cache reuse and fresh samples,
   artifact retrieval, and absence of simultaneous timing jobs on the worker.
3. Connect revision-specific PR authorization and GitHub checks. Demonstrate one Go
   change selecting only its dependent targets and a docs change selecting none.
4. Validate/version the common reporting workload, then enable its schedule.
5. Add the independent result index and Astro presentation, preserving history.

The manual commit entry point is not automatic PR authorization. Run the driver
from reviewed code; never dispatch an unreviewed replacement driver with client
credentials. `--base` needs those Git objects available in the trusted local checkout
for AST planning; source execution itself uses the immutable Dagger Git tree.

## Four executions and weekly eligibility (September 6)

The shared protocol is now `leibniz-1w-3m-v1`: one warmup, three measured
executions, with correctness output captured in the warmup. That removes one of
five executions (20% of execution work), without reducing the measured sample
count. This is a protocol change, not evidence that close results are stable.
The historical billion-term reporting workload and raw baseline remain unchanged.

`python scripts/weekly_plan.py --checkpoint STATE.json --environment RUNNER.json`
compares the current source tree with the last **successfully published source**,
not the later publication commit. Source/compiler/tooling/scmeta changes trigger a
full weekly run; `site/` changes request a website check only. The environment JSON
must include a stable `runner_id` and measurement `profile`, plus actual image,
CPU/OS identity, limits, workload and isolation configuration. Any identity change
invalidates the checkpoint. Missing history fails closed to a full run.

After publishing, call the same command with `--published-revision FETCHED_SHA
--run-id TIMESTAMP`. It checks the committed raw suite before atomically advancing
state, requiring consistent `SourceRevision`, `MeasurementProfile` and
`RunnerEnvironmentSHA256` evidence. A failed or incomplete run cannot advance it.
The historical baseline lacks that identity and intentionally cannot seed a new
runner checkpoint. The Argo adapter must supply and verify this identity and hold
a global runner mutex; this CLI alone does not enable the suspended schedule.

Vercel builds the separate Astro site from immutable published snapshots. The
optional homelab Postgres archive indexes those snapshots for queries without
putting a database request in the public page path. Publication, archive import
and website deployment can each be retried without repeating measurements.

Pass the same `RUNNER.json` with `benchmark.py --runner-environment RUNNER.json`.
The runner checks required container-visible hardware/OS/cgroup fields, rounds,
container image, profile and timeout against actual result metadata before attaching
`RunnerEnvironmentSHA256`. A mismatch fails the suite. This observed contract
complements host isolation and the Argo mutex; it cannot prove that a shared host
was idle. `environment.cpuset_cpus_effective` records the effective cgroup CPU set.
