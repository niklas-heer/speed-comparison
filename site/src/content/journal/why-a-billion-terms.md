---
title: "A billion terms. Three measurements. Why?"
date: "2026-09-06"
category: "Methodology"
summary: "The work inside one execution, the reason to repeat it, and how focused contribution checks avoid a full-suite wait."
---
A billion terms sounds like a billion benchmark runs. It is not. There are two different counts in this experiment, and they solve different problems.

## Terms are the workload; executions are the samples

Each program approximates π by adding terms of the Leibniz series. **One billion terms** is the amount of numerical work requested in a single execution. SIMD and paired-term implementations may process more than one term per loop iteration, so “terms” is more precise than “loop iterations.”

**Three measured executions** means starting that program three times and retaining three elapsed times. The website and refreshed image show the median of those samples. Their fastest-to-slowest range is observed spread, not a confidence interval. We retain the individual values so readers can inspect how much they disagree.

The [September 5 baseline](/runs/2026-09-05T193245/) used two warmups, three measured executions and one separate execution to capture π. That made **six executions per implementation**. The new Dagger protocol captures the output during its one warmup, then measures three executions: **four total**. It removes redundant work; it does not establish that three samples resolve every close result.

## Why make an execution long?

A very short program can spend much of its time starting a runtime, loading libraries or launching a shell. Increasing the numerical workload can make the computation easier to distinguish from those fixed costs. But longer is not automatically more informative: an interpreter already taking minutes per sample may just keep us waiting.

Hyperfine supports repeated measurements and unmeasured warmups. It also corrects for shell startup overhead, which can create substantial relative noise for very fast commands. Our tiny local checks have even produced corrected zero durations. Those checks establish functionality; they are unsuitable for a performance ranking. See [Hyperfine’s measurement guidance](https://github.com/sharkdp/hyperfine#intermediate-shell).

Warmups are separate process launches. They can warm filesystem caches, but **they do not keep a JIT compiler’s state alive in the next process**. JIT compilation and runtime startup can still be part of each measured execution. A longer input might reach different behavior within that process; this must be checked with scaling measurements rather than assumed.

One billion remains our historical reference workload. A six-language calibration made 100 million terms a candidate for routine reports, but the slow tail and runtime behavior still need validation on the isolated reporting worker. We will compare implementations at the same workload and keep different hardware or protocols in separate cohorts. More samples or further investigation are needed when spread obscures a difference; [pyperf’s guidance on unstable benchmarks](https://pyperf.readthedocs.io/en/latest/analyze.html#benchmark-stability) explains why summary numbers alone can mislead.

## Where the five hours went

The original Argo workflow took **5h 18m 46s** from checkout to termination. That includes environment setup, compilation, all 75 targets, analysis and a failed publication attempt. Publication was later retried successfully without re-running measurements. The [retained workflow timestamps](/report-evidence/2026-09-05T193245/workflow-timing.json) document that scope.

That clock is different from any one program’s median. The sum of all 75 medians was about **28.05 minutes for one execution of each implementation**. Scalar Octave contributed roughly **59.7%** of that sum; the five slowest targets contributed about **82%**. These are workload estimates derived from samples, not elapsed workflow times.

Persistent caches can avoid repeat package installation and compilation. They cannot eliminate the numerical work. Keeping build preparation separate from serial measurement also avoids measuring a program while another compiler competes for its CPU.

## A contribution should not wait for all 75

Nix identifies the dependencies for each declared language environment. The affected-target planner then selects implementations whose source, supporting files or compiler configuration changed. For example, a Go source-only change selects Go; a shared source file selects every variant using it. Shared execution changes legitimately select the whole suite. Website-only changes get website checks.

The common Python Dagger runner already supports explicit targets and revision-based selection. For a quick local check:

```sh
QUICK_TEST_ROUNDS=10000 USE_LOCAL_IMAGES=1 \
  uv run --locked --project dagger-poc python dagger-poc/benchmark.py go
```

Ten thousand terms here is a correctness smoke test, not a replacement ranking. The runner can also compare an authorized full source SHA with `--base BASE_SHA` and select affected targets automatically. Unknown Git history falls back to all targets so changes are not silently missed.

Live homelab dispatch still awaits an isolated Dagger worker. Once that boundary is validated, the intended routine is fast checks of affected languages, plus a full report at most weekly **when benchmark-relevant inputs changed since the last successful publication**. Failed runs do not advance that checkpoint. A new website layout or a retry of the archive never needs another billion-term suite.
