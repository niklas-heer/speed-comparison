# Python, Go and the Dagger pipeline

Measured 2026-09-05 against source `c71e4cd81464b238172605982806c5bf827a70a1`.
Keep Python declarations and the Python runner for now. Go reduces client overhead,
but does not accelerate the same compiler or Leibniz executable inside the engine.

Both SDKs and the local engine were pinned to 0.19.8. The host was an Apple M2 Pro,
with Python 3.14.7 and Go 1.26.6. Values below are medians of five runs per client,
after one warmup each, alternating order. These are synthetic orchestration-heavy
measurements, not a representative end-to-end language suite or x86 benchmark.

| Operation | Python seconds | Go seconds |
| --- | ---: | ---: |
| Connect to engine | 0.508 | 0.271 |
| Construct 75 lazy graphs of 20 environment operations | 0.484 | 0.000605 |
| 375 sequential cached file-content reads | 1.218 | 0.326 |
| Five forced fresh trivial container executions | 0.754 | 0.712 |
| Whole process, including startup and teardown | 3.455 | 1.566 |

The synthetic process is faster in Go; fresh execution differs by only about
0.04 seconds here. This does not establish a twofold improvement for the full
pipeline. The native Argo baseline did not use either Dagger SDK. Its 75 medians
sum to about 28 minutes for one execution per target, with six executions plus
setup making the full run take over five hours. Workload sizing, avoiding repeated
setup and selecting affected languages dominate the opportunity.

Retain Python's typed `Language` objects, shared constants, editor support and tests.
An experimental JSON export round-tripped all 75 dataclass instances without loss;
a later Go driver could consume that generated, revision-bound model while keeping
the authoring format. There is no need to maintain a second manual catalog.

KDL is a separate contributor-experience decision. It can represent packages,
sources, commands and methodology with comments and multiline strings, but shared
profiles, validation and defaults require an explicit schema. It also requires a
comment-preserving version updater and parity tests for all existing targets.
KDL does not make benchmark execution faster. Reconsider it after the common Dagger
path works, using representative simple and complex entries to assess usability.

The [architecture and rollout gates](pipeline-architecture.md) prioritize persistent
caches, serial fresh timing, affected-target checks and measured workload calibration.
Only reconsider a runner rewrite if profiling that actual pipeline shows substantial
remaining client overhead.
