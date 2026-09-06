# Source and suite-clock integration check

A real local Dagger 0.19.8 Go smoke run on September 6, 2026, using 10,000 terms
and the working-tree driver. This is an ARM Docker development engine with existing
caches; it is functional evidence, not a published performance comparison.

`run.json` retains the actual 1.58965-second invocation clock, including connection
and teardown, separately from its build/measurement phase totals. `go.json` retains
one warmup, three measured samples, first-warmup output capture, explicit null
(no configured measurement timeout), SDK/engine version, source text/checksum,
Devbox packages and observed environment including OS/libc and resource limits.
Unexposed ARM CPU model/cores remain empty; no reporting identity was invented.

Analyzing `targets/` discovered the sibling run record and checked its suite ID,
source and target set. Local publication then preserved the same elapsed value,
original source text and engine version in a temporary published snapshot.
No benchmark results were promoted to the public ranking.

The 157 Python tests, six site/archive tests, four desktop/mobile browser checks
and 192-page static build passed. Browser checks compare the displayed highlighted
Go source with the recovered file exactly, including its terminal newline. All
historical snapshot bytes remain unchanged; recovered baseline context and source
live separately under `docs/report-evidence/`.
