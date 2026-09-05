# Nix migration and repository overview

Snapshot: 2026-09-05. Integration branch: `codex/complete-nix-migration`.
Base: `codex/dagger-parity-repro`, merged with `master` at `8917f3c`.

## Project map

| Area | Responsibility |
| --- | --- |
| `src/` | Single-threaded Leibniz implementations and separate optimized variants |
| `dagger-poc/languages.py` | Nix/Devbox packages, setup, compile/run commands, variant names |
| `dagger-poc/native.py` | Restricted-container execution for homelab Argo |
| `dagger-poc/benchmark.py` | Optional portable Dagger/Docker execution |
| `dagger-poc/scmeta.py`, `result_metadata.py` | Hyperfine output, accuracy, sanity checks, methodology metadata |
| `analyze.py` | Combined CSV/JSON, chart, hardware metadata |
| `publish.py` | Timestamped history, latest result links, manifest |
| `docs/index.html` | Interactive historical results, CSV download, methodology details |
| `scripts/compare_results.py` | Migration comparison diagnostics; hardware must match for performance claims |
| `Earthfile`, `scmeta/` | Legacy Earthly/Crystal implementation, retained as a reference |
| `.buildkite/` | Previous migration-validation adapter, superseded by homelab Argo |
| Homelab `cluster/apps/speed-comparison/` | Argo execution, restricted namespace, artifacts, suspended weekly schedule |

The migration branch contained toolchain fixes but was eight commits behind master.
Those commits are integrated, preserving Swift improvements, Octave, Numba, Chez,
SBCL SIMD, and the CSV download button. The old bundled-updates branch is not used:
its tree predates several of those changes.

## Open PRs

Changes below are integrated locally; GitHub PRs remain open until review and
validation are complete. Credit remains with the original contributors linked here.

| PR | Disposition |
| --- | --- |
| [#295 Odin](https://github.com/niklas-heer/speed-comparison/pull/295) | Ported source and optimization flags; native cluster smoke passed |
| [#296 Rust variants](https://github.com/niklas-heer/speed-comparison/pull/296) | Added fast-math and AVX-512 targets using the migration's Rust pin; hardware guard added; native smoke passed |
| [#298 Gleam 1.14](https://github.com/niklas-heer/speed-comparison/pull/298) | Already present in migration configuration; native smoke passed |
| [#299 V](https://github.com/niklas-heer/speed-comparison/pull/299) | Ported implementation; retained pinned toolchain instead of runtime latest-tag lookup; native smoke passed |
| [#300 Elixir](https://github.com/niklas-heer/speed-comparison/pull/300) | Private helper functions ported; native smoke passed |
| [#301 requests](https://github.com/niklas-heer/speed-comparison/pull/301) | Lock updated to 2.33.0 |
| [#302 pygments](https://github.com/niklas-heer/speed-comparison/pull/302) | Lock updated to 2.20.0 |
| [#303 pytest](https://github.com/niklas-heer/speed-comparison/pull/303) | Lock updated to 9.0.3; regression suite passes |
| [#304 Hare](https://github.com/niklas-heer/speed-comparison/pull/304) | Ported; fixed hardcoded ten-character rounds parsing and release/output flags; native smoke passed, including seven rounds |
| [#305 urllib3](https://github.com/niklas-heer/speed-comparison/pull/305) | Lock updated to 2.7.0 |
| [#306 idna](https://github.com/niklas-heer/speed-comparison/pull/306) | Lock updated to 3.15 |
| [#314 Zig SIMD](https://github.com/niklas-heer/speed-comparison/pull/314) | Added distinct Nix target/display name, upgraded both targets to 0.16, fixed small-round underflow; native smoke passed |

## Open issues

| Issue | Work and remaining evidence |
| --- | --- |
| [#313 floating-point policy](https://github.com/niklas-heer/speed-comparison/issues/313) | Maintainer chose to retain optimized variants with explicit math/SIMD labels. Added metadata and convergence checks; bitwise equality is not the benchmark policy |
| [#312 OCaml timing](https://github.com/niklas-heer/speed-comparison/issues/312) | Compile with `ocamlopt -O3`; native smoke passes. Fresh full timings on recorded hardware are still required; no fixed C/OCaml speed ratio is promised |
| [#308 activity](https://github.com/niklas-heer/speed-comparison/issues/308) | Migration and contribution documentation updated; a maintainer response can reference this work after review |
| [#307 NASM](https://github.com/niklas-heer/speed-comparison/issues/307) | Added scalar Linux x86_64 target reading rounds.txt, respecting the ABI, printing 16 decimals; native smoke passed |
| [#297 contribution questions](https://github.com/niklas-heer/speed-comparison/issues/297) | Compile commands are in languages.py; new transpilers need a reproducible compiler/runtime setup, not their own Docker image. Optimizations can be proposed under the published rules; no merge-time promise |
| [#261 Odin](https://github.com/niklas-heer/speed-comparison/issues/261) | Covered by the #295 port; native smoke passed |
| [#260 Kotlin/Native](https://github.com/niklas-heer/speed-comparison/issues/260) | Added separate POSIX implementation; writable compiler cache and hash-verified libffi compatibility fix; native smoke passed |
| [#253 F# SIMD](https://github.com/niklas-heer/speed-comparison/issues/253) | Added separate Vector512 target with hardware guard and scalar tail; native smoke passed |
| [#148 Mojo](https://github.com/niklas-heer/speed-comparison/issues/148) | Added scalar Mojo 1.0.0 with complete file I/O and 16-decimal output. Compiler wheels and dependencies are SHA-256 locked; ELF interpreter/runtime use Nix libraries. Native smoke and seven-round check passed |

Mojo's [official installation guide](https://mojolang.org/install/) supports Python
packages. The compiler and Mojo libraries are pinned with wheel hashes in
`src/mojo-requirements.txt`. Its scalar baseline uses division rather than the
original issue snippet's approximate reciprocal instruction.

## Validation evidence

All **75 default targets** passed native 1,000-round smoke checks on the homelab
worker. [The incremental smoke report](validation/2026-09-05-native-smoke.json)
records outputs; these short timings are not rankings. Ten affected SIMD/native
targets also passed seven-round boundary checks. NumPy, vectorized Octave, and R
use bounded batches and passed 1,000,001-round checks; scalar Octave, MicroPython,
Raku, Python, and Ruby passed the same pilot.

The first Argo workflow `speed-comparison-manual-vmv76` passed checkout, native
Python execution, S3 upload, and analysis. Its combined archive was downloaded
to verify retrieval. Local Dagger Python execution also passed after changing
both adapters to use command files, preserving shell variable expansion.
The current Python regression suite has 84 passing tests.

Homelab PRs [#34](https://github.com/niklas-heer/homelab/pull/34) and
[#35](https://github.com/niklas-heer/homelab/pull/35) deployed the workflow,
fixed the Argo controller's archive connection deadlock, and added guarded
publication. The old Buildkite webhook is disabled. GitHub Actions now does
lightweight validation, with optional manual Dagger execution.

## Completion gates

- [x] Pull latest master and integrate the migration branch without losing newer changes.
- [x] Replace automatic Ubicloud benchmark jobs with lightweight GitHub validation.
- [x] Add restricted native Devbox execution and methodology/result checks.
- [x] Test the first group of implementations on native homelab x86_64.
- [x] Pass a native smoke test for every default target, including older toolchains.
- [x] Validate Argo checkout, execution, artifact storage, and analysis end to end.
- [ ] Finish reproducible result publication; quick/subset results must never replace latest.
- [ ] Run a complete full-round benchmark on recorded homelab hardware.
- [ ] Enable the weekly Argo schedule after successful validation.
- [ ] Review/merge the integration and settle the linked GitHub PRs/issues.

Rust's portable SIMD target is retained with an immutable Fenix commit and a
scalar tail for short inputs. The old `pony-nightly` target actually used the
moving `ponylang/ponyc:alpine` tag with the same source and flags as Pony. That
duplicate moving-image target is retired; pinned `pony` covers the language.
Historical results remain intact. Native smoke tests validate operation, not
published performance rankings.
