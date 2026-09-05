# OCaml timing control (issue #312)

Both runs used source `c71e4cd81464b238172605982806c5bf827a70a1`, one billion
rounds and the same homelab worker (`slick-badger`, virtualized AMD EPYC-Genoa,
Talos kernel 6.18.38). GCC was 15.2.0 and OCaml 5.3.0. Each target had two warmups,
three measured executions and a separate output execution. Timing was serial.

| Configuration | Median seconds | Math policy |
| --- | ---: | --- |
| Published C, native tuning/LTO/reassociation | 0.157898 | relaxed |
| Control C, `gcc -O3 -o leibniz leibniz.c -lm` | 0.725377 | compiler default |
| Control OCaml, `ocamlopt -O3 -o leibniz leibniz.ml` | 1.110856 | compiler default |
| Published OCaml, same `ocamlopt -O3` command | 1.136967 | compiler default |

The control OCaml/C ratio is **1.53**, compared with **7.20** for the published
optimized C/OCaml medians. The issue's plain `gcc -O3` command differs from the
published C command. Changing that flag set accounts for much of the gap on this
worker; these experiments do not isolate the effect of each individual flag.
They do not promise a fixed language ratio on another machine or compiler version.
The runs happened sequentially on the same VM, not on noise-free dedicated hardware.

The published optimized variants remain valid under the maintainer's chosen policy,
with explicit math/SIMD metadata. This control is diagnostic evidence and must not
replace the full public dataset. Raw control files retain every Hyperfine sample,
output check, compiler command and resolved Devbox environment. The full dataset is
in `docs/history/2026-09-05T193245/combined_results.json`.

To reproduce the control, check out the source revision above and use the pinned
native environment as usual, replacing only the C declaration's `compile` value
with `gcc -O3 -o leibniz leibniz.c -lm` before calling `native.run` for C and OCaml.
Use the same billion-round input and serial execution on the same worker. Do not
publish this two-target result as a complete report. The workflow and recovered
artifact identities are recorded in `provenance.json`.
