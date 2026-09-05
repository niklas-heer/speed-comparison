---
title: "From a table of timings to an inspectable experiment"
date: "2026-09-06"
category: "Project update"
summary: "Why package provenance, honest limitations and fast contribution checks belong together."
---
The useful question is more specific than “which language is fastest?” It is “what ran, under which conditions, and can I inspect it?” The refreshed site makes those conditions part of the result.

## What changed

The Nix migration puts language environments behind Python declarations, with recorded package versions, resolved revisions and store paths. Result pages now expose that evidence alongside compiler flags, math behavior, explicit SIMD, raw samples and the original source revision.

The September 5 baseline includes 75 implementations. It remains a historical native Devbox run on a shared virtual machine. We have preserved the original measurements, rather than presenting a later pipeline change as new benchmark evidence.

## Why Dagger stays

Dagger is the common execution layer. Argo will schedule it, and developers can use the same runner locally. Preparing environments concurrently and measuring sequentially keeps build work away from the timed phase. Cached preparation avoids paying the same setup cost for every invocation; fresh execution IDs prevent cached timings.

A small Go SDK experiment reduced orchestration overhead by seconds. The long part is the workload itself, especially the slower implementations. Rewriting the Python catalog would not address that bottleneck.

## A better balance

Contribution feedback should focus on the language variants a change affects. The next protocol starts with one warmup and three measured runs, while keeping the historical billion-term reporting workload until calibration justifies changing it. Weekly full reports should run only after relevant changes; successful website builds should not trigger hours of benchmarking.

These are separate stages of the rollout. The common Dagger runner is implemented. Isolated runner allocation and live Argo scheduling are still pending. Fewer repetitions are not a promise that tiny differences become trustworthy: noisy results require more measurements and investigation.

## Thank you

This migration incorporates contributions and feedback on Odin, Rust, Gleam, V, Elixir, Hare, Zig SIMD, OCaml and floating-point policy, alongside dependency and tooling updates. The questions about fairness and reproducibility helped shape this direction. [The repository](https://github.com/niklas-heer/speed-comparison) remains the place to discuss improvements and contribute implementations.
