# Dagger Pipeline (Proof of Concept)

This directory contains an experimental Dagger-based pipeline to replace the existing Earthly build system. The goal is to improve build times, caching, and maintainability.

## Why Dagger?

| Aspect | Earthly | Dagger |
|--------|---------|--------|
| Language | Earthfile DSL | Python (or Go/TypeScript) |
| Caching | Layer-based | Content-addressed, more granular |
| Base images | Built each run | Pre-built, stored in registry |
| Package management | Alpine/Debian packages | Nix/Devbox (reproducible) |
| Parallelism | Limited | Native async |

### Key Benefits

1. **Pre-built container images**: Language runtimes are built once and pushed to GHCR, then pulled for benchmarks. No more rebuilding compilers every run.

2. **Nix/Devbox for packages**: Reproducible, pinned versions across all languages. Easy version updates via `languages.py`.

3. **Python pipeline**: Easier to maintain, test, and extend than Earthfile DSL.

4. **MicroPython for scmeta**: Lightweight (~908KB) metadata tool that runs in containers without needing a full Python runtime.

## Directory Structure

```
dagger-poc/
├── languages.py        # Language definitions (59 languages)
├── build_images.py     # Build and push container images
├── benchmark.py        # Run benchmarks
├── scmeta.py          # Metadata extraction (MicroPython-compatible)
├── check_versions.py   # Check for version updates
├── test_*.py          # Unit tests
└── pyproject.toml     # Python dependencies
```

## Quick Start

### Prerequisites

- [just](https://github.com/casey/just) (command runner)
- [uv](https://github.com/astral-sh/uv) (Python package manager)
- [Dagger CLI](https://docs.dagger.io/cli)
- Docker

### Install dependencies

```bash
cd dagger-poc
just install
```

### Commands

Run `just` to see all available commands:

```bash
just                    # Show all commands
just help               # Show quick start guide
```

**Benchmarking:**
```bash
just test rust go       # Quick test (10k iterations, local build)
just bench rust         # Full benchmark (1B iterations, local build)
just bench-registry rust  # Use pre-built images from registry
```

**Container Images:**
```bash
just build rust go      # Build images locally
just push rust go       # Build and push to registry
just build-dry-run      # Show what would be built
```

**Version Management:**
```bash
just check-versions     # Check all languages for updates
just check-versions rust  # Check specific language
```

**Development:**
```bash
just tests              # Run all tests
just list-langs         # List all available languages
just lang-info rust     # Show details for a language
```

## CI/CD Workflows

Two GitHub Actions workflows are available:

### `dagger-build-images.yml`

Builds and pushes container images to GHCR.

- **Triggers**: Push to `dagger-poc/languages.py` or `build_images.py`, manual dispatch
- **Runners**: Ubicloud (standard-2 for prep, standard-4 for builds)

### `dagger-benchmark.yml`

Runs benchmarks using pre-built images.

- **Triggers**: Push to `src/leibniz.*`, PR changes, manual dispatch
- **Commands**: `/dagger-bench rust go` in PR comments
- **Options**:
  - `languages`: Specific languages to benchmark
  - `quick_test`: Use 10k iterations
  - `use_local_images`: Build locally instead of pulling
  - `skip_cache`: Force fresh benchmarks

## Adding a New Language

1. Add entry to `languages.py`:

```python
"mylang": Language(
    name="MyLang",
    file="leibniz.ml",
    nixpkgs=("mylang@1.2.3",),  # Devbox package
    version_cmd="mylang --version",
    compile="mylang build leibniz.ml",
    run="./leibniz",
),
```

2. For languages not in Devbox, use Nix flakes:

```python
"mylang": Language(
    name="MyLang",
    file="leibniz.ml",
    nix_flake=("github:NixOS/nixpkgs/nixos-24.11#mylang",),
    nix_flake_version="1.2.3",  # For image tagging
    version_cmd="mylang --version",
    run="./leibniz",
),
```

3. Create source file: `src/leibniz.ml`

4. Test locally:
```bash
QUICK_TEST_ROUNDS=10000 USE_LOCAL_IMAGES=1 uv run dagger run python benchmark.py mylang
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     GitHub Actions                          │
├─────────────────────────────────────────────────────────────┤
│  build-images.yml          │  benchmark.yml                 │
│  ┌───────────────┐         │  ┌───────────────┐            │
│  │ build_images  │──push──►│  │   benchmark   │            │
│  │     .py       │         │  │      .py      │            │
│  └───────────────┘         │  └───────────────┘            │
│         │                  │         │                      │
│         ▼                  │         ▼                      │
│  ┌───────────────┐         │  ┌───────────────┐            │
│  │     GHCR      │◄───pull─┤  │   scmeta.py   │            │
│  │  (containers) │         │  │ (micropython) │            │
│  └───────────────┘         │  └───────────────┘            │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
                       ┌─────────────┐
                       │   results/  │
                       │  *.json     │
                       └─────────────┘
```

## Feedback

This is a proof of concept. Please share feedback in the tracking issue!
