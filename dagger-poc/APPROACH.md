# Dagger + Devbox Migration Approach

This document captures the complete context and approach for migrating the speed-comparison benchmark from the current Earthfile-based system to Dagger + Devbox.

## Problem Statement

The current system has several pain points:

1. **Complex Earthfile** (719 lines) - Hard to maintain and understand
2. **Fragile version management** - Targets Docker image tags, not language versions
3. **Inconsistent base images** - 29 different Docker base images
4. **Hard to contribute** - Adding a language requires understanding Earthly syntax and Docker

## Solution: Dagger + Devbox

### Why Dagger?

- Pipeline as Python code (familiar to contributors)
- Built-in caching at every layer
- Runs anywhere Docker runs
- Easy to test and debug

### Why Devbox?

- Single source for all language versions via nixpkgs
- Reproducible environments
- Explicit version pinning (`go@1.23.0` not `go@latest`)
- No need to maintain separate Docker images per language

### Why Python?

- Official Dagger SDK support
- Familiar to most developers
- Type hints + dataclasses for validation
- Can be linted, tested, and type-checked

## Architecture

```
dagger-poc/
├── languages.py      # Single source of truth for all language configs
├── benchmark.py      # Dagger pipeline
├── test_languages.py # Validation tests
├── pyproject.toml    # UV/Python project config
└── APPROACH.md       # This file
```

### languages.py

The `Language` dataclass defines everything needed to benchmark a language:

```python
@dataclass(frozen=True)
class Language:
    name: str              # Display name (e.g., "Rust")
    nixpkgs: list[str]     # Packages with versions: ["go@1.23.0"]
    file: str              # Source file: "leibniz.rs"
    run: str               # Run command: "./leibniz"
    compile: str | None    # Compile command (if compiled language)
    version_cmd: str | None # Version command (defaults to first pkg --version)
    base: str | None       # Base language for variants (for icon mapping)
    category: str          # Category: "compiled", "interpreted", etc.
    version_regex: str     # Regex to extract version number
```

**Key validation rules:**
- `file` must start with "leibniz."
- All `nixpkgs` must have explicit versions (`pkg@x.y.z`)
- Duplicate names are not allowed

**Utility functions for CI/tooling:**
- `get_all_versions()` - Returns dict of all language versions
- `get_package_info(target)` - Returns package details for a target
- `extract_version(target)` - Parses version from output

### benchmark.py

The Dagger pipeline:

1. **Build scmeta** (cached) - Crystal tool that combines hyperfine output with metadata
2. **For each language:**
   - Start from `jetpackio/devbox:latest` base image
   - Add required nixpkgs + hyperfine
   - Copy source file and rounds.txt
   - Compile if needed
   - Run hyperfine benchmark
   - Run scmeta to generate result JSON
3. **Output results** to `results/<target>.json`

### test_languages.py

Pytest tests validating:
- Source files exist
- nixpkgs have explicit versions
- No duplicate names
- Base language is set for variants
- Run commands reference correct binaries
- etc.

## Current Status

### Completed

- [x] Created `languages.py` with 39 language definitions
- [x] Created `benchmark.py` Dagger pipeline with Docker fallback support
- [x] Created `test_languages.py` with 19 validation tests
- [x] Fixed scmeta to build inside container (cached)
- [x] Fixed version commands running inside devbox
- [x] Tested successfully: Go, Rust, Python, Odin, Crystal, Perl, Lua, LuaJIT, Haskell, Nim, PHP, Ruby, Node.js, V, Fortran, Zig, Java, C#
- [x] Implemented Docker fallback for Java and C# (ARM64 SIGILL issues with nixpkgs)
- [x] Fixed package versions (lua5_4, luajit, gfortran, zig, scala-cli, sbcl, racket, etc.)
- [x] Created GitHub Actions workflow template (`benchmark-workflow.yml`)

### Smart Docker Fallback

Some languages have issues when running Devbox containers locally on macOS due to Rosetta/QEMU emulation. The system **automatically detects the environment** and chooses the right approach:

| Environment | Java / C# | Reason |
|-------------|-----------|--------|
| **Local macOS** | Docker fallback | nixpkgs binaries crash under emulation |
| **CI (Linux)** | nixpkgs/Devbox | Native Linux, no emulation issues |

The detection works via environment variables:
```python
IN_CI = os.environ.get("CI") == "true" or os.environ.get("GITHUB_ACTIONS") == "true"
USE_DOCKER_FALLBACK = IS_MACOS and not IN_CI
```

**Docker fallback images used on macOS:**
| Language | Docker Image |
|----------|--------------|
| Java | `eclipse-temurin:21-alpine` |
| C# | `mcr.microsoft.com/dotnet/sdk:8.0` |

### Pending Tasks

1. **Add version update automation** - Script to check for new versions in nixpkgs
2. **Add Swift support** - May need Docker fallback (not well supported in nixpkgs)
3. **Add remaining language variants** - SIMD versions, additional interpreters

## How to Run

```bash
cd dagger-poc

# Install dependencies
uv sync

# Run tests
uv run pytest

# Benchmark a single language
uv run dagger run python benchmark.py rust

# Quick test with fewer iterations
QUICK_TEST_ROUNDS=10000 uv run dagger run python benchmark.py go

# Benchmark multiple languages
uv run dagger run python benchmark.py rust go python

# Benchmark all languages
uv run dagger run python benchmark.py
```

## Adding a New Language

1. Add entry to `LANGUAGES` dict in `languages.py`:

```python
"mylang": Language(
    name="MyLang",
    nixpkgs=["mylang@1.2.3"],  # Find version: devbox search mylang
    file="leibniz.ml",
    run="./leibniz",           # or "mylang leibniz.ml"
    compile="mylangc leibniz.ml -o leibniz",  # if compiled
    category="compiled",       # or "interpreted", "jit", "vm"
),
```

2. Create source file `src/leibniz.ml` implementing Leibniz formula

3. Run tests: `uv run pytest`

4. Test benchmark: `QUICK_TEST_ROUNDS=10000 uv run dagger run python benchmark.py mylang`

## Edge Cases: Smart Docker Fallback

Some languages don't work when running Devbox containers locally on macOS due to emulation issues. The system now **automatically detects the environment** and switches between nixpkgs and Docker:

```python
# Environment detection in languages.py
IN_CI = os.environ.get("CI") == "true" or os.environ.get("GITHUB_ACTIONS") == "true"
USE_DOCKER_FALLBACK_FOR_JVM = IS_MACOS and not IN_CI
USE_DOCKER_FALLBACK_FOR_DOTNET = IS_MACOS and not IN_CI

# Conditional configuration
"java": Language(
    name="Java",
    nixpkgs=["jdk@21.0.5"] if not USE_DOCKER_FALLBACK_FOR_JVM else [],
    file="leibniz.java",
    compile="javac leibniz.java",
    run="java leibniz",
    version_cmd="java --version",
    base="java",
    category="jvm",
    docker_image="eclipse-temurin:21-alpine" if USE_DOCKER_FALLBACK_FOR_JVM else None,
    docker_setup="apk add --no-cache hyperfine" if USE_DOCKER_FALLBACK_FOR_JVM else None,
),
```

**How it works:**
- **Local macOS**: `USE_DOCKER_FALLBACK_*` is `True` → uses Docker images
- **CI (Linux)**: `USE_DOCKER_FALLBACK_*` is `False` → uses nixpkgs via Devbox

The `Language` dataclass has these Docker-related fields:
- `docker_image`: The Docker image to use (e.g., `eclipse-temurin:21-alpine`)
- `docker_setup`: Shell commands to run after starting container (install hyperfine, etc.)
- `uses_docker_fallback`: Property that returns `True` if `docker_image` is set

The pipeline automatically detects which mode to use and runs the appropriate container setup.

## Integration with Existing Tools

### analyze.py

The existing `analyze.py` reads JSON files from `results/` directory. Our pipeline outputs the same format, so it works unchanged.

### scmeta

We build scmeta inside a Crystal container (cached by Dagger). The binary is then copied into each benchmark container.

### GitHub Pages

Results are published via existing `publish.py` which reads from `results/`. No changes needed.

## Future Improvements

1. **Parallel benchmarks** - Run multiple languages concurrently (careful with CPU contention)
2. **Pre-built devbox images** - Build per-language Devbox images for faster startup
3. **Version check automation** - Query nixpkgs for newer versions, auto-create PRs
4. **Result caching** - Skip benchmarks if source + version unchanged
5. **Comparison mode** - Compare results between branches/commits

## Design Decisions

### Why explicit version pins?

Reproducibility. `go@latest` means different things on different days. `go@1.23.0` is always the same.

### Why `base` parameter?

For variants like `rust-lto`, `python-pypy`, the icon/grouping should map to the base language (`rust`, `python`).

### Why frozen dataclass?

Immutability prevents accidental modifications. Language configs shouldn't change after definition.

### Why compile scmeta in container?

The local scmeta binary was outdated (2020) and missing `--target-name` flag. Building inside container ensures we always have the latest version and it's cached by Dagger.

## Debugging Tips

1. **Check nixpkgs version**: `devbox search <package>` or check https://search.nixos.org/packages
2. **Test devbox locally**: Create temp `devbox.json` with the packages and run commands
3. **Dagger logs**: Set `log_output=sys.stderr` in Dagger config
4. **Quick iterations**: Use `QUICK_TEST_ROUNDS=10000` for fast testing
5. **Container shell**: Add `.with_exec(["sh"])` to debug inside container
