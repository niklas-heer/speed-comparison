# CLAUDE.md

This file provides context for AI assistants working on this project.

## Project Overview

A benchmark comparing 40+ programming languages using the Leibniz formula for calculating π. Results are published to GitHub Pages with historical tracking.

## Key Files

- `Earthfile` - Build definitions for all languages (Earthly build system) - **legacy**
- `dagger-poc/` - Dagger + Devbox pipeline (primary build system)
- `analyze.py` - Generates charts and CSV from benchmark results
- `publish.py` - Publishes results to `docs/history/` with timestamped folders
- `src/` - Source implementations (`leibniz.*` files)
- `src/rounds.txt` - Number of iterations (currently 1 billion)
- `scmeta/` - Crystal tool that wraps hyperfine and extracts metadata
- `docs/` - GitHub Pages site with interactive results viewer

## Build Systems

### Legacy: Earthly

Uses [Earthly](https://earthly.dev/) with Docker. Each language has a target in `Earthfile`.

### Primary: Dagger + Devbox (in `dagger-poc/`)

A Python-based pipeline using [Dagger](https://dagger.io/) and [Devbox](https://www.jetpack.io/devbox/) for better maintainability and reproducibility.

**Why Dagger + Devbox?**
- Pipeline as Python code (easier to maintain than 700+ line Earthfile)
- Reproducible via Nix/Devbox with explicit version pinning
- Type-safe language configuration with validation
- Built-in caching at every layer

**Key files in `dagger-poc/`:**
- `languages.py` - Single source of truth for all language configs
- `benchmark.py` - Dagger pipeline for running benchmarks
- `build_images.py` - Builds and pushes container images to GHCR
- `check_images.py` - Checks which images exist in the registry
- `check_versions.py` - Checks for package version updates
- `detect_changes.py` - Detects which languages need rebuilding (for CI)
- `justfile` - Command runner with all common operations
- `scmeta.py` - MicroPython-compatible metadata extraction
- `fly/` - Fly.io remote builder configuration

```bash
# Test a single language
earthly +<language>      # e.g., earthly +rust, earthly +python

# Quick local testing with fewer rounds (much faster)
earthly --build-arg QUICK_TEST_ROUNDS=10000 +<language>

# Run all benchmarks
earthly +collect-data

# Run analysis only
earthly +analysis
```

## Adding a New Language

### Using the Dagger Pipeline (Preferred)

1. Add entry to `dagger-poc/languages.py`:

```python
"mylang": Language(
    name="MyLang",
    nixpkgs=("mylang@1.2.3",),  # Find version: devbox search mylang
    file="leibniz.ml",
    run="./leibniz",           # or "mylang leibniz.ml"
    compile="mylangc leibniz.ml -o leibniz",  # if compiled (omit for interpreted)
    category="compiled",       # or "interpreted", "jit", "vm"
),
```

2. Create source file: `src/leibniz.ml`

3. Run tests: `cd dagger-poc && uv run pytest`

4. Test benchmark: `cd dagger-poc && just test mylang`

5. If test passes, build and push the image: `cd dagger-poc && just push mylang`

### Using Legacy Earthfile

1. Create source file: `src/leibniz.<ext>`
2. Add Earthfile target following this pattern:
   ```
   mylang:
     FROM <base-image>
     DO +PREPARE_ALPINE  # or +PREPARE_DEBIAN
     DO +ADD_FILES --src="leibniz.<ext>"
     RUN --no-cache <compile-command>  # if compiled
     DO +BENCH --name="mylang" --lang="MyLang" --version="<version-cmd>" --cmd="<run-cmd>"
   ```
3. Add `BUILD +mylang` to the `collect-data` target
4. Add version source to `scripts/version-sources.json`:
   ```json
   "mylang": {
     "source": "docker",           // or "github", "alpine", "apt"
     "image": "mylang",            // Docker image name (for docker source)
     "repo": "org/repo",           // GitHub repo (for github source)
     "package": "mylang",          // Package name (for alpine/apt source)
     "earthfile_pattern": "mylang:(\\d+\\.\\d+)-alpine",  // Regex to extract current version
     "tag_filter": "^\\d+\\.\\d+-alpine$",                // Filter for Docker tags
     "source_file": "leibniz.<ext>"
   }
   ```
5. Test locally before committing

## Benchmark Rules

The benchmark measures **single-threaded computational performance**:

1. **No concurrency/parallelism** - Implementations must be single-threaded. No multi-threading, async, or parallel processing.
2. **SIMD is allowed but separate** - SIMD optimizations should be separate targets (e.g., `swift-simd`, `cpp-avx2`, `java-vecops`), not replacing standard implementations.
3. **Standard language features** - Use idiomatic code. Compiler optimization flags are fine. Auto-vectorization hints like `@simd` in Julia are OK (similar to `-march=native`).
4. **Same algorithm** - All implementations must use the Leibniz formula.

**Why no concurrency?** Results depend on core count, making comparisons meaningless across different hardware.

## Conventions

- **Prefer Alpine** over Debian/Fedora for smaller images and consistency
- **Avoid Fedora** - not currently supported (hyperfine download is arch-specific)
- **Version commands** that output to stderr may need `echo X.Y.Z` workaround
- **Version format for scmeta** - The version parser regex requires at least one decimal point. Use `echo 21.0.0` not `echo 21`
- **Don't break vectorization** - C/C++ code is optimized; variable declarations inside loops enable auto-vectorization
- **SIMD variants** should be separate targets (e.g., `swift-simd`, `cpp-avx2`)
- **Verify Docker image tags** - Always verify exact tags exist (e.g., `haskell:9.10-slim` doesn't exist, use `haskell:9.10-slim-bullseye`)

## CI/CD

GitHub Actions workflow (`.github/workflows/ci.yml`):
- Matrix builds run each language in parallel
- `scmeta` binary is built once and shared via artifacts
- Results are cached based on source file + Earthfile target + rounds.txt hash
- Caches invalidate when any of these change

### PR Benchmark Command

Trusted contributors (those who have had PRs merged) can trigger benchmarks on PRs using `/bench <target> [target2] ...`. Use Earthfile target names (e.g., `rust`, `cpython`, `nodejs`), not file extensions.

### Automated Version Updates

The `.github/workflows/version-check.yml` workflow runs daily at 6 AM UTC to detect and update language versions:

- **Version sources**: Configured in `scripts/version-sources.json`
- **Version checker**: `scripts/check-versions.py` queries Docker Hub, GitHub Releases, and Alpine package APIs
- **AI-powered updates**: Uses Claude Code via OpenRouter to update Earthfile and fix breaking changes
- **Models**: Haiku 4.5 for simple version bumps, Opus 4.5 for fixing breaking changes (up to 3 attempts)
- **Prompts**: Located in `.github/prompts/` directory

To manually trigger a version check:
```bash
gh workflow run version-check.yml
gh workflow run version-check.yml -f language=rust  # Check specific language
gh workflow run version-check.yml -f dry_run=true   # Check without creating PRs
```

## Common Issues

- **Zig 0.15 breaking changes**: `@intToFloat` → `@floatFromInt`, `.Optimized` → `.optimized`, new stdout API
- **ARM vs x86**: Some PREPARE functions download arch-specific binaries
- **Version parsing**: scmeta expects version number in stdout; some tools output to stderr
- **Alpine packages**: Some languages aren't in Alpine repos; check `alpine:edge` testing repo or use Debian

## Dagger Pipeline (dagger-poc/)

### Prerequisites

- [just](https://github.com/casey/just) - Command runner
- [uv](https://github.com/astral-sh/uv) - Python package manager
- [Dagger CLI](https://docs.dagger.io/cli)
- Docker

### Just Commands Reference

Run `just` or `just help` in `dagger-poc/` to see all commands.

**Benchmarking:**
```bash
just test rust go          # Quick test (10k iterations, local build)
just bench rust            # Full benchmark (1B iterations, local build)
just bench-registry rust   # Use pre-built images from GHCR
```

**Container Images:**
```bash
just build rust go         # Build images locally
just push rust go          # Build and push to registry
just build-dry-run         # Show what would be built
```

**Version Management:**
```bash
just check-versions        # Check all languages for updates
just check-versions rust   # Check specific language
just check-versions-unstable rust  # Include unstable/preview versions
```

**Development:**
```bash
just tests                 # Run all tests
just list-langs            # List all available languages
just lang-info rust        # Show details for a language
just count-langs           # Count total languages
just install               # Install dependencies (uv sync)
just clean                 # Clean Python cache
```

### Language Configuration

Languages are defined in `dagger-poc/languages.py` using a `Language` dataclass:

```python
@dataclass(frozen=True)
class Language:
    name: str              # Display name (e.g., "Rust")
    file: str              # Source file: "leibniz.rs"
    run: str               # Run command: "./leibniz"
    compile: str | None    # Compile command (if compiled)
    version_cmd: str | None # Version command (defaults to primary package)
    base: str | None       # Base language for icon mapping (e.g., "python" for "cpython")
    category: str          # "compiled", "interpreted", "jit", "vm", etc.
    version_regex: str     # Regex to extract version (default: r"(\d+\.\d+\.?\d*)")
    nixpkgs: tuple[str, ...] = ()  # Devbox packages: ("go@1.23.4",)
    nix_setup: str | None = None   # Post-install setup commands
    allow_insecure: bool = False   # Allow insecure packages (e.g., haxe needs mbedtls)
```

**Key validation rules:**
- All `nixpkgs` must have explicit versions (`pkg@x.y.z`, not `pkg@latest`)
- `file` must start with "leibniz."
- Must have at least one package in `nixpkgs`

### Container Image Strategy

Images are built with Devbox and pushed to GitHub Container Registry (GHCR).

**Image naming and deduplication:**
- Languages with the same `base` AND same `nixpkgs` packages share an image
- Languages with same `base` but different packages get separate images
- Example: `cpp` and `cpp-avx2` share an image (both use `gcc@15.2.0`)
- Example: `cpp-clang` gets its own image (uses `clang@21.1.2`)

**Image tags:** `ghcr.io/niklas-heer/sc-<base>:<version>`

The `get_base_image_name()` function in `languages.py` determines which languages can share images.

### Finding Package Versions

```bash
devbox search <package>           # Search Devbox packages
# Or visit https://search.nixos.org/packages
```

### Helper Functions in languages.py

- `get_base_image_name(target)` - Get the image name for a language (handles deduplication)
- `get_base_languages()` - Get deduplicated mapping of images to build
- `get_languages_by_category()` - Group languages by category
- `get_variants(base)` - Get all variants of a base language (e.g., all Python variants)
- `get_all_versions()` - Get version info for all languages

## Fly.io Remote Builds (ARM Mac Workaround)

Some languages have issues when running Devbox containers locally on macOS/Apple Silicon due to Rosetta/QEMU emulation. The Fly.io setup provides an x86_64 Linux machine for remote builds.

### Setup (One-time)

```bash
cd dagger-poc
just remote-setup  # Creates Fly app and deploys builder
```

This:
1. Creates a Fly.io app named `speed-comparison-builder`
2. Creates a 10GB persistent volume for Docker storage
3. Deploys a builder machine in Frankfurt (fra region)

### Remote Commands

```bash
just remote-start          # Wake up the machine
just remote-build rust go  # Build images on remote x86_64
just remote-test rust      # Quick benchmark on remote
just remote-bench rust     # Full benchmark on remote
just remote-shell          # SSH into remote for debugging
just remote-sync           # Sync project files to remote
just remote-status         # Check machine status
just remote-stop           # Stop machine (saves money)
```

### How It Works

1. `just remote-start` wakes up the Fly machine (auto-stops after 10min idle)
2. Project files are synced via SSH/tar
3. Dagger runs inside the remote machine with native x86_64 binaries
4. Results can be retrieved or machine stopped

### Fly.io Configuration

**Machine specs** (`fly/fly.toml`):
- `performance-2x`: 2 CPU, 4GB RAM (~$0.00005/sec)
- Auto-stops after 10 minutes of inactivity
- 10GB persistent Docker storage volume

**Builder image** (`fly/Dockerfile`):
- Base: `nixos/nix:latest`
- Includes: Docker, Dagger CLI, Devbox, uv, SSH
- Runs Docker daemon + SSH server

### When to Use Remote Builds

| Scenario | Recommended |
|----------|-------------|
| Testing most languages on ARM Mac | Local (`just test`) |
| Languages with emulation issues (Java, C#, WASM) | Remote (`just remote-test`) |
| Building for CI/registry | Remote or CI |
| Debugging container issues | Remote (`just remote-shell`) |

### Disk Space Issues

If the remote builder runs out of disk space (10GB limit), clean up Docker/Dagger caches:
```bash
just remote-shell
# Inside the machine:
rm -rf /var/lib/docker/dagger/*
docker system prune -af
```

## Dagger CI/CD Workflows

The Dagger pipeline uses GitHub Actions workflows in `.github/workflows/`:

### dagger-ci.yml

Combined workflow for building images and running benchmarks in a single pipeline.

**Triggers:**
- Push to `master` when source files or language config changes
- Pull requests that modify `dagger-poc/` or source files
- `/dagger-bench <language> [language2] ...` command in PR comments
- Manual dispatch with flexible options

**Pipeline stages:**
1. **prepare** - Detect what images need building and what to benchmark
2. **build** - Build and push missing/changed images (parallel matrix)
3. **benchmark** - Run benchmarks (parallel matrix, waits for build)
4. **analyze** - Generate charts and summary

**Key features:**
- Single workflow run shows entire pipeline status
- Only builds images that are missing or changed
- Caches benchmark results based on source + config hash
- Supports `build_only` and `benchmark_only` modes
- Quick test mode (10k iterations) for faster feedback

**Manual dispatch options:**
- `languages` - Space-separated list, or "all"
- `build_only` - Only build images, skip benchmarks
- `benchmark_only` - Only benchmark (assumes images exist)
- `quick_test` - Use 10k iterations instead of 1B
- `skip_cache` - Force fresh benchmarks
- `dry_run` - Don't push images to registry

**PR Benchmark Command:**
```
/dagger-bench rust go          # Benchmark specific languages
/dagger-bench help             # Show help
```

### dagger-version-check.yml

Checks for package version updates (similar to legacy version-check.yml but for Dagger).

## Chart Styling

- Tokyo Night neon color scheme (pink → purple → blue → cyan)
- Accuracy mapped to color (low accuracy = pink, high = cyan)
- Uses matplotlib with dark background (#1a1b26)

## GitHub CLI

The `gh` CLI is available for issue/PR management:
```bash
gh issue list
gh issue create --title "..." --body "..."
gh issue comment <num> --body "..."
gh issue close <num>
gh pr list
gh pr view <num>
gh pr close <num>
```
