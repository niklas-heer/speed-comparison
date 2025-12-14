# CLAUDE.md

This file provides context for AI assistants working on this project.

## Project Overview

A benchmark comparing 40+ programming languages using the Leibniz formula for calculating π. Results are published to GitHub Pages with historical tracking.

## Key Files

- `Earthfile` - Build definitions for all languages (Earthly build system)
- `analyze.py` - Generates charts and CSV from benchmark results
- `publish.py` - Publishes results to `docs/history/` with timestamped folders
- `src/` - Source implementations (`leibniz.*` files)
- `src/rounds.txt` - Number of iterations (currently 1 billion)
- `scmeta/` - Crystal tool that wraps hyperfine and extracts metadata
- `docs/` - GitHub Pages site with interactive results viewer

## Build System

Uses [Earthly](https://earthly.dev/) with Docker. Each language has a target in `Earthfile`.

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
