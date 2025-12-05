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
4. Test locally before committing

## Conventions

- **Prefer Alpine** over Debian/Fedora for smaller images and consistency
- **Avoid Fedora** - not currently supported (hyperfine download is arch-specific)
- **Version commands** that output to stderr may need `echo X.Y.Z` workaround
- **Don't break vectorization** - C/C++ code is optimized; variable declarations inside loops enable auto-vectorization
- **SIMD variants** should be separate targets (e.g., `swift-simd`, `cpp-avx2`)

## CI/CD

GitHub Actions workflow (`.github/workflows/ci.yml`):
- Matrix builds run each language in parallel
- `scmeta` binary is built once and shared via artifacts
- Results are cached based on source file + Earthfile target + rounds.txt hash
- Caches invalidate when any of these change

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
