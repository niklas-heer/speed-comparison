# Update Languages Command

Update all programming language versions in the benchmark suite to their latest stable releases.

## Benchmark Rules

Before making any changes, remember these rules for implementations:

1. **No concurrency/parallelism** - Implementations must be single-threaded. No multi-threading, async, or parallel processing.
2. **SIMD is allowed but separate** - SIMD optimizations should be separate targets (e.g., `swift-simd`, `cpp-avx2`, `java-vecops`), not replacing standard implementations.
3. **Standard language features** - Use idiomatic code. Compiler optimization flags are fine. Auto-vectorization hints like `@simd` in Julia are OK.
4. **Same algorithm** - All implementations must use the Leibniz formula.

## Instructions

Update all programming languages directly (do NOT use sub-agents as they don't persist changes reliably).

### Step 1: Check All Versions

```bash
cd dagger-poc
just check-versions
```

This queries nixhub.io for all packages and shows which have updates available.

For updates including unstable/preview versions:
```bash
just check-versions-unstable
```

### Step 2: List All Languages

```bash
cd dagger-poc
just list-langs
```

### Step 3: Update Each Language

Languages are configured in `dagger-poc/languages.py` using Devbox packages with pinned versions.

For each language with an available update:

1. **Edit `dagger-poc/languages.py`**:
   - Update `nixpkgs` version: `("pkg@old",)` → `("pkg@new",)`
   ```python
   nixpkgs=("rustc@1.83.0",)  →  nixpkgs=("rustc@1.85.0",)
   ```

2. **Check for breaking changes** (for major version bumps):
   - Search for "<language> X.Y migration guide"
   - Update `src/leibniz.<ext>` if needed

3. **Test the update**:
   ```bash
   just test <language>
   ```

### Step 4: Run Validation

After all updates:

```bash
cd dagger-poc

# Run all validation tests
uv run pytest

# Test a few key languages
just test rust go python nodejs
```

### Step 5: Summary and Commit

After all updates, provide a summary table:

```
## Language Version Updates

| Language | Package | Old Version | New Version | Status |
|----------|---------|-------------|-------------|--------|
| rust     | rustc   | 1.83.0      | 1.85.0      | OK     |
| go       | go      | 1.23.4      | 1.24.0      | OK     |
| python   | python3 | 3.12.8      | 3.13.0      | OK     |
...

**Total**: X languages updated, Y already current, Z failed
```

Ask if user wants to commit:
```bash
git add dagger-poc/languages.py src/
git commit -m "chore: update language versions to latest stable releases

- Rust: 1.83.0 → 1.85.0
- Go: 1.23.4 → 1.24.0
- Python: 3.12.8 → 3.13.0
... etc"
```

## Special Considerations

### ARM Mac Compatibility

Some languages have issues when running Devbox containers on macOS/Apple Silicon:
- Java, C#, Swift, WASM may fail with SIGILL errors
- Use remote builds for these: `just remote-test java csharp swift wasm`

### Package Not Found in nixhub

If a package isn't found:
1. Check https://search.nixos.org/packages for the correct name
2. Try alternative names (e.g., `python3` vs `python`, `rustc` vs `rust`)
3. For very new packages, they may need Nix flakes instead of Devbox

### Breaking Changes

Common breaking changes to watch for:
- **Zig**: API changes (`@intToFloat` → `@floatFromInt`, enum case changes)
- **Swift 6**: Strict concurrency checking
- **Python 3.13+**: Various deprecations
- **Node.js**: ESM vs CommonJS changes

## Quick Reference

| Command | Description |
|---------|-------------|
| `just check-versions` | Check all packages for updates |
| `just check-versions rust go` | Check specific languages |
| `just list-langs` | List all language targets |
| `just lang-info <lang>` | Show config for a language |
| `just test <lang>` | Quick benchmark test |
| `just tests` | Run validation tests |
| `devbox search <pkg>` | Search for package versions |

## Common Pitfalls

1. **Sub-agents don't persist**: Don't spawn sub-agents for updates - their file changes don't persist
2. **Unstable versions**: By default, version checker filters out alpha/beta/RC versions
3. **ARM64 compatibility**: Some packages may have issues on ARM Mac - use `just remote-test` for those
4. **Version format**: Package versions must include `@` separator (e.g., `go@1.23.4`)
