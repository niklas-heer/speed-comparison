# Update Single Language Command

Update a specific programming language to its latest stable version.

**Usage:** `/update-language <language-name>`

Example: `/update-language rust` or `/update-language python`

## Instructions

Update the specified language benchmark to its latest stable version.

### Step 1: Check Current Version

```bash
cd dagger-poc
just lang-info $ARGUMENTS
```

This shows the current configuration including package version.

### Step 2: Check for Available Updates

```bash
cd dagger-poc
just check-versions $ARGUMENTS
```

This queries nixhub.io for newer versions of the package.

### Step 3: Research Latest Version (if needed)

If the version checker doesn't find updates or you want to verify:

1. **Devbox search**: `devbox search <package>`
2. **Nixpkgs search**: https://search.nixos.org/packages
3. **Web search**: Search for "latest stable <language> version 2025"
4. **Prefer**:
   - Stable releases over nightly/beta/RC
   - Versions available in nixpkgs

### Step 4: Check for Breaking Changes

If the version jump is significant (e.g., major version bump):
1. Search for "<language> X.Y migration guide" or "breaking changes"
2. Check if the leibniz source file needs updates
3. Common issues:
   - Zig: API changes between versions (`@intToFloat` → `@floatFromInt`)
   - Rust: Usually backward compatible
   - Python: Print syntax, async changes
   - Go: Module system changes

### Step 5: Update `dagger-poc/languages.py`

Edit the language entry to update the version:

```python
# Before
"rust": Language(
    name="Rust",
    nixpkgs=("rustc@1.83.0",),  # Old version
    ...
),

# After
"rust": Language(
    name="Rust",
    nixpkgs=("rustc@1.85.0",),  # New version
    ...
),
```

### Step 6: Update Source File (if needed)

If there are breaking changes, update `src/leibniz.<ext>`:
- Keep changes minimal - only fix what's broken
- Preserve the algorithm - this is a benchmark, the math must stay the same
- Don't add features - focus only on compatibility

### Step 7: Test Locally

```bash
cd dagger-poc

# Run validation tests
uv run pytest

# Quick benchmark test
just test $ARGUMENTS
```

**On ARM Mac with emulation issues** (Java, C#, Swift, WASM):
```bash
just remote-test $ARGUMENTS
```

### Step 8: Report

Provide a summary:

```
## $ARGUMENTS Update

**Status**: SUCCESS / FAILED / SKIPPED

**Version**: X.Y.Z → A.B.C (or "Already at latest A.B.C")

**Changes made**:
- Updated nixpkgs version from `pkg@old` to `pkg@new`
- Updated source file: <description> (if applicable)

**Test result**: 
- Validation tests: PASS/FAIL
- Quick benchmark: PASS/FAIL

**Notes**: <any issues or warnings>
```

Do NOT commit. The user will decide whether to commit after reviewing.

## Quick Reference

| Command | Description |
|---------|-------------|
| `just lang-info <lang>` | Show current language config |
| `just check-versions <lang>` | Check for version updates |
| `just test <lang>` | Quick benchmark test (10k iterations) |
| `just bench <lang>` | Full benchmark (1B iterations) |
| `just remote-test <lang>` | Test on remote x86_64 (ARM Mac) |
| `devbox search <pkg>` | Search for package versions |
