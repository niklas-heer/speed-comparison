# Update Single Language Command

Update a specific programming language to its latest stable version.

**Usage:** `/update-language <language-name>`

Example: `/update-language rust` or `/update-language python`

## Instructions

Update the specified language benchmark to its latest stable version.

### Step 1: Identify Current State

1. Read the Earthfile and find the target for the specified language
2. Extract:
   - Current Docker image (FROM line)
   - Current version command
   - Any compilation flags or special setup

### Step 2: Research Latest Version

1. **Web Search**: Search for "latest stable <language> version 2025" or "<language> latest release"
2. **Docker Hub**: Check for available tags on the official Docker image
3. **Prefer**:
   - Official images over community images
   - Alpine variants over Debian (smaller, faster)
   - Stable releases over nightly/beta

### Step 3: Check for Breaking Changes

If the version jump is significant (e.g., major version bump):
1. Search for "<language> X.Y migration guide" or "breaking changes"
2. Check if the leibniz source file needs updates
3. Common issues:
   - Zig: API changes between versions
   - Rust: Usually backward compatible
   - Python: Print syntax, async changes
   - Go: Module system changes

### Step 4: Make Updates

1. Update the Earthfile target:
   - Docker image tag
   - Version command if needed
   - Any new dependencies

2. Update source file if needed (src/leibniz.*)

### Step 5: Test Locally

Run: `earthly +<language>`

- Verify it compiles/runs without errors
- Check that pi calculation output looks correct
- Note the execution time for reference

### Step 6: Report

Provide a summary:

```
## $LANGUAGE Update

**Status**: SUCCESS / FAILED / SKIPPED

**Version**: X.Y.Z → A.B.C (or "Already at latest A.B.C")

**Changes made**:
- Updated Docker image from `image:old` to `image:new`
- Updated source file: <description> (if applicable)

**Test result**: 
- Build: PASS/FAIL
- Runtime: X.XXs for 1B iterations

**Notes**: <any issues or warnings>
```

Do NOT commit. The user will decide whether to commit after reviewing.
