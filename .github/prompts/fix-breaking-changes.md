# Task: Fix Breaking Changes

The **{{LANGUAGE}}** build is failing after updating to version **{{NEW_VERSION}}**.

## Error Output
```
{{ERROR_OUTPUT}}
```

## Instructions

1. **Analyze the error** to understand what's failing:
   - Compilation errors (syntax changes, API changes)
   - Runtime errors (behavior changes)
   - Build system errors (missing dependencies, wrong flags)

2. **Research breaking changes**:
   - Use WebSearch to find the changelog/release notes for {{LANGUAGE}} {{NEW_VERSION}}
   - Look for migration guides or breaking changes documentation
   - Search for the specific error message if needed

3. **Fix the source code** (`src/leibniz.*`):
   - Apply minimal changes to fix the compilation/runtime issue
   - Keep the algorithm the same (Leibniz formula for Pi)
   - Maintain the same structure and style

4. **Fix the Earthfile** if needed:
   - Update compiler flags if deprecated
   - Add new required flags or dependencies
   - Update build commands for new syntax

5. **Validate the fix**:
   - Run: `earthly --build-arg QUICK_TEST_ROUNDS=10000 +{{LANGUAGE}}`
   - If it still fails, analyze the new error and try again

## Common Breaking Changes

### Zig
- Function renames: `@intToFloat` -> `@floatFromInt`
- Enum case changes: `.Optimized` -> `.optimized`
- Stdout API changes

### Rust
- Edition changes requiring `edition = "2024"` in code attributes
- Deprecated APIs being removed

### Go
- Module path changes
- Deprecated function removals

### Python
- f-string syntax changes
- Type hint requirements

### Swift
- Actor/async changes
- API renames

## Important Notes

- **Keep changes minimal** - only fix what's broken
- **Preserve the algorithm** - this is a benchmark, the math must stay the same
- **Don't add features** - focus only on compatibility
- **Document your changes** in the PR description if significant

## Source File Location

The source file is at: `src/leibniz.{{EXT}}`

Where `{{EXT}}` is the file extension for {{LANGUAGE}} (e.g., `.rs`, `.go`, `.py`, `.zig`)
