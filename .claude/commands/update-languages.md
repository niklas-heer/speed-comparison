# Update Languages Command

Update all programming language versions in the benchmark suite to their latest stable releases.

> **Note**: There is also an automated daily workflow (`.github/workflows/version-check.yml`) that checks for updates and creates PRs automatically. Use this manual command for bulk updates or when you want more control.

## Benchmark Rules

Before making any changes, remember these rules for implementations:

1. **No concurrency/parallelism** - Implementations must be single-threaded. No multi-threading, async, or parallel processing.
2. **SIMD is allowed but separate** - SIMD optimizations should be separate targets (e.g., `swift-simd`, `cpp-avx2`, `java-vecops`), not replacing standard implementations.
3. **Standard language features** - Use idiomatic code. Compiler optimization flags are fine. Auto-vectorization hints like `@simd` in Julia are OK.
4. **Same algorithm** - All implementations must use the Leibniz formula.

## Instructions

Update all programming languages directly (do NOT use sub-agents as they don't persist changes reliably).

### Step 1: Discover Languages

Read the Earthfile and extract all language targets from the `collect-data` section. Each `BUILD +<language>` line represents a language to update.

### Step 2: Categorize Languages

Languages fall into these categories:

1. **Alpine-based** (use `+alpine` or `FROM alpine`): These get updated automatically when the Alpine base version is bumped. Just update `alpine:X.XX` in the `alpine:` target.
   - Examples: c, cpp, d, elixir, lua, nim, perl, php, r, ruby, sbcl

2. **Official Docker images**: Update the image tag directly.
   - Examples: golang, rust, swift, haskell, python, julia, node, crystal, clojure

3. **Downloaded compilers**: Update download URLs and hardcoded version strings.
   - Examples: kotlin (GitHub releases), scala (scala-cli)

4. **Special cases**:
   - java/java-vecops: Use eclipse-temurin, update hardcoded `echo` version
   - java-graalvm: Uses SDKMAN, update the version identifier
   - php: Needs php-opcache package and JIT flags for fair benchmarking
   - ocaml: May need alpine:edge for latest version
   - pony: Docker Hub images are often stale (check before updating)

### Step 3: Update Each Language

For each language:

1. **Research latest version**: Search the web for latest stable release
2. **Update Earthfile**: Change Docker image tags or download URLs
3. **Update version strings**: Fix any hardcoded `echo "X.Y.Z"` commands
4. **Test locally with quick rounds**: Use `earthly --build-arg QUICK_TEST_ROUNDS=10000 +<language>` for fast iteration
5. **Check for ARM64 issues**: If using `-march=native` with GCC 15+, use the `SET_ARCH_FLAGS` function:
   ```earthfile
   DO +SET_ARCH_FLAGS
   RUN gcc ... $MARCH_FLAG ...
   ```

### Step 4: Special Considerations

- **scmeta version format**: The scmeta tool's version parser uses regex `/\d+(\.\d+)+/` which requires at least one decimal point. Versions like `21` won't work - use `21.0.0` instead:
  ```earthfile
  # WRONG - will fail with "No version number found"
  DO +BENCH ... --version="echo 21" ...
  
  # CORRECT
  DO +BENCH ... --version="echo 21.0.0" ...
  ```

- **PHP**: Must include opcache and JIT for fair benchmarking:
  ```earthfile
  RUN apk add --no-cache php84 php84-opcache
  DO +BENCH ... --cmd="php84 -dopcache.enable_cli=1 -dopcache.jit=1255 -dopcache.jit_buffer_size=64M leibniz.php"
  ```

- **Bun.js**: Use official `oven/bun` image, not legacy `jarredsumner/bun`

- **Node.js**: Prefer official `node:XX-alpine` over Alpine's `nodejs-current`

- **GraalVM**: Native Image is bundled in v21+, no need for `gu install native-image`

- **LDC (D)**: Newer Alpine versions don't need `llvm-libunwind-static llvm12`

### Step 5: Commit Changes

After all updates:

1. Show a summary table of all changes
2. Ask if user wants to commit
3. Create commits like:
   ```
   chore: update language versions to latest stable releases
   
   - Alpine base: 3.16 → 3.23
   - Go: 1.19.1 → 1.23
   - Rust: 1.64 → 1.83
   ... etc
   ```

### Step 6: Update Related Issues

Check for open issues about language versions (like #136) and:
1. Update issue checklists with completed items
2. Comment on the issue with summary of changes
3. Close the issue if all languages are updated

## Common Pitfalls

1. **Sub-agents don't persist**: Don't spawn sub-agents for updates - their file changes don't persist
2. **Cache invalidation**: CI caches based on Earthfile target content - changes should invalidate caches
3. **ARM64 compatibility**: GCC 15+ has issues with `-march=native` on ARM64 (SME/SVE2 detection)
4. **Stale Docker images**: Some projects (Pony) don't publish Docker images promptly
5. **Breaking API changes**: Check release notes for languages with major version bumps (Zig, Swift 6, etc.)
6. **Verify Docker image tags exist**: Always verify the exact Docker image tag exists before using it. Some images don't have all tag variants (e.g., `haskell:9.10-slim` doesn't exist, but `haskell:9.10-slim-bullseye` does). Check Docker Hub or use `docker pull` to verify.

## Version Research Tips

- **Use the version checker first**: Run `python scripts/check-versions.py --all` to see all available updates
- Use web search for "[language] latest stable version 2025"
- Check Docker Hub for available tags
- Verify Alpine package versions: `docker run --rm alpine:X.XX apk info [package]`
- For GitHub-released compilers, check the releases page

## Version Sources Configuration

When updating languages, ensure `scripts/version-sources.json` stays in sync:
- If you change a Docker image name, update the `image` field
- If you change the tag pattern, update `earthfile_pattern` and `tag_filter`
- If you switch from Docker to Alpine package (or vice versa), update the `source` field
