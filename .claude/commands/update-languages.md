# Update Languages Command

Update all programming language versions in the benchmark suite to their latest stable releases.

## Instructions

You are a coordinator agent that will update all programming languages in parallel. Follow these steps:

### Step 1: Discover Languages

Read the Earthfile and extract all language targets from the `collect-data` section. Each `BUILD +<language>` line represents a language to update.

### Step 2: Spawn Parallel Sub-Agents

For each language (or batch of 5-10 languages), spawn a sub-agent using the Task tool with this prompt:

```
Update the "$LANGUAGE" benchmark to its latest stable version.

## Steps:

1. **Find current version**: Read the Earthfile target for "$LANGUAGE" and identify:
   - Current Docker image and tag
   - Current version being reported

2. **Research latest version**: 
   - Search the web for the latest stable release of $LANGUAGE
   - Check Docker Hub for available image tags
   - Note: Prefer official images, Alpine variants when available

3. **Compare versions**: If already up-to-date, report "SKIPPED: $LANGUAGE already at latest version X.Y.Z"

4. **Update if needed**:
   - Update the Docker image tag in Earthfile
   - Update version command if needed
   - Check if the language has had breaking API changes that would affect src/leibniz.* file
   - If API changes needed, update the source file

5. **Test locally**: Run `earthly +$LANGUAGE` to verify it builds and runs correctly

6. **Report result**: 
   - SUCCESS: "$LANGUAGE updated from X.Y.Z to A.B.C"
   - SKIPPED: "$LANGUAGE already at latest X.Y.Z"  
   - FAILED: "$LANGUAGE update failed: <reason>"

Do NOT commit changes. Just make the updates and report back.
```

### Step 3: Collect Results

Wait for all sub-agents to complete. Compile a summary:

```
## Language Update Summary

### Updated (X languages)
- Language1: v1.0.0 → v2.0.0
- Language2: v3.1.0 → v3.2.0

### Skipped (Y languages - already current)
- Language3: v1.5.0
- Language4: v2.0.0

### Failed (Z languages)
- Language5: <error reason>
```

### Step 4: Final Steps

After all updates are complete:
1. Show the summary to the user
2. Ask if they want to commit and push the changes
3. If yes, create a single commit: "chore: update language versions" with details in the body

## Notes

- Prioritize official Docker images over community images
- Prefer Alpine-based images for consistency
- Some languages (Java, Kotlin) use hardcoded version strings - update those too
- If a language requires significant code changes due to API breaks, flag it for manual review
- Run updates in parallel batches to speed up the process
