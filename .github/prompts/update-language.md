# Task: Update Language Version

Update the **{{LANGUAGE}}** target in the Earthfile to use version **{{NEW_VERSION}}**.

## Current State
- Language target: `{{LANGUAGE}}`
- Current version: `{{CURRENT_VERSION}}`
- New version: `{{NEW_VERSION}}`

## Instructions

1. **Read the Earthfile** and locate the `{{LANGUAGE}}:` target section

2. **Identify what needs updating**:
   - If it's a Docker image (e.g., `FROM rust:1.83-alpine`), update the tag
   - If it's a downloaded binary/tool, update the version in the URL
   - If it's a package install, the package version comes from the base image

3. **Make the version update**:
   - Update the `FROM` line if it contains a version tag
   - Update any `wget`/`curl` URLs that contain version numbers
   - Update any version-specific installation commands

4. **Verify the change is correct**:
   - The change should be minimal - only update version numbers
   - Do NOT change optimization flags, build commands, or other logic
   - Do NOT add new dependencies or features

## Examples

### Docker image update:
```diff
-FROM rust:1.83-alpine
+FROM rust:1.84-alpine
```

### Downloaded binary update:
```diff
-RUN wget -q https://github.com/JetBrains/kotlin/releases/download/v2.1.20/kotlin-compiler-2.1.20.zip
+RUN wget -q https://github.com/JetBrains/kotlin/releases/download/v2.1.21/kotlin-compiler-2.1.21.zip
```

### Version in multiple places:
```diff
-RUN wget -q kotlin-compiler-2.1.20.zip && unzip -q kotlin-compiler-2.1.20.zip
+RUN wget -q kotlin-compiler-2.1.21.zip && unzip -q kotlin-compiler-2.1.21.zip
```

## Important Notes

- Do NOT run any tests or builds - just make the version update
- Keep changes minimal and focused on the version number only
- If the version appears in multiple places (URL and unzip command), update ALL occurrences
- Preserve exact whitespace and formatting of the Earthfile
- Use the EXACT version specified above ({{NEW_VERSION}}), do NOT use "latest" or other generic tags
- Do NOT run shell commands to verify - the build system will validate the changes
