# Task: Update Language Version

Update the **{{LANGUAGE}}** target in the Earthfile to version `{{NEW_VERSION}}`.

## Version Info
- Target: `{{LANGUAGE}}`  
- Current: `{{CURRENT_VERSION}}`
- New: `{{NEW_VERSION}}`

## Steps

1. Read the Earthfile
2. Find the `{{LANGUAGE}}:` target 
3. Edit the version number from `{{CURRENT_VERSION}}` to `{{NEW_VERSION}}`
4. Optionally verify the new version/tag exists (e.g., check Docker Hub or GitHub releases)

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

## Important

- Use EXACTLY `{{NEW_VERSION}}` - never use "latest" as a tag
- Update ALL occurrences if the version appears multiple times
- Preserve exact whitespace and formatting
