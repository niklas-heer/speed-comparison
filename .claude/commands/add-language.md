# Add a New Programming Language

Add a new programming language to the speed comparison benchmark.

Language to add: $ARGUMENTS

## Steps to Complete

### 1. Create Source Implementation
Create `src/leibniz.<ext>` with the Leibniz formula implementation:
- Read rounds from `rounds.txt`
- Calculate pi using the Leibniz formula: pi = 4 * (1 - 1/3 + 1/5 - 1/7 + ...)
- Print the result with full precision (no newline if possible)

### 2. Add Earthfile Target
Add a new target in `Earthfile` following this pattern:
```
<language>:
  FROM <base-image>
  DO +PREPARE_ALPINE  # or +PREPARE_DEBIAN if needed
  DO +ADD_FILES --src="leibniz.<ext>"
  RUN --no-cache <compile-command>  # if compiled language
  DO +BENCH --name="<language>" --lang="<Display Name>" --version="<version-cmd>" --cmd="<run-cmd>"
```

Key conventions:
- Prefer Alpine over Debian for smaller images
- Use dynamic version detection (no hardcoded versions)
- Make downloads architecture-aware for ARM64/x86_64 compatibility
- Add `BUILD +<language>` to the `collect-data` target

### 3. Update Extension-to-Target Mapping (if needed)
If the file extension differs from the Earthfile target name (e.g., `leibniz.rkt` but target is `racket`), add a mapping to the `extToTarget` object in `.github/workflows/ci.yml`.

This is required for the `/bench changed` command to correctly detect and run your language.

### 4. Add Language Icon
1. Check if icon exists in devicon: https://devicon.dev/
2. If yes: Add to `download_icons.py` ICON_MAP and run:
   ```
   DYLD_LIBRARY_PATH=/opt/homebrew/opt/cairo/lib uv run python download_icons.py
   ```
3. If no: Create custom SVG in `icons/<name>.svg` and convert to PNG
4. Add mapping to `ICON_MAP` in `analyze.py`

### 5. Test Locally
```bash
# Test the build
earthly +<language>

# Or with quick test rounds
earthly --build-arg QUICK_TEST_ROUNDS=1000000 +<language>
```

### 6. Commit
```bash
git add src/leibniz.<ext> Earthfile icons/<name>.png analyze.py download_icons.py
git commit -m "feat: add <Language> implementation"
```

## Checklist
- [ ] Source file created in `src/`
- [ ] Earthfile target added
- [ ] Target added to `collect-data`
- [ ] Extension-to-target mapping added in `ci.yml` (if extension ≠ target name)
- [ ] Icon added (devicon or custom)
- [ ] ICON_MAP updated in `analyze.py`
- [ ] ICON_MAP updated in `download_icons.py`
- [ ] Tested locally with `earthly +<language>`
