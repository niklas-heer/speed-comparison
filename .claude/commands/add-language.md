# Add a New Programming Language

Add a new programming language to the speed comparison benchmark.

Language to add: $ARGUMENTS

## Steps to Complete

### 1. Create Source Implementation
Create `src/leibniz.<ext>` with the Leibniz formula implementation:
- Read rounds from `rounds.txt`
- Calculate pi using the Leibniz formula: pi = 4 * (1 - 1/3 + 1/5 - 1/7 + ...)
- Print the result with full precision (no newline if possible)

### 2. Add Language to `dagger-poc/languages.py`

Add an entry to the `LANGUAGES` dict:

```python
"mylang": Language(
    name="MyLang",                    # Display name for charts
    nixpkgs=("mylang@1.2.3",),        # Devbox package with pinned version
    file="leibniz.ml",                # Source file in src/
    run="./leibniz",                  # Run command (or "mylang leibniz.ml")
    compile="mylangc leibniz.ml -o leibniz",  # Compile command (if compiled)
    version_cmd="mylang --version",   # Command to get version
    base="mylang",                    # Base language for icon mapping
    category="compiled",              # "compiled", "interpreted", "jit", "vm", "functional", etc.
),
```

**Key validation rules:**
- All `nixpkgs` must have explicit versions (`pkg@x.y.z`, not `pkg@latest`)
- `file` must start with "leibniz."
- Must have at least one package in `nixpkgs`

**Finding package versions:**
```bash
devbox search <package>           # Search Devbox packages
# Or visit https://search.nixos.org/packages
```

### 3. Add Language Icon
1. Check if icon exists in devicon: https://devicon.dev/
2. If yes: Add to `download_icons.py` ICON_MAP and run:
   ```bash
   DYLD_LIBRARY_PATH=/opt/homebrew/opt/cairo/lib uv run python download_icons.py
   ```
3. If no: Create custom SVG in `icons/<name>.svg` and convert to PNG
4. Add mapping to `ICON_MAP` in `analyze.py`

### 4. Run Tests and Validate

```bash
cd dagger-poc

# Run validation tests
uv run pytest

# Quick benchmark test (10k iterations)
just test mylang

# Full benchmark (1B iterations) - optional
just bench mylang
```

**On ARM Mac with emulation issues** (Java, C#, Swift, WASM):
```bash
just remote-test mylang    # Uses Fly.io x86_64 builder
```

### 5. Commit

```bash
git add src/leibniz.<ext> dagger-poc/languages.py icons/<name>.png analyze.py download_icons.py
git commit -m "feat: add <Language> implementation"
```

## Checklist
- [ ] Source file created in `src/leibniz.<ext>`
- [ ] Language added to `dagger-poc/languages.py`
- [ ] Validation tests pass (`uv run pytest` in dagger-poc/)
- [ ] Quick benchmark works (`just test mylang`)
- [ ] Icon added (devicon or custom)
- [ ] ICON_MAP updated in `analyze.py`
- [ ] ICON_MAP updated in `download_icons.py`

## Language Configuration Reference

```python
@dataclass(frozen=True)
class Language:
    name: str              # Display name (e.g., "Rust")
    file: str              # Source file: "leibniz.rs"
    run: str               # Run command: "./leibniz"
    compile: str | None    # Compile command (if compiled)
    version_cmd: str | None # Version command (defaults to primary package)
    base: str | None       # Base language for icon mapping (e.g., "python" for "cpython")
    category: str          # "compiled", "interpreted", "jit", "vm", etc.
    version_regex: str     # Regex to extract version (default: r"(\d+\.\d+\.?\d*)")
    nixpkgs: tuple[str, ...] = ()  # Devbox packages: ("go@1.23.4",)
    nix_setup: str | None = None   # Post-install setup commands
    allow_insecure: bool = False   # Allow insecure packages (e.g., haxe needs mbedtls)
```

## Common Categories

- `systems`: C, C++, Rust, Go, Zig, Nim, etc.
- `jvm`: Java, Kotlin, Scala, Clojure, Groovy
- `dotnet`: C#, F#
- `interpreted`: Python, Ruby, JavaScript, Lua, Perl, PHP
- `functional`: Haskell, OCaml, Elixir, Erlang, Racket, Lisp
- `compiled`: Other compiled languages
- `jit`: Languages with JIT compilation (Julia)
