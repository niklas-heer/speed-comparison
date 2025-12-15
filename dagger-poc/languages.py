"""
Language definitions for speed-comparison benchmarks.

This is the single source of truth for all benchmark configurations.
Each language defines how to build its container image and run the benchmark.

Container Image Strategy:
- Base images contain: language runtime + hyperfine (built with Nix/Devbox)
- Benchmark code (leibniz.*, scmeta, rounds.txt) is added at runtime
- Images are pushed to registry and cached

To add a new language:
1. Add an entry to LANGUAGES dict below
2. Create src/leibniz.{ext} source file
3. Run `pytest test_languages.py` to validate
4. Submit PR!
"""

from __future__ import annotations

import platform
import re
from dataclasses import dataclass
from typing import Optional

# =============================================================================
# Architecture Detection
# =============================================================================

ARCH = platform.machine()
IS_ARM = ARCH in ("aarch64", "arm64")

# Compiler optimization flags
MARCH_NATIVE = "-march=armv8-a" if IS_ARM else "-march=native"

# =============================================================================
# Pinned Nixpkgs Version (for reproducibility)
# =============================================================================

# Pin to a specific nixpkgs commit for reproducible builds
# Update this periodically to get newer package versions
# Find commits at: https://github.com/NixOS/nixpkgs/commits/nixos-unstable
NIXPKGS_REV = "nixos-24.11"  # Stable release branch
NIXPKGS_REF = f"github:NixOS/nixpkgs/{NIXPKGS_REV}"


def nix_pkg(name: str, version: str = "") -> str:
    """Create a pinned Nix flake reference.

    Args:
        name: Package name (e.g., "jdk21", "dotnet-sdk_8")
        version: Version for documentation/image tagging (not used in flake ref)

    Returns:
        Pinned flake reference like "github:NixOS/nixpkgs/nixos-24.11#jdk21"
    """
    return f"{NIXPKGS_REF}#{name}"


# =============================================================================
# Language Configuration
# =============================================================================


@dataclass(frozen=True)
class Language:
    """Configuration for a benchmark language.

    Container Build Strategy:
    - If `nixpkgs` is set: Use Devbox to install packages
    - If `nix_flake` is set: Use raw Nix flakes (for packages not in Devbox)
    - Both can be combined (Devbox base + extra Nix packages)

    Attributes:
        name: Display name shown in charts
        file: Source file name in src/ directory
        run: Command to execute the benchmark
        compile: Optional compilation command
        version_cmd: Command to get version string
        base: Base language for icon mapping (e.g., "rust" for "rust-nightly")
        category: Language category for organization
        version_regex: Regex to extract version from version_cmd output

        # Package configuration (choose one or combine):
        nixpkgs: List of Devbox packages ["pkg@version", ...]
        nix_flake: List of Nix flake refs ["nixpkgs#pkg", "github:org/repo#pkg", ...]
        nix_setup: Optional shell commands to run after Nix packages are installed
    """

    name: str
    file: str
    run: str
    compile: Optional[str] = None
    version_cmd: Optional[str] = None
    base: Optional[str] = None
    category: str = "other"
    version_regex: str = r"(\d+\.\d+\.?\d*)"

    # Package sources (at least one required)
    nixpkgs: tuple[str, ...] = ()  # Devbox packages: ("go@1.23.4", "gcc@14.2.0")
    nix_flake: tuple[str, ...] = ()  # Raw Nix flakes via nix_pkg(): (nix_pkg("jdk21"),)
    nix_flake_version: str = ""  # Version string for flake packages (for image tagging)
    nix_setup: Optional[str] = None  # Post-install setup commands

    # Platform limitations
    x86_64_only: bool = False  # Some packages only work on x86_64 (wasi-sdk, some JVMs)

    def __post_init__(self) -> None:
        """Validate configuration on creation."""
        # Validate file name
        filename = self.file.split("/")[-1].lower()
        valid = filename.startswith("leibniz") or "/" in self.file
        if not valid:
            raise ValueError(f"Source file must start with leibniz, got {self.file}")

        if not self.name:
            raise ValueError("Language name is required")

        # Must have at least one package source
        if not self.nixpkgs and not self.nix_flake:
            raise ValueError(f"Language {self.name} must have nixpkgs or nix_flake")

        # Validate nixpkgs have versions
        for pkg in self.nixpkgs:
            if "@" not in pkg:
                raise ValueError(f"Package '{pkg}' must have version (e.g., '{pkg}@1.0.0')")

    @property
    def uses_nix_flake(self) -> bool:
        """Check if this language uses raw Nix flakes (not just Devbox)."""
        return len(self.nix_flake) > 0

    @property
    def icon_key(self) -> str:
        """Get the key for icon lookup."""
        if self.base:
            return self.base
        return self.name.split()[0].lower()

    @property
    def primary_package(self) -> str:
        """Get the primary package name."""
        if self.nixpkgs:
            return self.nixpkgs[0].split("@")[0]
        elif self.nix_flake:
            # Extract package name from flake ref: "nixpkgs#jdk21" -> "jdk21"
            return self.nix_flake[0].split("#")[-1]
        return "unknown"

    @property
    def primary_version(self) -> str:
        """Get the pinned version of the primary package."""
        if self.nixpkgs:
            return self.nixpkgs[0].split("@")[1]
        elif self.nix_flake:
            # For flakes, use explicit version or derive from NIXPKGS_REV
            return self.nix_flake_version if self.nix_flake_version else NIXPKGS_REV
        return "unknown"

    @property
    def image_tag(self) -> str:
        """Generate a unique image tag for this language configuration."""
        # Use primary version + hash of all packages for cache invalidation
        return self.primary_version

    def extract_version(self, version_output: str) -> str:
        """Extract version string from command output using version_regex."""
        match = re.search(self.version_regex, version_output)
        if match:
            return match.group(1)
        return version_output.strip().split("\n")[0]


# =============================================================================
# Shared Compiler Flags
# =============================================================================

C_FLAGS = f"-O3 {MARCH_NATIVE} -ffast-math"
CPP_FLAGS = f"-O3 {MARCH_NATIVE} -ffast-math -std=c++17"
RUST_FLAGS = "-C opt-level=3 -C lto=fat -C codegen-units=1 -C panic=abort"


# =============================================================================
# Language Definitions
# =============================================================================

LANGUAGES: dict[str, Language] = {
    # =========================================================================
    # Systems Languages (Compiled, low-level)
    # =========================================================================
    "c": Language(
        name="C (gcc)",
        nixpkgs=("gcc@15.2.0",),
        file="leibniz.c",
        compile=f"gcc {C_FLAGS} -o leibniz leibniz.c -lm",
        run="./leibniz",
        version_cmd="gcc --version",
        base="c",
        category="systems",
    ),
    "c-clang": Language(
        name="C (clang)",
        nixpkgs=("clang@21.1.2",),
        file="leibniz.c",
        compile=f"clang {C_FLAGS} -o leibniz leibniz.c -lm",
        run="./leibniz",
        version_cmd="clang --version",
        base="c",
        category="systems",
    ),
    "rust": Language(
        name="Rust",
        nixpkgs=("rustc@1.91.1",),
        file="leibniz.rs",
        compile=f"rustc {RUST_FLAGS} -o leibniz leibniz.rs",
        run="./leibniz",
        version_cmd="rustc --version",
        base="rust",
        category="systems",
    ),
    "go": Language(
        name="Go",
        nixpkgs=("go@1.25.4",),
        file="leibniz.go",
        compile="go build -ldflags='-s -w' -o leibniz leibniz.go",
        run="./leibniz",
        version_cmd="go version",
        base="go",
        category="systems",
        version_regex=r"go(\d+\.\d+\.?\d*)",
    ),
    "odin": Language(
        name="Odin",
        nixpkgs=("odin@2025-11",),
        file="leibniz.odin",
        compile="odin build . -file -o:speed -out:leibniz",
        run="./leibniz",
        version_cmd="odin version",
        base="odin",
        category="systems",
        version_regex=r"dev-(\d{4}-\d+[a-z]?)",
    ),
    "zig": Language(
        name="Zig",
        nixpkgs=("zig@0.15.2",),
        file="leibniz.zig",
        compile="zig build-exe -OReleaseFast leibniz.zig -fno-stack-check",
        run="./leibniz",
        version_cmd="zig version",
        base="zig",
        category="systems",
    ),
    "nim": Language(
        name="Nim",
        nixpkgs=("nim@2.2.4",),
        file="leibniz.nim",
        compile=f"nim c -d:release --opt:speed --passC:'{MARCH_NATIVE}' -o:leibniz leibniz.nim",
        run="./leibniz",
        version_cmd="nim --version",
        base="nim",
        category="systems",
    ),
    "v": Language(
        name="V",
        nixpkgs=("vlang@0.4.11",),
        file="leibniz.v",
        compile="v -prod -o leibniz leibniz.v",
        run="./leibniz",
        version_cmd="v version",
        base="v",
        category="systems",
    ),
    "d": Language(
        name="D (GDC)",
        nixpkgs=("gdc@15.2.0",),
        file="leibniz.d",
        compile=f"gdc leibniz.d -o leibniz -O3 -frelease -flto -ffast-math {MARCH_NATIVE}",
        run="./leibniz",
        version_cmd="gdc --version",
        base="d",
        category="systems",
    ),
    "d-ldc": Language(
        name="D (LDC)",
        nixpkgs=("ldc@1.41.0",),
        file="leibniz.d",
        compile="ldc2 leibniz.d -of=leibniz -O3 -release -flto=thin -ffast-math",
        run="./leibniz",
        version_cmd="ldc2 --version",
        base="d",
        category="systems",
    ),
    # =========================================================================
    # C++ Variants
    # =========================================================================
    "cpp": Language(
        name="C++ (g++)",
        nixpkgs=("gcc@15.2.0",),
        file="leibniz.cpp",
        compile=f"g++ leibniz.cpp -o leibniz -O3 -s -flto {MARCH_NATIVE} -ffast-math",
        run="./leibniz",
        version_cmd="g++ --version",
        base="cplusplus",
        category="systems",
    ),
    "cpp-clang": Language(
        name="C++ (clang++)",
        nixpkgs=("clang@21.1.2", "lld@21.1.2"),
        file="leibniz.cpp",
        compile=f"clang++ -fuse-ld=lld leibniz.cpp -o leibniz -O3 -s -flto {MARCH_NATIVE} -ffast-math",
        run="./leibniz",
        version_cmd="clang++ --version",
        base="cplusplus",
        category="systems",
    ),
    "cpp-avx2": Language(
        name="C++ (AVX2)",
        nixpkgs=("gcc@15.2.0",),
        file="leibniz.cpp",
        compile="g++ leibniz.cpp -o leibniz -O3 -s -flto -mavx2 -mfma -ffast-math",
        run="./leibniz",
        version_cmd="g++ --version",
        base="cplusplus",
        category="systems",
    ),
    # =========================================================================
    # Crystal / Swift
    # =========================================================================
    "crystal": Language(
        name="Crystal",
        nixpkgs=("crystal@1.18.2",),
        file="leibniz.cr",
        compile="crystal build leibniz.cr --release",
        run="./leibniz",
        version_cmd="crystal -v",
        base="crystal",
        category="systems",
    ),
    "swift": Language(
        name="Swift",
        nixpkgs=("swift@5.10.1",),
        file="leibniz.swift",
        compile="swiftc -O -o leibniz leibniz.swift",
        run="./leibniz",
        version_cmd="swift --version",
        base="swift",
        category="systems",
    ),
    "swift-simd": Language(
        name="Swift (SIMD)",
        nixpkgs=("swift@5.10.1",),
        file="leibniz-simd.swift",
        compile="swiftc -O -o leibniz leibniz-simd.swift",
        run="./leibniz",
        version_cmd="swift --version",
        base="swift",
        category="systems",
    ),
    "swift-relaxed": Language(
        name="Swift (Relaxed)",
        nixpkgs=("swift@5.10.1",),
        file="leibniz.swift",
        compile="swiftc -O -enable-experimental-feature Extern -Xcc -ffast-math -o leibniz leibniz.swift",
        run="./leibniz",
        version_cmd="swift --version",
        base="swift",
        category="systems",
    ),
    "objc": Language(
        name="Objective-C",
        nixpkgs=("clang@21.1.2", "gnustep-base@1.30.0"),
        file="leibniz.m",
        compile=f"clang {C_FLAGS} $(gnustep-config --objc-flags) $(gnustep-config --base-libs) -o leibniz leibniz.m",
        run="./leibniz",
        version_cmd="clang --version",
        base="objectivec",
        category="systems",
    ),
    # =========================================================================
    # .NET Languages (using Nix flakes for better compatibility)
    # =========================================================================
    "csharp": Language(
        name="C#",
        nix_flake=(nix_pkg("dotnet-sdk_8"),),
        nix_flake_version="8.0.403",
        file="leibniz.cs",
        compile="dotnet new console -n leibniz -o _build --force && cp leibniz.cs _build/Program.cs && cd _build && dotnet publish -c Release -o ../out",
        run="./out/leibniz",
        version_cmd="dotnet --version",
        base="csharp",
        category="dotnet",
    ),
    "csharp-simd": Language(
        name="C# (SIMD)",
        nix_flake=(nix_pkg("dotnet-sdk_8"),),
        nix_flake_version="8.0.403",
        file="leibniz-simd.cs",
        compile="dotnet new console -n leibniz -o _build --force && cp leibniz-simd.cs _build/Program.cs && cd _build && dotnet publish -c Release -o ../out",
        run="./out/leibniz",
        version_cmd="dotnet --version",
        base="csharp",
        category="dotnet",
    ),
    "fsharp": Language(
        name="F#",
        nix_flake=(nix_pkg("dotnet-sdk_8"),),
        nix_flake_version="8.0.403",
        file="fs/Program.fs",
        compile="cd fs && dotnet publish -c Release -o ../out",
        run="./out/leibniz",
        version_cmd="dotnet --version",
        base="fsharp",
        category="dotnet",
    ),
    # =========================================================================
    # JVM Languages (using Nix flakes for JDK)
    # =========================================================================
    "java": Language(
        name="Java",
        nix_flake=(nix_pkg("jdk21"),),
        nix_flake_version="21.0.2",
        file="leibniz.java",
        compile="javac leibniz.java",
        run="java leibniz",
        version_cmd="java --version",
        base="java",
        category="jvm",
    ),
    "java-graalvm": Language(
        name="Java (GraalVM)",
        nix_flake=(nix_pkg("graalvm-ce"),),
        nix_flake_version="23.0.1",
        file="leibniz.java",
        compile="javac leibniz.java && native-image leibniz -o leibniz-native",
        run="./leibniz-native",
        version_cmd="java --version",
        base="java",
        category="jvm",
    ),
    "java-vecops": Language(
        name="Java (Vec Ops)",
        nix_flake=(nix_pkg("jdk23"),),  # Vector API needs JDK 23+
        nix_flake_version="23",
        file="leibnizVecOps.java",
        compile="javac --add-modules jdk.incubator.vector leibnizVecOps.java",
        run="java --add-modules jdk.incubator.vector leibnizVecOps",
        version_cmd="java -version 2>&1 | head -1",
        base="java",
        category="jvm",
    ),
    "kotlin": Language(
        name="Kotlin",
        nix_flake=(nix_pkg("kotlin"), nix_pkg("jdk21")),
        nix_flake_version="2.2.21",
        file="leibniz.kt",
        compile="kotlinc leibniz.kt -include-runtime -d leibniz.jar",
        run="java -jar leibniz.jar",
        version_cmd="kotlin -version",
        base="kotlin",
        category="jvm",
    ),
    "scala": Language(
        name="Scala",
        nixpkgs=("scala-cli@1.10.1",),
        file="leibniz.scala",
        compile="scala-cli package leibniz.scala -o leibniz --native --native-mode release-full --power",
        run="./leibniz",
        version_cmd="scala-cli version --scala",
        base="scala",
        category="jvm",
    ),
    "clojure": Language(
        name="Clojure",
        nix_flake=(nix_pkg("clojure"), nix_pkg("jdk21")),
        nix_flake_version="1.12.3.1577",
        file="leibniz.clj",
        run="clj leibniz.clj",
        version_cmd="clj --version",
        base="clojure",
        category="jvm",
    ),
    "groovy": Language(
        name="Groovy",
        nix_flake=(nix_pkg("groovy"), nix_pkg("jdk21")),
        nix_flake_version="5.0.2",
        file="leibniz.groovy",
        run="groovy leibniz.groovy",
        version_cmd="groovy --version",
        base="groovy",
        category="jvm",
    ),
    # =========================================================================
    # Interpreted Languages
    # =========================================================================
    "python": Language(
        name="Python (CPython)",
        nixpkgs=("python3@3.12.8",),
        file="leibniz.py",
        run="python leibniz.py",
        version_cmd="python --version",
        base="python",
        category="interpreted",
    ),
    "pypy": Language(
        name="Python (PyPy)",
        nixpkgs=("pypy3@7.3.20",),
        file="leibniz.py",
        run="pypy3 leibniz.py",
        version_cmd="pypy3 --version",
        base="python",
        category="interpreted",
    ),
    "cpython-numpy": Language(
        name="Python (NumPy)",
        nix_flake=(nix_pkg("python3"), nix_pkg("python3Packages.numpy")),
        nix_flake_version="3.12.8",
        file="leibniz_np.py",
        run="python3 leibniz_np.py",
        version_cmd="python3 --version",
        base="python",
        category="interpreted",
    ),
    "mypyc": Language(
        name="Python (mypyc)",
        nixpkgs=("python3@3.12.8", "gcc@14.2.0"),
        nix_setup="pip install mypy",
        file="leibniz.py",
        compile="mypyc leibniz.py && python -c 'import leibniz'",
        run="python -c 'import leibniz'",
        version_cmd="python --version",
        base="python",
        category="compiled",
    ),
    "ruby": Language(
        name="Ruby",
        nixpkgs=("ruby@3.4.7",),
        file="leibniz.rb",
        run="ruby leibniz.rb",
        version_cmd="ruby --version",
        base="ruby",
        category="interpreted",
    ),
    "nodejs": Language(
        name="Javascript (nodejs)",
        nixpkgs=("nodejs@25.2.1",),
        file="leibniz.js",
        run="node leibniz.js",
        version_cmd="node --version",
        base="nodejs",
        category="interpreted",
    ),
    "bun": Language(
        name="Javascript (bun)",
        nixpkgs=("bun@1.3.3",),
        file="leibniz.js",
        run="bun leibniz.js",
        version_cmd="bun --version",
        base="bun",
        category="interpreted",
    ),
    "deno": Language(
        name="Deno (TypeScript)",
        nixpkgs=("deno@2.5.6",),
        file="leibniz.ts",
        run="deno run --allow-read leibniz.ts",
        version_cmd="deno --version",
        base="deno",
        category="interpreted",
    ),
    "lua": Language(
        name="Lua",
        nixpkgs=("lua5_4@5.4.4",),
        file="leibniz.lua",
        run="lua leibniz.lua",
        version_cmd="lua -v",
        base="lua",
        category="interpreted",
    ),
    "luajit": Language(
        name="LuaJIT",
        nixpkgs=("luajit@2.1.1741730670",),
        file="leibniz.lua",
        run="luajit leibniz.lua",
        version_cmd="luajit -v",
        base="lua",
        category="interpreted",
    ),
    "perl": Language(
        name="Perl",
        nixpkgs=("perl@5.40.0",),
        file="leibniz.pl",
        run="perl leibniz.pl",
        version_cmd="perl -v",
        base="perl",
        category="interpreted",
        version_regex=r"v(\d+\.\d+\.\d+)",
    ),
    "php": Language(
        name="PHP",
        nixpkgs=("php@8.4.15",),
        file="leibniz.php",
        run="php -dopcache.enable_cli=1 -dopcache.jit=1255 -dopcache.jit_buffer_size=64M leibniz.php",
        version_cmd="php --version",
        base="php",
        category="interpreted",
    ),
    "r": Language(
        name="R",
        nixpkgs=("R@4.5.2",),
        file="leibniz.r",
        run="Rscript leibniz.r",
        version_cmd="R --version",
        base="r",
        category="interpreted",
    ),
    # =========================================================================
    # Functional Languages
    # =========================================================================
    "haskell": Language(
        name="Haskell (GHC)",
        nixpkgs=("ghc@9.10.3",),
        file="leibniz.hs",
        compile="ghc -O2 -o leibniz leibniz.hs",
        run="./leibniz",
        version_cmd="ghc --version",
        base="haskell",
        category="functional",
    ),
    "ocaml": Language(
        name="OCaml",
        nixpkgs=("ocaml@5.3.0",),
        file="leibniz.ml",
        compile="ocamlopt -O2 -o leibniz leibniz.ml",
        run="./leibniz",
        version_cmd="ocamlopt -version",
        base="ocaml",
        category="functional",
    ),
    "elixir": Language(
        name="Elixir",
        nixpkgs=("elixir@1.18.1",),
        file="leibniz.ex",
        run="elixir leibniz.ex",
        version_cmd="elixir --version",
        base="elixir",
        category="functional",
        version_regex=r"Elixir (\d+\.\d+\.\d+)",
    ),
    "erlang": Language(
        name="Erlang",
        nixpkgs=("erlang@27.2",),
        file="leibniz.erl",
        compile="erlc leibniz.erl",
        run="erl -noshell -s leibniz main -s init stop",
        version_cmd="erl -eval 'io:format(\"~s~n\", [erlang:system_info(otp_release)]), halt().' -noshell",
        base="erlang",
        category="functional",
    ),
    "racket": Language(
        name="Racket",
        nixpkgs=("racket@8.18",),
        file="leibniz.rkt",
        run="racket leibniz.rkt",
        version_cmd="racket --version",
        base="racket",
        category="functional",
    ),
    "sbcl": Language(
        name="Common Lisp (SBCL)",
        nixpkgs=("sbcl@2.5.10",),
        file="leibniz.lisp",
        run="sbcl --script leibniz.lisp",
        version_cmd="sbcl --version",
        base="lisp",
        category="functional",
    ),
    "gleam": Language(
        name="Gleam",
        nixpkgs=("gleam@1.13.0", "erlang@27.2"),
        file="leibniz.gleam",
        compile="gleam build",
        run="gleam run",
        version_cmd="gleam --version",
        base="gleam",
        category="functional",
    ),
    # =========================================================================
    # Other Compiled Languages
    # =========================================================================
    "fortran": Language(
        name="Fortran 90",
        nixpkgs=("gfortran@14.3.0",),
        file="leibniz.f90",
        compile=f"gfortran leibniz.f90 -o leibniz -O3 -flto {MARCH_NATIVE}",
        run="./leibniz",
        version_cmd="gfortran --version",
        base="fortran",
        category="systems",
    ),
    "pascal": Language(
        name="Pascal (FPC)",
        nixpkgs=("fpc@3.2.2",),
        file="leibniz.pas",
        compile="fpc -O3 -XX -Xs leibniz.pas",
        run="./leibniz",
        version_cmd="fpc -version",
        base="pascal",
        category="systems",
    ),
    "ada": Language(
        name="Ada (gnat-gcc)",
        nixpkgs=("gnat@13.4.0",),
        file="leibniz.adb",
        compile=f"gnatmake -O3 {MARCH_NATIVE} -gnatp leibniz.adb -o leibniz",
        run="./leibniz",
        version_cmd="gnat --version",
        base="ada",
        category="systems",
    ),
    # =========================================================================
    # Additional Languages
    # =========================================================================
    "dart": Language(
        name="Dart",
        nixpkgs=("dart@3.9.4",),
        file="leibniz.dart",
        run="dart leibniz.dart",
        version_cmd="dart --version",
        base="dart",
        category="interpreted",
    ),
    "dart-aot": Language(
        name="Dart (AOT)",
        nixpkgs=("dart@3.9.4",),
        file="leibniz.dart",
        compile="dart compile exe leibniz.dart -o leibniz",
        run="./leibniz",
        version_cmd="dart --version",
        base="dart",
        category="compiled",
    ),
    "janet-compiled": Language(
        name="Janet (compiled)",
        nixpkgs=("janet@1.39.1",),
        file="leibniz.janet",
        compile="janet -c leibniz.janet leibniz.jimage",
        run="janet leibniz.jimage",
        version_cmd="janet -v",
        base="janet",
        category="compiled",
    ),
    "julia": Language(
        name="Julia",
        nixpkgs=("julia@1.12.1",),
        file="leibniz.jl",
        run="julia leibniz.jl",
        version_cmd="julia --version",
        base="julia",
        category="jit",
    ),
    "raku": Language(
        name="Raku",
        nixpkgs=("rakudo@2025.06.1",),
        file="leibniz.raku",
        run="raku leibniz.raku",
        version_cmd="raku --version",
        base="raku",
        category="interpreted",
        version_regex=r"v(\d+\.\d+\.?\d*)",
    ),
    "pony": Language(
        name="Pony",
        nixpkgs=("ponyc@0.60.4",),
        file="leibniz.pony",
        compile="ponyc .",
        run="./leibniz",
        version_cmd="ponyc --version",
        base="pony",
        category="compiled",
    ),
    "haxe": Language(
        name="Haxe",
        nixpkgs=("haxe@4.3.6",),
        file="Leibniz.hx",
        compile="haxe -main Leibniz -cpp out && cd out && make",
        run="./out/Leibniz",
        version_cmd="haxe --version",
        base="haxe",
        category="compiled",
    ),
    # =========================================================================
    # WebAssembly
    # =========================================================================
    "wasm": Language(
        name="WASM (C via Wasmtime)",
        nix_flake=(nix_pkg("wasmtime"), nix_pkg("wasi-sdk")),
        nix_flake_version="39.0.1",
        file="leibniz.c",
        compile="$WASI_SDK_PATH/bin/clang -O3 -o leibniz.wasm leibniz.c",
        run="wasmtime --dir=. leibniz.wasm",
        version_cmd="wasmtime --version",
        base="wasm",
        category="compiled",
        x86_64_only=True,  # wasi-sdk not available for aarch64
    ),
}


# =============================================================================
# Utilities
# =============================================================================


def get_all_versions() -> dict[str, str]:
    """Get all pinned versions for external tooling."""
    return {name: lang.primary_version for name, lang in LANGUAGES.items()}


def get_languages_by_category() -> dict[str, list[str]]:
    """Group language names by category."""
    categories: dict[str, list[str]] = {}
    for name, lang in LANGUAGES.items():
        categories.setdefault(lang.category, []).append(name)
    return categories


def get_language(name: str) -> Language:
    """Get a language by target name."""
    return LANGUAGES[name]


def get_variants(base: str) -> list[str]:
    """Get all variant targets for a base language."""
    return [name for name, lang in LANGUAGES.items() if lang.base == base]


# =============================================================================
# Self-test
# =============================================================================

if __name__ == "__main__":
    print(f"Defined {len(LANGUAGES)} languages:\n")

    for category, langs in sorted(get_languages_by_category().items()):
        print(f"  {category.upper()}:")
        for name in sorted(langs):
            lang = LANGUAGES[name]
            pkg_type = "flake" if lang.uses_nix_flake else "devbox"
            compiled = "compiled" if lang.compile else "interpreted"
            print(f"    {name:15} -> {lang.name:20} [{pkg_type}] ({compiled})")
        print()

    print(f"Architecture: {ARCH}")
    print(f"March flag: {MARCH_NATIVE}")
