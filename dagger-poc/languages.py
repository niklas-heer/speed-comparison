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

import hashlib
import json
import os
import re
from dataclasses import dataclass
from typing import Optional

# =============================================================================
# Compiler Flags
# =============================================================================

# Compiler optimization flags for x86_64 targets
# Note: All builds run on x86_64 Linux (homelab Argo or the optional Dagger adapter)
MARCH_NATIVE = "-march=native"

# Shared tooling versions used for image builds
HYPERFINE_VERSION = "1.18.0"
MICROPYTHON_VERSION = "1.24.1"

# Pinned Devbox base image for reproducibility.
# Can be overridden by DEVBOX_IMAGE for experiments.
DEFAULT_DEVBOX_IMAGE = "jetpackio/devbox:0.16.0@sha256:0475601f3ddbc1d06be7f7d4d51143dcc005400593407d5e3627fdf7edf6c7dd"

# Swift 5.8 has cached binaries at this immutable Nixpkgs revision.
SWIFT_NIXPKGS = "github:NixOS/nixpkgs/b134951a4c9f3c995fd7be05f3243f8ecd65d798"
SWIFT_PACKAGES = tuple(f"{SWIFT_NIXPKGS}#{package}" for package in
                       ("swift", "gcc", "llvmPackages_15.clang", "swiftPackages.Dispatch"))
# Dispatch is a transitive runtime dependency outside Devbox's default outputs.
SWIFT_SETUP = (
    "printf 'export LD_LIBRARY_PATH=%s\\n' "
    f"\"$(nix --extra-experimental-features 'nix-command flakes' eval --raw {SWIFT_NIXPKGS}#swiftPackages.Dispatch.outPath)/lib\" > .swift-env"
)
SWIFT_COMPILER = (
    'env -u NIX_CC swiftc -tools-directory "$(dirname $(command -v clang))" '
    "-target x86_64-unknown-linux-gnu -O -o leibniz "
)

# =============================================================================
# Language Configuration
# =============================================================================


@dataclass(frozen=True)
class Language:
    """Configuration for a benchmark language.

    Container Build Strategy:
    Uses Devbox to install packages from nixpkgs.

    Attributes:
        name: Display name shown in charts
        file: Source file name in src/ directory
        extra_files: Additional source files needed at build/run time
        run: Command to execute the benchmark
        compile: Optional compilation command
        version_cmd: Command to get version string
        base: Base language for icon mapping (e.g., "rust" for "rust-nightly")
        category: Language category for organization
        version_regex: Regex to extract version from version_cmd output
        nixpkgs: List of Devbox packages ["pkg@version", ...]
        nix_flakes: List of Nix flake refs for packages not available in Devbox
                    or that need a specific nixpkgs channel for binary cache hits.
                    Format: ("github:NixOS/nixpkgs/b134951a4c9f3c995fd7be05f3243f8ecd65d798#swift",)
        nix_setup: Optional shell commands to run after packages are installed
    """

    name: str
    file: str
    run: str
    extra_files: tuple[str, ...] = ()
    compile: Optional[str] = None
    version_cmd: Optional[str] = None
    base: Optional[str] = None
    category: str = "other"
    version_regex: str = r"(\d+\.\d+\.?\d*)"

    # Devbox packages: ("go@1.23.4", "gcc@14.2.0")
    nixpkgs: tuple[str, ...] = ()
    # Nix flake refs for binary cache hits: ("github:NixOS/nixpkgs/b134951a4c9f3c995fd7be05f3243f8ecd65d798#swift",)
    nix_flakes: tuple[str, ...] = ()
    nix_setup: Optional[str] = None  # Post-install setup commands

    # Security - specify insecure dependency names that need to be allowed
    # e.g., haxe depends on mbedtls which is marked insecure
    allow_insecure: tuple[str, ...] = ()

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
        if not self.nixpkgs and not self.nix_flakes:
            raise ValueError(f"Language {self.name} must have nixpkgs or nix_flakes")

        # Validate nixpkgs have versions
        for pkg in self.nixpkgs:
            if "@" not in pkg or not re.fullmatch(r"[0-9][0-9A-Za-z.+_-]*", pkg.rsplit("@", 1)[1]):
                raise ValueError(f"Package '{pkg}' must have version (e.g., '{pkg}@1.0.0')")

        for ref in self.nix_flakes:
            if not re.fullmatch(r"github:[\w.-]+/[\w.-]+/[0-9a-f]{40}#[\w.^+-]+", ref):
                raise ValueError(f"Flake '{ref}' must use an immutable 40-character commit")

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
        elif self.nix_flakes:
            # Extract package from flake ref: "github:NixOS/nixpkgs/b134951a4c9f3c995fd7be05f3243f8ecd65d798#swift" -> "swift"
            return self.nix_flakes[0].split("#")[-1]
        return "unknown"

    @property
    def primary_version(self) -> str:
        """Get the pinned version of the primary package."""
        if self.nixpkgs:
            return self.nixpkgs[0].split("@")[1]
        elif self.nix_flakes:
            # Flake image versions use the immutable source revision.
            ref = self.nix_flakes[0]
            return ref.split("/")[-1].split("#")[0]
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

C_FLAGS = (
    f"-O3 -s -flto {MARCH_NATIVE} -mtune=native -fomit-frame-pointer "
    "-fno-signed-zeros -fno-trapping-math -fassociative-math"
)
CPP_FLAGS = (
    f"-O3 -s -flto {MARCH_NATIVE} -mtune=native -fomit-frame-pointer "
    "-fno-signed-zeros -fno-trapping-math -fassociative-math -std=c++17"
)
RUST_FLAGS = "-C opt-level=3 -C target-cpu=native -C lto=fat -C codegen-units=1 -C panic=abort"


# =============================================================================
# Language Definitions
# =============================================================================

LANGUAGES: dict[str, Language] = {
    # =========================================================================
    # Systems Languages (Compiled, low-level)
    # =========================================================================
    "c": Language(
        name="C (gcc)",
        nixpkgs=("gcc@16.2.0",),
        file="leibniz.c",
        compile=f"gcc {C_FLAGS} -o leibniz leibniz.c -lm",
        run="./leibniz",
        version_cmd="gcc --version",
        base="c",
        category="systems",
    ),
    "c-clang": Language(
        name="C (clang)",
        nixpkgs=("clang@21.1.8",),
        file="leibniz.c",
        compile=f"clang {C_FLAGS} -o leibniz leibniz.c -lm",
        run="./leibniz",
        version_cmd="clang --version",
        base="c",
        category="systems",
    ),
    "rust": Language(
        name="Rust",
        nixpkgs=("rustc@1.97.1",),
        file="leibniz.rs",
        compile=f"rustc {RUST_FLAGS} -o leibniz leibniz.rs",
        run="./leibniz",
        version_cmd="rustc --version",
        base="rust",
        category="systems",
    ),
    "rust-nightly": Language(
        name="Rust (nightly SIMD)",
        nixpkgs=("gcc@16.2.0",),
        nix_flakes=("github:nix-community/fenix/03864c059200a8a96f2ee6bb050c69eae96f57ca#minimal.toolchain",),
        file="leibniz_nightly.rs",
        compile="rustc -C debuginfo=0 -C opt-level=3 -C target-cpu=native -C lto=fat -C codegen-units=1 -C panic=abort leibniz_nightly.rs -o leibniz",
        run="./leibniz", version_cmd="rustc --version", base="rust", category="systems",
    ),
    "rust-fastmath": Language(
        name="Rust (fast-math)",
        nixpkgs=("rustc@1.97.1",),
        file="leibniz.rs",
        compile=f"rustc {RUST_FLAGS} -C llvm-args=-enable-unsafe-fp-math -o leibniz leibniz.rs",
        run="./leibniz", version_cmd="rustc --version", base="rust", category="systems",
    ),
    "rust-simd": Language(
        name="Rust (SIMD)",
        nixpkgs=("rustc@1.97.1",),
        file="leibniz_simd.rs",
        compile=f"rustc {RUST_FLAGS} -o leibniz leibniz_simd.rs",
        run="./leibniz", version_cmd="rustc --version", base="rust", category="systems",
    ),
    "zig-simd": Language(
        name="Zig (SIMD)", nixpkgs=("zig@0.16.0",), file="leibniz-simd.zig",
        compile="zig build-exe -OReleaseFast leibniz-simd.zig -fno-stack-check -femit-bin=leibniz",
        run="./leibniz", version_cmd="zig version", base="zig", category="systems",
    ),
    "hare": Language(
        name="Hare", nixpkgs=("hare@0.26.0.1",), file="leibniz.ha",
        compile="hare build -R -o leibniz leibniz.ha", run="./leibniz",
        version_cmd="hare version", base="hare", category="systems",
    ),
    "chezscheme": Language(
        name="Chez Scheme", nixpkgs=("chez@10.4.1",), file="leibniz.ss",
        compile="echo '(compile-program \"leibniz.ss\")' | scheme --optimize-level 3",
        run="scheme --program leibniz.so", version_cmd="scheme --version",
        base="lisp", category="functional",
    ),
    "janet": Language(
        name="Janet", nixpkgs=("janet@1.41.2",), file="leibniz.janet",
        run="janet leibniz.janet", version_cmd="janet --version", base="janet", category="interpreted",
    ),
    "sbcl-simd": Language(
        name="Common Lisp (SBCL SIMD)", nixpkgs=("sbcl@2.6.7",), file="leibniz-sbcl-simd.lisp",
        compile="sbcl --noinform --load leibniz-sbcl-simd.lisp --eval '(sb-ext:save-lisp-and-die \"leibniz\" :executable t :toplevel (quote cl-user::main) :purify t)'",
        run="./leibniz", version_cmd="sbcl --version", base="lisp", category="functional",
    ),
    "cpython-numba": Language(
        name="Python (Numba)", nixpkgs=("python3@3.12.8", "python312Packages.numba@0.61.0"),
        file="leibniz_numba.py", run="python3 leibniz_numba.py",
        version_cmd="python3 --version", base="python", category="jit",
    ),
    "mojo": Language(
        name="Mojo", nixpkgs=("python3@3.12.8", "uv@0.5.11", "gcc@16.2.0", "patchelf@0.15.2"),
        file="leibniz.mojo", extra_files=("mojo-requirements.txt",),
        compile="uv venv /app/.mojo && uv pip install --python /app/.mojo/bin/python --require-hashes -r mojo-requirements.txt && /app/.mojo/bin/mojo build -O3 leibniz.mojo -o leibniz && patchelf --force-rpath --set-interpreter \"$(cat $NIX_CC/nix-support/dynamic-linker)\" --set-rpath \"$(patchelf --print-rpath leibniz):$(dirname $(g++ -print-file-name=libstdc++.so.6))\" leibniz",
        run="./leibniz", version_cmd="/app/.mojo/bin/mojo --version",
        base="mojo", category="compiled",
    ),
    "nasm": Language(
        name="Assembly (NASM x64)", nixpkgs=("nasm@3.02", "gcc@16.2.0"),
        file="leibniz.asm", compile="nasm -f elf64 leibniz.asm -o leibniz.o && gcc -o leibniz leibniz.o",
        run="./leibniz", version_cmd="nasm -v", base="default", category="systems",
    ),
    "fsharp-simd": Language(
        name="F# (SIMD)", nixpkgs=("dotnet-sdk@8.0.424",), file="fs-simd/Program.fs",
        compile="dotnet publish fs-simd/leibniz.fsproj -c Release -o out",
        run="dotnet ./out/leibniz.dll", version_cmd="dotnet --version", base="fsharp", category="dotnet",
    ),
    "kotlin-native": Language(
        name="Kotlin (Native)", nixpkgs=("kotlin-native@2.2.21", "curl@8.11.1"), file="leibniz-native.kt",
        nix_setup=(
            "curl --fail --location --retry 3 https://download.jetbrains.com/kotlin/native/libffi-3.2.1-2-linux-x86-64.tar.gz -o /tmp/kotlin-libffi.tar.gz; "
            "echo '9d817bbca098a2fa0f1d5a8b9e57674c30d100bb4c6aeceff18d8acc5b9f382c  /tmp/kotlin-libffi.tar.gz' | sha256sum -c -; tar -xf /tmp/kotlin-libffi.tar.gz -C /tmp; "
            "KOTLIN_ROOT=$(dirname $(readlink -f $(command -v kotlinc-native)))/..; "
            "mkdir -p /tmp/kotlin-home/klib/cache; "
            "for kotlin_entry in \"$KOTLIN_ROOT\"/*; do test \"$(basename $kotlin_entry)\" = klib || ln -sf \"$kotlin_entry\" /tmp/kotlin-home/; done; "
            "for kotlin_entry in \"$KOTLIN_ROOT\"/klib/*; do test \"$(basename $kotlin_entry)\" = cache || ln -sf \"$kotlin_entry\" /tmp/kotlin-home/klib/; done"
        ),
        compile="LD_PRELOAD=/tmp/libffi-3.2.1-2-linux-x86-64/lib/libffi.so.7 kotlinc-native leibniz-native.kt -opt -Dkonan.home=/tmp/kotlin-home -o leibniz",
        run="./leibniz.kexe", version_cmd="kotlinc-native -version", base="kotlin", category="compiled",
    ),
    "go": Language(
        name="Go",
        nixpkgs=("go@1.27.0",),
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
        nixpkgs=("odin@2026-07a",),
        file="leibniz.odin",
        compile="odin build leibniz.odin -file -o:speed -no-bounds-check -disable-assert -out:leibniz",
        run="./leibniz",
        version_cmd="odin version",
        base="odin",
        category="systems",
        version_regex=r"dev-(\d{4}-\d+[a-z]?)",
    ),
    "zig": Language(
        name="Zig",
        nixpkgs=("zig@0.16.0",),
        file="leibniz.zig",
        compile="zig build-exe -OReleaseFast leibniz.zig -fno-stack-check",
        run="./leibniz",
        version_cmd="zig version",
        base="zig",
        category="systems",
    ),
    "nim": Language(
        name="Nim",
        nixpkgs=("nim@2.2.10",),
        file="leibniz.nim",
        compile=(
            "nim c --verbosity:0 -d:danger -d:lto --gc:arc "
            f"--passC:'{MARCH_NATIVE} -fno-signed-zeros -fno-trapping-math -fassociative-math' "
            "--passL:'-s' -o:leibniz leibniz.nim"
        ),
        run="./leibniz",
        version_cmd="nim --version",
        base="nim",
        category="systems",
    ),
    "v": Language(
        name="V",
        nixpkgs=("vlang@0.5.2",),
        file="leibniz.v",
        compile="v -prod -o leibniz leibniz.v",
        run="./leibniz",
        version_cmd="v version",
        base="v",
        category="systems",
    ),
    "d": Language(
        name="D (GDC)",
        nixpkgs=("gdc@11.5.0",),  # 15.2.0 not available in nixpkgs
        file="leibniz.d",
        compile=f"gdc leibniz.d -o leibniz -O3 -frelease -flto -ffast-math {MARCH_NATIVE}",
        run="./leibniz",
        version_cmd="gdc --version",
        base="d",
        category="systems",
    ),
    "d-ldc": Language(
        name="D (LDC)",
        nixpkgs=("ldc@1.42.0",),
        file="leibniz.d",
        compile=(
            # Avoid static link mode in nixpkgs hosted environments where libc static
            # artifacts are not available by default.
            # Keep codegen flags deterministic across hosts (no native CPU probing).
            "ldc2 leibniz.d -of=leibniz -O3 -release -flto=thin -ffast-math"
        ),
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
        nixpkgs=("gcc@16.2.0",),
        file="leibniz.cpp",
        compile=f"g++ leibniz.cpp -o leibniz {CPP_FLAGS}",
        run="./leibniz",
        version_cmd="g++ --version",
        base="cplusplus",
        category="systems",
    ),
    "cpp-clang": Language(
        name="C++ (clang++)",
        nixpkgs=("clang@21.1.8", "lld@21.1.2", "gcc@16.2.0"),
        file="leibniz.cpp",
        # Note: -static removed because nixpkgs doesn't include glibc.static by default
        compile=(
            "clang++ -fuse-ld=lld leibniz.cpp -o leibniz "
            f"{CPP_FLAGS} "
            "-Wl,-rpath,$(dirname $(gcc -print-file-name=libstdc++.so.6)) "
            "-Wl,-rpath,$(dirname $(gcc -print-file-name=libgcc_s.so.1))"
        ),
        run="./leibniz",
        version_cmd="clang++ --version",
        base="cplusplus",
        category="systems",
    ),
    "cpp-avx2": Language(
        name="C++ (AVX2)",
        nixpkgs=("gcc@16.2.0",),
        file="leibniz_avx2.cpp",
        compile=f"g++ leibniz_avx2.cpp -o leibniz_avx2 {CPP_FLAGS} -mavx2",
        run="./leibniz_avx2",
        version_cmd="g++ --version",
        base="cplusplus",
        category="systems",
    ),
    # =========================================================================
    # Crystal / Swift
    # =========================================================================
    "crystal": Language(
        name="Crystal",
        nixpkgs=("crystal@1.19.1",),
        file="leibniz.cr",
        compile="crystal build leibniz.cr --release",
        run="./leibniz",
        version_cmd="crystal -v",
        base="crystal",
        category="systems",
    ),
    "swift": Language(
        name="Swift",
        nix_flakes=SWIFT_PACKAGES,
        nix_setup=SWIFT_SETUP,
        file="leibniz.swift",
        compile=SWIFT_COMPILER + "leibniz.swift",
        run=". ./.swift-env && ./leibniz < rounds.txt",
        version_cmd="swift --version",
        base="swift",
        category="systems",
    ),
    "swift-simd": Language(
        name="Swift (SIMD)",
        nix_flakes=SWIFT_PACKAGES,
        nix_setup=SWIFT_SETUP,
        file="leibniz-simd.swift",
        compile=SWIFT_COMPILER + "leibniz-simd.swift",
        run=". ./.swift-env && ./leibniz < rounds.txt",
        version_cmd="swift --version",
        base="swift",
        category="systems",
    ),
    "swift-relaxed": Language(
        name="Swift (relaxed)",
        nix_flakes=SWIFT_PACKAGES,
        nix_setup=SWIFT_SETUP,
        file="leibniz-relaxed.swift",
        extra_files=("relaxed.h",),
        compile=SWIFT_COMPILER + "leibniz-relaxed.swift -import-objc-header relaxed.h",
        run=". ./.swift-env && ./leibniz < rounds.txt",
        version_cmd="swift --version",
        base="swift",
        category="systems",
    ),
    "objc": Language(
        name="Objective-C",
        nixpkgs=("clang@21.1.8", "gnustep-base@1.29.0"),  # 21.1.2/1.30.0 not available
        file="leibniz.m",
        compile=f"clang {C_FLAGS} $(gnustep-config --objc-flags) $(gnustep-config --base-libs) -o leibniz leibniz.m",
        run="./leibniz",
        version_cmd="clang --version",
        base="objectivec",
        category="systems",
    ),
    # =========================================================================
    # .NET Languages
    # =========================================================================
    "csharp": Language(
        name="C#",
        nixpkgs=("dotnet-sdk@8.0.424",),
        file="leibniz.cs",
        compile="dotnet new console -n leibniz -o _build --force && cp leibniz.cs _build/Program.cs && cd _build && dotnet publish -c Release -r linux-x64 --self-contained -o ../out",
        run="./out/leibniz",
        version_cmd="dotnet --version",
        base="csharp",
        category="dotnet",
    ),
    "csharp-simd": Language(
        name="C# (SIMD)",
        nixpkgs=("dotnet-sdk@8.0.424",),
        file="leibniz-simd.cs",
        compile="dotnet new console -n leibniz -o _build --force && cp leibniz-simd.cs _build/Program.cs && cd _build && dotnet publish -c Release -r linux-x64 --self-contained -o ../out",
        run="./out/leibniz",
        version_cmd="dotnet --version",
        base="csharp",
        category="dotnet",
    ),
    "fsharp": Language(
        name="F#",
        nixpkgs=("dotnet-sdk@8.0.424",),
        file="fs/Program.fs",
        compile="dotnet publish fs/leibniz.fsproj -c Release -o out",
        run="dotnet ./out/leibniz.dll",
        version_cmd="dotnet --version",
        base="fsharp",
        category="dotnet",
    ),
    # =========================================================================
    # JVM Languages
    # =========================================================================
    "java": Language(
        name="Java",
        nixpkgs=("jdk@25+36",),
        file="leibniz.java",
        compile="javac leibniz.java",
        run="java leibniz",
        version_cmd="java --version",
        base="java",
        category="jvm",
    ),
    "java-graalvm": Language(
        name="Java (GraalVM)",
        nixpkgs=("graalvm-ce@23.0.1",),
        file="leibniz.java",
        compile="javac leibniz.java && native-image leibniz -o leibniz-native",
        run="./leibniz-native",
        version_cmd="java --version",
        base="java-graalvm",
        category="jvm",
    ),
    "java-vecops": Language(
        name="Java (Vec Ops)",
        nixpkgs=("jdk@25+36",),  # Vector API needs JDK 23+
        file="leibnizVecOps.java",
        compile="javac --add-modules jdk.incubator.vector leibnizVecOps.java",
        run="java --add-modules jdk.incubator.vector leibnizVecOps",
        version_cmd="java -version 2>&1 | head -1",
        base="java-vecops",
        category="jvm",
    ),
    "kotlin": Language(
        name="Kotlin",
        nixpkgs=("kotlin@2.4.10", "jdk@25+36"),
        file="leibniz.kt",
        compile="kotlinc leibniz.kt -include-runtime -d leibniz.jar",
        run="java -jar leibniz.jar",
        version_cmd="kotlin -version",
        base="kotlin",
        category="jvm",
    ),
    "scala": Language(
        name="Scala",
        nixpkgs=("scala-cli@1.16.0", "clang@21.1.8", "gcc@14.2.0"),
        file="leibniz.scala",
        compile="scala-cli package leibniz.scala -o leibniz --native --native-mode release-full --power",
        run="./leibniz",
        version_cmd="scala-cli version --scala",
        base="scala",
        category="jvm",
    ),
    "clojure": Language(
        name="Clojure",
        nixpkgs=("clojure@1.12.5.1664", "jdk@25+36"),
        file="leibniz.clj",
        run="clj leibniz.clj",
        version_cmd="clj --version",
        base="clojure",
        category="jvm",
    ),
    "groovy": Language(
        name="Groovy",
        nixpkgs=("groovy@5.0.4", "jdk@25+36"),
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
        nixpkgs=("python3@3.12.8", "python312Packages.numpy@2.2.2"),
        file="leibniz_np.py",
        run="python3 leibniz_np.py",
        version_cmd="python3 --version",
        base="cpython-numpy",
        category="interpreted",
    ),
    "mypyc": Language(
        name="Python (mypyc)",
        nixpkgs=("python3@3.12.8", "gcc@14.2.0", "uv@0.5.11"),
        nix_setup="uv venv /app/.venv && . /app/.venv/bin/activate && uv pip install mypy==2.3.1 setuptools==84.0.0 ast-serialize==0.9.0 librt==0.15.0 mypy-extensions==1.1.0 pathspec==1.1.1 typing-extensions==4.16.0",
        file="leibniz_mypyc.py",
        compile=(
            '. /app/.venv/bin/activate && '
            "CFLAGS='-O3 -march=native -mtune=native -fomit-frame-pointer "
            "-fno-signed-zeros -fno-trapping-math -fassociative-math' "
            'mypyc leibniz_mypyc.py && python -c "import leibniz_mypyc"'
        ),
        # Use double quotes to avoid conflict with hyperfine's single quotes
        run='. /app/.venv/bin/activate && python -c "import leibniz_mypyc"',
        version_cmd="python --version",
        base="python",
        category="compiled",
    ),
    "micropython": Language(
        name="MicroPython",
        nixpkgs=("micropython@1.27.0",),
        file="leibniz.py",
        run="micropython leibniz.py",
        version_cmd="micropython --version",
        base="micropython",
        category="interpreted",
    ),
    "ruby": Language(
        name="Ruby",
        nixpkgs=("ruby@4.0.6",),
        file="leibniz.rb",
        run="ruby --yjit leibniz.rb",
        version_cmd="ruby --version",
        base="ruby",
        category="interpreted",
    ),
    "nodejs": Language(
        name="Javascript (nodejs)",
        nixpkgs=("nodejs@26.8.1",),
        file="leibniz.js",
        run="node leibniz.js",
        version_cmd="node --version",
        base="nodejs",
        category="interpreted",
    ),
    "bun": Language(
        name="Javascript (bun)",
        nixpkgs=("bun@1.3.13",),
        file="leibniz.js",
        run="bun run leibniz.js",
        version_cmd="bun --version",
        base="bun",
        category="interpreted",
    ),
    "deno": Language(
        name="Deno (TypeScript)",
        nixpkgs=("deno@2.9.5",),
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
        nixpkgs=("luajit@2.1.1785763465",),
        file="leibniz.lua",
        run="luajit leibniz.lua",
        version_cmd="luajit -v",
        base="lua",
        category="interpreted",
    ),
    "octave": Language(
        name="Octave",
        nixpkgs=("octave@11.3.0",),
        file="leibniz_octave.m",
        run="octave leibniz_octave.m",
        version_cmd="octave -v",
        base="octave",
        category="interpreted",
    ),
    "octave-vectorised": Language(
        name="Octave (Vectorised)",
        nixpkgs=("octave@11.3.0",),
        file="leibniz_octave_vectorised.m",
        run="octave leibniz_octave_vectorised.m",
        version_cmd="octave -v",
        base="octave",
        category="interpreted",
    ),
    "perl": Language(
        name="Perl",
        nixpkgs=("perl@5.42.3",),
        file="leibniz.pl",
        run="perl leibniz.pl",
        version_cmd="perl -v",
        base="perl",
        category="interpreted",
        version_regex=r"v(\d+\.\d+\.\d+)",
    ),
    "php": Language(
        name="PHP",
        nixpkgs=("php@8.5.9",),
        file="leibniz.php",
        run="php -dopcache.enable_cli=1 -dopcache.jit=1255 -dopcache.jit_buffer_size=64M leibniz.php",
        version_cmd="php --version",
        base="php",
        category="interpreted",
    ),
    "r": Language(
        name="R",
        nixpkgs=("R@4.6.1",),
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
        nixpkgs=("ocaml@5.4.1",),
        file="leibniz.ml",
        compile="ocamlopt -O3 -o leibniz leibniz.ml",
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
        nixpkgs=("racket@9.3",),
        file="leibniz.rkt",
        run="racket leibniz.rkt",
        version_cmd="racket --version",
        base="racket",
        category="functional",
    ),
    "sbcl": Language(
        name="Common Lisp (SBCL)",
        nixpkgs=("sbcl@2.6.7",),
        file="leibniz.lisp",
        compile="sbcl --noinform --eval '(compile-file \"leibniz.lisp\")' --quit",
        run="sbcl --noinform --load leibniz.fasl --eval '(main)' --quit",
        version_cmd="sbcl --version",
        base="lisp",
        category="functional",
    ),
    "gleam": Language(
        name="Gleam",
        nixpkgs=("gleam@1.18.1", "erlang@27.2"),
        nix_setup="gleam new leibniz_app && cd leibniz_app && sed -i 's/\\[dependencies\\]/[dependencies]\\nsimplifile = \"~> 2.0\"/' gleam.toml && gleam deps download",
        file="leibniz.gleam",
        # Build in leibniz_app, then run with output to /app level
        compile="cp leibniz.gleam leibniz_app/src/leibniz_app.gleam && cp rounds.txt leibniz_app/ && cd leibniz_app && gleam build",
        # Use subshell so cd doesn't affect pi.txt location for scmeta
        run="(cd leibniz_app && gleam run)",
        version_cmd="gleam --version",
        base="gleam",
        category="functional",
    ),
    # =========================================================================
    # Other Compiled Languages
    # =========================================================================
    "fortran": Language(
        name="Fortran 90",
        nixpkgs=("gfortran@15.3.0",),
        file="leibniz.f90",
        compile=f"gfortran leibniz.f90 -o leibniz -Ofast -flto -ffast-math {MARCH_NATIVE} -mtune=native",
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
        version_cmd="fpc -iV",
        base="pascal",
        category="systems",
    ),
    "ada": Language(
        name="Ada (gnat-gcc)",
        nixpkgs=("gnat@15.3.0",),
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
        nixpkgs=("dart@3.13.0",),
        file="leibniz.dart",
        run="dart run leibniz.dart",
        version_cmd="dart --version",
        base="dart",
        category="interpreted",
    ),
    "dart-aot": Language(
        name="Dart (AOT)",
        nixpkgs=("dart@3.13.0",),
        file="leibniz.dart",
        compile="dart compile exe leibniz.dart -o leibniz",
        run="./leibniz",
        version_cmd="dart --version",
        base="dart",
        category="compiled",
    ),
    "janet-compiled": Language(
        name="Janet (compiled)",
        nixpkgs=("janet@1.41.2", "git@2.47.1", "gcc@14.2.0", "gnumake@4.4.1"),
        nix_setup=(
            "rm -rf /tmp/jpm /tmp/janet-modules /tmp/jpm-prefix && "
            "mkdir -p /tmp/janet-modules /tmp/jpm-prefix && "
            "git clone https://github.com/janet-lang/jpm.git /tmp/jpm && git -C /tmp/jpm checkout --detach 2430dec269f485473502bcdc2049ee332bc63908 && "
            "cd /tmp/jpm && JANET_PATH=/tmp/janet-modules JANET_PREFIX=/tmp/jpm-prefix janet bootstrap.janet && "
            "mkdir -p /tmp/jpm-prefix/bin /tmp/jpm-prefix/lib /tmp/jpm-prefix/include && "
            "cp -r \"$(dirname $(readlink -f $(command -v janet)))/../include/\"* /tmp/jpm-prefix/include/ && ln -sf \"$(command -v janet)\" /tmp/jpm-prefix/bin/janet && "
            "ln -sf \"$(dirname $(command -v janet))/../lib/libjanet.a\" /tmp/jpm-prefix/lib/libjanet.a"
        ),
        file="leibniz_compiled.janet",
        # Keep Janet manifests/modules in writable storage (not /nix/store).
        compile=(
            "mkdir -p /tmp/janet-modules && "
            "JANET_PATH=/tmp/janet-modules /tmp/jpm-prefix/bin/jpm quickbin leibniz_compiled.janet leibniz"
        ),
        run="./leibniz",
        version_cmd="janet -v",
        base="janet",
        category="compiled",
    ),
    "julia": Language(
        name="Julia",
        nixpkgs=("julia@1.12.7", "gcc@16.2.0"),
        file="julia/src/LeibnizApp.jl",
        compile=(
            "julia -e 'using Pkg; Pkg.activate(\"julia\"); Pkg.instantiate(); "
            "isempty(Pkg.Registry.reachable_registries()) && Pkg.Registry.add(\"General\"); "
            "Pkg.Apps.add(PackageSpec(name=\"JuliaC\", version=\"0.3.10\"))' && "
            "/tmp/bench-julia-depot/bin/juliac --output-exe leibniz --trim --experimental "
            "--bundle bun julia"
        ),
        run="bun/bin/leibniz",
        version_cmd="julia --version",
        base="julia",
        category="jit",
    ),
    "raku": Language(
        name="Raku",
        nixpkgs=("rakudo@2026.07",),
        file="leibniz.raku",
        run="raku leibniz.raku",
        version_cmd="raku --version",
        base="raku",
        category="interpreted",
        version_regex=r"v(\d+\.\d+\.?\d*)",
    ),
    "pony": Language(
        name="Pony",
        nixpkgs=("ponyc@0.68.0",),
        file="leibniz.pony",
        compile="ponyc ./ -o=out --bin-name=leibniz",
        run="./out/leibniz",
        version_cmd="ponyc --version",
        base="pony",
        category="compiled",
    ),
    "haxe": Language(
        name="Haxe",
        nix_flakes=(
            "github:NixOS/nixpkgs/b134951a4c9f3c995fd7be05f3243f8ecd65d798#haxe",
            "github:NixOS/nixpkgs/b134951a4c9f3c995fd7be05f3243f8ecd65d798#gcc",
        ),
        nix_setup="mkdir -p /tmp/haxelib && haxelib setup /tmp/haxelib && haxelib install hxcpp 4.3.2",
        file="Leibniz.hx",
        compile="haxe -main Leibniz -cpp out",
        run="./out/Leibniz",
        version_cmd="haxe --version",
        base="haxe",
        category="compiled",
        allow_insecure=("mbedtls-2.28.10",),  # haxe depends on insecure mbedtls
    ),
    # =========================================================================
    # WebAssembly
    # =========================================================================
    "wasm": Language(
        name="WASM (C via Wasmtime)",
        nixpkgs=("wasmtime@48.0.0", "wget@1.25.0"),
        nix_setup=(
            "wget -q https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-25/wasi-sdk-25.0-x86_64-linux.tar.gz && "
            "tar -xf wasi-sdk-25.0-x86_64-linux.tar.gz && "
            "mv wasi-sdk-25.0-x86_64-linux /tmp/wasi-sdk && "
            "rm wasi-sdk-25.0-x86_64-linux.tar.gz"
        ),
        file="leibniz.c",
        compile="/tmp/wasi-sdk/bin/clang -O3 -o leibniz.wasm leibniz.c",
        run="wasmtime --dir=. leibniz.wasm",
        version_cmd="wasmtime --version",
        base="wasm",
        category="compiled",
    ),
}


# =============================================================================
# Utilities
# =============================================================================


def get_all_versions() -> dict[str, str]:
    """Get all pinned versions for external tooling."""
    return {name: lang.primary_version for name, lang in LANGUAGES.items()}


def get_devbox_image() -> str:
    """Get the Devbox base image reference.

    Defaults to a pinned image+digest for reproducibility.
    """
    return os.environ.get("DEVBOX_IMAGE", DEFAULT_DEVBOX_IMAGE)


def language_image_fingerprint(
    lang: Language,
    *,
    devbox_image: str | None = None,
    hyperfine_version: str = HYPERFINE_VERSION,
    micropython_version: str = MICROPYTHON_VERSION,
) -> str:
    """Build a deterministic fingerprint for language image inputs."""
    payload = {
        "allow_insecure": list(lang.allow_insecure),
        "devbox_image": devbox_image or get_devbox_image(),
        "hyperfine_version": hyperfine_version,
        "micropython_version": micropython_version,
        "nix_flakes": list(lang.nix_flakes),
        "nix_setup": lang.nix_setup or "",
        "nixpkgs": list(lang.nixpkgs),
    }
    canonical = json.dumps(payload, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()[:12]


def language_image_version_tag(
    lang: Language,
    *,
    devbox_image: str | None = None,
    hyperfine_version: str = HYPERFINE_VERSION,
    micropython_version: str = MICROPYTHON_VERSION,
) -> str:
    """Return a Docker tag suffix encoding version + config fingerprint."""
    version = lang.primary_version.replace("+", "-")
    fingerprint = language_image_fingerprint(
        lang,
        devbox_image=devbox_image,
        hyperfine_version=hyperfine_version,
        micropython_version=micropython_version,
    )
    return f"{version}-{fingerprint}"


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


def get_base_image_name(target: str, lang: Language | None = None) -> str:
    """Get the base image name for a language target.

    Languages can share images only if they have the same `base` AND the same
    `nixpkgs` packages. This prevents issues where cpp (gcc) and cpp-clang (clang)
    incorrectly share an image despite needing different compilers.

    Strategy:
    1. Group all languages with the same base by their packages
    2. For each package group, use a canonical image name:
       - If a target matches the base name, use that
       - Otherwise, use the alphabetically first target in the group

    Args:
        target: The language target name (e.g., "swift-simd")
        lang: Optional Language object (avoids dict lookup if already available)

    Returns:
        The base image name (e.g., "swift" or "cpp-clang" if packages differ)
    """
    if lang is None:
        lang = LANGUAGES[target]

    base = lang.base or target

    # Find all languages with the same base AND same image inputs as this one
    same_package_group = []
    for other_target, other_lang in LANGUAGES.items():
        other_base = other_lang.base or other_target
        if (
            other_base == base
            and other_lang.nixpkgs == lang.nixpkgs
            and other_lang.nix_flakes == lang.nix_flakes
            and other_lang.nix_setup == lang.nix_setup
            and other_lang.allow_insecure == lang.allow_insecure
        ):
            same_package_group.append(other_target)

    # Sort for deterministic ordering
    same_package_group.sort()

    # Prefer the base name if it's in the group, otherwise use first alphabetically
    if base in same_package_group:
        return base
    return same_package_group[0]


def get_base_languages() -> dict[str, tuple[str, Language]]:
    """Get a deduplicated mapping of base images to build.

    Groups languages by their base image name (which accounts for package
    differences). For each base, picks the canonical language to use for
    building (prefers the one where target == base_name, otherwise first found).

    Returns:
        Dict mapping base_name -> (canonical_target, Language)
    """
    bases: dict[str, tuple[str, Language]] = {}

    for target, lang in LANGUAGES.items():
        base_name = get_base_image_name(target, lang)

        if base_name not in bases:
            # First language with this base
            bases[base_name] = (target, lang)
        elif target == base_name:
            # Prefer the canonical target (e.g., "swift" over "swift-simd")
            bases[base_name] = (target, lang)

    return bases


def resolve_targets_to_bases(targets: list[str]) -> dict[str, tuple[str, Language]]:
    """Convert a list of targets to their base images.

    For example, if targets = ["swift-simd", "swift-relaxed"], this returns
    just {"swift": ("swift", swift_lang)} since both use the same base and packages.

    Languages with the same base but different packages get separate entries.

    Args:
        targets: List of language targets to build

    Returns:
        Dict mapping base_name -> (canonical_target, Language)
    """
    bases: dict[str, tuple[str, Language]] = {}

    for target in targets:
        lang = LANGUAGES[target]
        base_name = get_base_image_name(target, lang)

        if base_name not in bases:
            bases[base_name] = (target, lang)
        elif target == base_name:
            # Prefer the canonical target
            bases[base_name] = (target, lang)

    return bases


# =============================================================================
# Self-test
# =============================================================================

if __name__ == "__main__":
    print(f"Defined {len(LANGUAGES)} languages:\n")

    for category, langs in sorted(get_languages_by_category().items()):
        print(f"  {category.upper()}:")
        for name in sorted(langs):
            lang = LANGUAGES[name]
            compiled = "compiled" if lang.compile else "interpreted"
            print(f"    {name:15} -> {lang.name:20} ({compiled})")
        print()

    print(f"March flag: {MARCH_NATIVE}")
