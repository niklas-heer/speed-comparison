VERSION 0.8
FROM earthly/dind:alpine

# Variables (global so they propagate to all targets)
ARG --global iterations=3
ARG --global warmups=2
ARG --global timeas="second"

# Fast check iterations (fewer runs for quick PR feedback)
ARG fast_iterations=2
ARG fast_warmups=1

# Quick local testing: override rounds for faster iteration
# Usage: earthly --build-arg QUICK_TEST_ROUNDS=1000000 +java
ARG --global QUICK_TEST_ROUNDS=""

# CI matrix build optimization: use pre-built scmeta binary
ARG --global USE_PREBUILT_SCMETA=false

build:
  FROM crystallang/crystal:1.15-alpine
  WORKDIR /app
  COPY --dir scmeta ./
  WORKDIR /app/scmeta
  RUN shards install --production -v
  RUN crystal build src/scmeta.cr --release --static -o bin/scmeta
  SAVE ARTIFACT bin/scmeta /scmeta
  SAVE ARTIFACT bin/scmeta AS LOCAL ./scmeta-bin/scmeta

# Target to export scmeta binary for CI artifact sharing
export-scmeta:
  FROM crystallang/crystal:1.15-alpine
  COPY +build/scmeta ./scmeta
  SAVE ARTIFACT ./scmeta AS LOCAL ./scmeta-bin/scmeta

# Run scmeta tests
test-scmeta:
  FROM crystallang/crystal:1.15-alpine
  WORKDIR /app
  COPY --dir scmeta ./
  WORKDIR /app/scmeta
  RUN shards install -v
  RUN crystal spec --verbose

# Benchmark function which invokes `hyperfine` and `scmeta`
BENCH:
  FUNCTION
  ARG --required name
  ARG --required cmd
  ARG --required lang
  ARG --required version
  ARG index=0

  RUN --no-cache hyperfine "$cmd" --warmup $warmups --runs $iterations --time-unit $timeas --export-json "./hyperfine.json" --output "./pi.txt"
  RUN --no-cache ./scmeta --lang-name="$lang" --lang-version="$version" --hyperfine="./hyperfine.json" --pi="./pi.txt" --output="./scmeta.json" --lang-version-match-index="$index"
  SAVE ARTIFACT ./scmeta.json AS LOCAL ./results/$name.json

PREPARE_DEBIAN:
  FUNCTION
  RUN apt-get update && apt-get install -y wget
  RUN wget https://github.com/sharkdp/hyperfine/releases/download/v1.15.0/hyperfine_1.15.0_amd64.deb
  RUN dpkg -i hyperfine_1.15.0_amd64.deb

PREPARE_ALPINE:
  FUNCTION
  RUN apk add --no-cache hyperfine

ADD_FILES:
  FUNCTION
  ARG --required src
  WORKDIR /app
  # Use pre-built scmeta if available (for CI matrix builds), otherwise build it
  IF [ "$USE_PREBUILT_SCMETA" = "true" ]
    COPY ./scmeta-bin/scmeta ./
  ELSE
    COPY +build/scmeta ./
  END
  COPY ./src/rounds.txt ./
  # Override rounds for quick local testing if specified
  IF [ -n "$QUICK_TEST_ROUNDS" ]
    RUN echo "$QUICK_TEST_ROUNDS" > rounds.txt
  END
  COPY ./src/"$src" ./

# Sets MARCH_FLAG environment variable based on CPU architecture.
# On ARM64, -march=native can fail with GCC 15+ due to SME/SVE2 detection issues.
# Usage: DO +SET_ARCH_FLAGS, then use $MARCH_FLAG in compile commands.
SET_ARCH_FLAGS:
  FUNCTION
  ENV MARCH_FLAG=""
  IF [ "$(uname -m)" = "aarch64" ]
    ENV MARCH_FLAG="-march=armv8-a"
  ELSE
    ENV MARCH_FLAG="-march=native"
  END

alpine:
  ARG --required src
  FROM alpine:3.23
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="$src"

collect-data:
  # Preparing
  BUILD +build

  # Work through programming languages
  BUILD +ada
  BUILD +c
  BUILD +c-clang
  BUILD +clj
  BUILD +cpp
  BUILD +cpp-avx2
  BUILD +cpp-clang
  BUILD +crystal
  BUILD +cs
  BUILD +fs
  BUILD +d
  BUILD +d-ldc
  BUILD +elixir
  BUILD +fortran
  BUILD +go
  BUILD +haskell
  BUILD +java
  BUILD +java-vecops
  BUILD +java-graalvm
  BUILD +kotlin
  BUILD +julia
  BUILD +julia-compiled
  BUILD +julia-ux4
  BUILD +nodejs
  BUILD +bunjs
  BUILD +lua
  BUILD +luajit
  BUILD +nim
  BUILD +ocaml
  BUILD +pascal
  BUILD +php
  BUILD +perl
  BUILD +pony
  BUILD +pony-nightly
  BUILD +cpython
  BUILD +cpython-numpy
  BUILD +mypyc
  BUILD +pypy
  BUILD +r
  BUILD +ruby
  BUILD +rust
  BUILD +rust-nightly
  BUILD +sbcl
  BUILD +scala
  BUILD +swift
  BUILD +swift-simd
  BUILD +zig

all:
  BUILD +collect-data
  BUILD +analysis

# Fast check target for PR validation - runs a small subset of fast languages
# This provides quick feedback without running the full 46-language suite
fast-check:
  BUILD +build
  BUILD +c
  BUILD +go
  BUILD +rust
  BUILD +cpython

ada:
  FROM +alpine --src="leibniz.adb"
  RUN apk add --no-cache gcc-gnat build-base
  RUN --no-cache gcc -x ada -c leibniz.adb -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  RUN --no-cache gnatbind leibniz
  RUN --no-cache gnatlink leibniz
  DO +BENCH --name="ada" --lang="Ada (gnat-gcc)" --version="gcc --version" --cmd="./leibniz"

c:
  FROM +alpine --src="leibniz.c"
  RUN apk add --no-cache gcc build-base
  RUN --no-cache gcc leibniz.c -o leibniz -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="c" --lang="C (gcc)" --version="gcc --version" --cmd="./leibniz"

c-clang:
  FROM +alpine --src="leibniz.c"
  RUN apk add --no-cache clang lld build-base
  RUN --no-cache clang -fuse-ld=lld leibniz.c -o leibniz -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="c-clang" --lang="C (clang)" --version="clang --version" --cmd="./leibniz"

clj:
  FROM clojure:temurin-21-tools-deps-alpine
  DO +PREPARE_ALPINE
  # Seems to be a bug
  RUN apk add --no-cache rlwrap
  DO +ADD_FILES --src="leibniz.clj"
  DO +BENCH --name="clj" --lang="Clojure" --version="clj --version" --cmd="clj leibniz.clj"

cpp:
  FROM +alpine --src="leibniz.cpp"
  RUN apk add --no-cache gcc build-base
  RUN --no-cache g++ leibniz.cpp -o leibniz -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="cpp" --lang="C++ (g++)" --version="g++ --version" --cmd="./leibniz"

cpp-avx2:
  FROM +alpine --src="leibniz_avx2.cpp"
  RUN apk add --no-cache gcc build-base
  RUN --no-cache g++ leibniz_avx2.cpp -o leibniz_avx2 -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="cpp-avx2" --lang="C++ (avx2)" --version="g++ --version" --cmd="./leibniz_avx2"

cpp-clang:
  FROM +alpine --src="leibniz.cpp"
  RUN apk add --no-cache clang lld build-base
  RUN --no-cache clang++ -fuse-ld=lld leibniz.cpp -o leibniz -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="cpp-clang" --lang="C++ (clang++)" --version="clang++ --version" --cmd="./leibniz"

crystal:
  FROM crystallang/crystal:1.15-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.cr"
  RUN --no-cache crystal build leibniz.cr --release
  DO +BENCH --name="crystal" --lang="Crystal" --version="crystal -v" --cmd="./leibniz"

cs:
  # Use the dedicated image from Microsoft
  FROM mcr.microsoft.com/dotnet/sdk:9.0-alpine3.20
  DO +PREPARE_ALPINE
  WORKDIR /app

  # BUILD, first restore than build
  COPY ./src/cs/*.csproj .
  RUN dotnet restore
  COPY ./src/cs/*.cs .
  RUN --no-cache dotnet publish -c Release -o out --no-restore

  # Execute test run
  WORKDIR /app/out
  COPY +build/scmeta ./
  COPY ./src/rounds.txt ./
  DO +BENCH --name="cs" --lang="C#" --version="dotnet --version" --cmd="./leibniz"

fs:
  # Use the dedicated image from Microsoft (same as C#)
  FROM mcr.microsoft.com/dotnet/sdk:9.0-alpine3.20
  DO +PREPARE_ALPINE
  WORKDIR /app

  # BUILD, first restore than build
  COPY ./src/fs/*.fsproj .
  RUN dotnet restore
  COPY ./src/fs/*.fs .
  RUN --no-cache dotnet publish -c Release -o out --no-restore

  # Execute test run
  WORKDIR /app/out
  COPY +build/scmeta ./
  COPY ./src/rounds.txt ./
  # Override rounds for quick local testing if specified
  IF [ -n "$QUICK_TEST_ROUNDS" ]
    RUN echo "$QUICK_TEST_ROUNDS" > rounds.txt
  END
  DO +BENCH --name="fs" --lang="F#" --version="dotnet --version" --cmd="./leibniz"

d:
  FROM +alpine --src="leibniz.d"
  RUN apk add --no-cache gcc-gdc
  DO +SET_ARCH_FLAGS
  RUN --no-cache gdc leibniz.d -o leibniz -O3 -frelease -static -flto -ffast-math $MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="d" --lang="D (GDC)" --version="gdc --version" --cmd="./leibniz"

d-ldc:
  FROM +alpine --src="leibniz.d"
  RUN apk add --no-cache ldc gcc musl-dev
  DO +SET_ARCH_FLAGS
  # LDC needs -mcpu flag on x86, but it causes issues on ARM64
  RUN --no-cache if [ "$(uname -m)" = "aarch64" ]; then \
        ldc2 leibniz.d -of leibniz -O3 -release -static -flto=thin -ffast-math -Xcc="$MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math"; \
      else \
        ldc2 leibniz.d -of leibniz -O3 -release -mcpu=native -static -flto=thin -ffast-math -Xcc="$MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math"; \
      fi
  DO +BENCH --name="d-ldc" --lang="D (LDC)" --version="ldc2 --version" --cmd="./leibniz"

elixir:
  FROM +alpine --src="leibniz.ex"
  RUN apk add --no-cache elixir
  # The version number is in the second match index -> 1 (instead of 0)
  DO +BENCH --name="elixir" --lang="Elixir" --version="elixir --version" --cmd="elixir leibniz.ex" --index="1"

fortran:
  FROM ubuntu:latest
  DO +PREPARE_DEBIAN
  RUN apt-get update && apt-get install -y gfortran-14 wget
  DO +ADD_FILES --src="leibniz.f90"
  RUN --no-cache gfortran-14 -Ofast -march=native -mtune=native -flto -ffast-math leibniz.f90 -o leibniz
  DO +BENCH --name="fortran" --lang="Fortran 90" --version="gfortran-14 --version" --cmd="./leibniz"

go:
  # We can reuse the build image of the scbench tool
  FROM golang:1.23-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.go"
  RUN --no-cache go build leibniz.go
  DO +BENCH --name="go" --lang="Go" --version="go version" --cmd="./leibniz"

haskell:
  FROM haskell:9.10-slim-bullseye
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.hs"
  RUN --no-cache ghc -funfolding-use-threshold=16 -O2 -optc-O3 leibniz.hs
  DO +BENCH --name="haskell" --lang="Haskell (GHC)" --version="ghc --version" --cmd="./leibniz"

java:
  # Using a dedicated image due to the packages on alpine being not up to date.
  FROM eclipse-temurin:21-jdk-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.java"
  RUN --no-cache javac leibniz.java
  DO +BENCH --name="java" --lang="Java" --version="java -version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="java leibniz"

kotlin:
  FROM eclipse-temurin:21-jdk-alpine
  DO +PREPARE_ALPINE
  RUN apk add --no-cache bash
  # Install Kotlin compiler
  RUN wget -q https://github.com/JetBrains/kotlin/releases/download/v2.1.21/kotlin-compiler-2.1.21.zip && \
      unzip -q kotlin-compiler-2.1.21.zip && \
      rm kotlin-compiler-2.1.21.zip
  ENV PATH="/kotlinc/bin:${PATH}"
  DO +ADD_FILES --src="leibniz.kt"
  RUN --no-cache kotlinc leibniz.kt -include-runtime -d leibniz.jar
  DO +BENCH --name="kotlin" --lang="Kotlin" --version="kotlinc -version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="java -jar leibniz.jar"

java-graalvm:
  FROM ubuntu:latest
  DO +PREPARE_DEBIAN
  #https://stackoverflow.com/questions/53656537/install-sdkman-in-docker-image
  RUN rm /bin/sh && ln -s /bin/bash /bin/sh
  RUN apt install -y unzip zip curl build-essential libz-dev zlib1g-dev
  RUN curl -s "https://get.sdkman.io" | bash; \
      source "/root/.sdkman/bin/sdkman-init.sh" ; \
      sdk install java 25.0.1-graal
  ENV PATH=/root/.sdkman/candidates/java/current/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
  # Native Image is included by default in GraalVM 25+, no need for 'gu install'
  DO +ADD_FILES --src="leibniz.java"
  RUN javac leibniz.java
  RUN native-image -H:+ReportExceptionStackTraces leibniz
  DO +BENCH --name="java-graalvm" --lang="Java graalvm" --version="native-image --version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="./leibniz"

java-vecops:
  # Using a dedicated image due to the packages on alpine being not up to date.
  FROM eclipse-temurin:21-jdk-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibnizVecOps.java"
  RUN --no-cache javac --add-modules jdk.incubator.vector leibnizVecOps.java
  DO +BENCH --name="java-vecops" --lang="Java (Vec Ops)" --version="java -version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="java --add-modules jdk.incubator.vector leibnizVecOps"

julia:
  # We have to use a special image since there is no Julia package on alpine 🤷‍♂️
  FROM julia:1.11-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.jl"
  DO +BENCH --name="julia" --lang="Julia" --version="julia --version" --cmd="julia leibniz.jl"

julia-compiled:
  # We need the Debian version otherwise the build doesn't work
  FROM julia:1.11
  DO +PREPARE_DEBIAN
  RUN apt-get update && apt-get install -y gcc g++ build-essential cmake
  DO +ADD_FILES --src="leibniz_compiled.jl"
  COPY ./src/leibniz.jl ./
  RUN julia -e 'using Pkg; Pkg.add(["StaticCompiler", "StaticTools"]); using StaticCompiler, StaticTools; include("./leibniz_compiled.jl"); compile_executable(mainjl, (), "./")'
  DO +BENCH --name="julia-compiled" --lang="Julia (AOT compiled)" --version="julia --version" --cmd="./mainjl"

julia-ux4:
  # We have to use a special image since there is no Julia package on alpine 🤷‍♂️
  FROM julia:1.11-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz_ux4.jl"
  DO +BENCH --name="julia-ux4" --lang="Julia (ux4)" --version="julia --version" --cmd="julia leibniz_ux4.jl"

nodejs:
  FROM node:22-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.js"
  DO +BENCH --name="nodejs" --lang="Javascript (nodejs)" --version="node --version" --cmd="node leibniz.js"

bunjs:
  FROM oven/bun:1.2-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.js"
  DO +BENCH --name="bunjs" --lang="Javascript (bun)" --version="bun --version" --cmd="bun run leibniz.js"

lua:
  FROM +alpine --src="leibniz.lua"
  RUN apk add --no-cache lua5.4
  DO +BENCH --name="lua" --lang="Lua" --version="lua5.4 -v" --cmd="lua5.4 leibniz.lua"

luajit:
  FROM +alpine --src="leibniz.lua"
  RUN apk add --no-cache luajit
  DO +BENCH --name="luajit" --lang="LuaJIT" --version="luajit -v" --cmd="luajit leibniz.lua"

# `-fno-signed-zeros -fno-trapping-math -fassociative-math` GCC options allows vectorization
# `-march=native` allows using AVX instructions if build machine support it
# `--passL:"-s"` removes all symbol table and relocation information. It makes executable smaller but doesn't affect the speed
# Add `--passL:"-save-temps -dumpbase asmout/mycode"` option to the Nim command to generate the assembly code in directory asmout
nim:
  FROM +alpine --src="leibniz.nim"
  RUN apk add --no-cache gcc build-base nim
  RUN --no-cache nim c --verbosity:0 -d:danger -d:lto --gc:arc --passC:"-march=native -fno-signed-zeros -fno-trapping-math -fassociative-math" --passL:"-s" leibniz.nim
  DO +BENCH --name="nim" --lang="Nim" --version="nim --version" --cmd="./leibniz"

ocaml:
  FROM alpine:edge
  RUN apk add --no-cache hyperfine ocaml5 musl-dev --repository=http://dl-cdn.alpinelinux.org/alpine/edge/main
  DO +ADD_FILES --src="leibniz.ml"
  RUN --no-cache ocamlopt -O2 -o leibniz leibniz.ml
  DO +BENCH --name="ocaml" --lang="OCaml" --version="ocamlopt -version" --cmd="./leibniz"

php:
  FROM +alpine --src="leibniz.php"
  RUN apk add --no-cache php84 php84-opcache
  # Enable OPCACHE and JIT for production-like performance
  DO +BENCH --name="php" --lang="PHP" --version="php84 --version" --cmd="php84 -dopcache.enable_cli=1 -dopcache.jit=1255 -dopcache.jit_buffer_size=64M leibniz.php"

pascal:
  FROM alpine:edge
  RUN apk add --no-cache hyperfine binutils fpc --repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing
  DO +ADD_FILES --src="leibniz.pas"
  RUN --no-cache fpc -O3 -Xs leibniz.pas -oleibniz
  DO +BENCH --name="pascal" --lang="Pascal (FPC)" --version="fpc -iV" --cmd="./leibniz"

perl:
  FROM +alpine --src="leibniz.pl"
  RUN apk add --no-cache perl
  DO +BENCH --name="perl" --lang="Perl" --version="perl -v" --cmd="perl leibniz.pl"

pony:
  FROM ponylang/ponyc:release-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.pony"
  RUN --no-cache ponyc ./ -o=out --bin-name=leibniz
  DO +BENCH --name "pony" --lang="Pony" --version="ponyc --version" --cmd="./out/leibniz"

pony-nightly:
  FROM ponylang/ponyc:alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.pony"
  RUN --no-cache ponyc ./ -o=out --bin-name=leibniz
  DO +BENCH --name "pony-nightly" --lang="Pony(nightly)" --version="ponyc --version" --cmd="./out/leibniz"

cpython:
  FROM python:3.13-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.py"
  DO +BENCH --name="cpython" --lang="Python (CPython)" --version="python3 --version" --cmd="python3 leibniz.py"

cpython-numpy:
  FROM python:3.13-alpine
  DO +PREPARE_ALPINE
  RUN apk add --no-cache gcc build-base
  RUN pip install numpy
  DO +ADD_FILES --src="leibniz_np.py"
  DO +BENCH --name="cpython-numpy" --lang="Python (NumPy)" --version="python3 --version" --cmd="python3 leibniz_np.py"

mypyc:
  FROM python:3.13-alpine
  DO +PREPARE_ALPINE
  RUN apk add --no-cache gcc build-base
  RUN pip install mypy setuptools
  DO +ADD_FILES --src="leibniz_mypyc.py"
  RUN mypyc leibniz_mypyc.py
  DO +BENCH --name="mypyc" --lang="Python (MyPyC)" --version="mypy --version" --cmd="python3 -c 'import leibniz_mypyc'"

pypy:
  # There is no pypy package on alpine
  # We use the standard which is Debian
  FROM pypy:3.10
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.py"
  DO +BENCH --name="pypy" --lang="Python (PyPy)" --version="pypy --version" --cmd="pypy leibniz.py"

r:
  FROM +alpine --src="leibniz.r"
  RUN apk add --no-cache R
  DO +BENCH --name="r" --lang="R" --version="R --version" --cmd="Rscript --vanilla --default-packages=base leibniz.r"

ruby:
  FROM +alpine --src="leibniz.rb"
  RUN apk add --no-cache ruby
  DO +BENCH --name="ruby" --lang="Ruby" --version="ruby --version" --cmd="ruby leibniz.rb"

rust:
  FROM rust:1.83-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.rs"
  RUN --no-cache rustc -C debuginfo=0 -C opt-level=3 -C target-cpu=native -C lto=fat -C codegen-units=1 -C panic=abort leibniz.rs
  DO +BENCH --name="rust" --lang="Rust" --version="rustc --version" --cmd="./leibniz"

rust-nightly:
  FROM rustlang/rust:nightly-slim
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz_nightly.rs"
  RUN --no-cache rustc -C debuginfo=0 -C opt-level=3 -C target-cpu=native -C lto=fat -C codegen-units=1 -C panic=abort leibniz_nightly.rs
  DO +BENCH --name="rust-nightly" --lang="Rust (nightly)" --version="rustc --version" --cmd="./leibniz_nightly"

sbcl:
  FROM +alpine --src="leibniz.lisp"
  RUN apk add --no-cache sbcl
  RUN --no-cache sbcl --noinform --eval '(compile-file "leibniz.lisp")' --quit
  DO +BENCH --name="sbcl" --lang="Common Lisp (SBCL)" --version="sbcl --version" --cmd="sbcl --script leibniz.fasl"

scala:
  FROM +alpine --src="leibniz.scala"
  RUN apk add --no-cache clang musl-dev g++
  RUN wget -q https://github.com/VirtusLab/scala-cli/releases/download/v1.10.1/scala-cli-x86_64-pc-linux-static.gz && \
      gunzip scala-cli-x86_64-pc-linux-static.gz && \
      chmod +x scala-cli-x86_64-pc-linux-static && \
      mv scala-cli-x86_64-pc-linux-static /usr/local/bin/scala-cli
  RUN scala-cli package leibniz.scala -o leibniz --scala 3.7.4 --native-version 0.5.9 --native --native-mode release-full --power
  DO +BENCH --name="scala" --lang="Scala" --version="scala-cli version --scala 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="./leibniz"

swift:
  FROM swift:6.0-jammy
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.swift"
  RUN --no-cache swiftc leibniz.swift -O -o leibniz -clang-target native -lto=llvm-full
  DO +BENCH --name="swift" --lang="Swift" --version="swift --version" --cmd="./leibniz"

swift-simd:
  FROM swift:6.0-jammy
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz-simd.swift"
  RUN --no-cache swiftc leibniz-simd.swift -O -o leibniz -clang-target native -lto=llvm-full
  DO +BENCH --name="swift-simd" --lang="Swift (SIMD)" --version="swift --version" --cmd="./leibniz"

zig:
  # On 3.16 there is no zig package, but on edge there is
  FROM alpine:edge
  # https://pkgs.alpinelinux.org/package/edge/testing/aarch64/zig
  # https://stackoverflow.com/a/62218241
  RUN apk add --no-cache hyperfine zig --repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing
  DO +ADD_FILES --src="leibniz.zig"
  RUN --no-cache zig build-exe -OReleaseFast leibniz.zig
  DO +BENCH --name="zig" --lang="Zig" --version="zig version" --cmd="./leibniz"

analysis:
  FROM python:3.11-slim

  # Install uv
  RUN pip install uv

  # Copy project files
  COPY ./pyproject.toml ./
  COPY ./*.py ./
  COPY ./src/rounds.txt ./
  COPY --dir results ./

  # Install dependencies with uv
  RUN uv pip install --system -e .

  # Combine all results
  RUN --no-cache python analyze.py --folder ./results/ --out ./ --rounds ./rounds.txt
  SAVE ARTIFACT ./*.csv AS LOCAL ./results/
  SAVE ARTIFACT ./*.png AS LOCAL ./results/
