VERSION 0.8
FROM earthly/dind:alpine

# ============================================================================
# CONFIGURATION
# ============================================================================

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

# ============================================================================
# INFRASTRUCTURE TARGETS
# ============================================================================

build:
  FROM crystallang/crystal:1.15-alpine
  WORKDIR /app
  COPY --dir scmeta ./
  WORKDIR /app/scmeta
  RUN shards install --production -v
  RUN crystal build src/scmeta.cr --release --static -o bin/scmeta
  SAVE ARTIFACT bin/scmeta /scmeta
  SAVE ARTIFACT bin/scmeta AS LOCAL ./scmeta-bin/scmeta

export-scmeta:
  FROM crystallang/crystal:1.15-alpine
  COPY +build/scmeta ./scmeta
  SAVE ARTIFACT ./scmeta AS LOCAL ./scmeta-bin/scmeta

test-scmeta:
  FROM crystallang/crystal:1.15-alpine
  WORKDIR /app
  COPY --dir scmeta ./
  WORKDIR /app/scmeta
  RUN shards install -v
  RUN crystal spec --verbose

analysis:
  FROM python:3.11-slim
  RUN pip install uv
  COPY ./pyproject.toml ./
  COPY ./*.py ./
  COPY ./src/rounds.txt ./
  COPY --dir results ./
  RUN uv pip install --system -e .
  RUN --no-cache python analyze.py --folder ./results/ --out ./ --rounds ./rounds.txt
  SAVE ARTIFACT ./*.csv AS LOCAL ./results/
  SAVE ARTIFACT ./*.png AS LOCAL ./results/

# ============================================================================
# SHARED FUNCTIONS
# ============================================================================

BENCH:
  FUNCTION
  ARG --required name
  ARG --required cmd
  ARG --required lang
  ARG --required version
  ARG index=0
  RUN --no-cache hyperfine "$cmd" --warmup $warmups --runs $iterations --time-unit $timeas --export-json "./hyperfine.json" --output "./pi.txt"
  RUN --no-cache ./scmeta --lang-name="$lang" --target-name="$name" --lang-version="$version" --hyperfine="./hyperfine.json" --pi="./pi.txt" --output="./scmeta.json" --lang-version-match-index="$index"
  SAVE ARTIFACT ./scmeta.json AS LOCAL ./results/$name.json

PREPARE_DEBIAN:
  FUNCTION
  RUN apt-get update && apt-get install -y wget
  RUN ARCH=$(dpkg --print-architecture) && \
      wget -q https://github.com/sharkdp/hyperfine/releases/download/v1.18.0/hyperfine_1.18.0_${ARCH}.deb && \
      dpkg -i hyperfine_1.18.0_${ARCH}.deb

PREPARE_ALPINE:
  FUNCTION
  RUN apk add --no-cache hyperfine

ADD_FILES:
  FUNCTION
  ARG --required src
  WORKDIR /app
  IF [ "$USE_PREBUILT_SCMETA" = "true" ]
    COPY ./scmeta-bin/scmeta ./
  ELSE
    COPY +build/scmeta ./
  END
  COPY ./src/rounds.txt ./
  IF [ -n "$QUICK_TEST_ROUNDS" ]
    RUN echo "$QUICK_TEST_ROUNDS" > rounds.txt
  END
  COPY ./src/"$src" ./

SET_ARCH_FLAGS:
  FUNCTION
  ENV MARCH_FLAG=""
  IF [ "$(uname -m)" = "aarch64" ]
    ENV MARCH_FLAG="-march=armv8-a"
  ELSE
    ENV MARCH_FLAG="-march=native"
  END

# Helper for simple Alpine-based source
alpine:
  ARG --required src
  FROM alpine:3.23
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="$src"

# ============================================================================
# AGGREGATE TARGETS
# ============================================================================

collect-data:
  BUILD +build
  # Systems languages
  BUILD +c
  BUILD +c-clang
  BUILD +cpp
  BUILD +cpp-avx2
  BUILD +cpp-clang
  BUILD +d
  BUILD +d-ldc
  BUILD +go
  BUILD +nim
  BUILD +odin
  BUILD +rust
  BUILD +rust-nightly
  BUILD +v
  BUILD +zig
  # JVM languages
  BUILD +clj
  BUILD +groovy
  BUILD +java
  BUILD +java-graalvm
  BUILD +java-vecops
  BUILD +kotlin
  BUILD +scala
  # .NET languages
  BUILD +cs
  BUILD +cs-simd
  BUILD +fs
  # Functional languages
  BUILD +elixir
  BUILD +erlang
  BUILD +gleam
  BUILD +haskell
  BUILD +ocaml
  BUILD +racket
  BUILD +sbcl
  # Scripting languages
  BUILD +cpython
  BUILD +cpython-numpy
  BUILD +lua
  BUILD +luajit
  BUILD +mypyc
  BUILD +perl
  BUILD +php
  BUILD +pypy
  BUILD +r
  BUILD +raku
  BUILD +ruby
  # JavaScript runtimes
  BUILD +bunjs
  BUILD +deno
  BUILD +nodejs
  # Other compiled languages
  BUILD +ada
  BUILD +crystal
  BUILD +dart
  BUILD +dart-aot
  BUILD +fortran
  BUILD +haxe
  BUILD +janet-compiled
  BUILD +julia
  BUILD +objc
  BUILD +pascal
  BUILD +pony
  BUILD +pony-nightly
  BUILD +swift
  BUILD +swift-relaxed
  BUILD +swift-simd
  BUILD +wasm

all:
  BUILD +collect-data
  BUILD +analysis

fast-check:
  BUILD +build
  BUILD +c
  BUILD +go
  BUILD +rust
  BUILD +cpython

# ============================================================================
# SYSTEMS LANGUAGES (C, C++, Rust, Go, Zig, etc.)
# ============================================================================

c:
  FROM +alpine --src="leibniz.c"
  RUN apk add --no-cache gcc build-base
  DO +SET_ARCH_FLAGS
  RUN --no-cache gcc leibniz.c -o leibniz -O3 -s -static -flto $MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="c" --lang="C (gcc)" --version="gcc --version" --cmd="./leibniz"

c-clang:
  FROM +alpine --src="leibniz.c"
  RUN apk add --no-cache clang lld build-base
  DO +SET_ARCH_FLAGS
  RUN --no-cache clang -fuse-ld=lld leibniz.c -o leibniz -O3 -s -static -flto $MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="c-clang" --lang="C (clang)" --version="clang --version" --cmd="./leibniz"

cpp:
  FROM +alpine --src="leibniz.cpp"
  RUN apk add --no-cache gcc build-base
  DO +SET_ARCH_FLAGS
  RUN --no-cache g++ leibniz.cpp -o leibniz -O3 -s -static -flto $MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="cpp" --lang="C++ (g++)" --version="g++ --version" --cmd="./leibniz"

cpp-avx2:
  FROM +alpine --src="leibniz_avx2.cpp"
  RUN apk add --no-cache gcc build-base
  DO +SET_ARCH_FLAGS
  RUN --no-cache g++ leibniz_avx2.cpp -o leibniz_avx2 -O3 -s -static -flto $MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="cpp-avx2" --lang="C++ (avx2)" --version="g++ --version" --cmd="./leibniz_avx2"

cpp-clang:
  FROM +alpine --src="leibniz.cpp"
  RUN apk add --no-cache clang lld build-base
  DO +SET_ARCH_FLAGS
  RUN --no-cache clang++ -fuse-ld=lld leibniz.cpp -o leibniz -O3 -s -static -flto $MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="cpp-clang" --lang="C++ (clang++)" --version="clang++ --version" --cmd="./leibniz"

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
  RUN --no-cache if [ "$(uname -m)" = "aarch64" ]; then \
        ldc2 leibniz.d -of leibniz -O3 -release -static -flto=thin -ffast-math -Xcc="$MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math"; \
      else \
        ldc2 leibniz.d -of leibniz -O3 -release -mcpu=native -static -flto=thin -ffast-math -Xcc="$MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math"; \
      fi
  DO +BENCH --name="d-ldc" --lang="D (LDC)" --version="ldc2 --version" --cmd="./leibniz"

go:
  FROM golang:1.25-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.go"
  RUN --no-cache go build leibniz.go
  DO +BENCH --name="go" --lang="Go" --version="go version" --cmd="./leibniz"

nim:
  # GCC options: -fno-signed-zeros -fno-trapping-math -fassociative-math allow vectorization
  # --passL:"-s" removes symbol table and relocation info (smaller binary)
  FROM +alpine --src="leibniz.nim"
  RUN apk add --no-cache gcc build-base nim
  DO +SET_ARCH_FLAGS
  RUN --no-cache nim c --verbosity:0 -d:danger -d:lto --gc:arc --passC:"$MARCH_FLAG -fno-signed-zeros -fno-trapping-math -fassociative-math" --passL:"-s" leibniz.nim
  DO +BENCH --name="nim" --lang="Nim" --version="nim --version" --cmd="./leibniz"

odin:
  FROM ubuntu:latest
  DO +PREPARE_DEBIAN
  RUN apt-get update && apt-get install -y git clang llvm make
  RUN git clone --depth=1 --branch=dev-2025-12a https://github.com/odin-lang/Odin.git /opt/odin && \
      cd /opt/odin && ./build_odin.sh release
  ENV PATH="/opt/odin:${PATH}"
  DO +ADD_FILES --src="leibniz.odin"
  RUN --no-cache odin build . -file -o:speed -out:leibniz
  DO +BENCH --name="odin" --lang="Odin" --version="odin version 2>&1 | grep -oE '[0-9]+-[0-9]+' | head -1" --cmd="./leibniz"

rust:
  FROM rust:1.92-alpine
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

v:
  FROM ubuntu:latest
  DO +PREPARE_DEBIAN
  RUN apt-get update && apt-get install -y git gcc make
  RUN git clone --depth=1 --branch weekly.2025.50 https://github.com/vlang/v /opt/vlang && \
      cd /opt/vlang && make && ./v symlink
  DO +ADD_FILES --src="leibniz.v"
  RUN --no-cache v -prod -o leibniz leibniz.v
  DO +BENCH --name="v" --lang="V" --version="v version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="./leibniz"

zig:
  FROM alpine:edge
  RUN apk add --no-cache hyperfine zig --repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing
  DO +ADD_FILES --src="leibniz.zig"
  RUN --no-cache zig build-exe -OReleaseFast leibniz.zig
  DO +BENCH --name="zig" --lang="Zig" --version="zig version" --cmd="./leibniz"

# ============================================================================
# JVM LANGUAGES (Java, Kotlin, Scala, Clojure, Groovy)
# ============================================================================

clj:
  FROM clojure:temurin-25-tools-deps-alpine
  DO +PREPARE_ALPINE
  RUN apk add --no-cache rlwrap  # Bug workaround
  DO +ADD_FILES --src="leibniz.clj"
  DO +BENCH --name="clj" --lang="Clojure" --version="clj --version" --cmd="clj leibniz.clj"

groovy:
  FROM groovy:5-jdk21
  USER root
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.groovy"
  DO +BENCH --name="groovy" --lang="Groovy" --version="groovy --version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="groovy leibniz.groovy"

java:
  FROM eclipse-temurin:25-jdk-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.java"
  RUN --no-cache javac leibniz.java
  DO +BENCH --name="java" --lang="Java" --version="java -version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="java leibniz"

java-graalvm:
  FROM ubuntu:latest
  DO +PREPARE_DEBIAN
  RUN rm /bin/sh && ln -s /bin/bash /bin/sh
  RUN apt install -y unzip zip curl build-essential libz-dev zlib1g-dev
  RUN curl -s "https://get.sdkman.io" | bash; \
      source "/root/.sdkman/bin/sdkman-init.sh" ; \
      sdk install java 25.0.1-graal
  ENV PATH=/root/.sdkman/candidates/java/current/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
  DO +ADD_FILES --src="leibniz.java"
  RUN javac leibniz.java
  RUN native-image -H:+ReportExceptionStackTraces leibniz
  DO +BENCH --name="java-graalvm" --lang="Java graalvm" --version="native-image --version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="./leibniz"

java-vecops:
  FROM eclipse-temurin:25-jdk-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibnizVecOps.java"
  RUN --no-cache javac --add-modules jdk.incubator.vector leibnizVecOps.java
  DO +BENCH --name="java-vecops" --lang="Java (Vec Ops)" --version="java -version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="java --add-modules jdk.incubator.vector leibnizVecOps"

kotlin:
  FROM eclipse-temurin:21-jdk-alpine
  DO +PREPARE_ALPINE
  RUN apk add --no-cache bash
  RUN wget -q https://github.com/JetBrains/kotlin/releases/download/v2.3.0/kotlin-compiler-2.3.0.zip && \
      unzip -q kotlin-compiler-2.3.0.zip && \
      rm kotlin-compiler-2.3.0.zip
  ENV PATH="/kotlinc/bin:${PATH}"
  DO +ADD_FILES --src="leibniz.kt"
  RUN --no-cache kotlinc leibniz.kt -include-runtime -d leibniz.jar
  DO +BENCH --name="kotlin" --lang="Kotlin" --version="kotlinc -version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="java -jar leibniz.jar"

scala:
  FROM +alpine --src="leibniz.scala"
  RUN apk add --no-cache clang musl-dev g++
  RUN ARCH=$(uname -m) && \
      wget -q https://github.com/VirtusLab/scala-cli/releases/download/v1.10.1/scala-cli-${ARCH}-pc-linux-static.gz && \
      gunzip scala-cli-${ARCH}-pc-linux-static.gz && \
      chmod +x scala-cli-${ARCH}-pc-linux-static && \
      mv scala-cli-${ARCH}-pc-linux-static /usr/local/bin/scala-cli
  RUN scala-cli package leibniz.scala -o leibniz --scala 3.7.4 --native-version 0.5.9 --native --native-mode release-full --power
  DO +BENCH --name="scala" --lang="Scala" --version="scala-cli version --scala 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="./leibniz"

# ============================================================================
# .NET LANGUAGES (C#, F#)
# ============================================================================

cs:
  FROM +alpine --src="leibniz.cs"
  RUN apk add --no-cache clang build-base zlib-dev dotnet10-sdk
  RUN --no-cache dotnet publish leibniz.cs -o . -p:OptimizationPreference=Speed -p:IlcInstructionSet=native
  DO +BENCH --name="cs" --lang="C#" --version="dotnet --version" --cmd="./leibniz"

cs-simd:
  FROM +alpine --src="leibniz-simd.cs"
  RUN apk add --no-cache clang build-base zlib-dev dotnet10-sdk
  RUN --no-cache dotnet publish leibniz-simd.cs -o . -p:OptimizationPreference=Speed -p:IlcInstructionSet=native
  DO +BENCH --name="cs-simd" --lang="C# (SIMD)" --version="dotnet --version" --cmd="./leibniz-simd"

fs:
  FROM +alpine --src="fs/"
  RUN apk add --no-cache clang build-base zlib-dev dotnet10-sdk
  RUN --no-cache dotnet publish -o . -p:PublishAot=true -p:OptimizationPreference=Speed -p:IlcInstructionSet=native
  DO +BENCH --name="fs" --lang="F#" --version="dotnet --version" --cmd="./leibniz"

# ============================================================================
# FUNCTIONAL LANGUAGES (Haskell, OCaml, Erlang, Elixir, etc.)
# ============================================================================

elixir:
  FROM +alpine --src="leibniz.ex"
  RUN apk add --no-cache elixir
  DO +BENCH --name="elixir" --lang="Elixir" --version="elixir --version" --cmd="elixir leibniz.ex" --index="1"

erlang:
  FROM alpine:edge
  DO +PREPARE_ALPINE
  RUN apk add --no-cache erlang28
  DO +ADD_FILES --src="leibniz.erl"
  RUN --no-cache erlc leibniz.erl
  DO +BENCH --name="erlang" --lang="Erlang" --version="cat /usr/lib/erlang/releases/*/OTP_VERSION" --cmd="erl -noshell -s leibniz main -s init stop"

gleam:
  FROM ghcr.io/gleam-lang/gleam:v1.13.0-erlang-alpine
  DO +PREPARE_ALPINE
  WORKDIR /app
  RUN gleam new leibniz_app && cd leibniz_app && \
      sed -i 's/\[dependencies\]/[dependencies]\nsimplifile = "~> 2.0"/' gleam.toml
  COPY +build/scmeta ./leibniz_app/
  COPY ./src/rounds.txt ./leibniz_app/
  IF [ -n "$QUICK_TEST_ROUNDS" ]
    RUN echo "$QUICK_TEST_ROUNDS" > ./leibniz_app/rounds.txt
  END
  COPY ./src/leibniz.gleam ./leibniz_app/src/leibniz_app.gleam
  RUN cd leibniz_app && gleam deps download && gleam build
  WORKDIR /app/leibniz_app
  RUN apk add --no-cache hyperfine
  DO +BENCH --name="gleam" --lang="Gleam" --version="gleam --version" --cmd="gleam run"

haskell:
  FROM haskell:9.10-slim-bullseye
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.hs"
  RUN --no-cache ghc -funfolding-use-threshold=16 -O2 -optc-O3 leibniz.hs
  DO +BENCH --name="haskell" --lang="Haskell (GHC)" --version="ghc --version" --cmd="./leibniz"

ocaml:
  FROM alpine:edge
  RUN apk add --no-cache hyperfine ocaml5 musl-dev --repository=http://dl-cdn.alpinelinux.org/alpine/edge/main
  DO +ADD_FILES --src="leibniz.ml"
  RUN --no-cache ocamlopt -O2 -o leibniz leibniz.ml
  DO +BENCH --name="ocaml" --lang="OCaml" --version="ocamlopt -version" --cmd="./leibniz"

racket:
  FROM alpine:edge
  DO +PREPARE_ALPINE
  RUN apk add --no-cache racket
  DO +ADD_FILES --src="leibniz.rkt"
  DO +BENCH --name="racket" --lang="Racket" --version="racket --version" --cmd="racket leibniz.rkt"

sbcl:
  FROM +alpine --src="leibniz.lisp"
  RUN apk add --no-cache sbcl
  RUN --no-cache sbcl --noinform --eval '(compile-file "leibniz.lisp")' --quit
  DO +BENCH --name="sbcl" --lang="Common Lisp (SBCL)" --version="sbcl --version" --cmd="sbcl --script leibniz.fasl"

# ============================================================================
# SCRIPTING LANGUAGES (Python, Ruby, Perl, PHP, Lua, R)
# ============================================================================

cpython:
  FROM python:3.14-alpine
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

lua:
  FROM +alpine --src="leibniz.lua"
  RUN apk add --no-cache lua5.4
  DO +BENCH --name="lua" --lang="Lua" --version="lua5.4 -v" --cmd="lua5.4 leibniz.lua"

luajit:
  FROM +alpine --src="leibniz.lua"
  RUN apk add --no-cache luajit
  DO +BENCH --name="luajit" --lang="LuaJIT" --version="luajit -v" --cmd="luajit leibniz.lua"

mypyc:
  FROM python:3.14-alpine
  DO +PREPARE_ALPINE
  RUN apk add --no-cache gcc build-base
  RUN pip install mypy setuptools
  DO +ADD_FILES --src="leibniz_mypyc.py"
  RUN mypyc leibniz_mypyc.py
  DO +BENCH --name="mypyc" --lang="Python (MyPyC)" --version="mypy --version" --cmd="python3 -c 'import leibniz_mypyc'"

perl:
  FROM perl:5.42.0-slim
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.pl"
  DO +BENCH --name="perl" --lang="Perl" --version="perl -v" --cmd="perl leibniz.pl"

php:
  FROM +alpine --src="leibniz.php"
  RUN apk add --no-cache php84 php84-opcache
  DO +BENCH --name="php" --lang="PHP" --version="php84 --version" --cmd="php84 -dopcache.enable_cli=1 -dopcache.jit=1255 -dopcache.jit_buffer_size=64M leibniz.php"

pypy:
  FROM pypy:3.11
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.py"
  DO +BENCH --name="pypy" --lang="Python (PyPy)" --version="pypy --version" --cmd="pypy leibniz.py"

r:
  FROM +alpine --src="leibniz.r"
  RUN apk add --no-cache R
  DO +BENCH --name="r" --lang="R" --version="R --version" --cmd="Rscript --vanilla --default-packages=base leibniz.r"

raku:
  FROM rakudo-star:latest
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.raku"
  DO +BENCH --name="raku" --lang="Raku" --version="raku --version 2>&1 | grep -oE '[0-9]+\.[0-9]+' | head -1" --cmd="raku leibniz.raku"

ruby:
  FROM +alpine --src="leibniz.rb"
  RUN apk add --no-cache ruby
  DO +BENCH --name="ruby" --lang="Ruby" --version="ruby --version" --cmd="ruby --yjit leibniz.rb"

# ============================================================================
# JAVASCRIPT RUNTIMES (Node.js, Bun, Deno)
# ============================================================================

bunjs:
  FROM oven/bun:1.3-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.js"
  DO +BENCH --name="bunjs" --lang="Javascript (bun)" --version="bun --version" --cmd="bun run leibniz.js"

deno:
  FROM denoland/deno:debian
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.ts"
  DO +BENCH --name="deno" --lang="Deno (TypeScript)" --version="deno --version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="deno run --allow-read leibniz.ts"

nodejs:
  FROM node:25-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.js"
  DO +BENCH --name="nodejs" --lang="Javascript (nodejs)" --version="node --version" --cmd="node leibniz.js"

# ============================================================================
# OTHER COMPILED LANGUAGES
# ============================================================================

ada:
  FROM +alpine --src="leibniz.adb"
  RUN apk add --no-cache gcc-gnat build-base
  DO +SET_ARCH_FLAGS
  RUN --no-cache gcc -x ada -c leibniz.adb -O3 -s -static -flto $MARCH_FLAG -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  RUN --no-cache gnatbind leibniz
  RUN --no-cache gnatlink leibniz
  DO +BENCH --name="ada" --lang="Ada (gnat-gcc)" --version="gcc --version" --cmd="./leibniz"

crystal:
  FROM crystallang/crystal:1.18-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.cr"
  RUN --no-cache crystal build leibniz.cr --release
  DO +BENCH --name="crystal" --lang="Crystal" --version="crystal -v" --cmd="./leibniz"

dart:
  FROM dart:stable
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.dart"
  DO +BENCH --name="dart" --lang="Dart (JIT)" --version="dart --version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="dart run leibniz.dart"

dart-aot:
  FROM dart:stable
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.dart"
  RUN --no-cache dart compile exe leibniz.dart -o leibniz
  DO +BENCH --name="dart-aot" --lang="Dart (AOT)" --version="dart --version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1" --cmd="./leibniz"

fortran:
  FROM ubuntu:latest
  DO +PREPARE_DEBIAN
  RUN apt-get update && apt-get install -y gfortran-14 wget
  DO +ADD_FILES --src="leibniz.f90"
  DO +SET_ARCH_FLAGS
  RUN --no-cache gfortran-14 -Ofast $MARCH_FLAG -mtune=native -flto -ffast-math leibniz.f90 -o leibniz
  DO +BENCH --name="fortran" --lang="Fortran 90" --version="gfortran-14 --version" --cmd="./leibniz"

haxe:
  FROM ubuntu:latest
  DO +PREPARE_DEBIAN
  RUN apt-get update && apt-get install -y software-properties-common
  RUN add-apt-repository ppa:haxe/releases -y && apt-get update && apt-get install -y haxe neko g++
  RUN mkdir -p /usr/lib/haxe/lib && haxelib setup /usr/lib/haxe/lib
  RUN haxelib install hxcpp
  DO +ADD_FILES --src="Leibniz.hx"
  RUN --no-cache haxe -main Leibniz -cpp out -D HXCPP_CHECK_POINTER=0
  DO +BENCH --name="haxe" --lang="Haxe (C++)" --version="haxe --version" --cmd="./out/Leibniz"

janet:
  FROM alpine:edge
  DO +PREPARE_ALPINE
  RUN apk add --no-cache janet
  DO +ADD_FILES --src="leibniz.janet"
  DO +BENCH --name="janet" --lang="Janet" --version="janet --version" --cmd="janet leibniz.janet"

janet-compiled:
  FROM alpine:edge
  DO +PREPARE_ALPINE
  RUN apk add --no-cache janet janet-dev janet-static build-base git
  RUN cd /tmp && git clone --depth=1 https://github.com/janet-lang/jpm.git && \
      cd jpm && janet bootstrap.janet && \
      sed -i '1s|.*|#!/usr/bin/janet|' /usr/local/bin/jpm && \
      sed -i '2d' /usr/local/bin/jpm && \
      ln -s /usr/lib/libjanet.a /usr/local/lib/libjanet.a && \
      rm -rf /tmp/jpm
  DO +ADD_FILES --src="leibniz_compiled.janet"
  RUN --no-cache /usr/local/bin/jpm quickbin leibniz_compiled.janet leibniz
  DO +BENCH --name="janet-compiled" --lang="Janet (compiled)" --version="janet --version" --cmd="./leibniz"

julia:
  FROM julia:1.12
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.jl"
  RUN apt-get update && apt-get install -y gcc
  RUN julia -e 'print(VERSION); using Pkg; Pkg.activate("."); Pkg.update(); Pkg.Apps.add(["JuliaC"])'
  RUN ~/.julia/bin/juliac \
        --output-exe leibniz \
        --trim \
        --experimental \
        --bundle bun \
        --project . \
        leibniz.jl
  DO +BENCH --name="julia" --lang="Julia" --version="julia --version" --cmd="bun/bin/leibniz"

objc:
  FROM +alpine --src="leibniz.m"
  RUN apk add --no-cache clang
  RUN --no-cache clang -O3 leibniz.m -o leibniz
  DO +BENCH --name="objc" --lang="Objective-C" --version="clang --version" --cmd="./leibniz"

pascal:
  FROM alpine:edge
  RUN apk add --no-cache hyperfine binutils fpc --repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing
  DO +ADD_FILES --src="leibniz.pas"
  RUN --no-cache fpc -O3 -Xs leibniz.pas -oleibniz
  DO +BENCH --name="pascal" --lang="Pascal (FPC)" --version="fpc -iV" --cmd="./leibniz"

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

swift:
  FROM swift:6.2-jammy
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.swift"
  RUN --no-cache swiftc leibniz.swift -O -o leibniz -clang-target native -lto=llvm-full
  DO +BENCH --name="swift" --lang="Swift" --version="swift --version" --cmd="./leibniz"

swift-simd:
  FROM swift:6.2-jammy
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz-simd.swift"
  RUN --no-cache swiftc leibniz-simd.swift -O -o leibniz -clang-target native -lto=llvm-full
  DO +BENCH --name="swift-simd" --lang="Swift (SIMD)" --version="swift --version" --cmd="./leibniz"

swift-relaxed:
  FROM swift:6.2-jammy
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz-relaxed.swift"
  COPY ./src/relaxed.h ./
  RUN --no-cache swiftc leibniz-relaxed.swift -O -o leibniz -clang-target native -lto=llvm-full -import-objc-header relaxed.h
  DO +BENCH --name="swift-relaxed" --lang="Swift (relaxed)" --version="swift --version" --cmd="./leibniz"

wasm:
  FROM ubuntu:latest
  DO +PREPARE_DEBIAN
  RUN apt-get update && apt-get install -y wget xz-utils
  RUN ARCH=$(uname -m) && \
      wget -q https://github.com/bytecodealliance/wasmtime/releases/download/v39.0.1/wasmtime-v39.0.1-${ARCH}-linux.tar.xz && \
      tar -xf wasmtime-v39.0.1-${ARCH}-linux.tar.xz && \
      mv wasmtime-v39.0.1-${ARCH}-linux/wasmtime /usr/local/bin/ && \
      rm -rf wasmtime-v39.0.1-${ARCH}-linux*
  RUN ARCH=$(uname -m | sed 's/x86_64/x86_64/;s/aarch64/arm64/') && \
      wget -q https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-25/wasi-sdk-25.0-${ARCH}-linux.tar.gz && \
      tar -xf wasi-sdk-25.0-${ARCH}-linux.tar.gz && \
      mv wasi-sdk-25.0-${ARCH}-linux /opt/wasi-sdk && \
      rm wasi-sdk-25.0-${ARCH}-linux.tar.gz
  DO +ADD_FILES --src="leibniz.c"
  RUN --no-cache /opt/wasi-sdk/bin/clang -O3 -o leibniz.wasm leibniz.c
  DO +BENCH --name="wasm" --lang="WASM (C via Wasmtime)" --version="wasmtime --version" --cmd="wasmtime --dir=. leibniz.wasm"
