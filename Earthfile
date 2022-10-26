VERSION 0.6
FROM earthly/dind:alpine

# Variables
ARG iterations=3
ARG warmups=2
ARG timeas="second"

build:
  FROM crystallang/crystal:1.6-alpine
  WORKDIR /app
  COPY --dir scmeta ./
  WORKDIR /app/scmeta
  RUN shards install --production -v
  RUN crystal build src/scmeta.cr --release --static -o bin/scmeta
  SAVE ARTIFACT bin/scmeta /scmeta

# Benchmark function which invokes `hyperfine` and `scmeta`
BENCH:
  COMMAND
  ARG --required name
  ARG --required cmd
  ARG --required lang
  ARG --required version
  ARG index=0

  RUN --no-cache hyperfine "$cmd" --warmup $warmups --runs $iterations --time-unit $timeas --export-json "./hyperfine.json" --output "./pi.txt"
  RUN --no-cache ./scmeta --lang-name="$lang" --lang-version="$version" --hyperfine="./hyperfine.json" --pi="./pi.txt" --output="./scmeta.json" --lang-version-match-index="$index"
  SAVE ARTIFACT ./scmeta.json AS LOCAL ./results/$name.json

PREPARE_DEBIAN:
  COMMAND
  RUN apt-get update && apt-get install -y wget
  RUN wget https://github.com/sharkdp/hyperfine/releases/download/v1.15.0/hyperfine_1.15.0_amd64.deb
  RUN dpkg -i hyperfine_1.15.0_amd64.deb

PREPARE_ALPINE:
  COMMAND
  RUN apk add --no-cache hyperfine

ADD_FILES:
  COMMAND
  ARG --required src
  WORKDIR /app
  COPY +build/scmeta ./
  COPY ./src/rounds.txt ./
  COPY ./src/"$src" ./

alpine:
  ARG --required src
  FROM alpine:3.16
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="$src"

collect-data:
  # Preparing
  BUILD +build
  BUILD +alpine

  # Work through programming languages
  BUILD +c
  BUILD +clj
  BUILD +clj-bb
  BUILD +cpp
  BUILD +cpp-avx2
  BUILD +crystal
  BUILD +cs
  BUILD +d
  BUILD +elixir
  BUILD +fortran
  BUILD +go
  BUILD +java
  BUILD +julia
  BUILD +julia-compiled
  BUILD +julia-ux4
  BUILD +nodejs
  BUILD +lua
  BUILD +luajit
  BUILD +nim
  BUILD +php
  BUILD +perl
  BUILD +cpython
  BUILD +pypy
  BUILD +r
  BUILD +ruby
  BUILD +rust
  BUILD +sbcl
  BUILD +swift
  BUILD +zig

all:
  BUILD +collect-data
  BUILD +analysis

c:
  FROM +alpine --src="leibniz.c"
  RUN apk add --no-cache gcc build-base
  RUN --no-cache gcc leibniz.c -o leibniz -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="c" --lang="C (gcc)" --version="gcc --version" --cmd="./leibniz"

clj:
  FROM clojure:temurin-19-tools-deps-alpine
  DO +PREPARE_ALPINE
  # Seems to be a bug
  RUN apk add --no-cache rlwrap
  DO +ADD_FILES --src="leibniz.clj"
  DO +BENCH --name="clj" --lang="Clojure" --version="clj --version" --cmd="clj leibniz.clj"

clj-bb:
  # Uses https://babashka.org/
  FROM babashka/babashka:alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.clj"
  DO +BENCH --name="clj-bb" --lang="Clojure (Babashka)" --version="bb --version" --cmd="bb -f leibniz.clj"

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

crystal:
  FROM crystallang/crystal:1.6-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.cr"
  RUN --no-cache crystal build leibniz.cr --release
  DO +BENCH --name="crystal" --lang="Crystal" --version="crystal -v" --cmd="./leibniz"

cs:
  # Use the dedicated image from Microsoft
  FROM mcr.microsoft.com/dotnet/sdk:7.0-alpine3.16
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

d:
  FROM +alpine --src="leibniz.d"
  RUN apk add --no-cache gcc-gdc
  RUN --no-cache gdc leibniz.d -o leibniz -O3 -frelease -march=native
  DO +BENCH --name="d" --lang="D (GDC)" --version="gdc --version" --cmd="./leibniz"

elixir:
  FROM +alpine --src="leibniz.ex"
  RUN apk add --no-cache elixir
  # The version number is in the second match index -> 1 (instead of 0)
  DO +BENCH --name="elixir" --lang="Elixir" --version="elixir --version" --cmd="elixir leibniz.ex" --index="1"

fortran:
  FROM ubuntu:latest
  DO +PREPARE_DEBIAN
  RUN apt-get update && apt-get install -y gfortran-12 wget
  DO +ADD_FILES --src="leibniz.f90"
  RUN --no-cache gfortran-12 -Ofast -march=native -mtune=native -flto -ffast-math leibniz.f90 -o leibniz
  DO +BENCH --name="fortran" --lang="Fortran 90" --version="gfortran-12 --version" --cmd="./leibniz"

go:
  # We can reuse the build image of the scbench tool
  FROM golang:1.19.1-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.go"
  RUN --no-cache go build leibniz.go
  DO +BENCH --name="go" --lang="Go" --version="go version" --cmd="./leibniz"

java:
  # Using a dedicated image due to the packages on alpine being not up to date.
  FROM eclipse-temurin:19_36-jdk-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.java"
  RUN --no-cache javac leibniz.java
  # TODO: Change scbench to be able to handle Java version. For now it's static.
  # $ java -version
  # openjdk version "19" 2022-09-20
  # OpenJDK Runtime Environment Temurin-19+36 (build 19+36)
  # OpenJDK 64-Bit Server VM Temurin-19+36 (build 19+36, mixed mode, sharing)
  DO +BENCH --name="java" --lang="Java" --version="echo 19.36" --cmd="java leibniz"

julia:
  # We have to use a special image since there is no Julia package on alpine 🤷‍♂️
  FROM julia:1.8.2-alpine3.16
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.jl"
  DO +BENCH --name="julia" --lang="Julia" --version="julia --version" --cmd="julia leibniz.jl"

julia-compiled:
  # We need the Debian version otherwise the build doesn't work
  FROM julia:1.8.2
  DO +PREPARE_DEBIAN
  RUN apt-get update && apt-get install -y gcc g++ build-essential cmake
  DO +ADD_FILES --src="leibniz_compiled.jl"
  COPY ./src/leibniz.jl ./
  RUN julia -e 'using Pkg; Pkg.add(["StaticCompiler", "StaticTools"]); using StaticCompiler, StaticTools; include("./leibniz_compiled.jl"); compile_executable(mainjl, (), "./")'
  DO +BENCH --name="julia-compiled" --lang="Julia (AOT compiled)" --version="julia --version" --cmd="./mainjl"

julia-ux4:
  # We have to use a special image since there is no Julia package on alpine 🤷‍♂️
  FROM julia:1.8.2-alpine3.16
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz_ux4.jl"
  DO +BENCH --name="julia-ux4" --lang="Julia (ux4)" --version="julia --version" --cmd="julia leibniz_ux4.jl"

nodejs:
  FROM +alpine --src="leibniz.js"
  RUN apk add --no-cache nodejs-current
  DO +BENCH --name="nodejs" --lang="Javascript (nodejs)" --version="node --version" --cmd="node leibniz.js"

lua:
  FROM +alpine --src="leibniz.lua"
  RUN apk add --no-cache lua5.4
  DO +BENCH --name="lua" --lang="Lua" --version="lua5.4 -v" --cmd="lua5.4 leibniz.lua"

luajit:
  FROM +alpine --src="leibniz.lua"
  RUN apk add --no-cache luajit
  DO +BENCH --name="luajit" --lang="LuaJIT" --version="luajit -v" --cmd="luajit leibniz.lua"

nim:
  FROM +alpine --src="leibniz.nim"
  RUN apk add --no-cache gcc build-base nim
  RUN --no-cache nim c --verbosity:0 -d:danger --passC:"-flto"  --passL:"-flto" --gc:arc --out:leibniz leibniz.nim
  DO +BENCH --name="nim" --lang="Nim" --version="nim --version" --cmd="./leibniz"

php:
  FROM +alpine --src="leibniz.php"
  RUN apk add --no-cache php81
  DO +BENCH --name="php" --lang="PHP" --version="php81 --version" --cmd="php81 leibniz.php"

perl:
  FROM +alpine --src="leibniz.pl"
  RUN apk add --no-cache perl
  DO +BENCH --name="perl" --lang="Perl" --version="perl -v" --cmd="perl leibniz.pl"

cpython:
  FROM python:3.11-alpine
  DO +PREPARE_ALPINE
  DO +ADD_FILES --src="leibniz.py"
  DO +BENCH --name="cpython" --lang="Python (CPython)" --version="python3 --version" --cmd="python3 leibniz.py"

pypy:
  # There is no pypy package on alpine
  # We use the standard which is Debian
  FROM pypy:3.9
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
  FROM +alpine --src="leibniz.rs"
  RUN apk add --no-cache rust
  RUN --no-cache rustc -C debuginfo=0 -C opt-level=3 -C target-cpu=native -C lto=fat -C codegen-units=1 -C panic=abort leibniz.rs
  DO +BENCH --name="rust" --lang="Rust" --version="rustc --version" --cmd="./leibniz"

sbcl:
  FROM +alpine --src="leibniz.lisp"
  RUN apk add --no-cache sbcl
  RUN --no-cache sbcl --noinform --eval '(compile-file "leibniz.lisp")' --quit
  DO +BENCH --name="sbcl" --lang="Common Lisp (SBCL)" --version="sbcl --version" --cmd="sbcl --script leibniz.fasl"

swift:
  FROM swift:5.7-jammy
  DO +PREPARE_DEBIAN
  DO +ADD_FILES --src="leibniz.swift"
  RUN --no-cache swiftc leibniz.swift -O -o leibniz -clang-target native -lto=llvm-full
  DO +BENCH --name="swift" --lang="Swift" --version="swift --version" --cmd="./leibniz"

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
  # alpine doesn't seem to work with the pandas package 🤷‍♂️
  FROM python:3.10-slim

  COPY ./requirements.txt ./
  RUN pip install -r ./requirements.txt

  COPY ./*.py ./
  COPY ./src/rounds.txt ./
  COPY --dir results ./

  # Combine all results
  RUN --no-cache python analyze.py --folder ./results/ --out ./ --rounds ./rounds.txt
  SAVE ARTIFACT ./*.csv AS LOCAL ./results/
  SAVE ARTIFACT ./*.png AS LOCAL ./results/
