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

alpine:
  FROM alpine:3.16
  RUN apk add --no-cache hyperfine
  WORKDIR /app
  COPY +build/scmeta ./
  COPY ./src/rounds.txt ./

collect-data:
  # Preparing
  BUILD +build
  BUILD +alpine

  # Work through programming languages
  BUILD +c
  BUILD +clj
  BUILD +clj-bb
  BUILD +cpp
  BUILD +crystal
  BUILD +cs
  BUILD +elixir
  BUILD +fortran
  BUILD +go
  BUILD +java
  BUILD +julia
  BUILD +julia-compiled
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
  BUILD +swift
  BUILD +zig

all:
  BUILD +collect-data
  BUILD +analysis

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

HYPERFINE_DEBIAN:
  COMMAND
  RUN wget https://github.com/sharkdp/hyperfine/releases/download/v1.15.0/hyperfine_1.15.0_amd64.deb
  RUN dpkg -i hyperfine_1.15.0_amd64.deb

c:
  FROM +alpine
  RUN apk add --no-cache gcc build-base

  COPY ./src/leibniz.c ./
  RUN --no-cache gcc leibniz.c -o leibniz -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="c" --lang="C (gcc)" --version="gcc --version" --cmd="./leibniz"

clj:
  FROM clojure:temurin-19-tools-deps-alpine
  # Seems to be a bug
  RUN apk add --no-cache rlwrap hyperfine
  COPY +build/scmeta ./

  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.clj ./
  DO +BENCH --name="clj" --lang="Clojure" --version="clj --version" --cmd="clj leibniz.clj"

clj-bb:
  # Uses https://babashka.org/
  FROM babashka/babashka:alpine
  RUN apk add --no-cache hyperfine
  COPY +build/scmeta ./

  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.clj ./
  DO +BENCH --name="clj-bb" --lang="Clojure (Babashka)" --version="bb --version" --cmd="bb -f leibniz.clj"

cpp:
  FROM +alpine
  RUN apk add --no-cache gcc build-base

  COPY ./src/leibniz.cpp ./
  RUN --no-cache g++ leibniz.cpp -o leibniz -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer -fno-signed-zeros -fno-trapping-math -fassociative-math
  DO +BENCH --name="cpp" --lang="C++ (g++)" --version="g++ --version" --cmd="./leibniz"

crystal:
  FROM crystallang/crystal:1.6-alpine
  RUN apk add --no-cache hyperfine
  COPY +build/scmeta ./

  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.cr ./
  RUN --no-cache crystal build leibniz.cr --release
  DO +BENCH --name="crystal" --lang="Crystal" --version="crystal -v" --cmd="./leibniz"

cs:
  # Use the dedicated image from Microsoft
  FROM mcr.microsoft.com/dotnet/sdk:7.0-alpine3.16
  RUN apk add --no-cache hyperfine
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

elixir:
  FROM +alpine
  RUN apk add --no-cache elixir

  COPY ./src/leibniz.ex ./
  # The version number is in the second match index -> 1 (instead of 0)
  DO +BENCH --name="elixir" --lang="Elixir" --version="elixir --version" --cmd="elixir leibniz.ex" --index="1"

fortran:
  FROM ubuntu:latest
  RUN apt-get update && apt-get install -y gfortran-12 wget
  DO +HYPERFINE_DEBIAN
  COPY +build/scmeta ./
  
  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.f90 ./
  RUN --no-cache gfortran-12 -Ofast -march=native -mtune=native -flto -ffast-math leibniz.f90 -o leibniz
  DO +BENCH --name="fortran" --lang="Fortran 90" --version="gfortran-12 --version" --cmd="./leibniz"

go:
  # We can reuse the build image of the scbench tool
  FROM golang:1.19.1-alpine
  RUN apk add --no-cache hyperfine
  COPY +build/scmeta ./

  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.go ./
  RUN --no-cache go build leibniz.go
  DO +BENCH --name="go" --lang="Go" --version="go version" --cmd="./leibniz"

java:
  # Using a dedicated image due to the packages on alpine being not up to date.
  FROM eclipse-temurin:19_36-jdk-alpine
  RUN apk add --no-cache hyperfine
  COPY +build/scmeta ./

  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.java ./
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
  RUN apk add --no-cache hyperfine
  COPY +build/scmeta ./

  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.jl ./
  DO +BENCH --name="julia" --lang="Julia" --version="julia --version" --cmd="julia leibniz.jl"

julia-compiled:
  # We need the Debian version otherwise the build doesn't work
  FROM julia:1.8.2
  RUN apt-get update && apt-get install -y gcc g++ build-essential cmake wget
  DO +HYPERFINE_DEBIAN
  COPY +build/scmeta ./

  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.jl ./
  COPY ./src/leibniz_compiled.jl ./
  RUN julia -e 'using Pkg; Pkg.add(["StaticCompiler", "StaticTools"]); using StaticCompiler, StaticTools; include("./leibniz_compiled.jl"); compile_executable(mainjl, (), "./")'
  DO +BENCH --name="julia-compiled" --lang="Julia (AOT compiled)" --version="julia --version" --cmd="./mainjl"

nodejs:
  FROM +alpine
  RUN apk add --no-cache nodejs-current

  COPY ./src/leibniz.js ./
  DO +BENCH --name="nodejs" --lang="Javascript (nodejs)" --version="node --version" --cmd="node leibniz.js"

lua:
  FROM +alpine
  RUN apk add --no-cache lua5.4

  COPY ./src/leibniz.lua ./
  DO +BENCH --name="lua" --lang="Lua" --version="lua5.4 -v" --cmd="lua5.4 leibniz.lua"

luajit:
  FROM +alpine
  RUN apk add --no-cache luajit

  COPY ./src/leibniz.lua ./
  DO +BENCH --name="luajit" --lang="LuaJIT" --version="luajit -v" --cmd="luajit leibniz.lua"

nim:
  FROM +alpine
  RUN apk add --no-cache gcc build-base nim

  COPY ./src/leibniz.nim ./
  RUN --no-cache nim c --verbosity:0 -d:danger --passC:"-flto"  --passL:"-flto" --gc:arc --out:leibniz leibniz.nim
  DO +BENCH --name="nim" --lang="Nim" --version="nim --version" --cmd="./leibniz"

php:
  FROM +alpine
  RUN apk add --no-cache php81

  COPY ./src/leibniz.php ./
  DO +BENCH --name="php" --lang="PHP" --version="php81 --version" --cmd="php81 leibniz.php"

perl:
  FROM +alpine
  RUN apk add --no-cache perl

  COPY ./src/leibniz.pl ./
  DO +BENCH --name="perl" --lang="Perl" --version="perl -v" --cmd="perl leibniz.pl"

cpython:
  FROM +alpine
  RUN apk add --no-cache python3

  COPY ./src/leibniz.py ./
  DO +BENCH --name="cpython" --lang="Python (CPython)" --version="python3 --version" --cmd="python3 leibniz.py"

pypy:
  # There is no pypy package on alpine
  # We use the standard which is Debian
  FROM pypy:3.9
  DO +HYPERFINE_DEBIAN
  COPY +build/scmeta ./

  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.py ./
  DO +BENCH --name="pypy" --lang="Python (PyPy)" --version="pypy --version" --cmd="pypy leibniz.py"

r:
  FROM +alpine
  RUN apk add --no-cache R

  COPY ./src/leibniz.r ./
  DO +BENCH --name="r" --lang="R" --version="R --version" --cmd="Rscript --vanilla --default-packages=base leibniz.r"

ruby:
  FROM +alpine
  RUN apk add --no-cache ruby

  COPY ./src/leibniz.rb ./
  DO +BENCH --name="ruby" --lang="Ruby" --version="ruby --version" --cmd="ruby leibniz.rb"

rust:
  FROM +alpine
  RUN apk add --no-cache rust

  COPY ./src/leibniz.rs ./
  RUN --no-cache rustc -C debuginfo=0 -C opt-level=3 -C target-cpu=native -C lto=fat -C codegen-units=1 -C panic=abort leibniz.rs
  DO +BENCH --name="rust" --lang="Rust" --version="rustc --version" --cmd="./leibniz"

swift:
  FROM swift:5.7-jammy
  RUN apt-get update && apt-get install -y wget
  DO +HYPERFINE_DEBIAN
  COPY +build/scmeta ./

  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.swift ./
  RUN --no-cache swiftc leibniz.swift -O -o leibniz -clang-target native -lto=llvm-full
  DO +BENCH --name="swift" --lang="Swift" --version="swift --version" --cmd="./leibniz"

zig:
  # On 3.16 there is no zig package, but on edge there is
  FROM alpine:edge
  # https://pkgs.alpinelinux.org/package/edge/testing/aarch64/zig
  # https://stackoverflow.com/a/62218241
  RUN apk add --no-cache hyperfine zig --repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing
  WORKDIR /app
  COPY +build/scmeta ./

  COPY ./src/rounds.txt ./
  COPY ./src/leibniz.zig ./
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
