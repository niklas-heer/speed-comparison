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

  COPY ./src/rounds.txt ./
  COPY +build/scmeta ./

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

all:
  BUILD +collect-data
  BUILD +analysis

c:
  FROM +alpine
  RUN apk add --no-cache gcc build-base

  COPY ./src/leibniz.c ./
  RUN --no-cache gcc leibniz.c -o leibniz -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer
  RUN --no-cache hyperfine "./leibniz" --warmup $warmups --runs $iterations --time-unit $timeas --export-json "./hyperfine.json" --output "./pi.txt"
  RUN --no-cache ./scmeta --lang-name="C (gcc)" --lang-version="gcc --version" --hyperfine="./hyperfine.json" --pi="./pi.txt" --output="./scmeta.json"
  SAVE ARTIFACT ./scmeta.json AS LOCAL ./results/c.json

clj:
  FROM clojure:temurin-19-tools-deps-alpine
  COPY ./src/rounds.txt ./
  COPY +build/scmeta ./

  # Seems to be a bug
  RUN apk add --no-cache rlwrap hyperfine

  COPY ./src/leibniz.clj ./
  RUN --no-cache hyperfine "clj leibniz.clj" --warmup $warmups --runs $iterations --time-unit $timeas --export-json "./hyperfine.json" --output "./pi.txt"
  RUN --no-cache ./scmeta --lang-name="Clojure" --lang-version="clj --version" --hyperfine="./hyperfine.json" --pi="./pi.txt" --output="./scmeta.json"
  SAVE ARTIFACT ./scmeta.json AS LOCAL ./results/clj.json

clj-bb:
  # Uses https://babashka.org/
  FROM babashka/babashka:alpine
  COPY ./src/rounds.txt ./
  COPY +build/scmeta ./

  RUN apk add --no-cache hyperfine

  COPY ./src/leibniz.clj ./
  RUN --no-cache hyperfine "bb -f leibniz.clj" --warmup $warmups --runs $iterations --time-unit $timeas --export-json "./hyperfine.json" --output "./pi.txt"
  RUN --no-cache ./scmeta --lang-name="Clojure (Babashka)" --lang-version="bb --version" --hyperfine="./hyperfine.json" --pi="./pi.txt" --output="./scmeta.json"
  SAVE ARTIFACT ./scmeta.json AS LOCAL ./results/clj-bb.json

cpp:
  FROM +alpine
  RUN apk add --no-cache gcc build-base

  COPY ./src/leibniz.cpp ./
  RUN --no-cache g++ leibniz.cpp -o leibniz -O3 -s -static -flto -march=native -mtune=native -fomit-frame-pointer
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "g++ --version" --export json --lang "C++ (g++)"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/cpp.json

crystal:
  FROM +alpine
  RUN apk add --no-cache crystal

  COPY ./src/leibniz.cr ./
  RUN --no-cache crystal build leibniz.cr --release
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "crystal --version" --export json --lang "Crystal"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/crystal.json

cs:
  # Use the dedicated image from Microsoft
  FROM mcr.microsoft.com/dotnet/sdk:7.0-alpine3.16
  WORKDIR /app
  
  # BUILD, first restore than build
  COPY ./src/cs/*.csproj .
  RUN dotnet restore
  COPY ./src/cs/*.cs .
  RUN --no-cache dotnet publish -c Release -o out --no-restore

  # Execute test run
  WORKDIR /app/out
  COPY +build/scbench ./
  COPY ./src/rounds.txt ./
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "dotnet --version" --export json --lang "C#"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/cs.json

elixir:
  FROM +alpine
  RUN apk add --no-cache elixir

  COPY ./src/leibniz.ex ./

  # We need to selected the second version from the version command since first it displays the Erlang/OTP version.
  # flag: [...] -L 1 (it starts with 0)
  RUN --no-cache ./scbench "elixir leibniz.ex" -i $iterations -l "elixir --version" --export json --lang "Elixir" -L 1
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/elixir.json

fortran:
  FROM +alpine
  RUN apk add --no-cache gfortran build-base

  COPY ./src/leibniz.f90 ./
  RUN --no-cache gfortran -Ofast -flto leibniz.f90 -o leibniz
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "gfortran --version" --export json --lang "Fortran 90 (gfortran)"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/fortran.json

go:
  # We can reuse the build image of the scbench tool
  FROM golang:1.19.1-alpine
  COPY ./src/rounds.txt ./
  COPY +build/scbench ./

  COPY ./src/leibniz.go ./
  RUN --no-cache go build leibniz.go
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "go version" --export json --lang "Go"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/go.json

java:
  # Using a dedicated image due to the packages on alpine being not up to date.
  FROM eclipse-temurin:19_36-jdk-alpine
  COPY ./src/rounds.txt ./
  COPY +build/scbench ./

  COPY ./src/leibniz.java ./
  RUN --no-cache javac leibniz.java
  # TODO: Change scbench to be able to handle Java version. For now it's static.
  # $ java -version
  # openjdk version "19" 2022-09-20
  # OpenJDK Runtime Environment Temurin-19+36 (build 19+36)
  # OpenJDK 64-Bit Server VM Temurin-19+36 (build 19+36, mixed mode, sharing)
  RUN --no-cache ./scbench "java leibniz" -i $iterations -l "echo 19.36" --export json --lang "Java"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/java.json

julia:
  # We have to use a special image since there is no Julia package on alpine 🤷‍♂️
  FROM julia:1.8.2-alpine3.16
  COPY ./src/rounds.txt ./
  COPY +build/scbench ./

  COPY ./src/leibniz.jl ./
  RUN --no-cache ./scbench "julia leibniz.jl" -i $iterations -l "julia --version" --export json --lang "Julia"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/julia.json

julia-compiled:
  # We need the Debian version otherwise the build doesn't work
  FROM julia:1.8.2
  RUN apt-get update && apt-get install -y gcc g++ build-essential cmake
  COPY ./src/rounds.txt ./
  COPY +build/scbench ./

  COPY ./src/leibniz.jl ./
  COPY ./src/leibniz_compiled.jl ./
  RUN julia -e 'using Pkg; Pkg.add(["StaticCompiler", "StaticTools"]); using StaticCompiler, StaticTools; include("./leibniz_compiled.jl"); compile_executable(mainjl, (), "./")'
  RUN --no-cache ./scbench "./mainjl" -i $iterations -l "julia --version" --export json --lang "Julia (AOT compiled)"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/julia-compiled.json

nodejs:
  FROM +alpine
  RUN apk add --no-cache nodejs-current

  COPY ./src/leibniz.js ./
  RUN --no-cache ./scbench "node leibniz.js" -i $iterations -l "node --version" --export json --lang "Javascript (nodejs)"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/nodejs.json

lua:
  FROM +alpine
  RUN apk add --no-cache lua5.4

  COPY ./src/leibniz.lua ./
  RUN --no-cache ./scbench "lua5.4 leibniz.lua" -i $iterations -l "lua5.4 -v" --export json --lang "Lua"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/lua.json

luajit:
  FROM +alpine
  RUN apk add --no-cache luajit

  COPY ./src/leibniz.lua ./
  RUN --no-cache ./scbench "luajit leibniz.lua" -i $iterations -l "luajit -v" --export json --lang "LuaJIT"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/luajit.json

nim:
  FROM +alpine
  RUN apk add --no-cache gcc build-base nim

  COPY ./src/leibniz.nim ./
  RUN --no-cache nim c --verbosity:0 -d:danger --passC:"-flto"  --passL:"-flto" --gc:arc --out:leibniz leibniz.nim
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "nim --version" --export json --lang "Nim"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/nim.json

php:
  FROM +alpine
  RUN apk add --no-cache php81

  COPY ./src/leibniz.php ./
  RUN --no-cache ./scbench "php81 leibniz.php" -i $iterations -l "php81 --version" --export json --lang "PHP"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/php.json

perl:
  FROM +alpine
  RUN apk add --no-cache perl

  COPY ./src/leibniz.pl ./
  RUN --no-cache ./scbench "perl leibniz.pl" -i $iterations -l "perl -v" --export json --lang "Perl"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/perl.json

cpython:
  FROM +alpine
  RUN apk add --no-cache python3

  COPY ./src/leibniz.py ./
  RUN --no-cache ./scbench "python3 leibniz.py" -i $iterations -l "python3 --version" --export json --lang "Python (CPython)"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/cpython.json

pypy:
  # There is no pypy package on alpine
  FROM pypy:3.9-slim
  COPY ./src/rounds.txt ./
  COPY +build/scbench ./

  COPY ./src/leibniz.py ./
  RUN --no-cache ./scbench "pypy leibniz.py" -i $iterations -l "pypy --version" --export json --lang "Python (PyPy)"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/pypy.json

r:
  FROM +alpine
  RUN apk add --no-cache R

  COPY ./src/leibniz.r ./
  RUN --no-cache ./scbench "Rscript --vanilla --default-packages=base leibniz.r" -i $iterations -l "R --version" --export json --lang "R"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/R.json

ruby:
  FROM +alpine
  RUN apk add --no-cache ruby

  COPY ./src/leibniz.rb ./
  RUN --no-cache ./scbench "ruby leibniz.rb" -i $iterations -l "ruby --version" --export json --lang "Ruby"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/ruby.json

rust:
  FROM +alpine
  RUN apk add --no-cache rust

  COPY ./src/leibniz.rs ./
  RUN --no-cache rustc -C debuginfo=0 -C opt-level=3 -C target-cpu=native -C lto=fat -C codegen-units=1 -C panic=abort leibniz.rs
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "rustc --version" --export json --lang "Rust"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/rust.json

swift:
  # There is no swift package on alpine
  # TODO: try to use slim version again. For now it seems broken.
  # https://forums.swift.org/t/bug-in-docker-image-swift-5-7-slim-swift-command-not-found/60609
  FROM swift:5.7-jammy
  COPY ./src/rounds.txt ./
  COPY +build/scbench ./

  COPY ./src/leibniz.swift ./
  RUN --no-cache swiftc leibniz.swift -O -o leibniz -clang-target native -lto=llvm-full
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "swift --version" --export json --lang "Swift"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/swift.json

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
