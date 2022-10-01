VERSION 0.6
FROM earthly/dind:alpine

# Variables
ARG iterations=10


build:
  FROM golang:1.19.1-alpine
  WORKDIR /go-build
  COPY --dir scbench ./
  WORKDIR /go-build/scbench
  RUN go build -v -o build/scbench ./cmd/scbench
  SAVE ARTIFACT build/scbench /scbench

alpine:
  FROM alpine:3.16

  COPY ./src/rounds.txt ./
  COPY +build/scbench ./

all:
  # Preparing
  BUILD +build
  BUILD +alpine

  # Work through programming languages
  BUILD +c
  BUILD +cpp
  BUILD +crystal
  BUILD +elixir
  BUILD +go
  BUILD +java
  BUILD +julia
  BUILD +nodejs
  BUILD +lua
  # BUILD +nim
  # BUILD +php
  BUILD +python
  # BUILD +r
  BUILD +ruby
  # BUILD +rust
  # BUILD +swift

c:
  FROM +alpine
  RUN apk add --no-cache gcc build-base

  COPY ./src/leibniz.c ./
  RUN --no-cache gcc leibniz.c -o leibniz -O3 -s -march=native -mtune=native -fomit-frame-pointer
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "gcc --version" --export json --lang "C (gcc)"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/c.json

cpp:
  FROM +alpine
  RUN apk add --no-cache gcc build-base

  COPY ./src/leibniz.cpp ./
  RUN --no-cache g++ leibniz.cpp -o leibniz -O3 -s -march=native -mtune=native -fomit-frame-pointer
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "g++ --version" --export json --lang "C++ (g++)"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/cpp.json

crystal:
  FROM +alpine
  RUN apk add --no-cache crystal

  COPY ./src/leibniz.cr ./
  RUN --no-cache crystal build leibniz.cr
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "crystal --version" --export json --lang "Crystal"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/crystal.json

elixir:
  FROM +alpine
  RUN apk add --no-cache elixir

  COPY ./src/leibniz.ex ./

  # We need to selected the second version from the version command since first it displays the Erlang/OTP version.
  # flag: [...] -L 1 (it starts with 0)
  RUN --no-cache ./scbench "elixir leibniz.ex" -i $iterations -l "elixir --version" --export json --lang "Elixir" -L 1
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/elixir.json

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
  FROM julia:1.6.7-alpine3.16
  COPY ./src/rounds.txt ./
  COPY +build/scbench ./

  COPY ./src/leibniz.jl ./
  RUN --no-cache ./scbench "julia leibniz.jl" -i $iterations -l "julia --version" --export json --lang "Julia"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/julia.json

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

python:
  FROM +alpine
  RUN apk add --no-cache python3

  COPY ./src/leibniz.py ./
  RUN --no-cache ./scbench "python3 leibniz.py" -i $iterations -l "python3 --version" --export json --lang "Python (CPython)"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/python.json

ruby:
  FROM +alpine
  RUN apk add --no-cache ruby

  COPY ./src/leibniz.rb ./
  RUN --no-cache ./scbench "ruby leibniz.rb" -i $iterations -l "ruby --version" --export json --lang "Ruby"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/ruby.json

