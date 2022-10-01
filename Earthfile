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
  BUILD +build
  BUILD +alpine
  BUILD +c
  BUILD +cpp
  BUILD +crystal
  BUILD +elixir
  BUILD +julia
  BUILD +python
  BUILD +ruby

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
  # Use alpine, because there is an old version on Nix
  FROM +alpine
  RUN apk add --no-cache crystal

  COPY ./src/leibniz.cr ./
  RUN --no-cache crystal build leibniz.cr
  RUN --no-cache ./scbench "./leibniz" -i $iterations -l "crystal --version" --export json --lang "Crystal"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/crystal.json

elixir:
  # Use alpine, since there are some problems on Nix with UTF8
  FROM +alpine
  RUN apk add --no-cache elixir

  COPY ./src/leibniz.ex ./

  # We need to selected the second version from the version command since first it displays the Erlang/OTP version.
  # flag: [...] -L 1 (it starts with 0)
  RUN --no-cache ./scbench "elixir leibniz.ex" -i $iterations -l "elixir --version" --export json --lang "Elixir" -L 1
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/elixir.json

julia:
  # We have to use a special image since there is no Julia package on alpine 🤷‍♂️
  FROM julia:1.6.7-alpine3.16
  COPY ./src/rounds.txt ./
  COPY +build/scbench ./

  COPY ./src/leibniz.jl ./
  RUN --no-cache ./scbench "julia leibniz.jl" -i $iterations -l "julia --version" --export json --lang "Julia"
  SAVE ARTIFACT ./scbench-summary.json AS LOCAL ./results/julia.json

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

