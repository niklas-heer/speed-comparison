![plot](https://raw.github.com/niklas-heer/speed-comparison/master/.github/plot_v1.4.png "Speed comparison of programming languages")

---

# Speed comparison of programming languages

This projects tries to compare the speed of different programming languages.
In this project we don't really care about getting a precise calculation of pi. We only want to see how fast are the programming languages doing.

It uses an implementation of the [Leibniz formula for π](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) to do the comparison.

Here is a video which explains how it works: [Calculating π by hand](https://www.youtube.com/watch?v=HrRMnzANHHs)

## Disclaimer

I'm no expert in all these languages, so take my results with a grain of salt.<br>
Also the findings just show how good the languages can handle floating point operations, which is only one aspect of a programming language.

You are also welcome to contribute and help me make fix my possible misuse of some languages. :smile:

## Languages used in this comparison

- [Julia](http://julialang.org/) - JIT
- [R](https://www.r-project.org/) - interpreted
- [Python](https://www.python.org/) - interpreted (CPython)
- [Ruby](https://www.ruby-lang.org/) - interpreted
- [Swift](https://swift.org/) - compiled (in this test interpreted due to Linux Swift limitations)
- [Java](http://www.oracle.com/technetwork/java/index.html) - compiled, VM
- [Rust](https://www.rust-lang.org/)  - compiled
- [Javascript](https://www.ecma-international.org/publications/standards/Ecma-402.htm) using [Node.js](https://nodejs.org/) - interpreted, JIT
- [Lua](https://www.lua.org/) - interpreted
- [Nim](https://nim-lang.org/) - compiled
- [PHP](https://secure.php.net/) - interpreted
- [C++](https://isocpp.org/) - compiled
- [Crystal](https://crystal-lang.org/) - compiled
- [Go](https://golang.org/) - compiled
- [C](https://en.wikipedia.org/wiki/C_(programming_language)) - compiled

## Results

```text
======= Comparison =======
Iterations: 1000000

> Julia
Version: julia version 0.6.2
Speed (all): 747ms, 723ms, 715ms, 730ms, 724ms, 701ms, 718ms, 726ms, 710ms, 725ms
Speed (best): 701ms
Speed (worst): 747ms
Speed (median): 723.5ms
Result: 3.1415916535917745
Accuracy: 66.67%

> Python 3 (CPython)
Version: Python 3.6.4
Speed (all): 245ms, 259ms, 223ms, 235ms, 247ms, 255ms, 260ms, 242ms, 226ms, 228ms
Speed (best): 223ms
Speed (worst): 260ms
Speed (median): 243.5ms
Result: 3.1415936535887745
Accuracy: 72.22%

> R
Version: R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"
Speed (all): 175ms, 170ms, 182ms, 182ms, 181ms, 190ms, 177ms, 180ms, 181ms, 182ms
Speed (best): 170ms
Speed (worst): 190ms
Speed (median): 181.0ms
Result: 3.1415916535917745
Accuracy: 66.67%

> Ruby
Version: ruby 2.5.0p0 (2017-12-25 revision 61468) [x86_64-linux]
Speed (all): 139ms, 133ms, 125ms, 120ms, 124ms, 134ms, 127ms, 123ms, 133ms, 136ms
Speed (best): 120ms
Speed (worst): 139ms
Speed (median): 130.0ms
Result: 3.1415916535917745
Accuracy: 66.67%

> Swift
Version: Swift version 4.0.3 (swift-4.0.3-RELEASE)
Speed (all): 112ms, 100ms, 98ms, 97ms, 105ms, 98ms, 99ms, 98ms, 101ms, 103ms
Speed (best): 97ms
Speed (worst): 112ms
Speed (median): 99.5ms
Result: 3.14159165359177
Accuracy: 75.00%

> PHP 5.6
Version: PHP 5.6.33 (cli) (built: Mar  1 2018 23:44:30)
Speed (all): 84ms, 80ms, 78ms, 80ms, 79ms, 80ms, 77ms, 83ms, 79ms, 78ms
Speed (best): 77ms
Speed (worst): 84ms
Speed (median): 79.5ms
Result: 3.1415936535887745

> Java
Version: 8.u144-1
Speed (all): 72ms, 76ms, 73ms, 83ms, 75ms, 68ms, 77ms, 79ms, 76ms, 77ms
Speed (best): 68ms
Speed (worst): 83ms
Speed (median): 76.0ms
Result: 3.1415936535887745
Accuracy: 72.22%

> JS (node)
Version: v9.6.1
Speed (all): 69ms, 65ms, 67ms, 66ms, 66ms, 66ms, 66ms, 70ms, 72ms, 65ms
Speed (best): 65ms
Speed (worst): 72ms
Speed (median): 66.0ms
Result: 3.1415936535887745
Accuracy: 72.22%

> Python 3 (pypy)
Version: 5.10.0-5
Speed (all): 112ms, 40ms, 41ms, 43ms, 42ms, 41ms, 40ms, 41ms, 41ms, 43ms
Speed (best): 40ms
Speed (worst): 112ms
Speed (median): 41.0ms
Result: 3.1415936535887745
Accuracy: 72.22%

> Lua
Version: Lua 5.3.4  Copyright (C) 1994-2017 Lua.org, PUC-Rio
Speed (all): 74ms, 67ms, 72ms, 70ms, 72ms, 72ms, 72ms, 68ms, 75ms, 76ms
Speed (best): 67ms
Speed (worst): 76ms
Speed (median): 72.0ms
Result: 3.1415916535917745
Accuracy: 66.67%

> Rust
Version: rustc 1.24.0
Speed (all): 75ms, 74ms, 73ms, 71ms, 74ms, 71ms, 71ms, 67ms, 68ms, 70ms
Speed (best): 67ms
Speed (worst): 75ms
Speed (median): 71.0ms
Result: 3.1415936535887745
Accuracy: 72.22%

> PHP 7
Version: PHP 7.2.2 (cli) (built: Jan 30 2018 19:18:38) ( NTS )
Speed (all): 58ms, 55ms, 52ms, 55ms, 54ms, 52ms, 55ms, 57ms, 54ms, 53ms
Speed (best): 52ms
Speed (worst): 58ms
Speed (median): 54.5ms
Result: 3.1415936535887745
Accuracy: 72.22%

> Nim
Version: 0.17.2-2
Speed (all): 43ms, 39ms, 45ms, 41ms, 40ms, 39ms, 40ms, 40ms, 38ms, 39ms
Speed (best): 38ms
Speed (worst): 45ms
Speed (median): 40.0ms
Result: 3.1415916535917745
Accuracy: 66.67%

> C++
Version: g++ (GCC) 7.3.0
Speed (all): 11ms, 10ms, 10ms, 9ms, 10ms, 9ms, 10ms, 8ms, 8ms, 8ms
Speed (best): 8ms
Speed (worst): 11ms
Speed (median): 9.5ms
Result: 3.1415936535887745
Accuracy: 72.22%

> Crystal
Version: Crystal 0.24.1 (2017-12-20)
Speed (all): 12ms, 10ms, 9ms, 9ms, 9ms, 9ms, 9ms, 9ms, 9ms, 10ms
Speed (best): 9ms
Speed (worst): 12ms
Speed (median): 9.0ms
Result: 3.1415916535917745
Accuracy: 66.67%

> Go
Version: go version go1.10 linux/amd64
Speed (all): 7ms, 6ms, 6ms, 6ms, 5ms, 5ms, 6ms, 5ms, 6ms, 6ms
Speed (best): 5ms
Speed (worst): 7ms
Speed (median): 6.0ms
Result: 3.1415936535887745
Accuracy: 72.22%

> C
Version: gcc (GCC) 7.3.0
Speed (all): 9ms, 7ms, 6ms, 7ms, 7ms, 6ms, 6ms, 6ms, 6ms, 6ms
Speed (best): 6ms
Speed (worst): 9ms
Speed (median): 6.0ms
Result: 3.1415936535887745
Accuracy: 72.22%
```

## Run it yourself

Everything is run by a Docker container and a bash script which envokes the programs.

To measure the execution time a [python package](https://pypi.python.org/pypi/lauda/1.2.0) is used.

### Requirements

- `Docker`
- `Makefile` support

### Run measurement

Just run: `make`

### Print plot

`make plot`

### Run cli

`make cli`

## FAQ

> Why do you also count reading a file and printing the output?

Because I think this is a more realistic scenario to compare speeds.

> Are the compile times included in the measurements?

No they are not included, because when running the program in the real world this would also be done before.

## TODO

- [x] Add C++
- [x] Add run pypy for Python
- [x] Produce graphic with results
- [x] Add Java
- [x] Add Swift
- [x] Add R
- [x] Add [Php 5.6](https://aur.archlinux.org/packages/php56/)
- [ ] Add phpv8js
- [ ] Add python2
- [ ] Add Smalltalk
- [ ] Add C#
- [ ] Add Perl
- [ ] Add Kotlin
- [ ] Add Haskell
- [ ] Add Elixir
- [ ] Add Lisp
- [ ] Add Ada
- [ ] Add COBOL

## Thanks

This projects takes inspiration from [Thomas](https://www.thomaschristlieb.de) who did a similar comparison [on his blog](https://www.thomaschristlieb.de/performance-vergleich-zwischen-verschiedenen-programmiersprachen-und-systemen/).
