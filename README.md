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
- [Python](https://www.python.org/) - interpreted (CPython)
- [Ruby](https://www.ruby-lang.org/) - interpreted
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
Rounds used: 1000000


> Julia
Version: julia version 0.6.0
Time: 805 ms
Result: π = 3.1415916535917745

> Python 3 (CPython)
Version: Python 3.6.3
Time: 336 ms
Result: π = 3.1415936535887745

> Ruby
Version: ruby 2.4.3p205 (2017-12-14 revision 61247) [x86_64-linux-musl]
Time: 199 ms
Result: π = 3.1415916535917745

> Rust
Version: rustc 1.22.1
Time: 128 ms
Result: π = 3.1415936535887745

> JS (node.js)
Version: v8.9.3
Time: 93 ms
Result: π = 3.1415936535887745

> Lua
Version: Lua 5.3.4  Copyright (C) 1994-2017 Lua.org, PUC-Rio
Time: 85 ms
Result: π = 3.1415916535917745

> PHP
Version: PHP 7.1.14 (cli) (built: Feb  8 2018 15:10:11) ( NTS )
Time: 57 ms
Result: π = 3.1415936535887745

> Nim
Version: 0.17.2
Time: 31 ms
Result: π = 3.1415916535917745

> C++
Version: g++ (Alpine 6.4.0) 6.4.0
Time: 7 ms
Result: π = 3.1415936535887745

> Crystal
Version: Crystal 0.24.1 (2017-12-22)
Time: 6 ms
Result: π = 3.1415916535917745

> Go
Version: go version go1.9.2 linux/amd64
Time: 4 ms
Result: π = 3.1415936535887745

> C
Version: gcc (Alpine 6.4.0) 6.4.0
Time: 3 ms
Result: π = 3.141594
```

## Run it yourself

Everything is run by a Docker container and a bash script which envokes the programs.

To measure the execution time a [python package](https://pypi.python.org/pypi/lauda/1.2.0) is used.

### Requirements

- `Docker`
- `Makefile` support

### Execute it

Just run: `make`

## FAQ

> Why do you also count reading a file and printing the output?

Because I think this is a more realistic scenario to compare speeds.

> Are the compile times included in the measurements?

No they are not included, because when running the program in the real world this would also be done before.

## TODO

- [ ] Add an example for Ada
- [x] Add an example for C++
- [ ] Add an example for Haskell
- [ ] Add an example for Elixir
- [ ] Add an example for Java
- [ ] Use also pypy for Python

## Thanks

This projects takes inspiration from [Thomas](https://www.thomaschristlieb.de) who did a similar comparison [on his blog](https://www.thomaschristlieb.de/performance-vergleich-zwischen-verschiedenen-programmiersprachen-und-systemen/).
