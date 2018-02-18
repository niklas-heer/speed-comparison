# Speed comparison of programming languages

This projects tries to compare the speed of different programming languages.
In this project we don't really care about getting a precise calculation of pi. We only want to see how fast are the programming languages doing.

It uses an implementation of the [Leibniz formula for π](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) to do the comparison.

Here is a video which explains how it works: [Calculating π by hand](https://www.youtube.com/watch?v=HrRMnzANHHs)

## Languages used in this comparison

- [Julia](http://julialang.org/)
- [Python](https://www.python.org/)
- [Ruby](https://www.ruby-lang.org/)
- [Rust](https://www.rust-lang.org/)
- [Javascript](https://www.ecma-international.org/publications/standards/Ecma-402.htm) using [Node.js](https://nodejs.org/)
- [Lua](https://www.lua.org/)
- [PHP](https://secure.php.net/)
- [Crystal](https://crystal-lang.org/)
- [Go](https://golang.org/)
- [C](https://en.wikipedia.org/wiki/C_(programming_language))

## Run it yourself

Everything is run by a Docker container and a bash script which envokes the programs.

To measure the execution time a diff of the [`date`](http://man7.org/linux/man-pages/man1/date.1.html) command is used.
It is measure by nanoseconds but displayed in milliseconds.

### Requirements

- `Docker`
- `Makefile` support

### Execute it

Just run: `make`

## Results

```text
======= Comparison =======
Rounds used: 1000000


> Python 3 (CPython)
Version: Python 3.6.3
Time: 0m0.336s
Result: π = 3.1415936535887745

> Ruby
Version: ruby 2.4.3p205 (2017-12-14 revision 61247) [x86_64-linux-musl]
Time: 0m0.212s
Result: π = 3.1415916535917745

> Rust
Version: rustc 1.22.1
Time: 0m0.128s
Result: π = 3.1415936535887745

> JS (node.js)
Version: v8.9.3
Time: 0m0.094s
Result: π = 3.1415936535887745

> Lua
Version: Lua 5.3.4  Copyright (C) 1994-2017 Lua.org, PUC-Rio
Time: 0m0.086s
Result: π = 3.1415916535917745

> PHP
Version: PHP 7.1.14 (cli) (built: Feb  8 2018 15:10:11) ( NTS )
Time: 0m0.059s
Result: π = 3.1415936535887745

> Crystal
Version: Crystal 0.24.1 (2017-12-22)
Time: 0m0.005s
Result: π = 3.1415916535917745

> Go
Version: go version go1.9.2 linux/amd64
Time: 0m0.004s
Result: π = 3.1415936535887745

> C
Version: gcc (Alpine 6.4.0) 6.4.0
Time: 0m0.003s
Result: π = 3.141594
```

## FAQ

> Why do you also count reading a file and printing the output?

Because I think this is a more realistic scenario to compare speeds.

## Thanks

This projects takes inspiration from [Thomas](https://www.thomaschristlieb.de) who did a similar comparison [on his blog](https://www.thomaschristlieb.de/performance-vergleich-zwischen-verschiedenen-programmiersprachen-und-systemen/).
