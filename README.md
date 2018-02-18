# Speed comparison of programming languages

This projects tries to compare the speed of different programming languages.
In this project we don't really care about getting a precise calculation of pi. We only want to see how fast are the programming languages doing.

It uses an implementation of the [Leibniz formula for π](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) to do the comparison.

Here is a video which explains how it works: [Calculating π by hand](https://www.youtube.com/watch?v=HrRMnzANHHs)

## Languages used in this comparison

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
To measure the execution time the command [`time`](https://en.wikipedia.org/wiki/Time_(Unix)) is used.

### Requirements

- `Docker`
- `Makefile` support

### Execute it

Just run: `make`

## Results

```text
======= Comparison =======
Rounds used: 1000000


> Python 3
Version: Python 3.6.3
Time: 0m0.340s
Result: π = 3.1415936535887745

> Rust
Version: rustc 1.22.1
Time: 0m0.131s
Result: π = 3.1415936535887745

> JS (node.js)
Version: v8.9.3
Time: 0m0.094s
Result: π = 3.1415936535887745

> PHP
Version: PHP 7.1.14 (cli) (built: Feb  8 2018 15:10:11) ( NTS )
Copyright (c) 1997-2018 The PHP Group
Zend Engine v3.1.0, Copyright (c) 1998-2018 Zend Technologies
Time: 0m0.060s
Result: π = 3.1415936535887745

> Go
Version: go version go1.9.2 linux/amd64
Time: 0m0.005s
Result: π = 3.1415936535887745
```

## Thanks

This projects tries to recreate the results which [Thomas](https://www.thomaschristlieb.de) did [on his blog](https://www.thomaschristlieb.de/performance-vergleich-zwischen-verschiedenen-programmiersprachen-und-systemen/).
It also tries to include languages which Thomas didn't cover in his post.
