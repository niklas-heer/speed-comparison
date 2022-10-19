![plot](https://niklas-heer.github.io/speed-comparison/assets/latest/combined_results.png "Speed comparison of programming languages")

---

# Speed comparison of programming languages

This projects tries to compare the speed of different programming languages.
In this project we don't really care about getting a precise calculation of pi. We only want to see how fast are the programming languages doing. <br />
It uses an implementation of the [Leibniz formula for π](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) to do the comparison. <br />
Here is a video which explains how it works: [Calculating π by hand](https://www.youtube.com/watch?v=HrRMnzANHHs)

You can find the results here: https://niklas-heer.github.io/speed-comparison/

## Disclaimer

I'm no expert in all these languages, so take my results with a grain of salt.<br>
Also the findings just show how good the languages can handle floating point operations, which is only one aspect of a programming language.

You are also welcome to contribute and help me fix my possible horrible code in some languages. :smile:

## Adding new languages

[<img src="https://github.com/niklas-heer/speed-comparison/raw/master/assets/how-to-contribute_thumbnail.png" width="50%">](https://www.youtube.com/watch?v=ksV4WObYSiQ "Contributing to speed comparison ")

## Run it yourself

Everything is run by a Docker container and a bash script which envokes the programs.

To measure the execution time a [python package](https://pypi.python.org/pypi/lauda/1.2.0) is used.

### Requirements
- `Docker`
- [earthly](https://earthly.dev/)

### Run everything
Earthly allows to run everything with a single command:
```bash
earthly --config earthly-config.yml +all
```
This will run all tasks to collect all measurements and then run the analysis.

### Collect data
To collect data for all languages run:
```bash
earthly --config earthly-config.yml +collect-data
```

To collect data for a single languages run:
```bash
earthly --config earthly-config.yml +<replace me with language name>
```

### Analyse results
To generate the combined CSV out of all results use this command:
```bash
earthly --config earthly-config.yml +analysis
```

## FAQ

> Why do you also count reading a file and printing the output?

Because I think this is a more realistic scenario to compare speeds.

> Are the compile times included in the measurements?

No they are not included, because when running the program in the real world this would also be done before.

## TODO
<!-- TODO: move to github tasks instead -->
- [x] Add C++
- [x] Add run pypy for Python
- [x] Produce graphic with results
- [x] Add Java
- [x] Add Swift
- [x] Add R
- [x] Add Elixir
- [ ] Add Smalltalk
- [x] Add C#
- [x] Add Perl
- [ ] Add Kotlin
- [ ] Add Haskell
- [ ] Add Lisp
- [ ] Add Ada
- [ ] Add COBOL

## Thanks
The list of Contributors is automatically generated.

### Contributors

<table>
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/niklas-heer>
            <img src=https://avatars.githubusercontent.com/u/1914063?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Niklas Heer/>
            <br />
            <sub style="font-size:14px"><b>Niklas Heer</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/Moelf>
            <img src=https://avatars.githubusercontent.com/u/5306213?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Jerry Ling/>
            <br />
            <sub style="font-size:14px"><b>Jerry Ling</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/0xB00B>
            <img src=https://avatars.githubusercontent.com/u/68330937?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=0xB00B/>
            <br />
            <sub style="font-size:14px"><b>0xB00B</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/francescoalemanno>
            <img src=https://avatars.githubusercontent.com/u/50984334?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Francesco Alemanno/>
            <br />
            <sub style="font-size:14px"><b>Francesco Alemanno</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/jonocarroll>
            <img src=https://avatars.githubusercontent.com/u/9496865?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Jonathan Carroll/>
            <br />
            <sub style="font-size:14px"><b>Jonathan Carroll</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/gauteh>
            <img src=https://avatars.githubusercontent.com/u/56827?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Gaute Hope/>
            <br />
            <sub style="font-size:14px"><b>Gaute Hope</b></sub>
        </a>
    </td>
</tr>
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/gbaraldi>
            <img src=https://avatars.githubusercontent.com/u/28694980?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Gabriel Baraldi/>
            <br />
            <sub style="font-size:14px"><b>Gabriel Baraldi</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/mcabbott>
            <img src=https://avatars.githubusercontent.com/u/32575566?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Michael Abbott/>
            <br />
            <sub style="font-size:14px"><b>Michael Abbott</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/oscardssmith>
            <img src=https://avatars.githubusercontent.com/u/11729272?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Oscar Smith/>
            <br />
            <sub style="font-size:14px"><b>Oscar Smith</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/DirkieDurky>
            <img src=https://avatars.githubusercontent.com/u/72947540?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=DirkieDurky/>
            <br />
            <sub style="font-size:14px"><b>DirkieDurky</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/PatrickTheDev>
            <img src=https://avatars.githubusercontent.com/u/69093169?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=RDPatrickTheDev/>
            <br />
            <sub style="font-size:14px"><b>RDPatrickTheDev</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/serioga>
            <img src=https://avatars.githubusercontent.com/u/527113?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Sergey Trofimov/>
            <br />
            <sub style="font-size:14px"><b>Sergey Trofimov</b></sub>
        </a>
    </td>
</tr>
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/tacaswell>
            <img src=https://avatars.githubusercontent.com/u/199813?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Thomas A Caswell/>
            <br />
            <sub style="font-size:14px"><b>Thomas A Caswell</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/yinheli>
            <img src=https://avatars.githubusercontent.com/u/235094?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=yinheli/>
            <br />
            <sub style="font-size:14px"><b>yinheli</b></sub>
        </a>
    </td>
</tr>
</table>


### Special thanks

#### sharkdp

For creating [hyperfine](https://github.com/sharkdp/hyperfine) which is used for the fundamental benchmarking.

#### Thomas

This projects takes inspiration from [Thomas](https://www.thomaschristlieb.de) who did a similar comparison [on his blog](https://www.thomaschristlieb.de/performance-vergleich-zwischen-verschiedenen-programmiersprachen-und-systemen/).

## Languages used in this comparison

- [C](https://en.wikipedia.org/wiki/C_(programming_language))
- [Fortran](https://en.wikipedia.org/wiki/Fortran)
- [Clojure](https://en.wikipedia.org/wiki/Clojure)
- [C++](https://en.wikipedia.org/wiki/C%2B%2B)
- [Crystal](https://en.wikipedia.org/wiki/Crystal_(programming_language))
- [Elixir](https://en.wikipedia.org/wiki/Elixir_(programming_language))
- [C#](https://de.wikipedia.org/wiki/C-Sharp)
- [Go](https://en.wikipedia.org/wiki/Go_(programming_language))
- [Java](https://en.wikipedia.org/wiki/Java_(programming_language))
- [Julia](https://en.wikipedia.org/wiki/Julia_(programming_language))
- [Javascript](https://en.wikipedia.org/wiki/JavaScript) using [Node.js](https://en.wikipedia.org/wiki/Node.js)
- [Lua](https://en.wikipedia.org/wiki/Lua_(programming_language))
- [Nim](https://en.wikipedia.org/wiki/Nim_(programming_language))
- [PHP](https://en.wikipedia.org/wiki/PHP)
- [Python](https://en.wikipedia.org/wiki/Python_(programming_language))
- [R](https://en.wikipedia.org/wiki/R_(programming_language))
- [Ruby](https://en.wikipedia.org/wiki/Ruby_(programming_language))
- [Rust](https://en.wikipedia.org/wiki/Rust_(programming_language))
- [Swift](https://en.wikipedia.org/wiki/Swift_(programming_language))
