[![CI](https://github.com/niklas-heer/speed-comparison/actions/workflows/ci.yml/badge.svg)](https://github.com/niklas-heer/speed-comparison/actions/workflows/ci.yml)

![plot](https://niklas-heer.github.io/speed-comparison/history/2023-02-05T185235/combined_results.png "Speed comparison of programming languages")

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

<!-- TODO: Create a new video for hyperfine and scmeta -->
<!-- ## Adding new languages

[<img src="https://github.com/niklas-heer/speed-comparison/raw/master/assets/how-to-contribute_thumbnail.png" width="50%">](https://www.youtube.com/watch?v=ksV4WObYSiQ "Contributing to speed comparison ") -->

## Run it yourself

Everything is run by a Docker container and a bash script which envokes the programs.

To measure the execution time a [python package](https://pypi.python.org/pypi/lauda/1.2.0) is used.

### Requirements
- `Docker`
- [earthly](https://earthly.dev/)

### Run everything
Earthly allows to run everything with a single command:
```bash
earthly +all
```
This will run all tasks to collect all measurements and then run the analysis.

### Collect data
To collect data for all languages run:
```bash
earthly +collect-data
```

To collect data for a single language run:
```bash
earthly +rust    # or any other language target
```

### Available language targets
Language targets are auto-discovered from the Earthfile. You can list them with:
```bash
./scripts/discover-languages.sh
```

### Analyse results
To generate the combined CSV and chart from all results:
```bash
earthly +analysis
```

### Fast check (subset)
For quick testing, run only a subset of fast languages:
```bash
earthly +fast-check   # runs: c, go, rust, cpython
```

## CI/CD

The project uses GitHub Actions with a **parallel matrix build**:

1. **Auto-discovery**: Language targets are automatically detected from the Earthfile
2. **Parallel execution**: All 43+ languages run simultaneously in separate jobs
3. **Isolation**: Each language gets a fresh runner environment
4. **Results collection**: All results are merged and analyzed together
5. **Auto-publish**: Results are published to [GitHub Pages](https://niklas-heer.github.io/speed-comparison/)

### PR Commands

Repository maintainers can trigger benchmarks on PRs using comments:

```
/bench rust go c     # Run specific languages
```

### Labels

- `enable-ci`: Trigger full benchmark suite on a PR
- `skip-ci`: Skip the fast-check on a PR

## FAQ

> Why do you also count reading a file and printing the output?

Because I think this is a more realistic scenario to compare speeds.

> Are the compile times included in the measurements?

No they are not included, because when running the program in the real world this would also be done before.

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
        <a href=https://github.com/francescoalemanno>
            <img src=https://avatars.githubusercontent.com/u/50984334?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Francesco Alemanno/>
            <br />
            <sub style="font-size:14px"><b>Francesco Alemanno</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/viscropst>
            <img src=https://avatars.githubusercontent.com/u/16207250?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Yu Zhu/>
            <br />
            <sub style="font-size:14px"><b>Yu Zhu</b></sub>
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
        <a href=https://github.com/gauteh>
            <img src=https://avatars.githubusercontent.com/u/56827?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Gaute Hope/>
            <br />
            <sub style="font-size:14px"><b>Gaute Hope</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/frak0d>
            <img src=https://avatars.githubusercontent.com/u/68330937?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=frak0d/>
            <br />
            <sub style="font-size:14px"><b>frak0d</b></sub>
        </a>
    </td>
</tr>
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/jonocarroll>
            <img src=https://avatars.githubusercontent.com/u/9496865?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Jonathan Carroll/>
            <br />
            <sub style="font-size:14px"><b>Jonathan Carroll</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/Akaame>
            <img src=https://avatars.githubusercontent.com/u/11539091?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Sıddık AÇIL/>
            <br />
            <sub style="font-size:14px"><b>Sıddık AÇIL</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/cyrusmsk>
            <img src=https://avatars.githubusercontent.com/u/1385803?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Serg Gini/>
            <br />
            <sub style="font-size:14px"><b>Serg Gini</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/gbaraldi>
            <img src=https://avatars.githubusercontent.com/u/28694980?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Gabriel Baraldi/>
            <br />
            <sub style="font-size:14px"><b>Gabriel Baraldi</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/Demonstrandum>
            <img src=https://avatars.githubusercontent.com/u/26842759?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Samuel/>
            <br />
            <sub style="font-size:14px"><b>Samuel</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/joelandman>
            <img src=https://avatars.githubusercontent.com/u/2421934?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Joe Landman/>
            <br />
            <sub style="font-size:14px"><b>Joe Landman</b></sub>
        </a>
    </td>
</tr>
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/eternalfrustation>
            <img src=https://avatars.githubusercontent.com/u/64073903?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Sandeep Kumar/>
            <br />
            <sub style="font-size:14px"><b>Sandeep Kumar</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/mattn>
            <img src=https://avatars.githubusercontent.com/u/10111?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=mattn/>
            <br />
            <sub style="font-size:14px"><b>mattn</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/npeters>
            <img src=https://avatars.githubusercontent.com/u/935249?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Nicolas Peters/>
            <br />
            <sub style="font-size:14px"><b>Nicolas Peters</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/yawaramin>
            <img src=https://avatars.githubusercontent.com/u/6997?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Yawar Amin/>
            <br />
            <sub style="font-size:14px"><b>Yawar Amin</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/PallHaraldsson>
            <img src=https://avatars.githubusercontent.com/u/8005416?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Páll Haraldsson/>
            <br />
            <sub style="font-size:14px"><b>Páll Haraldsson</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/oscardssmith>
            <img src=https://avatars.githubusercontent.com/u/11729272?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Oscar Smith/>
            <br />
            <sub style="font-size:14px"><b>Oscar Smith</b></sub>
        </a>
    </td>
</tr>
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/mcabbott>
            <img src=https://avatars.githubusercontent.com/u/32575566?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Michael Abbott/>
            <br />
            <sub style="font-size:14px"><b>Michael Abbott</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/lolgab>
            <img src=https://avatars.githubusercontent.com/u/5793054?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Lorenzo Gabriele/>
            <br />
            <sub style="font-size:14px"><b>Lorenzo Gabriele</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/LazyKernel>
            <img src=https://avatars.githubusercontent.com/u/10184101?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=LazyKernel/>
            <br />
            <sub style="font-size:14px"><b>LazyKernel</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/cnuernber>
            <img src=https://avatars.githubusercontent.com/u/40426?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Chris Nuernberger/>
            <br />
            <sub style="font-size:14px"><b>Chris Nuernberger</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/bkaestner>
            <img src=https://avatars.githubusercontent.com/u/6301773?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Benjamin Kästner/>
            <br />
            <sub style="font-size:14px"><b>Benjamin Kästner</b></sub>
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
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/notcancername>
            <img src=https://avatars.githubusercontent.com/u/119271574?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=cancername/>
            <br />
            <sub style="font-size:14px"><b>cancername</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/motoshira>
            <img src=https://avatars.githubusercontent.com/u/51407064?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Kohei Hosoki/>
            <br />
            <sub style="font-size:14px"><b>Kohei Hosoki</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/mosmeh>
            <img src=https://avatars.githubusercontent.com/u/1721932?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Yuta Imazu/>
            <br />
            <sub style="font-size:14px"><b>Yuta Imazu</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/pestatije>
            <img src=https://avatars.githubusercontent.com/u/16606369?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=pestatije/>
            <br />
            <sub style="font-size:14px"><b>pestatije</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/genmeblog>
            <img src=https://avatars.githubusercontent.com/u/38646601?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=genmeblog/>
            <br />
            <sub style="font-size:14px"><b>genmeblog</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/demotomohiro>
            <img src=https://avatars.githubusercontent.com/u/1882512?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Tomohiro/>
            <br />
            <sub style="font-size:14px"><b>Tomohiro</b></sub>
        </a>
    </td>
</tr>
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/aetelani>
            <img src=https://avatars.githubusercontent.com/u/4200661?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=aetelani/>
            <br />
            <sub style="font-size:14px"><b>aetelani</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/tacaswell>
            <img src=https://avatars.githubusercontent.com/u/199813?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Thomas A Caswell/>
            <br />
            <sub style="font-size:14px"><b>Thomas A Caswell</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/serioga>
            <img src=https://avatars.githubusercontent.com/u/527113?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Sergey Trofimov/>
            <br />
            <sub style="font-size:14px"><b>Sergey Trofimov</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/proudust>
            <img src=https://avatars.githubusercontent.com/u/20186429?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Proudust/>
            <br />
            <sub style="font-size:14px"><b>Proudust</b></sub>
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
        <a href=https://github.com/manuel-sommer>
            <img src=https://avatars.githubusercontent.com/u/47991713?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=manuelsommer/>
            <br />
            <sub style="font-size:14px"><b>manuelsommer</b></sub>
        </a>
    </td>
</tr>
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/MacAndor>
            <img src=https://avatars.githubusercontent.com/u/10120551?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=MacAndor/>
            <br />
            <sub style="font-size:14px"><b>MacAndor</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/guizhenwei>
            <img src=https://avatars.githubusercontent.com/u/12146658?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Gui Zhen Wei/>
            <br />
            <sub style="font-size:14px"><b>Gui Zhen Wei</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/DirkieDurky>
            <img src=https://avatars.githubusercontent.com/u/72947540?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Dirk Freijters/>
            <br />
            <sub style="font-size:14px"><b>Dirk Freijters</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/dennisblokland>
            <img src=https://avatars.githubusercontent.com/u/16186605?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Dennis Blokland/>
            <br />
            <sub style="font-size:14px"><b>Dennis Blokland</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/elihwyma>
            <img src=https://avatars.githubusercontent.com/u/26681721?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Amelia/>
            <br />
            <sub style="font-size:14px"><b>Amelia</b></sub>
        </a>
    </td>
</tr>
</table>


### Special thanks

#### sharkdp

For creating [hyperfine](https://github.com/sharkdp/hyperfine) which is used for the fundamental benchmarking.

#### Thomas

This projects takes inspiration from [Thomas](https://www.thomaschristlieb.de) who did a similar comparison [on his blog](https://www.thomaschristlieb.de/performance-vergleich-zwischen-verschiedenen-programmiersprachen-und-systemen/).
