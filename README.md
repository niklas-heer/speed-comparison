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
            <img src=https://avatars.githubusercontent.com/u/68330937?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=frakod/>
            <br />
            <sub style="font-size:14px"><b>frakod</b></sub>
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
        <a href=https://github.com/Demonstrandum>
            <img src=https://avatars.githubusercontent.com/u/26842759?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=S Knutsen/>
            <br />
            <sub style="font-size:14px"><b>S Knutsen</b></sub>
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
        <a href=https://github.com/cyrusmsk>
            <img src=https://avatars.githubusercontent.com/u/1385803?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Serg Gini/>
            <br />
            <sub style="font-size:14px"><b>Serg Gini</b></sub>
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
        <a href=https://github.com/cnuernber>
            <img src=https://avatars.githubusercontent.com/u/40426?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Chris Nuernberger/>
            <br />
            <sub style="font-size:14px"><b>Chris Nuernberger</b></sub>
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
</tr>
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/PallHaraldsson>
            <img src=https://avatars.githubusercontent.com/u/8005416?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Páll Haraldsson/>
            <br />
            <sub style="font-size:14px"><b>Páll Haraldsson</b></sub>
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
        <a href=https://github.com/guizhenwei>
            <img src=https://avatars.githubusercontent.com/u/12146658?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Gui Zhen Wei/>
            <br />
            <sub style="font-size:14px"><b>Gui Zhen Wei</b></sub>
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
        <a href=https://github.com/PatrickTheDev>
            <img src=https://avatars.githubusercontent.com/u/69093169?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=RDPatrickTheDev/>
            <br />
            <sub style="font-size:14px"><b>RDPatrickTheDev</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/proudust>
            <img src=https://avatars.githubusercontent.com/u/20186429?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Proudust/>
            <br />
            <sub style="font-size:14px"><b>Proudust</b></sub>
        </a>
    </td>
</tr>
<tr>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/serioga>
            <img src=https://avatars.githubusercontent.com/u/527113?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Sergey Trofimov/>
            <br />
            <sub style="font-size:14px"><b>Sergey Trofimov</b></sub>
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
        <a href=https://github.com/demotomohiro>
            <img src=https://avatars.githubusercontent.com/u/1882512?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Tomohiro/>
            <br />
            <sub style="font-size:14px"><b>Tomohiro</b></sub>
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
        <a href=https://github.com/mosmeh>
            <img src=https://avatars.githubusercontent.com/u/1721932?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Yuta Imazu/>
            <br />
            <sub style="font-size:14px"><b>Yuta Imazu</b></sub>
        </a>
    </td>
    <td align="center" style="word-wrap: break-word; width: 150.0; height: 150.0">
        <a href=https://github.com/motoshira>
            <img src=https://avatars.githubusercontent.com/u/51407064?v=4 width="100;"  style="border-radius:50%;align-items:center;justify-content:center;overflow:hidden;padding-top:10px" alt=Kohei Hosoki/>
            <br />
            <sub style="font-size:14px"><b>Kohei Hosoki</b></sub>
        </a>
    </td>
</tr>
<tr>
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
