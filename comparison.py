#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""This module measures the performance of various programming languages."""

import os
import math
import csv
import logging
import statistics
import delegator
from lauda import StopWatch
from termcolor import colored


class Measure:
    """Initializes the Measurement

        Arguments:
            name : str  -- Name of the languages.
            version_cmd : str -- Command to get the version.
            run_cmd : str -- Command to run the program.
            compile_cmd : str, optional -- Command to compile.
    """

    def __init__(self, name, version_cmd, run_cmd, compile_cmd=None, debug=False):
        self.name = name
        self.run_cmd = run_cmd
        self.version_cmd = version_cmd
        self.compile_cmd = compile_cmd
        self.debug = debug
        self.results = {"name": self.name}

    def run(self):
        """Run a measurement.

        Returns:
            list -- List of results.
        """

        print(f"> {self.name}")

        if self.compile_cmd:
            cmd_compile = delegator.run(self.compile_cmd)
            if self.debug or cmd_compile.return_code is not 0:
                logging.info(cmd_compile.out)

        print(f"Version: {delegator.run(self.version_cmd).out.splitlines()[0]}")

        times = []
        count = 0
        while count < 10:
            count += 1
            # Measure execution time
            watch = StopWatch()
            watch.start()
            cmd_run = delegator.run(self.run_cmd)
            watch.stop()

            # Convert the measurement into milliseconds
            times.append(int(watch.elapsed_time * 1000))

        print(f"Speed (all): {'ms, '.join(map(str, times))}ms")
        print(f"Speed (best): {min(times)}ms")
        print(f"Speed (worst): {max(times)}ms")
        print(f"Speed (median): {statistics.median(times)}ms")

        # Strip output from new line
        result = cmd_run.out.strip()
        print(f"Result: {result}")

        # Calculate accuracy
        accuracy = self.diff_letters("{:.16f}".format(math.pi), result)
        print("Accuracy: {:.2%}".format(accuracy))

        print()  # new line

        self.results["median"] = statistics.median(times)
        self.results["best"] = min(times)
        self.results["worst"] = max(times)
        self.results["accuracy"] = f"{accuracy*100:.4}"

        return self.results

    def diff_letters(self, first, second):
        """Measure differences between result and actual π.

        Arguments:
            first: str -- First number for comparison.
            second : str -- Second number for comparison.
        """

        if self.debug:
            # Print also a visual diff between the numbers
            combined = zip(first, second)
            diff = []
            for i, j in combined:
                if i == j:
                    print(i, "--", j)
                else:
                    diff.append(j)
                    print(i, "  ", j)
            return(len(first) - len(diff)) / len(first)

        # Standard diff algorithm
        return (len(first) - sum(first[i] != second[i] for i in range(len(first)))) / len(first)


def ask_yes_no(question, default="yes"):
    """Helper function to ask a yes/no question.

    Arguments:
        question : str -- Question which will be displayed.
        default : str, optional -- Set the default answer to the question.
    Returns:
        True if the answer is yes, else it returns False.
    """
    valid = {"yes": True, "y": True,
             "no": False, "n": False}
    if default is None:
        prompt = " [y/n] "
    elif default == "yes":
        prompt = " [Y/n] "
    elif default == "no":
        prompt = " [y/N] "
    else:
        raise ValueError("invalid default answer: '%s'" % default)

    while True:
        choice = input(f"\n{colored('❓', 'red')}  {question}{prompt}").lower()
        if default is not None and choice == '':
            return valid[default]
        elif choice in valid:
            return valid[choice]
        else:
            print("Please respond with 'yes' or 'no'.\n")

def measurement():
    """Management function for the measurement.
    """

    # Change the working directory to the `src` folder
    cwd = os.getcwd()
    os.chdir(os.path.join(cwd, "src"))

    with open("rounds.txt") as file:
        iterations = file.read().strip()

    print("\n======= Comparison =======")
    print(f"Iterations: {iterations}\n")

    languages = [
        # language name / version command / execute command / compile command
        ["Julia", "julia --version", "julia leibniz.jl"],
        ["Python (CPython)", "python --version", "python leibniz.py"],
        ["Ruby", "ruby --version", "ruby leibniz.rb"],
        ["Java", "pacman -Qi jdk8-openjdk | grep 'Version' | cut -d: -f2- | cut -d ' ' -f2", "java leibniz", "javac leibniz.java"],
        ["Lua", "lua -v", "lua leibniz.lua"],
        ["Rust", "rustc --version", "./leibniz", "export RUST_BACKTRACE=1; rustc leibniz.rs"],
        ["JS (node)", "node --version", "node leibniz.js"],
        ["PHP", "php --version", "php leibniz.php"],
        ["Python (pypy)", "pacman -Qi pypy | grep 'Version' | cut -d: -f2- | cut -d ' ' -f2", "pypy leibniz.py"],
        ["Nim", "pacman -Qi nim | grep 'Version' | cut -d: -f2 | cut -d ' ' -f2", "./leibniz", "nim c --verbosity:0 leibniz.nim"],
        ["C++", "g++ --version", "./leibniz", "g++ leibniz.cpp -o leibniz"],
        ["Crystal", "crystal --version", "./leibniz", "crystal build leibniz.cr"],
        ["C", "gcc --version", "./leibniz", "gcc leibniz.c -o leibniz"],
        ["Go", "go version", "./leibniz", "go build leibniz.go"],
    ]

    complete_results = []

    for language in languages:
        complete_results.append(Measure(*language).run())

    if ask_yes_no("Do you want to save the results?"):
        # Write csv file with results
        os.chdir(os.path.join(cwd, "results"))

        keys = complete_results[0].keys()

        with open(f"data.csv", "w", newline="", encoding="utf8") as file:
            dict_writer = csv.DictWriter(file, keys)
            dict_writer.writeheader()
            dict_writer.writerows(complete_results)

        print("Saved the results in: {0}".format(colored(f"results/data.csv", 'red', attrs=['bold'])))

measurement()
