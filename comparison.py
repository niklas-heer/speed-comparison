#!/usr/bin/env python3
# coding=utf-8
import os
import math
import delegator
import logging
from lauda import stopwatchcm


def stopwatch_sum_cb(watch):
    # Convert the measurement into milliseconds
    time = int(watch.elapsed_time * 1000)
    print("Time: {0}ms".format(time))

class Measure:
    """Initializes the Measurement

        Parameters
        ----------
            name : str
                The name of the languages. Will be used as the headline.
            version_cmd : str
                The command as a string to get the version of the language/compiler.
            run_cmd : str
                The command as a string to run the program.
            compile_cmd : str, optional
                The command to compile the program first. Will be run before the `run_cmd`.
    """

    def __init__(self, name, version_cmd, run_cmd, compile_cmd=None, debug=False):
        self.name = name
        self.run_cmd = run_cmd
        self.version_cmd = version_cmd
        self.compile_cmd = compile_cmd
        self.debug = debug

    def run(self):
        print("> {0}".format(self.name))

        if self.compile_cmd:
            cmd_compile = delegator.run(self.compile_cmd)
            if self.debug or cmd_compile.return_code is not 0:
                logging.info(cmd_compile.out)

        print("Version: {0}".format(delegator.run(self.version_cmd).out.splitlines()[0]))

        with stopwatchcm(callback=stopwatch_sum_cb):
            cmd_run = delegator.run(self.run_cmd)

        print("Result: π = {0}\n".format(cmd_run.out.rstrip()))

def accuracy(expected, actual):
    if expected < actual:
        return (actual - expected) / expected
    else:
        return (expected - actual) / expected

def measure_accuracy():
    # print("{:.16%}".format(accuracy(math.pi, pi_result)))
    # -0.0000318309565063%
    # -0.0000318309561953%
    pass

def measurement():
    # Change the working directory to the `src` folder
    cwd = os.getcwd()
    os.chdir(os.path.join(cwd, "src"))

    with open("rounds.txt") as file:
        rounds = file.read()

    print("\n======= Comparison =======")
    print("Rounds used: {0}\n".format(rounds))

    Measure("Julia", "julia --version", "julia leibniz.jl").run()
    Measure("Python 3 (CPython)", "python3 --version", "python3 leibniz.py").run()
    Measure("Ruby", "ruby --version", "ruby leibniz.rb").run()
    Measure("Rust", "rustc --version", "./leibniz", "export RUST_BACKTRACE=1; rustc leibniz.rs").run()
    Measure("JS (node.js)", "node --version", "node leibniz.js").run()
    Measure("Lua", "lua5.3 -v", "lua5.3 leibniz.lua").run()
    Measure("PHP", "php7 --version", "php7 leibniz.php").run()
    Measure("Nim", "echo '0.17.2'", "./leibniz", "nim c --verbosity:0 leibniz.nim").run()  # printing nim --version is not possible
    Measure("C++", "g++ --version", "./leibniz", "g++ leibniz.cpp -o leibniz").run()
    Measure("Crystal", "crystal --version", "./leibniz", "crystal build leibniz.cr").run()
    Measure("Go", "go version", "./leibniz", "go build leibniz.go").run()
    Measure("C", "gcc --version", "./leibniz", "gcc leibniz.c -o leibniz").run()

measurement()
