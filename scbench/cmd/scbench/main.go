package main

import (
	"fmt"
	"strings"
	"time"
	"regexp"
	"errors"
	"math"
	"strconv"

	"scbench/internal"
	"github.com/thatisuday/commando"
	execute "github.com/alexellis/go-execute/pkg/v1"
)

const (
	// NAME is the executable name.
	NAME = "speed-comparison benchmark (scbench)"
	// VERSION is the executable version.
	VERSION = "1.0.0"
)

// NO_COLOR is a global variable that is used to determine whether or not to enable color output.
var NO_COLOR bool = false

// Get version number out of string
func getVersionNumberFromText(text string) []string {
	parsed := regexp.MustCompile(`\d+(\.\d+)+`).FindAllString(text, -1)
	return parsed
}

func getVersion(command []string, selection int)  (string, error) {
	task := execute.ExecTask{
		Command: command[0],
		Args:    command[1:],
		Shell:   false,
	}
	res, err := task.Execute()
	if err != nil {
		panic(err)
	}

	version := getVersionNumberFromText(res.Stdout)[selection]
	return version, nil
}

func run(command []string) (time.Duration, string, error) {
	init := time.Now()
	task := execute.ExecTask{
		Command: command[0],
		Args:    command[1:],
		Shell:   false,
	}
	res, err := task.Execute()
	if err != nil {
		panic(err)
	}
	duration := time.Since(init)

	return duration, res.Stdout, nil
}

func hammingDistancePct(base string, other string) (float64, error) {
	// stings in Go are slices of bytes so we have first to convert them to runes
	// read more here https://blog.golang.org/strings
	rbase := []rune(base)
	rother := []rune(other)

	if len(rbase) != len(rother) {
		return 0, errors.New("strings do not have the same length")
	}

	var correct float64 = 0.0;
	var weight float64 = 0.0;
	for i := range rbase {
		if rother[i] == rbase[i] {
			correct+=1.0/float64(i+1)
		}
		weight+=1.0/float64(i+1)
	}

	return correct/weight, nil
}

// Truncate string.
func truncateString(str string, length int) string {
	if length <= 0 {
			return ""
	}

	truncated := ""
	count := 0
	for _, char := range str {
		truncated += string(char)
		count++
		if count >= length {
				break
		}
	}
	return truncated
}

func cutPi(pi float64, prc int) string {
	piStr := strconv.FormatFloat(pi, 'f', 40, 64)
	return truncateString(piStr, prc)
}

func main() {
	internal.Log("white", fmt.Sprintf("%v %v\n", NAME, VERSION))

	// * basic configuration
	commando.
		SetExecutableName(NAME).
		SetVersion(VERSION).
		SetDescription("scbench is a simple CLI tool to make the speed comparison possible. \nIt's based on https://github.com/Shravan-1908/bench.")

	// * root command
	commando.
		Register(nil).
		SetShortDescription("Benchmark a command for given number of iterations.").
		SetDescription("Benchmark a command for given number of iterations.").
		AddArgument("command...", "The command to run for benchmarking.", "").
		AddFlag("iterations,i", "The number of iterations to perform", commando.Int, 10).
		AddFlag("lang", "The name of the programming language", commando.String, "none").
		AddFlag("lang-version,l", "The command to get a version for the programming language", commando.String, "none").
		AddFlag("lang-version-select,L", "If there are mutiple versions in the output select which you want.", commando.Int, 0).
		AddFlag("export,e", "Export the benchmarking summary in a json or text format.", commando.String, "none").
		AddFlag("no-color", "Disable colored output.", commando.Bool, false).
		SetAction(func(args map[string]commando.ArgValue, flags map[string]commando.FlagValue) {

			// * getting args and flag values
			if strings.TrimSpace(args["command"].Value) == "" {
				fmt.Println("Error: not enough arguments.")
				return
			}

			command := strings.Split(args["command"].Value, ",")

			iterations, e := flags["iterations"].GetInt()
			if e != nil {
				internal.Log("red", "The number of iterations must be an integer!")
				internal.Log("white", e.Error())
				return
			}

			language, ierr := flags["lang"].GetString()
			if ierr != nil {
				internal.Log("red", "Application error: cannot parse flag values.")
			}

			versionLang, ierr := flags["lang-version"].GetString()
			if ierr != nil {
				internal.Log("red", "Application error: cannot parse flag values.")
			}

			langVersionSelection, ierr := flags["lang-version-select"].GetInt()
			if ierr != nil {
				internal.Log("red", "Application error: cannot parse flag values.")
			}

			languageVersion := versionLang

			if versionLang != "none" {
				returned_version, err := getVersion(strings.Split(versionLang, ","), langVersionSelection)
				if err != nil {
					internal.Log("red", "Application error: version command did not work.")
				}

				languageVersion = returned_version
			}

			NO_COLOR, e = (flags["color"].GetBool())
			if e != nil {
				internal.Log("red", "Application error: cannot parse flag values.")
			}
			internal.NO_COLOR = !NO_COLOR

			var sum time.Duration
			started := time.Now().Format("02-01-2006 15:04:05")


			accuracyPct := 1.0
			resultPi := "3.14"

			// * looping for given iterations
			for i := 1; i <= iterations; i++ {
				internal.Log("purple", fmt.Sprintf("***********\nRunning iteration %d\n***********", i))

				dur, calculatedPi, e := run(command)
				if e != nil {
					return
				}
				sum += dur

				// Calculate accuracy of pi
				calculatedPiTrimmed := strings.TrimSpace(calculatedPi)
				resultPi = calculatedPiTrimmed

				accuracyPct, e = hammingDistancePct(cutPi(math.Pi, len(calculatedPiTrimmed)), calculatedPiTrimmed)
				if e != nil {
					internal.Log("red", "Pi calulcation error!")
				}
			}

			ended := time.Now().Format("02-01-2006 15:04:05")

			// * intialising the template struct
			result := internal.Result{
				Started:      started,
				Ended:        ended,
				Language:     language,
				Version:      languageVersion,
				Command:      strings.Join(command, " "),
				Accuracy:     accuracyPct,
				CalculatedPi: resultPi,
				Iterations:   iterations,
				Average:      (sum / time.Duration(iterations)).String(),
			}

			result.Consolify()

			// * getting export values
			exportFormat, ierr := flags["export"].GetString()
			if ierr != nil {
				internal.Log("red", "Application error: cannot parse flag values.")
			}
			result.Export(exportFormat)

		})

	commando.Parse(nil)
}
