#!/usr/bin/env nim
## scmeta - Nim variant
## Compile: nim c -d:release --opt:size -d:strip scmeta.nim
## Result: ~18 KB static binary!

import std/[json, math, strutils, strscans, os, osproc, re, parseopt]

const
  VERSION = "1.0.0"
  PI = 3.141592653589793

proc usage() =
  echo """
Usage: scmeta [arguments]

Options:
  --lang-name=NAME        Language name
  --target-name=TARGET    Earthfile target name
  --lang-version=CMD      Command to get version
  --lang-version-match-index=N  Version match index (default 0)
  --hyperfine=FILE        Path to hyperfine JSON
  --pi=FILE               Path to pi.txt
  --output=FILE           Output JSON path
  -h, --help              Show this help
  -v, --version           Show version"""

proc piAccuracy(value: float): float =
  ## Calculate pi accuracy: -log10(|1 - (value / PI)|)
  let ratio = value / PI
  let diff = abs(1.0 - ratio)
  if diff == 0.0:
    return 999.0
  return -log10(diff)

proc getVersion(text: string, matchIndex: int = 0): string =
  ## Extract version from text at given index
  var versions: seq[string] = @[]
  for match in text.findAll(re"\d+\.\d+[\.\d]*"):
    versions.add(match)
  if versions.len > matchIndex:
    return versions[matchIndex]
  raise newException(ValueError, "No version found at index " & $matchIndex)

proc toTimedelta(num: float): string =
  return $num & "s"

proc runCmd(cmd: string): string =
  ## Run shell command and capture output
  let (output, _) = execCmdEx(cmd)
  return output

type
  Args = object
    langName: string
    targetName: string
    langVersionCmd: string
    langVersionMatchIndex: int
    hyperfineFile: string
    piFile: string
    outputFile: string

proc parseArgs(): Args =
  var args = Args(langVersionMatchIndex: 0)

  var p = initOptParser(commandLineParams())
  while true:
    p.next()
    case p.kind
    of cmdEnd: break
    of cmdShortOption, cmdLongOption:
      case p.key
      of "h", "help":
        usage()
        quit(0)
      of "v", "version":
        echo "scmeta ", VERSION
        quit(0)
      of "lang-name":
        args.langName = p.val
      of "target-name":
        args.targetName = p.val
      of "lang-version":
        args.langVersionCmd = p.val
      of "lang-version-match-index":
        args.langVersionMatchIndex = parseInt(p.val)
      of "hyperfine":
        args.hyperfineFile = p.val
      of "pi":
        args.piFile = p.val
      of "output":
        args.outputFile = p.val
    of cmdArgument:
      discard

  return args

proc main() =
  let args = parseArgs()

  # Validate required arguments
  if args.langName == "":
    stderr.writeLine("ERROR: --lang-name is required!")
    quit(1)
  if args.targetName == "":
    stderr.writeLine("ERROR: --target-name is required!")
    quit(1)
  if args.hyperfineFile == "":
    stderr.writeLine("ERROR: --hyperfine is required!")
    quit(1)
  if args.piFile == "":
    stderr.writeLine("ERROR: --pi is required!")
    quit(1)
  if args.outputFile == "":
    stderr.writeLine("ERROR: --output is required!")
    quit(1)
  if args.langVersionCmd == "":
    stderr.writeLine("ERROR: --lang-version is required!")
    quit(1)

  # Read pi value and calculate accuracy
  let computedPi = readFile(args.piFile).strip()
  if computedPi == "":
    stderr.writeLine("ERROR: Pi file is empty!")
    quit(1)
  let accuracy = piAccuracy(parseFloat(computedPi))

  # Get language version
  let versionOutput = runCmd(args.langVersionCmd)
  let langVersion = getVersion(versionOutput, args.langVersionMatchIndex)

  # Parse hyperfine JSON
  let hyperfine = parseJson(readFile(args.hyperfineFile))
  let result = hyperfine["results"][0]

  # Build output JSON
  let metadata = %* {
    "Language": args.langName,
    "Target": args.targetName,
    "Version": langVersion,
    "Command": result["command"].getStr(),
    "CalculatedPi": computedPi,
    "Accuracy": accuracy,
    "Mean": toTimedelta(result["mean"].getFloat()),
    "Stddev": toTimedelta(result["stddev"].getFloat()),
    "UserTime": toTimedelta(result["user"].getFloat()),
    "SystemTime": toTimedelta(result["system"].getFloat()),
    "Median": toTimedelta(result["median"].getFloat()),
    "Min": toTimedelta(result["min"].getFloat()),
    "Max": toTimedelta(result["max"].getFloat()),
    "TimesPerRun": result["times"],
    "ExitCodesPerRun": result["exit_codes"]
  }

  # Write output with pretty formatting
  writeFile(args.outputFile, metadata.pretty() & "\n")

  echo "Successfully created metadata"
  echo "Language: ", args.langName, " (", langVersion, ")"
  echo "Output: ", args.outputFile

when isMainModule:
  main()
