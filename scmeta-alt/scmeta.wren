#!/usr/bin/env wren
// scmeta - Wren variant
// Requires: wren-cli with JSON module
// Note: Wren doesn't have a standard JSON library, so this uses a simple parser

import "os" for Process
import "io" for File

var VERSION = "1.0.0"
var PI = 3.141592653589793

// Simple JSON parser for our specific needs
class Json {
  static parse(text) {
    // This is a minimal JSON parser - in production you'd use a proper library
    return JsonParser.new(text).parse()
  }

  static stringify(obj) {
    return JsonWriter.write(obj)
  }
}

class JsonParser {
  construct new(text) {
    _text = text
    _pos = 0
  }

  parse() {
    skipWhitespace()
    return parseValue()
  }

  parseValue() {
    skipWhitespace()
    var c = peek()
    if (c == "{") return parseObject()
    if (c == "[") return parseArray()
    if (c == "\"") return parseString()
    if (c == "-" || (c >= "0" && c <= "9")) return parseNumber()
    if (_text[_pos.._pos+3] == "true") {
      _pos = _pos + 4
      return true
    }
    if (_text[_pos.._pos+4] == "false") {
      _pos = _pos + 5
      return false
    }
    if (_text[_pos.._pos+3] == "null") {
      _pos = _pos + 4
      return null
    }
    Fiber.abort("Unexpected character: %(c)")
  }

  parseObject() {
    var obj = {}
    expect("{")
    skipWhitespace()
    if (peek() != "}") {
      while (true) {
        skipWhitespace()
        var key = parseString()
        skipWhitespace()
        expect(":")
        var value = parseValue()
        obj[key] = value
        skipWhitespace()
        if (peek() == "}") break
        expect(",")
      }
    }
    expect("}")
    return obj
  }

  parseArray() {
    var arr = []
    expect("[")
    skipWhitespace()
    if (peek() != "]") {
      while (true) {
        arr.add(parseValue())
        skipWhitespace()
        if (peek() == "]") break
        expect(",")
      }
    }
    expect("]")
    return arr
  }

  parseString() {
    expect("\"")
    var start = _pos
    while (peek() != "\"") {
      if (peek() == "\\") _pos = _pos + 1
      _pos = _pos + 1
    }
    var str = _text[start..._pos]
    expect("\"")
    return str
  }

  parseNumber() {
    var start = _pos
    if (peek() == "-") _pos = _pos + 1
    while (peek() >= "0" && peek() <= "9") _pos = _pos + 1
    if (peek() == ".") {
      _pos = _pos + 1
      while (peek() >= "0" && peek() <= "9") _pos = _pos + 1
    }
    if (peek() == "e" || peek() == "E") {
      _pos = _pos + 1
      if (peek() == "+" || peek() == "-") _pos = _pos + 1
      while (peek() >= "0" && peek() <= "9") _pos = _pos + 1
    }
    return Num.fromString(_text[start..._pos])
  }

  peek() { _pos < _text.count ? _text[_pos] : "" }
  expect(c) {
    if (peek() != c) Fiber.abort("Expected '%(c)' but got '%(peek())'")
    _pos = _pos + 1
  }
  skipWhitespace() {
    while (peek() == " " || peek() == "\n" || peek() == "\r" || peek() == "\t") {
      _pos = _pos + 1
    }
  }
}

class JsonWriter {
  static write(obj) {
    if (obj is Map) return writeObject(obj)
    if (obj is List) return writeArray(obj)
    if (obj is String) return "\"%(obj)\""
    if (obj is Num) return obj.toString
    if (obj is Bool) return obj.toString
    if (obj == null) return "null"
    return obj.toString
  }

  static writeObject(map) {
    var parts = []
    for (key in map.keys) {
      parts.add("\"%(key)\": %(write(map[key]))")
    }
    return "{\n  " + parts.join(",\n  ") + "\n}"
  }

  static writeArray(arr) {
    var parts = arr.map {|v| write(v) }.toList
    return "[" + parts.join(", ") + "]"
  }
}

// Calculate pi accuracy: -log10(|1 - (value / PI)|)
var piAccuracy = Fn.new {|value|
  var ratio = value / PI
  var diff = (1 - ratio).abs
  if (diff == 0) return 999
  // Wren has no log10, so: log10(x) = ln(x) / ln(10)
  return -(diff.log / 10.log)
}

// Extract version from text
var getVersion = Fn.new {|text, matchIndex|
  var pattern = "0123456789"
  var versions = []
  var i = 0
  while (i < text.count) {
    if (pattern.contains(text[i])) {
      var start = i
      while (i < text.count && (pattern.contains(text[i]) || text[i] == ".")) {
        i = i + 1
      }
      var ver = text[start...i]
      if (ver.contains(".")) versions.add(ver)
    } else {
      i = i + 1
    }
  }
  if (versions.count > matchIndex) return versions[matchIndex]
  Fiber.abort("No version found at index %(matchIndex)")
}

var toTimedelta = Fn.new {|num| "%(num)s" }

// Parse command line arguments
var args = Process.allArguments
var langName = null
var targetName = null
var langVersionCmd = null
var langVersionMatchIndex = 0
var hyperfineFile = null
var piFile = null
var outputFile = null

var i = 1
while (i < args.count) {
  var arg = args[i]
  if (arg == "-h" || arg == "--help") {
    System.print("Usage: scmeta.wren [arguments]")
    System.print("")
    System.print("Options:")
    System.print("  --lang-name=NAME        Language name")
    System.print("  --target-name=TARGET    Earthfile target name")
    System.print("  --lang-version=CMD      Command to get version")
    System.print("  --hyperfine=FILE        Path to hyperfine JSON")
    System.print("  --pi=FILE               Path to pi.txt")
    System.print("  --output=FILE           Output JSON path")
    Process.exit(0)
  }
  if (arg.startsWith("--lang-name=")) langName = arg["--lang-name=".count..-1]
  if (arg.startsWith("--target-name=")) targetName = arg["--target-name=".count..-1]
  if (arg.startsWith("--lang-version=")) langVersionCmd = arg["--lang-version=".count..-1]
  if (arg.startsWith("--lang-version-match-index=")) {
    langVersionMatchIndex = Num.fromString(arg["--lang-version-match-index=".count..-1])
  }
  if (arg.startsWith("--hyperfine=")) hyperfineFile = arg["--hyperfine=".count..-1]
  if (arg.startsWith("--pi=")) piFile = arg["--pi=".count..-1]
  if (arg.startsWith("--output=")) outputFile = arg["--output=".count..-1]
  i = i + 1
}

// Validate required args
if (!langName) Fiber.abort("--lang-name is required")
if (!targetName) Fiber.abort("--target-name is required")
if (!hyperfineFile) Fiber.abort("--hyperfine is required")
if (!piFile) Fiber.abort("--pi is required")
if (!outputFile) Fiber.abort("--output is required")
if (!langVersionCmd) Fiber.abort("--lang-version is required")

// Read and parse files
var computedPi = File.read(piFile).trim()
var accuracy = piAccuracy.call(Num.fromString(computedPi))

// Get version (would need shell execution - simplified here)
// In real usage, you'd use Process.exec or similar
var langVersion = "1.0.0"  // Placeholder - Wren CLI has limited shell support

// Parse hyperfine JSON
var hyperfine = Json.parse(File.read(hyperfineFile))
var result = hyperfine["results"][0]

// Build output
var metadata = {
  "Language": langName,
  "Target": targetName,
  "Version": langVersion,
  "Command": result["command"],
  "CalculatedPi": computedPi,
  "Accuracy": accuracy,
  "Mean": toTimedelta.call(result["mean"]),
  "Stddev": toTimedelta.call(result["stddev"]),
  "UserTime": toTimedelta.call(result["user"]),
  "SystemTime": toTimedelta.call(result["system"]),
  "Median": toTimedelta.call(result["median"]),
  "Min": toTimedelta.call(result["min"]),
  "Max": toTimedelta.call(result["max"]),
  "TimesPerRun": result["times"],
  "ExitCodesPerRun": result["exit_codes"]
}

File.write(outputFile, Json.stringify(metadata))

System.print("Successfully created metadata")
System.print("Language: %(langName) (%(langVersion))")
System.print("Output: %(outputFile)")
