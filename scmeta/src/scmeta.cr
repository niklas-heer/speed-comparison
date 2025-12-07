require "json"
require "option_parser"
require "math"

# Third party dependencies. See shard.yml
require "json_on_steroids"

NAME    = "scmeta"
VERSION = "1.1.1"

# Calculates how accurate the computed pi value is compared to Math::PI
# Returns the number of correct decimal places (higher is better)
# https://github.com/niklas-heer/speed-comparison/issues/78#issuecomment-1292708468
def pi_accuracy(input : String) : Float64
  value = input.to_f?
  if value.nil?
    raise ArgumentError.new("Invalid pi value: '#{input}' is not a valid number")
  end

  if value == 0.0
    raise ArgumentError.new("Invalid pi value: cannot be zero")
  end

  accuracy = 1 - (value / Math::PI)
  if accuracy.abs == 0.0
    # Perfect match (unlikely but handle it)
    return Float64::INFINITY
  end
  -Math.log(accuracy.abs, 10)
end

# Extracts version number from text using regex
# match_index allows selecting which match to use when multiple versions are present
def get_version(text : String, match_index : Int32 = 0) : String
  matches = text.scan(/\d+(\.\d+)+/)

  if matches.empty?
    raise ArgumentError.new("No version number found in: '#{text}'")
  end

  if match_index < 0 || match_index >= matches.size
    available = matches.map_with_index { |m, i| "  #{i}: #{m[0]}" }.join("\n")
    raise ArgumentError.new(
      "Invalid match index #{match_index}. Available versions:\n#{available}"
    )
  end

  matches[match_index][0]
end

# Runs a shell command and returns exit code and output
# Uses sh -c to handle pipes and complex shell commands
def run_cmd(command : String) : Tuple(Int32, String)
  stdout = IO::Memory.new
  stderr = IO::Memory.new
  status = Process.run("sh", args: ["-c", command], output: stdout, error: stderr)
  if status.success?
    {status.exit_code, stdout.to_s}
  else
    {status.exit_code, stderr.to_s}
  end
end

# Returns a given number as seconds string
def to_timedelta(number) : String
  "#{number}s"
end

# Validates that a required option is set
def require_option(variable, name : String)
  if variable.nil?
    STDERR.puts "ERROR: #{name} is required!"
    exit(1)
  end
end

# Main entry point - only runs when executed directly (not when required for tests)
def main
  lang_name : String? = nil
  lang_version : String? = nil
  lang_version_match_index = 0
  lang_version_cmd : String? = nil

  # Files
  hyperfine_file : String? = nil
  pi_file : String? = nil
  output_file : String? = nil

  OptionParser.parse do |parser|
    parser.banner = "Usage: #{NAME} [arguments]"

    parser.on("--lang-name=LANGUAGE", "Specifies the name of the programming language") { |lang| lang_name = lang }

    parser.on("--lang-version=COMMAND", "Specifies the command to get the version of the programming language") { |cmd| lang_version_cmd = cmd }

    parser.on("--lang-version-match-index=NUMBER", "Specifies the match index to select from the command to get the version of the programming language. Default 0") { |num| lang_version_match_index = num.to_i }

    parser.on("--hyperfine=FILE", "Specifies the path to the JSON result of hyperfine") { |file| hyperfine_file = file }

    parser.on("--pi=FILE", "Specifies the path to txt file output of hyperfine") { |file| pi_file = file }

    parser.on("--output=FILE", "Specifies the path where to output all metadata as JSON") { |file| output_file = file }

    parser.on("-v", "--version", "Show version") do
      puts "#{NAME} #{VERSION}"
      exit
    end

    parser.on("-h", "--help", "Show this help") do
      puts parser
      exit
    end

    parser.invalid_option do |flag|
      STDERR.puts "ERROR: #{flag} is not a valid option."
      STDERR.puts parser
      exit(1)
    end
  end

  # Check if we got all required options
  require_option(lang_name, "--lang-name")
  require_option(hyperfine_file, "--hyperfine")
  require_option(pi_file, "--pi")
  require_option(output_file, "--output")
  require_option(lang_version_cmd, "--lang-version")

  # Read JSON results from hyperfine
  hyperfine = File.open(hyperfine_file.not_nil!) do |file|
    JSON.parse(file).on_steroids!
  end

  # Calculate pi accuracy
  computed_pi = File.read(pi_file.not_nil!).strip
  if computed_pi.empty?
    STDERR.puts "ERROR: Pi file is empty!"
    exit(1)
  end

  begin
    accuracy = pi_accuracy(computed_pi)
  rescue ex : ArgumentError
    STDERR.puts "ERROR: #{ex.message}"
    exit(1)
  end

  # Get the version by the provided command
  version_raw = run_cmd(lang_version_cmd.not_nil!)
  if version_raw[0] == 0
    begin
      lang_version = get_version(version_raw[1], lang_version_match_index)
    rescue ex : ArgumentError
      STDERR.puts "ERROR: #{ex.message}"
      exit(1)
    end
  else
    STDERR.puts "ERROR: Getting language version failed!"
    STDERR.puts version_raw[1]
    exit(1)
  end

  # Build new JSON
  metadata = JSON::OnSteroids.new
  metadata["Language"] = lang_name
  metadata["Version"] = lang_version
  metadata["Command"] = hyperfine.dig("results.0.command")
  metadata["CalculatedPi"] = computed_pi
  metadata["Accuracy"] = accuracy
  metadata["Mean"] = to_timedelta(hyperfine.dig("results.0.mean"))
  metadata["Stddev"] = to_timedelta(hyperfine.dig("results.0.stddev"))
  metadata["UserTime"] = to_timedelta(hyperfine.dig("results.0.user"))
  metadata["SystemTime"] = to_timedelta(hyperfine.dig("results.0.system"))
  metadata["Median"] = to_timedelta(hyperfine.dig("results.0.median"))
  metadata["Min"] = to_timedelta(hyperfine.dig("results.0.min"))
  metadata["Max"] = to_timedelta(hyperfine.dig("results.0.max"))
  metadata["TimesPerRun"] = hyperfine.dig("results.0.times")
  metadata["ExitCodesPerRun"] = hyperfine.dig("results.0.exit_codes")

  # Write results to output_file as pretty JSON
  File.open(output_file.not_nil!, "w") do |file|
    json = JSON.parse(metadata.to_json)
    file.print json.to_pretty_json
  end

  puts "Successfully created metadata"
  puts "Language: #{lang_name} (#{lang_version})"
  puts "Output: #{output_file}"
end

# Run main when executed directly (not when required for tests)
# The SCMETA_TESTING env var is set by the spec_helper to prevent main from running
unless ENV["SCMETA_TESTING"]?
  main
end
