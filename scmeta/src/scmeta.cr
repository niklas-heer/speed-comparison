require "json"
require "option_parser"
require "big"

# Third party dependencies. See shard.yml
require "json_on_steroids"

NAME = "scmeta"
VERSION = "1.1.0"

upcase = false
lang_name = nil
lang_version = nil
lang_version_match_index = 0
lang_version_cmd = nil

# Files
hyperfine_file = nil
pi_file = nil
output_file = nil

OptionParser.parse do |parser|
  parser.banner = "Usage: #{NAME} [arguments]"

  parser.on("--lang-name=LANGUAGE", "Specifies the name of the programming language") { |lang| lang_name = lang }

  parser.on("--lang-version=COMMAND", "Specifies the command to get the version of the programming language") { |cmd| lang_version_cmd = cmd }

  parser.on("--lang-version-match-index=NUMBER", "Specifies the match index to select from the command to get the version of the programming language. Default 0") { |num| lang_version_match_index = num }

  parser.on("--hyperfine=FILE", "Specifies the path to the JSON result of hyperfine") { |file| hyperfine_file = file }

  parser.on("--pi=FILE", "Specifies the path to txt file output of hyperfine") { |file| pi_file = file }

  parser.on("--output=FILE", "Specifies the path where to output all metadata as JSON") { |file| output_file = file }

  parser.on("-v", "--version", "Show verison") do
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

def run_cmd(command : String)
  cmd_array = command.split(" ", 2)
  cmd = cmd_array[0]
  args = cmd_array[1].split

  stdout = IO::Memory.new
  stderr = IO::Memory.new
  status = Process.run(cmd, args: args, output: stdout, error: stderr)
  if status.success?
    {status.exit_code, stdout.to_s}
  else
    {status.exit_code, stderr.to_s}
  end
end

def set?(variable, msg)
  if !variable
    puts "#{msg}"
    exit(1)
  end
end

def get_version(text, match_index)
  matches = text.scan(/\d+(\.\d+)+/)
  begin
    matches[match_index].try &.[0]
  rescue ex
    puts ex.message
    puts "These are the caputed versions.\n"

    matches.each_with_index do |match, i|
      puts "#{i}: #{match.try &.[0]}"
    end
    exit(1)
  end
end

# Check if we got all variables set
set?(lang_name, "Please provide the language name!")
set?(hyperfine_file, "Please provide the hyperfine JSON file!")
set?(pi_file, "Please provide the pi txt file!")
set?(output_file, "Please provide the output JSON file!")
set?(lang_version_cmd, "Please provide the language version command!")

# Read JSON results from hyperfine
hyperfine = File.open("#{hyperfine_file}") do |file|
  JSON.parse(file).on_steroids!
end

def pi_accuracy(input : String)
  # https://www.wolframalpha.com/input?i=N%5BPi%2C+32%5D
  const_pi = BigFloat.new(3.1415926535897932384626433832795)
  pi = input.to_big_f

  if pi > const_pi
    accuracy = 1 - ( pi / const_pi)
  else
    accuracy = ( pi / const_pi)
  end
  accuracy.abs.to_f
end

# Calculate pi accuracy
computed_pi = File.open("#{pi_file}") do |file|
  file.gets_to_end.delete("\n")
end
accuracy = pi_accuracy(computed_pi)

# Get the version by the provided command
version_raw = run_cmd(lang_version_cmd.to_s)
if version_raw[0] == 0
  lang_version = get_version(version_raw[1].to_s, lang_version_match_index.to_i)
else
  puts "Getting language version failed!"
  puts version_raw[1]
  exit(1)
end

# Returns a given number as seconds
def to_timedelta(number)
  "#{number.to_s}s"
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
File.open("#{output_file}", "w") do |file|
  json = JSON.parse(metadata.to_json)
  file.print json.to_pretty_json
end

puts "Successfully created metadata\n"
puts "Language: #{lang_name} (#{lang_version})"
puts "Output: #{output_file}"
