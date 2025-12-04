require "./spec_helper"

describe "pi_accuracy" do
  it "calculates accuracy for a close approximation of pi" do
    # 3.14159265 is accurate to about 8 decimal places
    result = pi_accuracy("3.14159265")
    result.should be > 7
    result.should be < 9
  end

  it "calculates accuracy for a rough approximation of pi" do
    # 3.14 is accurate to about 2-3 decimal places
    result = pi_accuracy("3.14")
    result.should be > 2
    result.should be < 4
  end

  it "calculates accuracy for a very accurate pi value" do
    # Very close to actual pi
    result = pi_accuracy("3.141592653589793")
    result.should be > 14
  end

  it "handles pi values slightly above actual pi" do
    result = pi_accuracy("3.15")
    result.should be > 1
    result.should be < 3
  end

  it "raises an error for non-numeric input" do
    expect_raises(ArgumentError, /not a valid number/) do
      pi_accuracy("not a number")
    end
  end

  it "raises an error for empty string" do
    expect_raises(ArgumentError, /not a valid number/) do
      pi_accuracy("")
    end
  end

  it "raises an error for zero" do
    expect_raises(ArgumentError, /cannot be zero/) do
      pi_accuracy("0")
    end
  end

  it "handles whitespace in input" do
    # to_f? handles leading/trailing whitespace
    result = pi_accuracy("  3.14159265  ")
    result.should be > 7
  end
end

describe "get_version" do
  it "extracts version from simple version string" do
    result = get_version("1.2.3")
    result.should eq("1.2.3")
  end

  it "extracts version from text with prefix" do
    result = get_version("Python 3.11.4")
    result.should eq("3.11.4")
  end

  it "extracts version from gcc-style output" do
    result = get_version("gcc (Alpine 12.2.1_git20220924-r4) 12.2.1 20220924")
    result.should eq("12.2.1")
  end

  it "extracts version from multi-line output" do
    output = "rustc 1.64.0 (a55dd71d5 2022-09-19)\nbinary: rustc\nhost: x86_64-unknown-linux-musl"
    result = get_version(output)
    result.should eq("1.64.0")
  end

  it "uses match_index to select specific version" do
    # Go version output has multiple version numbers
    output = "go version go1.19.1 linux/amd64"
    result = get_version(output, 0)
    result.should eq("1.19.1")
  end

  it "handles elixir-style output with match_index" do
    output = "Erlang/OTP 25 [erts-13.1]\nElixir 1.14.0 (compiled with Erlang/OTP 25)"
    # First match is erts version, second is Elixir version
    result = get_version(output, 1)
    result.should eq("1.14.0")
  end

  it "raises error when no version found" do
    expect_raises(ArgumentError, /No version number found/) do
      get_version("no version here")
    end
  end

  it "raises error for invalid match_index" do
    expect_raises(ArgumentError, /Invalid match index/) do
      get_version("1.2.3", 5)
    end
  end

  it "raises error for negative match_index" do
    expect_raises(ArgumentError, /Invalid match index/) do
      get_version("1.2.3", -1)
    end
  end

  it "extracts four-part version numbers" do
    result = get_version("Version 1.2.3.4")
    result.should eq("1.2.3.4")
  end
end

describe "to_timedelta" do
  it "formats integer as seconds string" do
    result = to_timedelta(5)
    result.should eq("5s")
  end

  it "formats float as seconds string" do
    result = to_timedelta(3.14159)
    result.should eq("3.14159s")
  end

  it "formats zero as seconds string" do
    result = to_timedelta(0)
    result.should eq("0s")
  end
end

describe "run_cmd" do
  it "runs a simple command successfully" do
    exit_code, output = run_cmd("echo hello")
    exit_code.should eq(0)
    output.strip.should eq("hello")
  end

  it "returns non-zero exit code for failed commands" do
    exit_code, _ = run_cmd("false")
    exit_code.should_not eq(0)
  end

  it "handles commands with multiple arguments" do
    exit_code, output = run_cmd("echo one two three")
    exit_code.should eq(0)
    output.strip.should eq("one two three")
  end
end
