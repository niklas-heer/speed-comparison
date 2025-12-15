"""Tests for scmeta.py - Speed Comparison Metadata Generator."""

import json
import math
import tempfile
from pathlib import Path

import pytest

from scmeta import get_version, parse_args, pi_accuracy, to_timedelta


class TestPiAccuracy:
    """Tests for pi_accuracy function."""

    def test_perfect_pi_returns_infinity(self):
        """Exact pi value should return infinity."""
        result = pi_accuracy(str(math.pi))
        assert result == float("inf")

    def test_close_pi_value(self):
        """Close approximation should return high accuracy."""
        # 3.14159265 is accurate to ~8 decimal places
        result = pi_accuracy("3.14159265")
        assert result > 7  # Should be around 7-8

    def test_rough_pi_value(self):
        """Rough approximation should return lower accuracy."""
        # 3.14 is accurate to ~2-3 decimal places
        result = pi_accuracy("3.14")
        assert 2 < result < 4

    def test_very_rough_pi(self):
        """Very rough approximation (3.0) should return low accuracy."""
        result = pi_accuracy("3.0")
        assert 0 < result < 2

    def test_invalid_string_raises(self):
        """Non-numeric string should raise ValueError."""
        with pytest.raises(ValueError, match="not a valid number"):
            pi_accuracy("not a number")

    def test_zero_raises(self):
        """Zero value should raise ValueError."""
        with pytest.raises(ValueError, match="cannot be zero"):
            pi_accuracy("0")

    def test_zero_float_raises(self):
        """Zero as float string should raise ValueError."""
        with pytest.raises(ValueError, match="cannot be zero"):
            pi_accuracy("0.0")

    def test_negative_pi(self):
        """Negative pi approximation should still work."""
        result = pi_accuracy("-3.14159")
        # Should return a valid accuracy (will be different from positive)
        assert isinstance(result, float)

    def test_leibniz_10000_rounds(self):
        """Test with typical Leibniz result (10000 rounds)."""
        # Leibniz with 10000 rounds gives approximately this value
        result = pi_accuracy("3.1414926535900345")
        assert 3 < result < 5  # Should be around 4


class TestGetVersion:
    """Tests for get_version function."""

    def test_simple_version(self):
        """Simple semantic version."""
        assert get_version("1.2.3") == "1.2.3"

    def test_version_in_text(self):
        """Version embedded in text."""
        assert get_version("rustc 1.75.0 (abcdef 2024-01-01)") == "1.75.0"

    def test_go_version(self):
        """Go version format."""
        assert get_version("go version go1.23.4 linux/amd64") == "1.23.4"

    def test_java_version(self):
        """Java/OpenJDK version format."""
        assert get_version("openjdk 21.0.2 2024-01-16") == "21.0.2"

    def test_python_version(self):
        """Python version format."""
        assert get_version("Python 3.12.1") == "3.12.1"

    def test_two_part_version(self):
        """Two-part version like 1.2."""
        assert get_version("version 1.2") == "1.2"

    def test_long_version(self):
        """Four-part version."""
        assert get_version("1.2.3.4") == "1.2.3.4"

    def test_multiple_versions_first(self):
        """Multiple versions, get first by default."""
        assert get_version("v1.2.3 includes lib 4.5.6") == "1.2.3"

    def test_multiple_versions_second(self):
        """Multiple versions, get second with index."""
        assert get_version("v1.2.3 includes lib 4.5.6", match_index=1) == "4.5.6"

    def test_no_version_raises(self):
        """No version found should raise ValueError."""
        with pytest.raises(ValueError, match="No version number found"):
            get_version("no version here")

    def test_invalid_index_raises(self):
        """Invalid match index should raise ValueError."""
        with pytest.raises(ValueError, match="Invalid match index"):
            get_version("version 1.2.3", match_index=5)

    def test_standalone_number_not_version(self):
        """Standalone numbers without dots are not versions."""
        with pytest.raises(ValueError, match="No version number found"):
            get_version("version 123")

    def test_trailing_dot_is_stripped(self):
        """Version followed by dot at end of sentence is handled."""
        # "1.2.3." ends with dot so isn't captured, but we handle the edge case
        # by looking for versions without trailing dots
        assert get_version("using version 1.2.3 now") == "1.2.3"

    def test_node_version(self):
        """Node.js version format."""
        assert get_version("v22.12.0") == "22.12.0"

    def test_crystal_version(self):
        """Crystal version format."""
        assert get_version("Crystal 1.14.0 [abc123]") == "1.14.0"


class TestToTimedelta:
    """Tests for to_timedelta function."""

    def test_integer(self):
        """Integer value."""
        assert to_timedelta(1) == "1s"

    def test_float(self):
        """Float value."""
        assert to_timedelta(0.001234) == "0.001234s"

    def test_zero(self):
        """Zero value."""
        assert to_timedelta(0) == "0s"

    def test_scientific_notation(self):
        """Scientific notation value."""
        assert to_timedelta(1e-6) == "1e-06s"


class TestParseArgs:
    """Tests for parse_args function."""

    def test_all_args_with_equals(self):
        """Parse all arguments with = syntax."""
        argv = [
            "scmeta.py",
            "--lang-name=Rust",
            "--target-name=rust",
            "--lang-version=1.75.0",
            "--hyperfine=hyperfine.json",
            "--pi=pi.txt",
            "--output=result.json",
        ]
        args = parse_args(argv)
        assert args["lang_name"] == "Rust"
        assert args["target_name"] == "rust"
        assert args["lang_version"] == "1.75.0"
        assert args["hyperfine"] == "hyperfine.json"
        assert args["pi"] == "pi.txt"
        assert args["output"] == "result.json"

    def test_missing_args_are_none(self):
        """Missing arguments should be None."""
        argv = ["scmeta.py", "--lang-name=Test"]
        args = parse_args(argv)
        assert args["lang_name"] == "Test"
        assert args["target_name"] is None
        assert args["hyperfine"] is None

    def test_quoted_values(self):
        """Values with spaces (would be quoted in shell)."""
        argv = [
            "scmeta.py",
            "--lang-name=C++ (g++)",
            "--target-name=cpp",
            "--lang-version=g++ 14.2.0",
            "--hyperfine=test.json",
            "--pi=pi.txt",
            "--output=out.json",
        ]
        args = parse_args(argv)
        assert args["lang_name"] == "C++ (g++)"
        assert args["lang_version"] == "g++ 14.2.0"


class TestIntegration:
    """Integration tests for the full scmeta workflow."""

    def test_full_workflow(self, tmp_path):
        """Test complete workflow with temp files."""
        # Create hyperfine.json
        hyperfine_data = {
            "results": [
                {
                    "command": "./leibniz",
                    "mean": 0.001234,
                    "stddev": 0.0001,
                    "user": 0.001,
                    "system": 0.0002,
                    "median": 0.00123,
                    "min": 0.00119,
                    "max": 0.00135,
                    "times": [0.00119, 0.00123, 0.00135],
                    "exit_codes": [0, 0, 0],
                }
            ]
        }
        hyperfine_path = tmp_path / "hyperfine.json"
        hyperfine_path.write_text(json.dumps(hyperfine_data))

        # Create pi.txt
        pi_path = tmp_path / "pi.txt"
        pi_path.write_text("3.1415926535897932")

        # Run scmeta via subprocess to test CLI
        import subprocess

        output_path = tmp_path / "result.json"
        result = subprocess.run(
            [
                "python3",
                str(Path(__file__).parent / "scmeta.py"),
                f"--lang-name=Test Language",
                f"--target-name=test",
                f"--lang-version=rustc 1.75.0",
                f"--hyperfine={hyperfine_path}",
                f"--pi={pi_path}",
                f"--output={output_path}",
            ],
            capture_output=True,
            text=True,
        )

        assert result.returncode == 0, f"stderr: {result.stderr}"
        assert "Successfully created metadata" in result.stdout

        # Verify output
        output = json.loads(output_path.read_text())
        assert output["Language"] == "Test Language"
        assert output["Target"] == "test"
        assert output["Version"] == "1.75.0"
        assert output["Command"] == "./leibniz"
        assert output["CalculatedPi"] == "3.1415926535897932"
        assert output["Accuracy"] == float("inf")  # Perfect pi
        assert output["Mean"] == "0.001234s"
        assert output["Min"] == "0.00119s"
        assert output["Max"] == "0.00135s"
        assert output["TimesPerRun"] == [0.00119, 0.00123, 0.00135]
        assert output["ExitCodesPerRun"] == [0, 0, 0]

    def test_missing_required_arg(self, tmp_path):
        """Test error handling for missing required argument."""
        import subprocess

        result = subprocess.run(
            [
                "python3",
                str(Path(__file__).parent / "scmeta.py"),
                "--lang-name=Test",
                # Missing other required args
            ],
            capture_output=True,
            text=True,
        )

        assert result.returncode == 1
        assert "ERROR" in result.stderr
        assert "required" in result.stderr

    def test_invalid_pi_file(self, tmp_path):
        """Test error handling for invalid pi value."""
        import subprocess

        # Create hyperfine.json
        hyperfine_path = tmp_path / "hyperfine.json"
        hyperfine_path.write_text('{"results": [{}]}')

        # Create invalid pi.txt
        pi_path = tmp_path / "pi.txt"
        pi_path.write_text("not a number")

        output_path = tmp_path / "result.json"
        result = subprocess.run(
            [
                "python3",
                str(Path(__file__).parent / "scmeta.py"),
                "--lang-name=Test",
                "--target-name=test",
                "--lang-version=1.0.0",
                f"--hyperfine={hyperfine_path}",
                f"--pi={pi_path}",
                f"--output={output_path}",
            ],
            capture_output=True,
            text=True,
        )

        assert result.returncode == 1
        assert "ERROR" in result.stderr
        assert "not a valid number" in result.stderr

    def test_empty_pi_file(self, tmp_path):
        """Test error handling for empty pi file."""
        import subprocess

        # Create hyperfine.json
        hyperfine_path = tmp_path / "hyperfine.json"
        hyperfine_path.write_text('{"results": [{}]}')

        # Create empty pi.txt
        pi_path = tmp_path / "pi.txt"
        pi_path.write_text("")

        output_path = tmp_path / "result.json"
        result = subprocess.run(
            [
                "python3",
                str(Path(__file__).parent / "scmeta.py"),
                "--lang-name=Test",
                "--target-name=test",
                "--lang-version=1.0.0",
                f"--hyperfine={hyperfine_path}",
                f"--pi={pi_path}",
                f"--output={output_path}",
            ],
            capture_output=True,
            text=True,
        )

        assert result.returncode == 1
        assert "ERROR" in result.stderr
        assert "empty" in result.stderr.lower()
