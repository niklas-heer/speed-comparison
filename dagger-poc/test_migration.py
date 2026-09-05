import json
import math
from pathlib import Path
import subprocess
import sys

import pytest

from languages import LANGUAGES, Language
from result_metadata import enrich_result, methodology
from update_versions import apply_updates
import check_versions


@pytest.mark.parametrize("version", ["", "latest", "*", ">=1.2", "1.2;echo bad"])
def test_unpinned_package_rejected(version):
    with pytest.raises(ValueError):
        Language(name="Example", file="leibniz.c", run="./leibniz", nixpkgs=(f"gcc@{version}",))


@pytest.mark.parametrize("value", ["nan", "inf", "-inf", "0", "4"])
def test_invalid_results_cannot_be_published(value):
    with pytest.raises(ValueError):
        enrich_result({"CalculatedPi": value}, "rust", LANGUAGES["rust"], 10000)


def test_optimized_results_record_methodology_and_rounds():
    result = {"CalculatedPi": str(math.pi - 0.0001)}
    enrich_result(result, "zig-simd", LANGUAGES["zig-simd"], 10000)
    assert result["MathMode"] == "relaxed"
    assert result["ExplicitSIMD"] is True
    assert result["Rounds"] == 10000
    assert methodology("rust", LANGUAGES["rust"])["MathMode"] == "compiler-default"


def test_flake_revisions_are_not_compiler_versions(monkeypatch):
    monkeypatch.setattr(
        check_versions, "get_nixhub_versions", lambda *a, **k: pytest.fail("no lookup")
    )
    info = check_versions.check_language_version("haxe", LANGUAGES["haxe"])
    assert info.package_type == "flake"
    assert not info.update_available


def test_version_checker_sorts_release_numbers(monkeypatch):
    monkeypatch.setattr(check_versions, "get_nixhub_versions", lambda *a, **k: ["1.9.0", "1.100.0"])
    info = check_versions.check_language_version("rust", LANGUAGES["rust"])
    assert info.latest == "1.100.0"
    assert info.update_available


def test_updates_cover_variants_without_rewriting_shell_commands():
    source = 'a = ("rustc@1.92.0",)\nb = ("rustc@1.92.0",)\ncommand = "echo rustc@1.92.0"\n'
    report = [
        {
            "package_type": "devbox",
            "package": "rustc",
            "current": "1.92.0",
            "latest": "1.93.0",
            "update_available": True,
        }
    ]
    updated, count = apply_updates(source, report)
    assert count == 2
    assert updated.count('"rustc@1.93.0"') == 2
    assert 'command = "echo rustc@1.92.0"' in updated


def test_version_cli_imports_without_removed_constant():
    script = Path(__file__).with_name("check_versions.py")
    subprocess.run([sys.executable, str(script), "--help"], check=True, capture_output=True)


def test_new_and_recently_merged_targets_are_available():
    assert {
        "hare",
        "zig-simd",
        "rust-simd",
        "rust-fastmath",
        "octave",
        "octave-vectorised",
        "cpython-numba",
        "sbcl-simd",
        "chezscheme",
        "janet",
    } <= LANGUAGES.keys()
