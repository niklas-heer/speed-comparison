"""A workflow clock must belong to the exact suite being reported."""

import importlib.util
from pathlib import Path

import pytest

spec = importlib.util.spec_from_file_location(
    "report_metadata", Path(__file__).parents[1] / "report_metadata.py"
)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


def example():
    return (
        {
            "status": "succeeded",
            "run_id": "one",
            "source_revision": "a" * 40,
            "targets": ["go"],
            "completed_targets": ["go"],
            "elapsed_seconds": 98.4,
        },
        [{"Target": "go", "SuiteRunID": "one", "SourceRevision": "a" * 40}],
    )


def test_suite_clock_is_preserved_without_summing_samples():
    record, rows = example()
    rows[0]["TimesPerRun"] = [1, 2, 3]
    assert module.execution_metadata(record, rows)["elapsed_seconds"] == 98.4


@pytest.mark.parametrize(
    "field,value",
    [
        ("status", "failed"),
        ("targets", ["go", "c"]),
        ("run_id", "other"),
        ("source_revision", "b" * 40),
        ("elapsed_seconds", None),
        ("elapsed_seconds", -1),
        ("elapsed_seconds", float("nan")),
        ("elapsed_seconds", True),
    ],
)
def test_rejects_incomplete_foreign_or_invalid_clocks(field, value):
    record, rows = example()
    record[field] = value
    with pytest.raises(ValueError):
        module.execution_metadata(record, rows)


@pytest.mark.parametrize("elapsed", [None, 0, 98.4])
@pytest.mark.parametrize("scope", [None, "", "Includes setup and measurement."])
def test_readme_publication_accepts_nullable_clock_scope(tmp_path, elapsed, scope):
    publisher_spec = importlib.util.spec_from_file_location(
        "publish", Path(__file__).parents[1] / "publish.py"
    )
    publisher = importlib.util.module_from_spec(publisher_spec)
    publisher_spec.loader.exec_module(publisher)
    readme = tmp_path / "README.md"
    readme.write_text("Intro\n<!-- latest-run:start -->old<!-- latest-run:end -->\nEnd")
    publisher.update_readme(readme, "example", 75, {"execution": {
        "elapsed_seconds": elapsed, "elapsed_scope": scope,
    }})
    result = readme.read_text()
    assert result.startswith("Intro\n") and result.endswith("\nEnd")
    assert "75 implementations" in result
    expected_scope = scope or (
        "Whole-run time was not recorded" if elapsed is None
        else "Run-clock scope was not recorded."
    )
    assert expected_scope in result
    if elapsed == 0:
        assert "0h 0m 0s" in result
