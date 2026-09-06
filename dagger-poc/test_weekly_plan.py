"""Exercise scheduling against real Git trees, including reverted history."""

import subprocess
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
from weekly_plan import environment_digest, plan

ENV = {
    "runner_id": "isolated-test-worker",
    "profile": "leibniz-1w-3m-v1",
    "cpu_max": "100000 100000",
}


@pytest.fixture
def repo(tmp_path, monkeypatch):
    def git(*args):
        return subprocess.check_output(["git", *args], cwd=tmp_path, text=True).strip()

    git("init", "-q")
    git("config", "user.email", "test@example.invalid")
    git("config", "user.name", "Test")
    (tmp_path / "dagger-poc").mkdir()
    (tmp_path / "src").mkdir()
    (tmp_path / "site").mkdir()
    (tmp_path / "dagger-poc/languages.py").write_text(
        "LANGUAGES = {'go': Language(file='go.go'), 'rust': Language(file='rust.rs')}\n"
    )
    (tmp_path / "src/go.go").write_text("original")
    (tmp_path / "src/rust.rs").write_text("original")

    def commit():
        git("add", ".")
        git("commit", "-qm", "change")
        return git("rev-parse", "HEAD")

    base = commit()
    monkeypatch.chdir(tmp_path)
    checkpoint = {
        "schema_version": 1,
        "status": "published",
        "source_revision": base,
        "environment_sha256": environment_digest(ENV),
    }
    return tmp_path, git, commit, checkpoint


def test_website_only_skips_benchmarks_but_rebuilds_report(repo):
    root, git, commit, checkpoint = repo
    (root / "site/index.astro").write_text("new website")
    commit()
    result = plan(checkpoint, "HEAD", ENV)
    assert not result["run_full_suite"]
    assert result["report_check"]


def test_source_change_requires_full_weekly_run_but_identifies_affected_target(repo):
    root, git, commit, checkpoint = repo
    (root / "src/go.go").write_text("changed")
    commit()
    result = plan(checkpoint, "HEAD", ENV)
    assert result["run_full_suite"]
    assert result["affected_targets"] == ["go"]


def test_environment_change_and_missing_checkpoint_trigger_full(repo):
    *_, checkpoint = repo
    assert plan(checkpoint, "HEAD", {**ENV, "cpu_max": "200000 100000"})["run_full_suite"]
    assert plan(None, "HEAD", ENV)["run_full_suite"]
    assert not plan(checkpoint, "HEAD", ENV)["run_full_suite"]
    with pytest.raises(ValueError):
        plan({**checkpoint, "status": "failed"}, "HEAD", ENV)


def test_reverted_history_compares_published_tree_not_merge_base(repo):
    root, git, commit, checkpoint = repo
    base = checkpoint["source_revision"]
    (root / "src/go.go").write_text("measured change")
    checkpoint["source_revision"] = commit()
    assert plan(checkpoint, base, ENV)["run_full_suite"]


@pytest.mark.parametrize("source", ["HEAD", "master", "", "abc123", None, 123])
def test_checkpoint_rejects_moving_refs_and_invalid_source_ids(repo, source):
    *_, checkpoint = repo
    with pytest.raises(ValueError, match="immutable source revision"):
        plan({**checkpoint, "source_revision": source}, "HEAD", ENV)
