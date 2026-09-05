from pathlib import Path
import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'scripts'))
from affected_targets import affected

CATALOG = '''
FLAGS = "-O2"
SWIFT_FLAGS = "-O"
HYPERFINE_VERSION = "1.18.0"
LANGUAGES = {
 "c": Language(name="C", file="leibniz.c", compile=f"gcc {FLAGS}"),
 "c-clang": Language(name="Clang", file="leibniz.c", compile=f"clang {FLAGS}"),
 "swift": Language(name="Swift", file="leibniz.swift", compile=f"swift {SWIFT_FLAGS}"),
 "fs": Language(name="F#", file="fs/Program.fs", extra_files=("shared.fs",)),
}
'''


def test_shared_source_selects_all_its_variants():
    assert affected(CATALOG, CATALOG, ['src/leibniz.c'])['targets'] == ['c', 'c-clang']


def test_directory_and_extra_files_select_the_owner():
    for path in ['src/fs/project.fsproj', 'src/shared.fs']:
        assert affected(CATALOG, CATALOG, [path])['targets'] == ['fs']


def test_transitive_flags_select_only_dependents():
    old = CATALOG.replace('FLAGS = "-O2"', 'OPT = "-O2"\nFLAGS = OPT')
    new = old.replace('OPT = "-O2"', 'OPT = "-O3"')
    assert affected(old, new, ['dagger-poc/languages.py'])['targets'] == ['c', 'c-clang']


def test_definition_edit_selects_only_changed_language():
    new = CATALOG.replace('name="Swift"', 'name="Swift 6"')
    assert affected(CATALOG, new, ['dagger-poc/languages.py'])['targets'] == ['swift']


def test_shared_runner_and_tooling_changes_select_everything():
    all_targets = ['c', 'c-clang', 'fs', 'swift']
    assert affected(CATALOG, CATALOG, ['dagger-poc/measurement.py'])['targets'] == all_targets
    new = CATALOG.replace('1.18.0', '1.19.0')
    assert affected(CATALOG, new, ['dagger-poc/languages.py'])['targets'] == all_targets


def test_docs_and_comments_do_not_trigger_benchmarks():
    assert affected(CATALOG, CATALOG + '\n# explanation\n', ['README.md', 'dagger-poc/README.md', 'scripts/README.rst', 'dagger-poc/languages.py'])['targets'] == []


def test_source_rename_and_removed_target_are_reported():
    new = CATALOG.replace('file="leibniz.swift"', 'file="leibniz-new.swift"')
    assert affected(CATALOG, new, ['src/leibniz.swift', 'src/leibniz-new.swift'])['targets'] == ['swift']
    new = '\n'.join(line for line in CATALOG.splitlines() if '"swift":' not in line)
    result = affected(CATALOG, new, ['src/leibniz.swift'])
    assert result['targets'] == []
    assert result['removed_targets'] == ['swift']


def test_planning_never_executes_catalog_code(tmp_path):
    marker = tmp_path / 'executed'
    source = f'open({str(marker)!r}, "w").write("bad")\n' + CATALOG
    affected(source, source, ['src/leibniz.c'])
    assert not marker.exists()


def test_unmapped_source_falls_back_to_full_validation():
    assert len(affected(CATALOG, CATALOG, ['src/unknown-runtime.lock'])['targets']) == 4


def test_no_base_catalog_validates_all_new_targets():
    result = affected(None, CATALOG, [])
    assert len(result['targets']) == 4
    assert result['publication_eligible'] is False


def test_report_dependencies_select_report_check_without_benchmarks():
    paths = ['uv.lock', 'pyproject.toml', 'analyze.py', 'publish.py',
             'scripts/publish_results.py', 'scripts/validate_publish.py',
             'scripts/compare_results.py', 'scripts/check_report.py', 'icons/go.png',
             'docs/validation/2026-09-05-workload-calibration/c/100000000/c.json']
    for path in paths:
        plan = affected(CATALOG, CATALOG, [path])
        assert plan['targets'] == [], path
        assert plan['report_check'] is True, path


def test_mixed_report_and_source_changes_preserve_both_selections():
    plan = affected(CATALOG, CATALOG, ['uv.lock', 'src/leibniz.c'])
    assert plan['targets'] == ['c', 'c-clang']
    assert plan['report_check'] is True


def test_execution_lock_still_selects_all_targets():
    plan = affected(CATALOG, CATALOG, ['dagger-poc/uv.lock'])
    assert len(plan['targets']) == 4
    assert plan['report_check'] is False


def test_revision_planning_handles_missing_history(tmp_path, monkeypatch):
    import subprocess
    from affected_targets import plan_revisions

    def git(*args):
        return subprocess.check_output(['git', *args], cwd=tmp_path, text=True).strip()

    git('init', '-q')
    git('config', 'user.name', 'Planner Test')
    git('config', 'user.email', 'planner@example.invalid')
    (tmp_path / 'dagger-poc').mkdir()
    (tmp_path / 'dagger-poc/languages.py').write_text(CATALOG)
    git('add', '.')
    git('commit', '-qm', 'Initial catalog')
    first = git('rev-parse', 'HEAD')
    monkeypatch.chdir(tmp_path)
    for base in ('', '0' * 40, 'f' * 40):
        plan = plan_revisions(base, first)
        assert plan['targets'] == ['c', 'c-clang', 'fs', 'swift']
        assert plan['report_check'] is True
        assert plan['base_revision'] is None
        assert plan['head_revision'] == first
        assert plan['requested_base_revision'] == base
        assert plan['publication_eligible'] is False
        assert 'full validation' in plan['fallback_reason']
    (tmp_path / 'src').mkdir()
    (tmp_path / 'src/leibniz.c').write_text('/* changed */')
    git('add', '.')
    git('commit', '-qm', 'C source')
    selective = plan_revisions(first, 'HEAD')
    assert selective['targets'] == ['c', 'c-clang']
    assert selective['report_check'] is False
    git('checkout', '--orphan', 'unrelated')
    git('commit', '-qm', 'Unrelated catalog')
    assert plan_revisions(first, 'HEAD')['base_revision'] is None
    assert len(plan_revisions(first, 'HEAD')['targets']) == 4
