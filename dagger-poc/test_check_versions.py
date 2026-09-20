import check_versions
from languages import LANGUAGES


def test_held_micropython_does_not_report_available_update(monkeypatch):
    monkeypatch.setattr(
        check_versions, "get_nixhub_versions", lambda *a, **k: ["1.27.0", "1.26.0"]
    )
    info = check_versions.check_language_version("micropython", LANGUAGES["micropython"])
    assert info.update_available is False
    assert info.hold
    assert info.latest == "1.27.0"


def test_unheld_package_still_reports_available_update(monkeypatch):
    monkeypatch.setattr(
        check_versions, "get_nixhub_versions", lambda *a, **k: ["99.0.0", "1.0.0"]
    )
    info = check_versions.check_language_version("rust", LANGUAGES["rust"])
    assert info.update_available is True
    assert info.hold is None
