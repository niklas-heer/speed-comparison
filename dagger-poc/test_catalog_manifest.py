"""Validate the data boundary independently of the optional Dagger resolver."""

import hashlib
import json
import subprocess
import sys
from dataclasses import asdict
from pathlib import Path

import pytest

from catalog_manifest import MAX_MANIFEST_BYTES, load_manifest
from languages import (
    HYPERFINE_VERSION,
    LANGUAGES,
    MICROPYTHON_VERSION,
    get_devbox_image,
    language_image_fingerprint,
)
from result_metadata import methodology

REVISION = "a" * 40
DIGEST = hashlib.sha256(Path(__file__).with_name("languages.py").read_bytes()).hexdigest()


@pytest.fixture
def document():
    return {
        "schema_version": 1,
        "source_revision": REVISION,
        "catalog_sha256": DIGEST,
        "tooling": {
            "devbox_image": get_devbox_image(),
            "hyperfine": HYPERFINE_VERSION,
            "micropython": MICROPYTHON_VERSION,
        },
        "languages": {"go": asdict(LANGUAGES["go"])},
    }


def load(document):
    return load_manifest(json.dumps(document), source_revision=REVISION, catalog_sha256=DIGEST)


def test_all_targets_preserve_configuration_fingerprints_and_methodology(document):
    document["languages"] = {target: asdict(lang) for target, lang in LANGUAGES.items()}
    resolved = load(document)
    assert resolved.languages == LANGUAGES
    assert resolved.tooling == document["tooling"]
    for target, lang in resolved.languages.items():
        assert language_image_fingerprint(lang) == language_image_fingerprint(LANGUAGES[target])
        assert methodology(target, lang) == methodology(target, LANGUAGES[target])


def test_exporter_round_trip_for_trusted_source(tmp_path):
    output = tmp_path / "catalog.json"
    subprocess.run(
        [
            sys.executable,
            "-I",
            str(Path(__file__).with_name("catalog_export.py")),
            "--catalog",
            str(Path(__file__).with_name("languages.py")),
            "--revision",
            REVISION,
            "--output",
            str(output),
        ],
        check=True,
        capture_output=True,
    )
    resolved = load_manifest(output.read_text(), source_revision=REVISION, catalog_sha256=DIGEST)
    assert resolved.languages == LANGUAGES


@pytest.mark.parametrize("key,value", [("source_revision", "b" * 40), ("catalog_sha256", "0" * 64)])
def test_identity_mismatch_is_rejected(document, key, value):
    document[key] = value
    with pytest.raises(ValueError, match="identity"):
        load(document)


@pytest.mark.parametrize("value", [True, 2, "1"])
def test_schema_version_is_strict(document, value):
    document["schema_version"] = value
    with pytest.raises(ValueError, match="schema version"):
        load(document)


def test_duplicate_keys_are_rejected(document):
    payload = json.dumps(document).replace(
        '"schema_version": 1', '"schema_version": 1, "schema_version": 1'
    )
    with pytest.raises(ValueError, match="Duplicate"):
        load_manifest(payload, source_revision=REVISION, catalog_sha256=DIGEST)


@pytest.mark.parametrize("scope", ["root", "tooling", "language"])
def test_unknown_fields_are_rejected(document, scope):
    value = {
        "root": document,
        "tooling": document["tooling"],
        "language": document["languages"]["go"],
    }[scope]
    value["unknown"] = "unexpected"
    with pytest.raises(ValueError, match="expected exactly"):
        load(document)


@pytest.mark.parametrize(
    "path",
    [
        "/tmp/leibniz.go",
        "../leibniz.go",
        "dir/../../leibniz.go",
        "dir//leibniz.go",
        "dir/./leibniz.go",
        "dir\\leibniz.go",
        ".",
    ],
)
def test_source_path_escape_and_ambiguity_are_rejected(document, path):
    document["languages"]["go"]["file"] = path
    with pytest.raises(ValueError, match="relative POSIX"):
        load(document)


def test_extra_file_path_escape_is_rejected(document):
    document["languages"]["go"]["extra_files"] = ["../secret"]
    with pytest.raises(ValueError, match="relative POSIX"):
        load(document)


@pytest.mark.parametrize(
    "packages", [["go@latest"], ["go;echo bad@1.2"], ["go@1.2 --flag"], "go@1.2"]
)
def test_packages_cannot_smuggle_flags_or_unpinned_versions(document, packages):
    document["languages"]["go"]["nixpkgs"] = packages
    with pytest.raises(ValueError):
        load(document)


def test_moving_tooling_image_is_rejected(document):
    document["tooling"]["devbox_image"] = "jetpackio/devbox:latest"
    with pytest.raises(ValueError, match="pinned digest"):
        load(document)


def test_moving_flake_is_rejected(document):
    document["languages"]["go"]["nix_flakes"] = ["github:NixOS/nixpkgs/master#go"]
    with pytest.raises(ValueError, match="immutable"):
        load(document)


def test_commands_remain_data(document, tmp_path):
    marker = tmp_path / "executed"
    document["languages"]["go"]["run"] = f"touch {marker}"
    assert load(document).languages["go"].run == f"touch {marker}"
    assert not marker.exists()


def test_manifest_size_limit_precedes_json_parsing():
    with pytest.raises(ValueError, match="size limit"):
        load_manifest(
            " " * (MAX_MANIFEST_BYTES + 1), source_revision=REVISION, catalog_sha256=DIGEST
        )


def test_base_image_identifier_cannot_escape_its_namespace(document):
    document["languages"]["go"]["base"] = "../another-image"
    with pytest.raises(ValueError, match="base-image identifier"):
        load(document)


def test_catalog_reader_accepts_only_regular_files_under_the_source_root(tmp_path):
    from catalog_manifest import read_catalog_source

    root = tmp_path / "source"
    (root / "dagger-poc").mkdir(parents=True)
    source = root / "dagger-poc/languages.py"
    source.write_text("# catalog\n")
    assert read_catalog_source(root, "dagger-poc/languages.py") == "# catalog\n"
    with pytest.raises(ValueError):
        read_catalog_source(root, "../outside.py")
    with pytest.raises(ValueError):
        read_catalog_source(root, "dagger-poc")
    outside = tmp_path / "outside.py"
    outside.write_text("private client data")
    source.unlink()
    source.symlink_to(outside)
    with pytest.raises(ValueError, match="symlinks"):
        read_catalog_source(root, "dagger-poc/languages.py")


def test_catalog_reader_rejects_symlinked_parent_directories(tmp_path):
    from catalog_manifest import read_catalog_source

    outside = tmp_path / "outside"
    outside.mkdir()
    (outside / "languages.py").write_text("private client data")
    root = tmp_path / "source"
    root.mkdir()
    (root / "dagger-poc").symlink_to(outside, target_is_directory=True)
    with pytest.raises(ValueError, match="symlinks"):
        read_catalog_source(root, "dagger-poc/languages.py")
