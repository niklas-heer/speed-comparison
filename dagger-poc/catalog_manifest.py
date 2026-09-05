"""Validate data returned by an isolated catalog resolver; never import its source."""

from __future__ import annotations

import json
import re
from dataclasses import dataclass, fields
from pathlib import PurePosixPath

from languages import Language

MAX_MANIFEST_BYTES = 4 * 1024 * 1024
MAX_TARGETS = 512
STRING_FIELDS = {"name", "file", "run", "category", "version_regex"}
OPTIONAL_FIELDS = {"compile", "version_cmd", "base", "nix_setup"}
LIST_FIELDS = {"extra_files", "nixpkgs", "nix_flakes", "allow_insecure"}
TARGET_PATTERN = r"[a-z0-9][a-z0-9-]*"
VERSION_PATTERN = r"[0-9][0-9A-Za-z.+_-]*"


@dataclass(frozen=True)
class CatalogManifest:
    source_revision: str
    catalog_sha256: str
    tooling: dict[str, str]
    languages: dict[str, Language]


def _object(pairs: list[tuple[str, object]]) -> dict:
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"Duplicate JSON key: {key!r}")
        result[key] = value
    return result


def _keys(value: object, expected: set[str], location: str) -> dict:
    if not isinstance(value, dict) or set(value) != expected:
        raise ValueError(f"{location}: expected exactly {sorted(expected)}")
    return value


def _string(value: object, location: str, *, optional: bool = False) -> str | None:
    if optional and value is None:
        return None
    if (
        not isinstance(value, str)
        or (not optional and not value)
        or len(value) > 131072
        or "\0" in value
    ):
        raise ValueError(f"{location}: invalid string")
    return value


def _path(value: str) -> None:
    path = PurePosixPath(value)
    if (
        not value
        or "\\" in value
        or path.is_absolute()
        or ".." in path.parts
        or path.as_posix() != value
        or value == "."
        or any(ord(c) < 32 for c in value)
    ):
        raise ValueError("Source paths must be canonical relative POSIX paths")


def load_manifest(payload: str, *, source_revision: str, catalog_sha256: str) -> CatalogManifest:
    """Check identity against caller-owned values before constructing trusted types.

    The caller must supply a verified source revision and the hash of the catalog
    it sent to the resolver. The manifest itself is data, not an authorization token
    or proof that a working directory matches the claimed Git revision.
    """
    if not re.fullmatch(r"[0-9a-f]{40}", source_revision):
        raise ValueError("Expected a full source commit SHA")
    if not re.fullmatch(r"[0-9a-f]{64}", catalog_sha256):
        raise ValueError("Expected a SHA-256 catalog digest")
    if len(payload.encode("utf-8")) > MAX_MANIFEST_BYTES:
        raise ValueError("Catalog manifest exceeds the size limit")
    try:
        document = json.loads(payload, object_pairs_hook=_object)
    except (json.JSONDecodeError, RecursionError) as error:
        raise ValueError("Invalid catalog JSON") from error
    _keys(
        document,
        {"schema_version", "source_revision", "catalog_sha256", "tooling", "languages"},
        "manifest",
    )
    if type(document["schema_version"]) is not int or document["schema_version"] != 1:
        raise ValueError("Unsupported catalog schema version")
    if (
        document["source_revision"] != source_revision
        or document["catalog_sha256"] != catalog_sha256
    ):
        raise ValueError("Catalog identity does not match the requested source")
    tooling = _keys(document["tooling"], {"devbox_image", "hyperfine", "micropython"}, "tooling")
    for name, value in tooling.items():
        _string(value, name)
    if not re.fullmatch(r"[a-z0-9][a-z0-9./:_-]*@sha256:[0-9a-f]{64}", tooling["devbox_image"]):
        raise ValueError("Devbox image must have a pinned digest")
    for key in ("hyperfine", "micropython"):
        if not re.fullmatch(VERSION_PATTERN, tooling[key]):
            raise ValueError(f"{key}: expected a pinned version")
    entries = document["languages"]
    if not isinstance(entries, dict) or not 1 <= len(entries) <= MAX_TARGETS:
        raise ValueError("Invalid target count")
    expected_fields = STRING_FIELDS | OPTIONAL_FIELDS | LIST_FIELDS
    if expected_fields != {field.name for field in fields(Language)}:
        raise ValueError("Language schema changed; update the manifest schema explicitly")
    languages = {}
    for target, value in entries.items():
        if not re.fullmatch(TARGET_PATTERN, target):
            raise ValueError("Invalid target identifier")
        config = dict(_keys(value, expected_fields, target))
        for key in STRING_FIELDS | OPTIONAL_FIELDS:
            _string(config[key], f"{target}.{key}", optional=key in OPTIONAL_FIELDS)
        for key in ("name", "base", "category"):
            if config[key] is not None and any(ord(c) < 32 for c in config[key]):
                raise ValueError(f"{target}.{key}: control characters are not allowed")
        for key in LIST_FIELDS:
            if not isinstance(config[key], list) or len(config[key]) > 256:
                raise ValueError(f"{target}.{key}: expected a bounded list")
            for item in config[key]:
                _string(item, f"{target}.{key}")
            config[key] = tuple(config[key])
        for path in (config["file"], *config["extra_files"]):
            _path(path)
        for package in config["nixpkgs"]:
            if not re.fullmatch(r"[A-Za-z0-9._+-]+@" + VERSION_PATTERN, package):
                raise ValueError(f"{target}: invalid package declaration")
        for package in config["allow_insecure"]:
            if not re.fullmatch(r"[A-Za-z0-9._+-]+", package):
                raise ValueError(f"{target}: invalid insecure-package exception")
        try:
            pattern = re.compile(config["version_regex"])
        except re.error as error:
            raise ValueError(f"{target}: invalid version regex") from error
        if pattern.groups < 1:
            raise ValueError(f"{target}: version regex needs a capture group")
        languages[target] = Language(**config)
    return CatalogManifest(source_revision, catalog_sha256, dict(tooling), languages)
