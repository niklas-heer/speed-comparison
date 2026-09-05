#!/usr/bin/env python3
"""Optional real-Dagger check of catalog parity and the client/container boundary."""

from __future__ import annotations

import argparse
import asyncio
import hashlib
import json
import os
import subprocess
import uuid
from importlib.metadata import version
from pathlib import Path
from tempfile import TemporaryDirectory

import dagger

from catalog_resolver import RESOLVER_IMAGE, resolve_catalog
from languages import LANGUAGES

ROOT = Path(__file__).resolve().parents[1]


async def check() -> dict:
    revision = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip()
    catalog = ROOT / "dagger-poc/languages.py"
    environment_key = "SPEED_CATALOG_CLIENT_" + uuid.uuid4().hex.upper()
    os.environ[environment_key] = "client-only-test-value"
    marker = Path("/tmp") / ("speed-catalog-sentinel-" + uuid.uuid4().hex)
    evidence = {
        "source_revision": revision,
        "resolver_image": RESOLVER_IMAGE,
        "dagger_sdk": version("dagger-io"),
    }
    try:
        with TemporaryDirectory(prefix="speed-catalog-check-") as directory:
            async with dagger.Connection() as client:
                full = await resolve_catalog(
                    client, client.host().file(str(catalog)), source_revision=revision
                )
                assert full.languages == LANGUAGES
                assert full.catalog_sha256 == hashlib.sha256(catalog.read_bytes()).hexdigest()
                evidence["targets_round_tripped"] = len(full.languages)
                evidence["catalog_sha256"] = full.catalog_sha256
                fixture = Path(directory) / "catalog.py"
                text = catalog.read_text()
                text += (
                    "\nfrom pathlib import Path\nfrom dataclasses import replace\n"
                    f'Path({str(marker)!r}).write_text("container only")\n'
                    'print("catalog stdout is not JSON")\n'
                    'LANGUAGES = {"go": replace(LANGUAGES["go"], '
                    f'name=os.environ.get({environment_key!r}, "not-forwarded"))}}\n'
                )
                fixture.write_text(text)
                result = await resolve_catalog(
                    client, client.host().file(str(fixture)), source_revision=revision
                )
                assert result.languages["go"].name == "not-forwarded"
                assert not marker.exists(), "Catalog code wrote to the client filesystem"
                evidence["client_environment_not_forwarded"] = True
                evidence["container_side_effect_absent_on_client"] = True
                evidence["non_json_stdout_ignored"] = True
                invalid = Path(directory) / "invalid.py"
                invalid.write_text(
                    text + '\nLANGUAGES["go"]=replace(LANGUAGES["go"],file="../leibniz.go")\n'
                )
                try:
                    await resolve_catalog(
                        client, client.host().file(str(invalid)), source_revision=revision
                    )
                except ValueError as error:
                    assert "relative POSIX" in str(error)
                    evidence["exported_path_escape_rejected"] = True
                else:
                    raise AssertionError("Invalid source path was accepted")
                assert not marker.exists()
    finally:
        os.environ.pop(environment_key, None)
    return evidence


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    text = json.dumps(asyncio.run(check()), indent=2) + "\n"
    if args.output:
        args.output.write_text(text)
    print(text, end="")


if __name__ == "__main__":
    main()
