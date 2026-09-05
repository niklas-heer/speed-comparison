#!/usr/bin/env python3
"""Resolve Python declarations through Dagger without importing the input on the client.

Run this trusted driver separately from any unreviewed source checkout. The caller
owns revision authorization and must supply a verified commit snapshot. This helper
alone does not authorize PR execution or establish an isolated homelab runner.
"""

from __future__ import annotations

import argparse
import asyncio
import hashlib
import json
import re
import sys
from dataclasses import asdict
from pathlib import Path

import dagger

from catalog_manifest import MAX_MANIFEST_BYTES, CatalogManifest, load_manifest, read_catalog_source

RESOLVER_IMAGE = (
    "python:3.12-slim@sha256:78387bc3881b8273120a12ebe6c1ab22b018ccc2c9adf565ae1ac9b536e184ea"
)
EXPORTER = Path(__file__).with_name("catalog_export.py")


async def resolve_catalog(
    client: dagger.Client,
    catalog: dagger.File,
    *,
    source_revision: str,
) -> CatalogManifest:
    """Evaluate only the supplied catalog in a credential-free container.

    No host directory, socket, secret or caller environment is mounted. The exporter
    comes from this driver's trusted checkout. Its output is treated as untrusted
    data, including the revision/digest fields it returns.
    """
    if not re.fullmatch(r"[0-9a-f]{40}", source_revision):
        raise ValueError("Expected a verified full source commit SHA")
    async with asyncio.timeout(120):
        if await catalog.size() > MAX_MANIFEST_BYTES:
            raise ValueError("Catalog source exceeds the size limit")
        contents = await catalog.contents()
        digest = hashlib.sha256(contents.encode("utf-8")).hexdigest()
        container = (
            client.container()
            .from_(RESOLVER_IMAGE)
            .with_file("/input/languages.py", catalog)
            .with_new_file("/tools/export.py", contents=EXPORTER.read_text())
            .with_exec(["mkdir", "-p", "/out"])
            .with_exec(
                [
                    "timeout",
                    "--kill-after=2s",
                    "30s",
                    "python",
                    "-I",
                    "/tools/export.py",
                    "--catalog",
                    "/input/languages.py",
                    "--revision",
                    source_revision,
                    "--output",
                    "/out/catalog.json",
                ]
            )
        )
        output = container.file("/out/catalog.json")
        if await output.size() > MAX_MANIFEST_BYTES:
            raise ValueError("Resolved catalog exceeds the size limit")
        payload = await output.contents()
        return load_manifest(payload, source_revision=source_revision, catalog_sha256=digest)


def encode_manifest(manifest: CatalogManifest) -> str:
    """Write the validated model in canonical JSON form, retaining all language fields."""
    return json.dumps({"schema_version": 1, **asdict(manifest)}, sort_keys=True, indent=2) + "\n"


async def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-root", type=Path, required=True)
    parser.add_argument("--catalog", default="dagger-poc/languages.py")
    parser.add_argument("--source-revision", required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    contents = read_catalog_source(args.source_root, args.catalog)
    async with dagger.Connection(dagger.Config(log_output=sys.stderr)) as client:
        manifest = await resolve_catalog(
            client,
            client.directory()
            .with_new_file("languages.py", contents=contents)
            .file("languages.py"),
            source_revision=args.source_revision,
        )
    args.output.write_text(encode_manifest(manifest))
    print(f"Validated {len(manifest.languages)} targets; catalog SHA-256 {manifest.catalog_sha256}")


if __name__ == "__main__":
    asyncio.run(main())
