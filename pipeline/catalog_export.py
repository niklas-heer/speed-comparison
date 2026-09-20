"""Container-side export helper. Execute unreviewed catalogs only in an isolated runner.

This file intentionally depends only on the standard library. The trusted resolver
supplies it separately from the catalog being evaluated. Its output is untrusted
until catalog_manifest.load_manifest validates it in the calling client.
"""

import argparse
import hashlib
import importlib.util
import json
import sys
from dataclasses import asdict
from pathlib import Path


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--catalog", type=Path, required=True)
    parser.add_argument("--revision", required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    digest = hashlib.sha256(args.catalog.read_bytes()).hexdigest()
    spec = importlib.util.spec_from_file_location("benchmark_catalog_input", args.catalog)
    if spec is None or spec.loader is None:
        raise ValueError("Cannot load catalog")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    result = {
        "schema_version": 1,
        "source_revision": args.revision,
        "catalog_sha256": digest,
        "tooling": {
            "devbox_image": module.get_devbox_image(),
            "hyperfine": module.HYPERFINE_VERSION,
            "micropython": module.MICROPYTHON_VERSION,
        },
        "languages": {target: asdict(lang) for target, lang in module.LANGUAGES.items()},
    }
    args.output.write_text(json.dumps(result, sort_keys=True, allow_nan=False) + "\n")


if __name__ == "__main__":
    main()
