"""Optional real-engine check of manifest tooling, source binding and metadata quoting."""

import asyncio
import json
from dataclasses import replace

import dagger

import benchmark
from catalog_resolver import resolve_catalog

REVISION = "3d0220e28f495d3feaca03f730a00b7259924848"


async def check():
    async with dagger.Connection() as client:
        tree = (
            client.git("https://github.com/niklas-heer/speed-comparison.git")
            .commit(REVISION)
            .tree()
        )
        manifest = await resolve_catalog(
            client, tree.file("dagger-poc/languages.py"), source_revision=REVISION
        )
        # Deliberately differ from the host checkout's billion-round input.
        source = tree.directory("src").with_new_file("rounds.txt", contents="17\n")
        lang = replace(manifest.languages["go"], name='Go "quoted" $(exit 97)', base="new-go")
        original = (
            benchmark.HYPERFINE_VERSION,
            benchmark.MICROPYTHON_VERSION,
            benchmark.get_devbox_image,
        )
        try:
            # If execution silently falls back to client defaults, the build fails.
            benchmark.HYPERFINE_VERSION = benchmark.MICROPYTHON_VERSION = "0.0.0"
            benchmark.get_devbox_image = lambda: "invalid-client-default"
            prepared = await benchmark.prepare_benchmark(
                client,
                "new-go",
                lang,
                source,
                benchmark.get_scmeta_script(client),
                use_local=True,
                tooling=manifest.tooling,
            )
            result = await benchmark.measure_benchmark(prepared, timeout_seconds=60)
        finally:
            (
                benchmark.HYPERFINE_VERSION,
                benchmark.MICROPYTHON_VERSION,
                benchmark.get_devbox_image,
            ) = original
        assert result["Rounds"] == 17
        assert result["Language"] == lang.name
        assert result["ImageTag"] is None
        packages = result["DevboxConfig"]["packages"]
        assert "hyperfine@" + manifest.tooling["hyperfine"] in packages
        assert "micropython@" + manifest.tooling["micropython"] in packages
        assert result["DevboxImage"] == manifest.tooling["devbox_image"]
        return {
            "provided_source_rounds": result["Rounds"],
            "display_name_preserved": True,
            "new_target_ran": result["Target"],
            "manifest_tooling_used": True,
            "scope": "Local integration checks, not performance measurements",
        }


if __name__ == "__main__":
    print(json.dumps(asyncio.run(check()), indent=2))
