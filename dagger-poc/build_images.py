#!/usr/bin/env python3
"""
Build and push base container images for each language.

Each image contains:
- Language runtime/compiler (via Nix/Devbox)
- hyperfine benchmarking tool
- Basic tools (bash, coreutils)

Images are tagged with the primary package version for cache invalidation.

**Base Image Deduplication:**
Languages that share the same `base` field and identical package configuration
will share a single base image. For example:
- swift, swift-simd, swift-relaxed all use base="swift" -> one swift image
- java, java-graalvm, java-vecops all use base="java" -> one java image

This significantly reduces build times for language families.

Usage:
    # Build all images
    dagger run python build_images.py

    # Build specific languages
    dagger run python build_images.py rust go python

    # Build and push to registry
    REGISTRY=ghcr.io/niklas-heer/speed-comparison dagger run python build_images.py --push

    # Just list what would be built
    dagger run python build_images.py --dry-run
"""

from __future__ import annotations

import asyncio
import os
import sys
from pathlib import Path

import dagger

from languages import (
    HYPERFINE_VERSION,
    MICROPYTHON_VERSION,
    LANGUAGES,
    Language,
    get_devbox_image,
    get_base_image_name,
    get_base_languages,
    get_language,
    language_image_version_tag,
    resolve_targets_to_bases,
)

# =============================================================================
# Configuration
# =============================================================================

# Default registry (can be overridden via REGISTRY env var)
DEFAULT_REGISTRY = "ghcr.io/niklas-heer/speed-comparison"

# =============================================================================
# Image Building
# =============================================================================


async def build_devbox_image(
    client: dagger.Client,
    target: str,
    lang: Language,
) -> dagger.Container:
    """Build a container image using Devbox for package management.

    Supports two package sources:
    - nixpkgs: Standard Devbox packages with versions (e.g., "go@1.23.4")
    - nix_flakes: Nix flake refs for binary cache hits (e.g., "github:NixOS/nixpkgs/nixos-24.05#swift")

    Both can be combined in the same language definition.
    """
    container = client.container().from_(get_devbox_image())

    # Allow insecure packages if needed (e.g., haxe depends on mbedtls)
    if lang.allow_insecure:
        container = container.with_env_variable("NIXPKGS_ALLOW_INSECURE", "1")
        # Set NIX_CONFIG to allow insecure packages by name
        # This helps with nix-build level checks
        insecure_list = " ".join(lang.allow_insecure)
        container = container.with_env_variable(
            "NIX_CONFIG", f"extra-allowed-insecure-packages = {insecure_list}"
        )

    # Initialize devbox
    container = container.with_workdir("/app").with_exec(["devbox", "init"])

    # Add standard nixpkgs packages (including hyperfine and micropython)
    packages = list(lang.nixpkgs) + [
        f"hyperfine@{HYPERFINE_VERSION}",
        f"micropython@{MICROPYTHON_VERSION}",
    ]
    if packages:
        packages_str = " ".join(packages)
        # Use --allow-insecure=<pkg> for packages that depend on insecure deps (e.g., haxe -> mbedtls)
        # The flag requires the insecure dependency name(s), not the package being installed
        if lang.allow_insecure:
            insecure_flags = " ".join(f"--allow-insecure={pkg}" for pkg in lang.allow_insecure)
            container = container.with_exec(
                ["sh", "-c", f"devbox add {packages_str} {insecure_flags}"]
            )
        else:
            container = container.with_exec(["sh", "-c", f"devbox add {packages_str}"])

    # Add nix flake packages (for binary cache hits from specific nixpkgs channels)
    for flake_ref in lang.nix_flakes:
        container = container.with_exec(["devbox", "add", flake_ref])

    # Run any post-install setup
    if lang.nix_setup:
        container = container.with_exec(["devbox", "run", "--", "sh", "-c", lang.nix_setup])

    # Create a shell script that activates devbox environment
    # This makes it easy to run commands in the container
    entrypoint_script = """#!/bin/bash
exec devbox run -- "$@"
"""
    container = container.with_new_file(
        "/entrypoint.sh", entrypoint_script, permissions=0o755
    ).with_entrypoint(["/entrypoint.sh"])

    return container


async def build_image(
    client: dagger.Client,
    target: str,
    lang: Language,
) -> dagger.Container:
    """Build a container image for a language using Devbox."""
    return await build_devbox_image(client, target, lang)


def get_image_tag(registry: str, target: str, lang: Language) -> str:
    """Generate the full image tag for a language.

    Uses the base image name for languages that share a base.
    E.g., swift-simd uses the "swift" image.
    """
    base_name = get_base_image_name(target)
    version = language_image_version_tag(
        lang,
        devbox_image=get_devbox_image(),
        hyperfine_version=HYPERFINE_VERSION,
        micropython_version=MICROPYTHON_VERSION,
    )
    return f"{registry}/{base_name}:{version}"


async def build_and_push(
    client: dagger.Client,
    target: str,
    lang: Language,
    registry: str,
    push: bool = False,
    dry_run: bool = False,
) -> tuple[str, bool, str]:
    """Build an image and optionally push it.

    Returns:
        (target, success, message)
    """
    image_tag = get_image_tag(registry, target, lang)

    if dry_run:
        return (target, True, f"Would build {image_tag}")

    print(f"\n{'=' * 60}")
    print(f"Building: {target} -> {image_tag}")
    print(f"{'=' * 60}")

    try:
        container = await build_image(client, target, lang)

        # Verify the image works by checking package version
        version_cmd = lang.version_cmd or "echo unknown"
        result = await container.with_exec(
            ["devbox", "run", "--", "sh", "-c", version_cmd]
        ).stdout()

        version = result.strip().split("\n")[0]
        print(f"  Version check: {version}")

        if push:
            # Push to registry
            print(f"  Pushing to {image_tag}...")
            await container.publish(image_tag)
            return (target, True, f"Pushed {image_tag}")
        else:
            return (target, True, f"Built (not pushed): {image_tag}")

    except Exception as e:
        return (target, False, f"ERROR: {e}")


async def main(
    targets: list[str] | None = None,
    push: bool = False,
    dry_run: bool = False,
) -> int:
    """Build images for specified targets (or all if none specified).

    Images are deduplicated by base: if multiple targets share the same
    base (e.g., swift, swift-simd, swift-relaxed all use base="swift"),
    only one image is built.
    """

    registry = os.environ.get("REGISTRY", DEFAULT_REGISTRY)

    # Determine which base images to build
    if targets:
        invalid = [t for t in targets if t not in LANGUAGES]
        if invalid:
            print(f"Unknown targets: {invalid}")
            print(f"Available: {list(LANGUAGES.keys())}")
            return 1
        # Convert targets to their deduplicated base images
        bases_to_build = resolve_targets_to_bases(targets)
        original_count = len(targets)
    else:
        # Build all unique base images
        bases_to_build = get_base_languages()
        original_count = len(LANGUAGES)

    print(f"Registry: {registry}")
    print(f"Requested targets: {original_count}")
    print(f"Unique base images: {len(bases_to_build)}")
    print(f"Push: {push}")
    print(f"Dry run: {dry_run}")

    if dry_run:
        print("\n--- DRY RUN ---\n")
        for base_name, (target, lang) in bases_to_build.items():
            tag = get_image_tag(registry, target, lang)
            variants = [t for t, l in LANGUAGES.items() if get_base_image_name(t, l) == base_name]
            if len(variants) > 1:
                print(f"  {base_name:15} -> {tag}")
                print(f"                    (covers: {', '.join(variants)})")
            else:
                print(f"  {base_name:15} -> {tag}")
        return 0

    results: list[tuple[str, bool, str]] = []

    config = dagger.Config(log_output=sys.stderr)

    async with dagger.Connection(config) as client:
        for base_name, (target, lang) in bases_to_build.items():
            result = await build_and_push(client, target, lang, registry, push, dry_run)
            results.append(result)

    # Summary
    print(f"\n{'=' * 60}")
    print("SUMMARY")
    print(f"{'=' * 60}")

    succeeded = [r for r in results if r[1]]
    failed = [r for r in results if not r[1]]

    print(f"Succeeded: {len(succeeded)}/{len(results)}")

    if failed:
        print("\nFailed:")
        for target, _, msg in failed:
            print(f"  {target}: {msg}")

    return 0 if not failed else 1


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Build language container images")
    parser.add_argument("targets", nargs="*", help="Specific targets to build")
    parser.add_argument("--push", action="store_true", help="Push images to registry")
    parser.add_argument("--dry-run", action="store_true", help="Just show what would be built")

    args = parser.parse_args()

    exit_code = asyncio.run(
        main(
            targets=args.targets if args.targets else None,
            push=args.push,
            dry_run=args.dry_run,
        )
    )
    sys.exit(exit_code)
