#!/usr/bin/env python3
"""
Check which language images exist in the container registry.

This script queries the GitHub Container Registry (GHCR) to determine
which images need to be built. It outputs a space-separated list of
languages that are missing from the registry.

Usage:
    # Check all languages, output missing ones
    python check_images.py

    # Check specific languages
    python check_images.py rust go python

    # Check with custom registry
    REGISTRY=ghcr.io/niklas-heer/speed-comparison python check_images.py

Environment:
    REGISTRY: Container registry URL (default: ghcr.io/niklas-heer/speed-comparison)
    GITHUB_TOKEN: Token for authenticating with GHCR (optional, for private repos)
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed

from languages import LANGUAGES, Language, get_language

# Default registry
DEFAULT_REGISTRY = "ghcr.io/niklas-heer/speed-comparison"


def get_image_tag(registry: str, target: str, lang: Language) -> str:
    """Generate the full image tag for a language."""
    version = lang.primary_version.replace("+", "-")
    return f"{registry}/{target}:{version}"


def check_image_exists(image_tag: str) -> bool:
    """Check if an image exists in the registry using docker manifest inspect.

    This is a lightweight check that doesn't pull the image.
    """
    try:
        result = subprocess.run(
            ["docker", "manifest", "inspect", image_tag],
            capture_output=True,
            text=True,
            timeout=30,
        )
        return result.returncode == 0
    except subprocess.TimeoutExpired:
        # Timeout - assume image doesn't exist
        return False
    except FileNotFoundError:
        # Docker not installed, fall back to skopeo
        return check_image_exists_skopeo(image_tag)


def check_image_exists_skopeo(image_tag: str) -> bool:
    """Check if an image exists using skopeo (fallback if docker not available)."""
    try:
        result = subprocess.run(
            ["skopeo", "inspect", f"docker://{image_tag}"],
            capture_output=True,
            text=True,
            timeout=30,
        )
        return result.returncode == 0
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return False


def check_image_exists_crane(image_tag: str) -> bool:
    """Check if an image exists using crane (another fallback)."""
    try:
        result = subprocess.run(
            ["crane", "manifest", image_tag],
            capture_output=True,
            text=True,
            timeout=30,
        )
        return result.returncode == 0
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return False


def check_single_image(target: str, lang: Language, registry: str) -> tuple[str, bool, str]:
    """Check if a single image exists.

    Returns:
        (target, exists, image_tag)
    """
    image_tag = get_image_tag(registry, target, lang)
    exists = check_image_exists(image_tag)
    return (target, exists, image_tag)


def check_all_images(
    targets: list[str] | None = None,
    registry: str = DEFAULT_REGISTRY,
    verbose: bool = False,
) -> list[str]:
    """Check which images are missing from the registry.

    Args:
        targets: Specific targets to check (or all if None)
        registry: Container registry URL
        verbose: Print detailed status for each image

    Returns:
        List of target names that are missing from the registry
    """
    if targets:
        invalid = [t for t in targets if t not in LANGUAGES]
        if invalid:
            print(f"Unknown targets: {invalid}", file=sys.stderr)
            sys.exit(1)
        languages_to_check = {t: get_language(t) for t in targets}
    else:
        languages_to_check = LANGUAGES

    missing = []

    # Check images in parallel for speed
    with ThreadPoolExecutor(max_workers=10) as executor:
        futures = {
            executor.submit(check_single_image, target, lang, registry): target
            for target, lang in languages_to_check.items()
        }

        for future in as_completed(futures):
            target, exists, image_tag = future.result()
            if verbose:
                status = "✓ exists" if exists else "✗ missing"
                print(f"  {target:15} {status:12} {image_tag}", file=sys.stderr)

            if not exists:
                missing.append(target)

    return sorted(missing)


def main():
    """Main entry point."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Check which language images are missing from the registry"
    )
    parser.add_argument("targets", nargs="*", help="Specific targets to check")
    parser.add_argument("--verbose", "-v", action="store_true", help="Print status for each image")
    parser.add_argument(
        "--json", action="store_true", help="Output as JSON instead of space-separated"
    )

    args = parser.parse_args()

    registry = os.environ.get("REGISTRY", DEFAULT_REGISTRY)

    if args.verbose:
        print(f"Registry: {registry}", file=sys.stderr)
        print(f"Checking images...", file=sys.stderr)

    missing = check_all_images(
        targets=args.targets if args.targets else None,
        registry=registry,
        verbose=args.verbose,
    )

    if args.json:
        print(json.dumps({"missing": missing, "count": len(missing)}))
    else:
        # Output space-separated list (compatible with workflow matrix generation)
        print(" ".join(missing))


if __name__ == "__main__":
    main()
