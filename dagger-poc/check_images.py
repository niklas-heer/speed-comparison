#!/usr/bin/env python3
"""
Check which language images exist in the container registry.

This script queries the GitHub Container Registry (GHCR) to determine
which images need to be built. It outputs a space-separated list of
languages that are missing from the registry.

**Base Image Deduplication:**
Languages that share the same `base` field use the same container image.
For example, swift, swift-simd, and swift-relaxed all share the "swift" image.
This script checks for base images only and reports missing languages.

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

from languages import (
    HYPERFINE_VERSION,
    MICROPYTHON_VERSION,
    LANGUAGES,
    Language,
    get_base_image_name,
    get_devbox_image,
    get_language,
    language_image_version_tag,
)

# Default registry
DEFAULT_REGISTRY = "ghcr.io/niklas-heer/speed-comparison"


def get_image_tag(registry: str, target: str, lang: Language) -> str:
    """Generate the full image tag for a language.

    Uses the base image name for languages that share a base.
    """
    base_name = get_base_image_name(target)
    version = language_image_version_tag(
        lang,
        devbox_image=get_devbox_image(),
        hyperfine_version=HYPERFINE_VERSION,
        micropython_version=MICROPYTHON_VERSION,
    )
    return f"{registry}/{base_name}:{version}"


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


def get_bases_to_check(targets: list[str] | None) -> dict[str, list[str]]:
    """Get base images to check and their associated targets.

    Args:
        targets: Specific targets to check (or all if None)

    Returns:
        Dict mapping base_name -> list of targets that use that base
    """
    if targets:
        invalid = [t for t in targets if t not in LANGUAGES]
        if invalid:
            print(f"Unknown targets: {invalid}", file=sys.stderr)
            sys.exit(1)
        languages_to_check = {t: get_language(t) for t in targets}
    else:
        languages_to_check = LANGUAGES

    # Group targets by base image
    bases: dict[str, list[str]] = {}
    for target in languages_to_check:
        base_name = get_base_image_name(target)
        if base_name not in bases:
            bases[base_name] = []
        bases[base_name].append(target)

    return bases


def check_all_images(
    targets: list[str] | None = None,
    registry: str = DEFAULT_REGISTRY,
    verbose: bool = False,
) -> list[str]:
    """Check which images are missing from the registry.

    This checks base images only - if a base image is missing, all targets
    that use that base are returned as missing.

    Args:
        targets: Specific targets to check (or all if None)
        registry: Container registry URL
        verbose: Print detailed status for each image

    Returns:
        List of target names that are missing from the registry
    """
    bases_to_check = get_bases_to_check(targets)

    missing_targets = []
    checked_count = 0

    # Check base images in parallel for speed
    with ThreadPoolExecutor(max_workers=10) as executor:
        # Submit checks for each unique base image
        futures = {}
        for base_name, base_targets in bases_to_check.items():
            # Use the first target to get the language config (they share the same base config)
            # Pass the actual target name (e.g., "cpp"), not the base name (e.g., "cplusplus")
            canonical_target = base_targets[0]
            lang = get_language(canonical_target)
            futures[executor.submit(check_single_image, canonical_target, lang, registry)] = (
                base_name,
                base_targets,
            )

        for future in as_completed(futures):
            base_name, base_targets = futures[future]
            _, exists, image_tag = future.result()
            checked_count += 1

            if verbose:
                status = "✓ exists" if exists else "✗ missing"
                if len(base_targets) > 1:
                    targets_str = f" (covers: {', '.join(base_targets)})"
                else:
                    targets_str = ""
                print(f"  {base_name:15} {status:12} {image_tag}{targets_str}", file=sys.stderr)

            if not exists:
                # All targets that use this base are missing
                missing_targets.extend(base_targets)

    if verbose:
        print(
            f"Checked {checked_count} base images for {len(bases_to_check)} targets",
            file=sys.stderr,
        )

    return sorted(missing_targets)


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
