#!/usr/bin/env python3
"""
Detect which languages need image builds.

This script detects languages that need their images rebuilt by checking:
1. Languages whose definitions changed between HEAD~1 and HEAD
2. Languages whose images don't exist in the registry (optional)

**Base Image Deduplication:**
Output is deduplicated by base image. For example, if swift-simd and swift-relaxed
both need building, only "swift" is output since they share the same base image.

Usage:
    # Detect changed languages only (default, fast)
    python detect_changes.py

    # Also check for missing images in registry (slower, requires network)
    python detect_changes.py --check-registry

    # Check registry with custom URL
    REGISTRY=ghcr.io/niklas-heer/speed-comparison python detect_changes.py --check-registry

Environment:
    REGISTRY: Container registry URL (default: ghcr.io/niklas-heer/speed-comparison)
    CHECK_REGISTRY: Set to "1" or "true" to enable registry checks (alternative to --check-registry)
"""

import os
import subprocess
import sys

from languages import (
    DEFAULT_DEVBOX_IMAGE,
    HYPERFINE_VERSION,
    MICROPYTHON_VERSION,
    get_base_image_name,
)


def targets_to_bases(targets: list[str]) -> list[str]:
    """Convert a list of targets to unique base image names.

    For example, ["swift", "swift-simd", "swift-relaxed"] -> ["swift"]
    """
    bases = set()
    for target in targets:
        bases.add(get_base_image_name(target))
    return sorted(bases)


def language_image_key(lang) -> str:
    """Get a string key representing the fields that affect the container image.

    Only compares fields that affect the built image, not all fields.
    This prevents false positives when the Language dataclass adds new fields.
    """
    # These fields affect what goes into the container image
    return (
        f"{lang.nixpkgs}|{getattr(lang, 'nix_flakes', ())}|{lang.nix_setup}|"
        f"{getattr(lang, 'allow_insecure', ())}"
    )


def get_toolchain_key(namespace: dict) -> str:
    """Get key for global image toolchain inputs."""
    return "|".join(
        [
            str(namespace.get("DEFAULT_DEVBOX_IMAGE", "")),
            str(namespace.get("HYPERFINE_VERSION", "")),
            str(namespace.get("MICROPYTHON_VERSION", "")),
        ]
    )


def get_old_languages():
    """Get LANGUAGES dict from the previous commit."""
    result = subprocess.run(
        ["git", "show", "HEAD~1:dagger-poc/languages.py"],
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        # First commit or file didn't exist
        return None

    try:
        exec_globals = {"__builtins__": __builtins__}
        exec(result.stdout, exec_globals)
        old_langs = exec_globals.get("LANGUAGES", {})
        old_toolchain = get_toolchain_key(exec_globals)
        # Extract only image-relevant fields
        return ({k: language_image_key(v) for k, v in old_langs.items()}, old_toolchain)
    except Exception:
        return None


def get_changed_languages() -> list[str]:
    """Get list of languages that changed between HEAD~1 and HEAD."""
    old_data = get_old_languages()

    if old_data is None:
        # Can't get old version - rebuild all
        from languages import LANGUAGES

        return list(LANGUAGES.keys())
    old_languages, old_toolchain = old_data

    # Get current languages
    from languages import LANGUAGES

    current_languages = {k: language_image_key(v) for k, v in LANGUAGES.items()}
    current_toolchain = "|".join([DEFAULT_DEVBOX_IMAGE, HYPERFINE_VERSION, MICROPYTHON_VERSION])

    if old_toolchain != current_toolchain:
        return list(LANGUAGES.keys())

    # Find changed or new languages
    changed = []
    for lang, key in current_languages.items():
        if lang not in old_languages:
            changed.append(lang)  # New language
        elif old_languages[lang] != key:
            changed.append(lang)  # Changed definition

    return changed


def get_missing_images() -> list[str]:
    """Get list of languages whose images are missing from the registry."""
    try:
        from check_images import check_all_images
    except ImportError:
        print("Warning: check_images.py not found, skipping registry check", file=sys.stderr)
        return []

    registry = os.environ.get("REGISTRY", "ghcr.io/niklas-heer/speed-comparison")
    return check_all_images(registry=registry, verbose=False)


def main():
    import argparse

    parser = argparse.ArgumentParser(description="Detect languages that need image builds")
    parser.add_argument(
        "--check-registry",
        action="store_true",
        help="Also check for missing images in the registry",
    )
    args = parser.parse_args()

    # Check environment variable as alternative to flag
    check_registry = args.check_registry or os.environ.get("CHECK_REGISTRY", "").lower() in (
        "1",
        "true",
        "yes",
    )

    # Get changed languages
    changed = get_changed_languages()

    # Optionally check for missing images
    if check_registry:
        print("Checking registry for missing images...", file=sys.stderr)
        missing = get_missing_images()
        # Combine and deduplicate
        all_targets = sorted(set(changed) | set(missing))
        if missing:
            print(f"Found {len(missing)} missing images in registry", file=sys.stderr)
    else:
        all_targets = changed

    # Convert to base images (deduplicate variants like swift, swift-simd, swift-relaxed)
    base_images = targets_to_bases(all_targets)

    if len(base_images) < len(all_targets):
        print(
            f"Deduplicated {len(all_targets)} targets to {len(base_images)} base images",
            file=sys.stderr,
        )

    # Output space-separated list of base images
    if base_images:
        print(" ".join(base_images))
    else:
        print("")  # No changes


if __name__ == "__main__":
    main()
