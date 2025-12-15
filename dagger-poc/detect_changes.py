#!/usr/bin/env python3
"""
Detect which languages need image builds.

This script detects languages that need their images rebuilt by checking:
1. Languages whose definitions changed between HEAD~1 and HEAD
2. Languages whose images don't exist in the registry (optional)

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
        return {k: repr(v) for k, v in exec_globals.get("LANGUAGES", {}).items()}
    except Exception:
        return None


def get_changed_languages() -> list[str]:
    """Get list of languages that changed between HEAD~1 and HEAD."""
    old_languages = get_old_languages()

    if old_languages is None:
        # Can't get old version - rebuild all
        from languages import LANGUAGES

        return list(LANGUAGES.keys())

    # Get current languages
    from languages import LANGUAGES

    current_languages = {k: repr(v) for k, v in LANGUAGES.items()}

    # Find changed or new languages
    changed = []
    for lang, definition in current_languages.items():
        if lang not in old_languages:
            changed.append(lang)  # New language
        elif old_languages[lang] != definition:
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
        all_needed = sorted(set(changed) | set(missing))
        if missing:
            print(f"Found {len(missing)} missing images in registry", file=sys.stderr)
    else:
        all_needed = changed

    # Output space-separated list
    if all_needed:
        print(" ".join(all_needed))
    else:
        print("")  # No changes


if __name__ == "__main__":
    main()
