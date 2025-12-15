#!/usr/bin/env python3
"""
Check for available package version updates in nixpkgs.

This script queries nixhub.io API to find newer versions of packages
used in languages.py. It handles both:
- Devbox packages (nixpkgs with @version syntax)
- Nix flake packages (pinned to NIXPKGS_REV)

Usage:
    python check_versions.py              # Check all packages
    python check_versions.py rust go      # Check specific languages
    python check_versions.py --json       # Output as JSON
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from dataclasses import dataclass
from typing import Optional
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

from languages import LANGUAGES, NIXPKGS_REV, Language


@dataclass
class VersionInfo:
    """Version comparison result."""

    language: str
    package: str
    current: str
    latest: Optional[str]
    update_available: bool
    package_type: str  # "devbox" or "flake"


# Patterns that indicate unstable/preview versions
UNSTABLE_PATTERNS = [
    r"alpha",
    r"beta",
    r"preview",
    r"rc\d*",  # rc, rc1, rc2, etc.
    r"dev",
    r"nightly",
    r"snapshot",
    r"pre",
    r"canary",
    r"insider",
    r"unstable",
    r"experimental",
    r"test",
    r"next",  # e.g., "next" channel releases
]

# Compiled regex for efficiency
UNSTABLE_REGEX = re.compile(
    r"[-._](" + "|".join(UNSTABLE_PATTERNS) + r")[-._\d]*$",
    re.IGNORECASE,
)


def is_stable_version(version: str) -> bool:
    """Check if a version string represents a stable release.

    Filters out:
    - Alpha/beta/preview/RC versions
    - Dev/nightly builds
    - Versions with unstable suffixes

    Args:
        version: Version string to check

    Returns:
        True if the version appears to be stable.
    """
    # Check for unstable patterns
    if UNSTABLE_REGEX.search(version):
        return False

    # Also check for common patterns without separators
    lower = version.lower()
    for pattern in ["alpha", "beta", "preview", "nightly", "dev", "canary"]:
        if pattern in lower:
            return False

    return True


def get_nixhub_versions(package: str, stable_only: bool = True) -> list[str]:
    """Query nixhub.io for available versions of a package.

    Scrapes the HTML package page since there's no public JSON API.

    Args:
        package: Package name (without version, e.g., "rustc", "go")
        stable_only: If True, filter out preview/unstable versions

    Returns:
        List of available version strings, newest first.
    """
    url = f"https://www.nixhub.io/packages/{package}"

    try:
        request = Request(
            url,
            headers={
                "Accept": "text/html",
                "User-Agent": "speed-comparison-version-checker/1.0",
            },
        )
        with urlopen(request, timeout=30) as response:
            html = response.read().decode("utf-8")

            # Version info is in list items with id="VERSION"
            # Pattern: <li id="1.23.4" ...> or <li id="21.0.5" ...>
            version_pattern = r'<li\s+id="([0-9][0-9a-zA-Z._-]*)"'
            matches = re.findall(version_pattern, html)

            # Filter to only valid-looking versions (must contain a digit)
            versions = [v for v in matches if re.search(r"\d", v)]

            # Filter out unstable versions if requested
            if stable_only:
                versions = [v for v in versions if is_stable_version(v)]

            return versions

    except (URLError, HTTPError) as e:
        return []


def parse_version(version: str) -> tuple:
    """Parse version string into comparable tuple.

    Handles various formats:
    - "1.23.4" -> (1, 23, 4)
    - "2025-11" -> (2025, 11)
    - "nixos-24.11" -> (24, 11)
    """
    # Extract numeric parts
    numbers = re.findall(r"\d+", version)
    return tuple(int(n) for n in numbers) if numbers else (0,)


def compare_versions(current: str, latest: str) -> bool:
    """Check if latest version is newer than current.

    Returns:
        True if latest > current
    """
    current_tuple = parse_version(current)
    latest_tuple = parse_version(latest)
    return latest_tuple > current_tuple


def check_language_version(target: str, lang: Language, stable_only: bool = True) -> VersionInfo:
    """Check if a language has updates available.

    Args:
        target: Target name (e.g., "rust")
        lang: Language configuration
        stable_only: If True, only consider stable versions

    Returns:
        VersionInfo with comparison results.
    """
    package = lang.primary_package
    current = lang.primary_version

    # Get available versions from nixhub
    versions = get_nixhub_versions(package, stable_only=stable_only)

    latest = versions[0] if versions else None

    # Determine if update is available
    update_available = False
    if latest and current:
        update_available = compare_versions(current, latest)

    return VersionInfo(
        language=target,
        package=package,
        current=current,
        latest=latest,
        update_available=update_available,
        package_type=package_type,
    )


def check_all_versions(
    targets: list[str] | None = None, stable_only: bool = True
) -> list[VersionInfo]:
    """Check versions for all or specified languages.

    Args:
        targets: Optional list of language targets to check.
                 If None, checks all languages.
        stable_only: If True, only consider stable versions

    Returns:
        List of VersionInfo results.
    """
    if targets:
        languages = {t: LANGUAGES[t] for t in targets if t in LANGUAGES}
    else:
        languages = LANGUAGES

    results = []
    for target, lang in languages.items():
        info = check_language_version(target, lang, stable_only=stable_only)
        results.append(info)

    return results


def print_results(results: list[VersionInfo], as_json: bool = False) -> None:
    """Print version check results.

    Args:
        results: List of VersionInfo to display.
        as_json: If True, output as JSON.
    """
    if as_json:
        data = [
            {
                "language": r.language,
                "package": r.package,
                "current": r.current,
                "latest": r.latest,
                "update_available": r.update_available,
                "package_type": r.package_type,
            }
            for r in results
        ]
        print(json.dumps(data, indent=2))
        return

    # Group by package type
    devbox_results = [r for r in results if r.package_type == "devbox"]
    flake_results = [r for r in results if r.package_type == "flake"]

    print(f"Nixpkgs pin: {NIXPKGS_REV}")
    print()

    # Print Devbox packages
    if devbox_results:
        print("=== Devbox Packages ===")
        print(f"{'Language':<15} {'Package':<20} {'Current':<15} {'Latest':<15} {'Status'}")
        print("-" * 80)

        for r in devbox_results:
            if r.update_available:
                status = "UPDATE AVAILABLE"
            elif r.latest:
                status = "up to date"
            else:
                status = "(check failed)"

            print(
                f"{r.language:<15} {r.package:<20} {r.current:<15} {r.latest or 'N/A':<15} {status}"
            )

        print()

    # Print Nix flake packages
    if flake_results:
        print("=== Nix Flake Packages ===")
        print(f"{'Language':<15} {'Package':<25} {'Version':<15} {'Latest':<15} {'Status'}")
        print("-" * 85)

        for r in flake_results:
            if r.update_available:
                status = "UPDATE AVAILABLE"
            elif r.latest:
                status = "up to date"
            else:
                status = "(check failed)"

            print(
                f"{r.language:<15} {r.package:<25} {r.current:<15} {r.latest or 'N/A':<15} {status}"
            )

        print()


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Check for package version updates in nixpkgs")
    parser.add_argument(
        "languages",
        nargs="*",
        help="Specific languages to check (default: all)",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="Output as JSON",
    )
    parser.add_argument(
        "--updates-only",
        action="store_true",
        help="Only show packages with updates available",
    )
    parser.add_argument(
        "--include-unstable",
        action="store_true",
        help="Include preview/alpha/beta/RC versions in results",
    )

    args = parser.parse_args()

    # Validate targets
    if args.languages:
        invalid = [t for t in args.languages if t not in LANGUAGES]
        if invalid:
            print(f"Unknown languages: {invalid}", file=sys.stderr)
            print(f"Available: {list(LANGUAGES.keys())}", file=sys.stderr)
            return 1

    stable_only = not args.include_unstable
    results = check_all_versions(args.languages or None, stable_only=stable_only)

    # Filter if requested
    if args.updates_only:
        results = [r for r in results if r.update_available]

    print_results(results, as_json=args.json)

    # Summary
    updates = [r for r in results if r.update_available]
    if updates and not args.json:
        print(f"Total: {len(updates)} update(s) available")
        print("\nTo update a Devbox package version:")
        print('  Edit languages.py: nixpkgs=("package@NEW_VERSION",)')
        print("\nTo update Nix flake packages, update NIXPKGS_REV in languages.py")

    return 0


if __name__ == "__main__":
    sys.exit(main())
