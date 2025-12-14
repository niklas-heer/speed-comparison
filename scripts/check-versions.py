#!/usr/bin/env python3
"""
Version checker for programming language benchmarks.

Checks for new versions of languages using various APIs:
- Docker Hub Registry API
- GitHub Releases API
- Alpine package index
- endoflife.date API

Usage:
    python check-versions.py <language>           # Check single language
    python check-versions.py --all                # Check all languages
    python check-versions.py --all --json         # Output as JSON
"""

import argparse
import csv
import json
import os
import re
import sys
from pathlib import Path
from typing import Optional
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

SCRIPT_DIR = Path(__file__).parent
ROOT_DIR = SCRIPT_DIR.parent
VERSION_SOURCES_FILE = SCRIPT_DIR / "version-sources.json"
EARTHFILE_PATH = ROOT_DIR / "Earthfile"
RESULTS_CSV_PATH = ROOT_DIR / "docs" / "history" / "latest" / "combined_results.csv"


def load_version_sources() -> dict:
    """Load the version sources configuration."""
    with open(VERSION_SOURCES_FILE) as f:
        data = json.load(f)
    # Remove metadata keys
    return {
        k: v for k, v in data.items() if not k.startswith("$") and not k.startswith("_")
    }


def load_current_versions_from_csv() -> dict:
    """Load current versions from the benchmark results CSV.

    Returns a dict mapping target name to version string.
    """
    versions = {}

    if not RESULTS_CSV_PATH.exists():
        print(
            f"  Warning: Results CSV not found at {RESULTS_CSV_PATH}", file=sys.stderr
        )
        return versions

    try:
        with open(RESULTS_CSV_PATH) as f:
            reader = csv.DictReader(f)
            for row in reader:
                # Use 'target' column if available, otherwise skip
                target = row.get("target", "")
                version = row.get("version", "")
                if target and version:
                    versions[target] = version
    except Exception as e:
        print(f"  Warning: Failed to read CSV: {e}", file=sys.stderr)

    return versions


def get_current_version_from_csv(language: str, csv_versions: dict) -> Optional[str]:
    """Get current version for a language from the CSV data."""
    return csv_versions.get(language)


def fetch_json(url: str, headers: Optional[dict] = None) -> dict:
    """Fetch JSON from URL."""
    req_headers = {
        "Accept": "application/json",
        "User-Agent": "speed-comparison-version-checker",
    }
    if headers:
        req_headers.update(headers)

    request = Request(url, headers=req_headers)
    try:
        with urlopen(request, timeout=30) as response:
            return json.loads(response.read().decode())
    except (URLError, HTTPError) as e:
        print(f"  Error fetching {url}: {e}", file=sys.stderr)
        return {}


def get_docker_latest_version(image: str, tag_filter: str) -> Optional[str]:
    """Get latest version from Docker Hub."""
    # Handle namespaced images
    if "/" in image:
        namespace, name = image.split("/", 1)
        base_url = (
            f"https://registry.hub.docker.com/v2/repositories/{namespace}/{name}/tags"
        )
    else:
        base_url = (
            f"https://registry.hub.docker.com/v2/repositories/library/{image}/tags"
        )

    # Extract a keyword from tag_filter to use as name filter (e.g., "alpine" from ".*-alpine$")
    name_hint = ""
    for keyword in ["alpine", "slim", "bullseye", "jammy", "debian"]:
        if keyword in tag_filter:
            name_hint = keyword
            break

    # Build URL with optional name filter for better results
    if name_hint:
        url = f"{base_url}?page_size=100&name={name_hint}"
    else:
        url = f"{base_url}?page_size=100"

    data = fetch_json(url)
    if not data or "results" not in data:
        return None

    # Filter and sort tags
    pattern = re.compile(tag_filter)
    matching_tags = [
        tag["name"] for tag in data["results"] if pattern.match(tag["name"])
    ]

    if not matching_tags:
        return None

    # Sort by version (extract numbers and compare)
    def version_key(tag: str) -> tuple:
        numbers = re.findall(r"\d+", tag)
        return tuple(int(n) for n in numbers) if numbers else (0,)

    matching_tags.sort(key=version_key, reverse=True)
    return matching_tags[0] if matching_tags else None


def get_github_latest_version(repo: str) -> Optional[str]:
    """Get latest release version from GitHub."""
    url = f"https://api.github.com/repos/{repo}/releases/latest"

    # Use token if available
    headers = {}
    token = os.environ.get("GITHUB_TOKEN")
    if token:
        headers["Authorization"] = f"token {token}"

    data = fetch_json(url, headers)
    if not data:
        return None

    tag = data.get("tag_name", "")
    # Remove common prefixes
    version = re.sub(r"^[vV]", "", tag)
    return version if version else None


def get_alpine_latest_version(package: str, edge: bool = False) -> Optional[str]:
    """Get latest version from Alpine package index."""
    branch = "edge" if edge else "v3.23"

    # Try main first, then community, then testing
    for repo in ["main", "community", "testing"]:
        url = f"https://pkgs.alpinelinux.org/package/{branch}/{repo}/x86_64/{package}"
        try:
            request = Request(
                url, headers={"User-Agent": "speed-comparison-version-checker"}
            )
            with urlopen(request, timeout=30) as response:
                html = response.read().decode()
                # Extract version from the package page
                # Handles: <th>Version</th> <td> <strong>2.2.0-r0</strong> </td>
                match = re.search(
                    r"<th[^>]*>Version</th>\s*<td>\s*(?:<strong>)?([^<]+?)(?:</strong>)?\s*</td>",
                    html,
                    re.DOTALL,
                )
                if match:
                    version = match.group(1).strip()
                    # Remove Alpine revision suffix (e.g., -r0, -r1)
                    version = re.sub(r"-r\d+$", "", version)
                    return version
        except (URLError, HTTPError):
            continue

    return None


def get_endoflife_latest_version(product: str) -> Optional[str]:
    """Get latest version from endoflife.date API."""
    url = f"https://endoflife.date/api/{product}.json"
    data = fetch_json(url)

    if not data or not isinstance(data, list):
        return None

    # First entry is typically the latest
    if data and "latest" in data[0]:
        return data[0]["latest"]
    elif data and "cycle" in data[0]:
        return data[0]["cycle"]

    return None


def check_language_version(language: str, config: dict, csv_versions: dict) -> dict:
    """Check version for a single language."""
    result = {
        "language": language,
        "source": config.get("source"),
        "current_version": None,
        "latest_version": None,
        "has_update": False,
        "error": None,
    }

    # Get current version from CSV results
    result["current_version"] = get_current_version_from_csv(language, csv_versions)

    source = config.get("source")

    try:
        if source == "docker":
            image = config.get("image", "")
            tag_filter = config.get("tag_filter", ".*")
            result["latest_version"] = get_docker_latest_version(image, tag_filter)

        elif source == "github":
            repo = config.get("repo", "")
            result["latest_version"] = get_github_latest_version(repo)

        elif source == "alpine":
            package = config.get("package", "")
            is_edge = "edge" in config.get("earthfile_pattern", "")
            result["latest_version"] = get_alpine_latest_version(package, edge=is_edge)

        elif source == "apt":
            # APT packages are harder to check programmatically
            # Would need to query Ubuntu package repos
            result["error"] = "APT version checking not implemented"

        else:
            result["error"] = f"Unknown source type: {source}"

    except Exception as e:
        result["error"] = str(e)

    # Determine if update is available
    if result["current_version"] and result["latest_version"]:
        current = result["current_version"]
        latest = result["latest_version"]

        # Normalize versions for comparison
        current_nums = tuple(int(n) for n in re.findall(r"\d+", current))
        latest_nums = tuple(int(n) for n in re.findall(r"\d+", latest))

        if latest_nums > current_nums:
            result["has_update"] = True

    return result


def main():
    parser = argparse.ArgumentParser(description="Check for language version updates")
    parser.add_argument(
        "language", nargs="?", help="Language to check (e.g., rust, go)"
    )
    parser.add_argument("--all", action="store_true", help="Check all languages")
    parser.add_argument("--json", action="store_true", help="Output as JSON")
    parser.add_argument(
        "--updates-only", action="store_true", help="Only show languages with updates"
    )
    args = parser.parse_args()

    if not args.language and not args.all:
        parser.print_help()
        sys.exit(1)

    sources = load_version_sources()

    # Load current versions from CSV (requires 'target' column from scmeta v1.2.0+)
    csv_versions = load_current_versions_from_csv()
    if not csv_versions:
        print(
            "Note: No current versions found in CSV. Run benchmarks first to populate.",
            file=sys.stderr,
        )

    if args.all:
        languages = list(sources.keys())
    else:
        if args.language not in sources:
            print(f"Unknown language: {args.language}", file=sys.stderr)
            print(
                f"Available languages: {', '.join(sorted(sources.keys()))}",
                file=sys.stderr,
            )
            sys.exit(1)
        languages = [args.language]

    results = []
    updates_found = []

    for lang in languages:
        config = sources[lang]
        if not args.json:
            print(f"Checking {lang}...", file=sys.stderr)

        result = check_language_version(lang, config, csv_versions)
        results.append(result)

        if result["has_update"]:
            updates_found.append(result)

    # Filter if requested
    if args.updates_only:
        results = updates_found

    # Output results
    if args.json:
        output = {
            "results": results,
            "updates_count": len(updates_found),
            "languages_to_update": [r["language"] for r in updates_found],
        }
        print(json.dumps(output, indent=2))
    else:
        print("\n" + "=" * 60)
        print("VERSION CHECK RESULTS")
        print("=" * 60)

        for r in results:
            status = ""
            if r["has_update"]:
                status = " [UPDATE AVAILABLE]"
            elif r["error"]:
                status = f" [ERROR: {r['error']}]"

            print(f"\n{r['language']}:{status}")
            print(f"  Source: {r['source']}")
            print(f"  Current: {r['current_version'] or 'unknown'}")
            print(f"  Latest:  {r['latest_version'] or 'unknown'}")

        print("\n" + "=" * 60)
        print(f"Total: {len(results)} languages checked")
        print(f"Updates available: {len(updates_found)}")

        if updates_found:
            print("\nLanguages to update:")
            for r in updates_found:
                print(
                    f"  - {r['language']}: {r['current_version']} -> {r['latest_version']}"
                )

    # Set GitHub Actions outputs if running in CI
    if os.environ.get("GITHUB_OUTPUT"):
        with open(os.environ["GITHUB_OUTPUT"], "a") as f:
            f.write(f"has_updates={'true' if updates_found else 'false'}\n")
            f.write(f"updates_count={len(updates_found)}\n")
            f.write(
                f"languages_to_update={json.dumps([r['language'] for r in updates_found])}\n"
            )

            # For single language check
            if len(results) == 1:
                r = results[0]
                f.write(f"has_update={'true' if r['has_update'] else 'false'}\n")
                f.write(f"current_version={r['current_version'] or ''}\n")
                f.write(f"new_version={r['latest_version'] or ''}\n")

    # Always exit with 0 - use outputs to determine if updates are available
    sys.exit(0)


if __name__ == "__main__":
    main()
