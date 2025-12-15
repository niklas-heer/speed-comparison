#!/usr/bin/env python3
"""Detect which languages have changed between HEAD~1 and HEAD."""

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


def main():
    old_languages = get_old_languages()

    if old_languages is None:
        # Can't get old version - rebuild all
        from languages import LANGUAGES

        print(" ".join(LANGUAGES.keys()))
        return

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

    if changed:
        print(" ".join(changed))
    else:
        print("")  # No changes


if __name__ == "__main__":
    main()
