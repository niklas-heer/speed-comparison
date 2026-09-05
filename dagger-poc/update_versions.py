"""Apply explicit Devbox package updates from the version checker's JSON report."""

import ast
import io
import json
from pathlib import Path
import re
import sys
import tokenize


def apply_updates(source: str, report: list[dict]) -> tuple[str, int]:
    replacements = {}
    for row in report:
        if row["package_type"] != "devbox" or not row["update_available"]:
            continue
        version = row["latest"]
        if not re.fullmatch(r"[0-9][0-9A-Za-z.+_-]*", version or ""):
            raise ValueError(f"Invalid proposed package version: {version!r}")
        old = f"{row['package']}@{row['current']}"
        new = f"{row['package']}@{version}"
        if old in replacements and replacements[old] != new:
            raise ValueError(f"Conflicting updates for {old}")
        replacements[old] = new
    tokens = []
    changed = 0
    for token in tokenize.generate_tokens(io.StringIO(source).readline):
        if token.type == tokenize.STRING:
            try:
                value = ast.literal_eval(token.string)
            except (ValueError, SyntaxError):
                value = None
            if isinstance(value, str) and value in replacements:
                token = token._replace(string=json.dumps(replacements[value]))
                changed += 1
        tokens.append(token)
    updated = tokenize.untokenize(tokens)
    ast.parse(updated)
    return updated, changed


if __name__ == "__main__":
    path = Path(__file__).with_name("languages.py")
    source, count = apply_updates(path.read_text(), json.loads(Path(sys.argv[1]).read_text()))
    path.write_text(source)
    print(f"Updated {count} package declarations")
