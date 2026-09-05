#!/usr/bin/env python3
"""Plan affected benchmark targets without executing either revision's Python code."""

from __future__ import annotations

import argparse
import ast
import json
from pathlib import Path, PurePosixPath
import re
import subprocess

CATALOG = 'dagger-poc/languages.py'
GLOBAL_INPUTS = {'src/rounds.txt'}
REPORT_INPUTS = {
    'pyproject.toml', 'uv.lock', 'analyze.py', 'publish.py', 'download_icons.py',
    'scripts/publish_results.py', 'scripts/validate_publish.py',
    'scripts/compare_results.py', 'scripts/check_report.py', '.github/workflows/ci.yml',
}
REPORT_PREFIXES = ('icons/', 'docs/validation/2026-09-05-workload-calibration/')
GLOBAL_CONSTANTS = {'HYPERFINE_VERSION', 'MICROPYTHON_VERSION', 'DEFAULT_DEVBOX_IMAGE', 'MARCH_NATIVE'}


def catalog(source: str) -> tuple[dict, str, dict]:
    tree = ast.parse(source)
    entries = None
    constants = {}
    framework = []
    for node in tree.body:
        name = None
        if isinstance(node, ast.Assign) and len(node.targets) == 1 and isinstance(node.targets[0], ast.Name):
            name = node.targets[0].id
        elif isinstance(node, ast.AnnAssign) and isinstance(node.target, ast.Name):
            name = node.target.id
        if name == 'LANGUAGES':
            if not isinstance(node.value, ast.Dict):
                raise ValueError('LANGUAGES must be a literal dictionary')
            entries = node.value
        elif name:
            constants[name] = node.value
        elif not (isinstance(node, ast.Expr) and isinstance(node.value, ast.Constant) and isinstance(node.value.value, str)):
            framework.append(node)
    if entries is None:
        raise ValueError('Missing LANGUAGES dictionary')
    definitions = {}
    for key, value in zip(entries.keys, entries.values):
        target = ast.literal_eval(key)
        if not isinstance(target, str) or not re.fullmatch(r'[a-z0-9][a-z0-9-]*', target) or target in definitions:
            raise ValueError(f'Invalid or duplicate target: {target}')
        if not isinstance(value, ast.Call) or not isinstance(value.func, ast.Name) or value.func.id != 'Language':
            raise ValueError(f'{target}: expected Language declaration')
        dependencies = {}
        pending = [n.id for n in ast.walk(value) if isinstance(n, ast.Name)]
        while pending:
            name = pending.pop()
            if name in constants and name not in dependencies:
                dependencies[name] = ast.dump(constants[name])
                pending.extend(n.id for n in ast.walk(constants[name]) if isinstance(n, ast.Name))
        fields = {kw.arg: kw.value for kw in value.keywords}
        source_file = ast.literal_eval(fields['file'])
        extras = ast.literal_eval(fields['extra_files']) if 'extra_files' in fields else ()
        paths = [source_file, *extras]
        for path in paths:
            if not isinstance(path, str) or PurePosixPath(path).is_absolute() or '..' in PurePosixPath(path).parts:
                raise ValueError(f'{target}: invalid source path')
        # The runner copies all of a directory-based target's source directory.
        prefix = 'src/' + source_file.split('/')[0] + '/' if '/' in source_file else None
        definitions[target] = {'definition': ast.dump(value), 'dependencies': dependencies,
                               'paths': ['src/' + p for p in paths], 'prefix': prefix}
    shared = {name: ast.dump(constants[name]) for name in GLOBAL_CONSTANTS if name in constants}
    return definitions, ast.dump(ast.Module(body=framework, type_ignores=[])), shared


def affected(base_source: str | None, head_source: str, paths: list[str]) -> dict:
    head, framework, shared = catalog(head_source)
    reasons: dict[str, list[str]] = {}

    def include(targets, reason):
        for target in targets:
            reasons.setdefault(target, []).append(reason)

    if base_source is None:
        include(head, 'No base catalog; validate every target')
        base = {}
    else:
        base, old_framework, old_shared = catalog(base_source)
        if framework != old_framework or shared != old_shared:
            include(head, 'Shared catalog/tooling behavior changed')
        for target, config in head.items():
            if target not in base or config != base[target]:
                include([target], 'Language definition or shared compiler settings changed')
    for path in paths:
        if path in REPORT_INPUTS:
            continue
        if path == CATALOG:
            continue
        if path.startswith(('dagger-poc/', 'scripts/')) and path.endswith(('.md', '.rst')):
            continue
        if path in GLOBAL_INPUTS or path.startswith(('dagger-poc/', 'scripts/')):
            include(head, f'Shared pipeline input: {path}')
            continue
        if not path.startswith('src/'):
            continue
        matched = set()
        for target in head:
            for config in (head.get(target), base.get(target)):
                if config and (path in config['paths'] or (config['prefix'] and path.startswith(config['prefix']))):
                    matched.add(target)
        if matched:
            include(matched, f'Source dependency: {path}')
        elif not any(path in config['paths'] or (config['prefix'] and path.startswith(config['prefix'])) for config in base.values()):
            include(head, f'Unmapped source input: {path}')
    selected = sorted(reasons)
    return {'schema_version': 1, 'targets': selected,
            'removed_targets': sorted(set(base) - set(head)),
            'reasons': {t: sorted(set(reasons[t])) for t in selected},
            'report_check': any(path in REPORT_INPUTS or path.startswith(REPORT_PREFIXES) for path in paths),
            'publication_eligible': False}


def git(*args: str, required: bool = True) -> str | None:
    result = subprocess.run(['git', *args], capture_output=True, text=True)
    if result.returncode:
        if not required:
            return None
        raise ValueError(result.stderr.strip())
    return result.stdout


def plan_revisions(base_ref: str, head_ref: str) -> dict:
    head = git('rev-parse', '--verify', '--end-of-options', head_ref + '^{commit}').strip()
    base = git('rev-parse', '--verify', '--end-of-options', base_ref + '^{commit}', required=False) if base_ref else None
    merge_base = git('merge-base', base.strip(), head, required=False) if base else None
    if merge_base:
        merge_base = merge_base.strip()
        changes = git('diff', '--name-only', '--no-renames', '-z', merge_base, head).split('\0')
        plan = affected(git('show', f'{merge_base}:{CATALOG}', required=False),
                        git('show', f'{head}:{CATALOG}'), [p for p in changes if p])
    else:
        # A force push can make the previous commit unreachable even in a full
        # checkout. Comparing only HEAD^ would silently miss earlier changes.
        plan = affected(None, git('show', f'{head}:{CATALOG}'), [])
        plan['report_check'] = True
        plan['fallback_reason'] = 'Base commit unavailable or no common ancestor; full validation required'
    plan.update(base_revision=merge_base, head_revision=head, requested_base_revision=base_ref)
    return plan


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--base', required=True)
    parser.add_argument('--head', default='HEAD')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    plan = plan_revisions(args.base, args.head)
    text = json.dumps(plan, indent=2) + '\n'
    if args.output:
        args.output.write_text(text)
    else:
        print(text, end='')


if __name__ == '__main__':
    main()
