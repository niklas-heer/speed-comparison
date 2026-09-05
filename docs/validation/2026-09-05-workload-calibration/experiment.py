"""Bounded diagnostic calibration on one prepared native target, never publication."""
import json
import os
from pathlib import Path
import shlex
import signal
import subprocess
import sys
import time
import uuid

sys.path.insert(0, '/workspace/repo/dagger-poc')
from languages import LANGUAGES
from measurement import measurement_command, measurement_metadata
import native
from result_metadata import enrich_result

TARGET = os.environ['TARGET']
POINTS = [100_000, 1_000_000, 10_000_000, 100_000_000, 1_000_000_000]
BUDGET = 30
WORK = Path('/app')
OUT = Path('/workspace/calibration') / TARGET
OUT.mkdir(parents=True, exist_ok=True)
lang = LANGUAGES[TARGET]
summary = {'target': TARGET, 'point_budget_seconds': BUDGET, 'round_counts': POINTS,
           'publication_eligible': False, 'points': [],
           'source_revision': Path('/workspace/revision').read_text().strip()}


def checkpoint():
    (OUT / 'calibration.json').write_text(json.dumps(summary, indent=2) + '\n')


def shell(command, logfile, timeout):
    script = WORK / '.calibration-command.sh'
    script.write_text('unset NIX_ENFORCE_NO_NATIVE; ' + command + '\n')
    with logfile.open('w') as log:
        process = subprocess.Popen(['devbox', 'run', '--', 'sh', '-e', str(script)],
                                   cwd=WORK, stdout=log, stderr=subprocess.STDOUT,
                                   start_new_session=True)
        try:
            code = process.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                pass
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.wait()
            return 'timeout'
        return 'succeeded' if code == 0 else f'failed-exit-{code}'


# Prepare once and retain the resolved environment, compiled program and first result.
start = time.monotonic()
native.run(TARGET, Path('/workspace/repo/src'), WORK, OUT / str(POINTS[0]), POINTS[0])
first = json.loads((OUT / str(POINTS[0]) / f'{TARGET}.json').read_text())
summary['initial_setup_and_measurement_seconds'] = time.monotonic() - start
summary['points'].append({'rounds': POINTS[0], 'status': 'succeeded',
                          'result': f'{POINTS[0]}/{TARGET}.json'})
checkpoint()
stopped = False
for rounds in POINTS[1:]:
    if stopped:
        summary['points'].append({'rounds': rounds, 'status': 'skipped',
                                  'reason': 'previous-point-exceeded-budget-or-failed'})
        checkpoint()
        continue
    point = OUT / str(rounds)
    point.mkdir()
    (WORK / 'rounds.txt').write_text(str(rounds) + '\n')
    for name in ['hyperfine.json', 'pi.txt', 'result.json']:
        (WORK / name).unlink(missing_ok=True)
    start = time.monotonic()
    status = shell(measurement_command(lang.run), point / 'measurement.log', BUDGET)
    entry = {'rounds': rounds, 'status': status,
             'measurement_wall_seconds': time.monotonic() - start}
    if status == 'succeeded':
        args = ['micropython', 'scmeta.py', f'--lang-name={lang.name}',
                f'--target-name={TARGET}', f'--lang-version={first["Version"]}',
                '--hyperfine=hyperfine.json', '--pi=pi.txt', '--output=result.json']
        status = shell(shlex.join(args), point / 'metadata.log', 30)
        if status == 'succeeded':
            result = {**first, **json.loads((WORK / 'result.json').read_text())}
            enrich_result(result, TARGET, lang, rounds)
            result.update(measurement_metadata(), MeasurementID=uuid.uuid4().hex)
            (point / f'{TARGET}.json').write_text(json.dumps(result, indent=2, allow_nan=False))
            entry['result'] = f'{rounds}/{TARGET}.json'
        else:
            entry['status'] = 'metadata-' + status
    stopped = entry['status'] != 'succeeded'
    summary['points'].append(entry)
    checkpoint()
print(json.dumps(summary, indent=2))
