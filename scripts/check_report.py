#!/usr/bin/env python3
"""Exercise report rendering and local publication using recorded samples only."""

import csv
from io import BytesIO
import json
from pathlib import Path
import shutil
import subprocess
import sys
from tempfile import TemporaryDirectory

import cairosvg
from PIL import Image

ROOT = Path(__file__).resolve().parents[1]
FIXTURE = ROOT / 'docs/validation/2026-09-05-workload-calibration'


def run(script: str, *args: str) -> None:
    subprocess.run([sys.executable, str(ROOT / script), *args], cwd=ROOT, check=True)


def main() -> None:
    with TemporaryDirectory(prefix='speed-report-check-') as name:
        temporary = Path(name)
        results = temporary / 'results'
        results.mkdir()
        expected = {}
        for target in ('c', 'go'):
            source = FIXTURE / target / '100000000' / f'{target}.json'
            expected[target] = json.loads(source.read_text())
            shutil.copyfile(source, results / source.name)
        revision = json.loads((FIXTURE / 'provenance.json').read_text())['source_revision']
        (results / 'source-revision.txt').write_text(revision + '\n')
        rounds = results / 'rounds.txt'
        rounds.write_text('100000000\n')
        run('analyze.py', '--folder', str(results), '--out', str(results), '--rounds', str(rounds))

        rows = json.loads((results / 'combined_results.json').read_text())
        assert {row['target'] for row in rows} == set(expected)
        for row in rows:
            source = expected[row['target']]
            for public, raw in (
                ('times_per_run', 'TimesPerRun'), ('exit_codes_per_run', 'ExitCodesPerRun'),
                ('calculated_pi', 'CalculatedPi'), ('warmup_runs', 'WarmupRuns'),
                ('measured_runs', 'MeasuredRuns'), ('output_capture', 'OutputCapture'),
                ('measurement_id', 'MeasurementID'), ('compile', 'Compile'),
                ('devbox_lock', 'DevboxLock'), ('rounds', 'Rounds'),
            ):
                assert row[public] == source.get(raw), (row['target'], public)
        with (results / 'combined_results.csv').open() as stream:
            assert len(list(csv.DictReader(stream))) == len(expected)
        with Image.open(results / 'combined_results.png') as chart:
            chart.verify()
        metadata = json.loads((results / 'run_metadata.json').read_text())
        assert metadata['source_revision'] == revision

        docs = temporary / 'docs'
        stale = docs / 'history/latest/raw/stale.json'
        stale.parent.mkdir(parents=True)
        stale.write_text('{}')
        run('publish.py', '--results', str(results), '--docs', str(docs))
        manifest = json.loads((docs / 'history/manifest.json').read_text())
        assert len(manifest['runs']) == 1
        assert manifest['runs'][0]['languages'] == len(expected)
        for folder in ('latest', manifest['runs'][0]['id']):
            published = docs / 'history' / folder
            assert (published / 'source-revision.txt').read_text().strip() == revision
            assert (published / 'combined_results.json').read_bytes() == (
                results / 'combined_results.json'
            ).read_bytes()
            assert {path.stem for path in (published / 'raw').glob('*.json')} == set(expected)
            assert (published / 'raw/source-revision.txt').read_text().strip() == revision
            assert (published / 'raw/rounds.txt').read_text().strip() == '100000000'
            for target in expected:
                assert (published / 'raw' / f'{target}.json').read_bytes() == (
                    results / f'{target}.json'
                ).read_bytes()

        # download_icons.py uses this conversion; the check needs no network/downloads.
        svg = (b'<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16">'
               b'<rect width="16" height="16" fill="#006b76"/></svg>')
        with Image.open(BytesIO(cairosvg.svg2png(bytestring=svg))) as icon:
            assert icon.size == (16, 16)
            icon.load()
    print('Report check passed: rendering, samples, raw evidence, provenance and local publication')


if __name__ == '__main__':
    main()
