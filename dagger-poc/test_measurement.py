"""Exercise the shared shell protocol with a recording hyperfine stand-in."""

import json
import os
import shlex
import subprocess
import sys

from measurement import measurement_command


def test_four_executions_retain_three_measurements_and_first_output(tmp_path):
    hyperfine = tmp_path / "hyperfine"
    hyperfine.write_text(
        f"#!{sys.executable}\n"
        "import json, subprocess, sys\n"
        "args = sys.argv[1:]\n"
        "warmups = int(args[args.index('--warmup') + 1])\n"
        "runs = int(args[args.index('--runs') + 1])\n"
        "for _ in range(warmups + runs):\n"
        "    subprocess.run(args[-1], shell=True, check=True)\n"
        "open('protocol.json', 'w').write(json.dumps({'warmups':warmups,'runs':runs}))\n"
    )
    hyperfine.chmod(0o755)
    program = tmp_path / "program with spaces.py"
    program.write_text(
        "from pathlib import Path\n"
        "p = Path('count')\n"
        "n = int(p.read_text()) + 1 if p.exists() else 1\n"
        "p.write_text(str(n))\n"
        "print(n)\n"
    )
    command = shlex.join([sys.executable, str(program)])
    subprocess.run(
        ["sh", "-ec", measurement_command(command, show_output=True)],
        cwd=tmp_path,
        check=True,
        capture_output=True,
        env={**os.environ, "PATH": str(tmp_path) + os.pathsep + os.environ["PATH"]},
    )
    assert (tmp_path / "count").read_text() == "4"
    assert (tmp_path / "pi.txt").read_text() == "1\n"
    assert json.loads((tmp_path / "protocol.json").read_text()) == {"warmups": 0, "runs": 3}


def test_failed_first_warmup_aborts_before_measurement(tmp_path):
    result = subprocess.run(
        ["sh", "-ec", measurement_command("exit 7")],
        cwd=tmp_path,
        capture_output=True,
    )
    assert result.returncode == 7
    assert not (tmp_path / "hyperfine.json").exists()
