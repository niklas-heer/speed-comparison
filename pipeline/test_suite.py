import asyncio

import pytest

from suite import run_phases


def test_builds_overlap_but_all_finish_before_serial_measurements():
    async def check():
        active = 0
        peak = 0
        built = set()
        measured = []
        saved = []
        rendezvous = asyncio.Event()

        async def prepare(target):
            nonlocal active, peak
            active += 1
            peak = max(peak, active)
            if active == 2:
                rendezvous.set()
            await asyncio.wait_for(rendezvous.wait(), timeout=1)
            await asyncio.sleep(0)
            built.add(target)
            active -= 1
            return target

        async def measure(target):
            assert built == {"a", "b", "c"}
            assert active == 0
            assert len(saved) == len(measured)
            measured.append(target)
            await asyncio.sleep(0)
            return {"target": target}

        result = await run_phases(
            ["a", "b", "c"],
            prepare,
            measure,
            lambda target, value: saved.append((target, value)),
            concurrency=2,
        )
        assert peak == 2
        assert measured == ["a", "b", "c"]
        assert list(result["results"]) == measured
        assert not result["errors"]

    asyncio.run(check())


def test_failures_keep_successful_results_and_skip_failed_preparations():
    async def check():
        async def prepare(target):
            if target == "broken-build":
                raise ValueError("compiler failed")
            return target

        async def measure(target):
            if target == "broken-run":
                raise ValueError("runtime failed")
            return {"ok": True}

        saved = []
        result = await run_phases(
            ["ok", "broken-build", "broken-run"],
            prepare,
            measure,
            lambda target, value: saved.append(target),
        )
        assert saved == ["ok"]
        assert result["errors"]["broken-build"]["phase"] == "prepare"
        assert result["errors"]["broken-run"]["phase"] == "measure"
        assert "measure_seconds" not in result["target_phases"]["broken-build"]
        assert result["results"] == {"ok": {"ok": True}}

    asyncio.run(check())


def test_evidence_write_failure_is_not_reported_as_success():
    async def identity(value):
        return value

    def fail(*args):
        raise OSError("disk full")

    with pytest.raises(OSError, match="disk full"):
        asyncio.run(run_phases(["a"], identity, identity, fail))


def test_empty_plan_starts_no_work():
    async def unexpected(*args):
        pytest.fail("Empty plan must not execute targets")

    result = asyncio.run(run_phases([], unexpected, unexpected, unexpected))
    assert result["results"] == result["errors"] == {}


@pytest.mark.parametrize("jobs", [0, -1, 9])
def test_invalid_build_concurrency_is_rejected(jobs):
    with pytest.raises(ValueError, match="concurrency"):
        asyncio.run(run_phases([], None, None, None, concurrency=jobs))
