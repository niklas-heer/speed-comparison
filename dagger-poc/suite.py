"""One preparation barrier followed by serial, independently retained measurements."""

from __future__ import annotations

import asyncio
from time import perf_counter


async def run_phases(targets, prepare, measure, save, *, concurrency=2):
    """Materialize preparations with bounded concurrency before timing any target.

    ``prepare`` must await the actual build (Dagger graphs are lazy). A failure is
    recorded per target; successful measurements are saved immediately. Persistence
    failures propagate instead of claiming evidence was retained.
    """
    if not 1 <= concurrency <= 8:
        raise ValueError("Preparation concurrency must be between 1 and 8")
    if len(set(targets)) != len(targets):
        raise ValueError("Duplicate targets")
    started = perf_counter()
    semaphore = asyncio.Semaphore(concurrency)
    prepared, errors, durations, results = {}, {}, {}, {}

    async def build(target):
        async with semaphore:
            before = perf_counter()
            try:
                prepared[target] = await prepare(target)
            except Exception as error:
                errors[target] = {"phase": "prepare", "error": str(error)}
            finally:
                durations[target] = {"prepare_seconds": perf_counter() - before}

    await asyncio.gather(*(build(target) for target in targets))
    preparation_seconds = perf_counter() - started
    for target in targets:
        if target not in prepared:
            continue
        before = perf_counter()
        try:
            result = await measure(prepared.pop(target))
        except Exception as error:
            errors[target] = {"phase": "measure", "error": str(error)}
        else:
            save(target, result)
            results[target] = result
        finally:
            durations[target]["measure_seconds"] = perf_counter() - before
    return {
        "results": results,
        "errors": errors,
        "target_phases": durations,
        "preparation_wall_seconds": preparation_seconds,
        "total_wall_seconds": perf_counter() - started,
    }
