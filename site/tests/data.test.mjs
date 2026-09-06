import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { createHash } from "node:crypto";
import {
  runs,
  runData,
  historyDir,
  seconds,
  recordedTiming,
} from "../src/lib/results.mjs";
const current = runData("2026-09-05T193245");

test("raw seconds stay unrounded and original evidence remains intact", () => {
  assert.equal(current.results.length, 75);
  for (const r of current.results) {
    assert.equal(r.median, Number(r.raw.Median.slice(0, -1)));
    const bytes = readFileSync(
      `${historyDir}/${current.id}/raw/${r.target}.json`,
    );
    assert.equal(r.sha256, createHash("sha256").update(bytes).digest("hex"));
    assert.equal(r.raw.Rounds, 1_000_000_000);
  }
  const sums = readFileSync(
    `${historyDir}/${current.id}/raw/SHA256SUMS`,
    "utf8",
  );
  for (const r of current.results)
    assert.ok(sums.includes(`${r.sha256}  ${r.target}.json`));
});
test("old snapshots never borrow current metadata or invented target identities", () => {
  const old = runData("2025-12-21T203810");
  assert.equal(old.results.length, 0);
  for (const run of runs) assert.doesNotThrow(() => runData(run.id));
  assert.throws(() => runData("../latest"));
  assert.throws(() => seconds("NaNs"));
});

test("missing historical extremes stay unknown while recorded times retain units", () => {
  for (const value of [undefined, null, ""])
    assert.equal(recordedTiming(value), "Not recorded");
  assert.equal(recordedTiming("0.125s"), "125.00 ms");
  assert.equal(recordedTiming(1.5), "1.500 s");
  assert.throws(() => recordedTiming("NaNs"), /Invalid timing/);
});

test("run duration and recovered source stay bound to original evidence", async () => {
  const { duration, validateSources, runData } =
    await import("../src/lib/results.mjs");
  assert.equal(duration(19126), "5h 18m 46s");
  assert.equal(duration(null), "Not recorded");
  assert.equal(duration(0), "0s");
  assert.throws(() => duration(-1));
  const run = runData("2026-09-05T193245");
  assert.equal(run.execution.elapsed_seconds, 19126);
  assert.equal(run.results.filter((r) => r.sources.length).length, 75);
  const go = run.results.find((r) => r.target === "go");
  assert.match(go.sources[0].content, /package main/);
  assert.equal(go.sourcePrimary, "src/leibniz.go");
  assert.throws(
    () => validateSources({ "src/a": { content: "changed", sha256: "bad" } }),
    /checksum/,
  );
  assert.throws(() => validateSources({ "src/../secret": {} }), /path/);
  assert.deepEqual(runData("2022-10-15T164557").execution, {});
});
