import { test } from "node:test";
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { snapshot, importSnapshot } from "../scripts/archive.mjs";
test("archive covers original files and rejects conflicting snapshots atomically", async () => {
  const entry = snapshot("2026-09-05T193245");
  assert.equal(entry.data.results.length, 75);
  assert.ok(entry.artifacts.find((a) => a.path === "raw/go.json"));
  const calls = [];
  const client = {
    query: async (sql) => {
      calls.push(sql);
      return {
        rows: sql.startsWith("SELECT snapshot")
          ? [{ snapshot_sha256: "different" }]
          : [],
      };
    },
  };
  await assert.rejects(
    () => importSnapshot(client, entry),
    /Immutable snapshot changed/,
  );
  assert.equal(calls.at(-1), "ROLLBACK");
  assert.ok(!calls.some((c) => c.startsWith("INSERT")));
});

test("pinned pg enables certificate-verified TLS from the Argo environment", () => {
  // Separate process avoids changing other tests' connection environment.
  const ssl = JSON.parse(
    execFileSync(
      process.execPath,
      [
        "--input-type=module",
        "-e",
        'import pg from "pg"; console.log(JSON.stringify(new pg.Client().ssl));',
      ],
      { env: { ...process.env, PGSSLMODE: "verify-full" }, encoding: "utf8" },
    ),
  );
  assert.equal(ssl, true); // Node TLS verifies certificates by default; no rejectUnauthorized:false.
});
