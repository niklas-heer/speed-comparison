import { readFileSync, readdirSync } from "node:fs";
import { createHash } from "node:crypto";
import { fileURLToPath } from "node:url";
import path from "node:path";
import pg from "pg";
import { historyDir, runs, runData } from "../src/lib/results.mjs";
const hash = (bytes) => createHash("sha256").update(bytes).digest("hex");
export function snapshot(id) {
  const data = runData(id);
  const dir = path.join(historyDir, id);
  const walk = (base, prefix = "") =>
    readdirSync(base, { withFileTypes: true })
      .sort((a, b) => a.name.localeCompare(b.name))
      .flatMap((entry) => {
        const relative = prefix + entry.name;
        const absolute = path.join(base, entry.name);
        if (entry.isSymbolicLink())
          throw new Error("Symlinks are not archive artifacts");
        if (entry.isDirectory()) return walk(absolute, relative + "/");
        const contents = readFileSync(absolute);
        return [{ path: relative, contents, sha256: hash(contents) }];
      });
  const artifacts = walk(dir);
  return {
    data,
    artifacts,
    digest: hash(JSON.stringify(artifacts.map((a) => [a.path, a.sha256]))),
  };
}
export async function importSnapshot(client, entry) {
  const { data, artifacts, digest } = entry;
  await client.query("BEGIN");
  try {
    await client.query(
      "SELECT pg_advisory_xact_lock(hashtext('speed-comparison-archive'))",
    );
    const existing = await client.query(
      "SELECT snapshot_sha256 FROM benchmark_runs WHERE id=$1",
      [data.id],
    );
    if (existing.rows.length) {
      if (existing.rows[0].snapshot_sha256 !== digest)
        throw new Error(`Immutable snapshot changed: ${data.id}`);
      // Check that an interrupted or externally altered archive cannot pass as complete.
      const saved = await client.query(
        "SELECT path,sha256,contents FROM benchmark_artifacts WHERE run_id=$1 ORDER BY path",
        [data.id],
      );
      const expected = new Map(artifacts.map((a) => [a.path, a.sha256]));
      if (
        saved.rows.length !== artifacts.length ||
        saved.rows.some(
          (a) =>
            expected.get(a.path) !== a.sha256 || hash(a.contents) !== a.sha256,
        )
      )
        throw new Error(`Archive integrity mismatch: ${data.id}`);
      await client.query("COMMIT");
      return false;
    }
    await client.query(
      "INSERT INTO benchmark_runs(id,source_revision,snapshot_sha256,metadata) VALUES($1,$2,$3,$4)",
      [data.id, data.revision, digest, data.metadata],
    );
    for (const a of artifacts)
      await client.query(
        "INSERT INTO benchmark_artifacts(run_id,path,sha256,contents) VALUES($1,$2,$3,$4)",
        [data.id, a.path, a.sha256, a.contents],
      );
    for (const r of data.results)
      if (r.raw)
        await client.query(
          "INSERT INTO benchmark_results(run_id,target,language,version,median_seconds,rounds,raw) VALUES($1,$2,$3,$4,$5,$6,$7)",
          [data.id, r.target, r.name, r.version, r.median, r.rounds, r.raw],
        );
    await client.query("COMMIT");
    return true;
  } catch (error) {
    await client.query("ROLLBACK");
    throw error;
  }
}
async function main() {
  const mode = process.argv[2];
  if (!["plan", "import"].includes(mode))
    throw new Error("Usage: npm run archive -- plan|import");
  if (mode === "plan") {
    console.log(
      JSON.stringify(
        runs.map((r) => {
          const s = snapshot(r.id);
          return {
            id: r.id,
            artifacts: s.artifacts.length,
            results: s.data.results.filter((r) => r.raw).length,
            sha256: s.digest,
          };
        }),
        null,
        2,
      ),
    );
    return;
  }
  // Credentials are passed only through the environment, never CLI arguments.
  if (
    !process.env.PGHOST ||
    !process.env.PGDATABASE ||
    !process.env.PGUSER ||
    !process.env.PGPASSWORD
  )
    throw new Error("PGHOST, PGDATABASE, PGUSER and PGPASSWORD are required");
  const client = new pg.Client({
    connectionTimeoutMillis: 10000,
    application_name: "speed-comparison-archive",
    statement_timeout: 30000,
  });
  await client.connect();
  try {
    await client.query(
      readFileSync(new URL("./archive.sql", import.meta.url), "utf8"),
    );
    let imported = 0;
    for (const run of runs)
      if (await importSnapshot(client, snapshot(run.id))) imported++;
    console.log(JSON.stringify({ imported, verified: runs.length }));
  } finally {
    await client.end();
  }
}
if (process.argv[1] === fileURLToPath(import.meta.url))
  main().catch((error) => {
    console.error(error.message);
    process.exitCode = 1;
  });
