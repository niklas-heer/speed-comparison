import { readFileSync, existsSync } from "node:fs";
import { createHash } from "node:crypto";
import path from "node:path";
export const historyDir = path.resolve(process.cwd(), "../docs/history");
export const repo = "https://github.com/niklas-heer/speed-comparison";
export const readJSON = (p) => JSON.parse(readFileSync(p, "utf8"));
export const manifest = readJSON(path.join(historyDir, "manifest.json"));
export const runs = manifest.runs;
export const latest = runs[0];
export function validId(id) {
  if (!/^\d{4}-\d{2}-\d{2}T\d{6}$/.test(id)) throw new Error("Invalid run ID");
  return id;
}
export function seconds(value) {
  const n = Number(String(value).replace(/s$/, ""));
  if (!Number.isFinite(n) || n <= 0) throw new Error("Invalid timing");
  return n;
}
export function timing(n) {
  return n < 1
    ? `${(n * 1000).toFixed(2)} ms`
    : `${n.toFixed(n < 10 ? 3 : 2)} s`;
}
export function recordedTiming(value) {
  return value === undefined || value === null || value === ""
    ? "Not recorded"
    : timing(seconds(value));
}
export function runData(id) {
  const dir = path.join(historyDir, validId(id));
  const file = path.join(dir, "combined_results.json");
  const storedSummary = existsSync(file) ? readJSON(file) : [];
  // Older exports lacked target identities; retain their downloadable summary,
  // but do not invent target mappings from display names.
  const summary = storedSummary.some((row) => !row.target) ? [] : storedSummary;
  const metaFile = path.join(dir, "run_metadata.json");
  const metadata = existsSync(metaFile) ? readJSON(metaFile) : {};
  const sourceFile = path.join(dir, "source-revision.txt");
  const revision = existsSync(sourceFile)
    ? readFileSync(sourceFile, "utf8").trim()
    : null;
  const seen = new Set();
  const results = summary
    .map((row) => {
      if (!/^[a-z0-9][a-z0-9-]*$/.test(row.target) || seen.has(row.target))
        throw new Error("Invalid or duplicate target");
      seen.add(row.target);
      const rawPath = path.join(dir, "raw", `${row.target}.json`);
      const rawBytes = existsSync(rawPath) ? readFileSync(rawPath) : null;
      const raw = rawBytes ? JSON.parse(rawBytes) : null;
      if (raw && raw.Target !== row.target)
        throw new Error("Raw/summary target mismatch");
      const median = raw ? seconds(raw.Median) : row.median / 1000; // Historical summaries use milliseconds.
      if (!Number.isFinite(median) || median <= 0)
        throw new Error("Invalid summary timing");
      return {
        target: row.target,
        name: row.name,
        version: row.version,
        median,
        category: raw?.Category ?? row.category ?? "unrecorded",
        math: raw?.MathMode ?? row.math_mode ?? "unrecorded",
        simd: raw?.ExplicitSIMD ?? row.explicit_simd ?? null,
        algorithm: raw?.Algorithm ?? row.algorithm ?? "unrecorded",
        rounds: raw?.Rounds ?? row.rounds ?? null,
        raw,
        summary: row,
        sha256: rawBytes
          ? createHash("sha256").update(rawBytes).digest("hex")
          : null,
      };
    })
    .sort((a, b) => a.median - b.median);
  return {
    id,
    metadata,
    revision,
    results,
    files: [
      "combined_results.csv",
      "combined_results.json",
      "combined_results.png",
      "run_metadata.json",
      "source-revision.txt",
      "raw/SHA256SUMS",
    ].filter((f) => existsSync(path.join(dir, f))),
  };
}
export const current = runData(latest.id);
export const categories = [
  ...new Set(current.results.map((r) => r.category)),
].sort();
