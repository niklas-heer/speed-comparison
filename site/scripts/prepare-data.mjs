import { cpSync, mkdirSync, rmSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { historyDir, runs, runData } from "../src/lib/results.mjs";
// Validate before copying. No network access or live database is needed to build.
for (const run of runs) runData(run.id);
const destination = fileURLToPath(
  new URL("../public/history/", import.meta.url),
);
rmSync(destination, { recursive: true, force: true });
mkdirSync(destination, { recursive: true });
cpSync(historyDir, destination, { recursive: true });
console.log(`Validated and copied ${runs.length} historical snapshots.`);
