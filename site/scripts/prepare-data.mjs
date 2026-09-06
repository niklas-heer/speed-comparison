import { cpSync, mkdirSync, rmSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import {
  historyDir,
  evidenceDir,
  imagesDir,
  runs,
  runData,
  current,
} from "../src/lib/results.mjs";
// Validate before copying. No network access or live database is needed to build.
for (const run of runs) runData(run.id);
const destination = fileURLToPath(
  new URL("../public/history/", import.meta.url),
);
rmSync(destination, { recursive: true, force: true });
mkdirSync(destination, { recursive: true });
cpSync(historyDir, destination, { recursive: true });
for (const [name, source] of [
  ["report-evidence", evidenceDir],
  ["report-images", imagesDir],
]) {
  const target = fileURLToPath(new URL(`../public/${name}/`, import.meta.url));
  rmSync(target, { recursive: true, force: true });
  mkdirSync(target, { recursive: true });
  if (existsSync(source)) cpSync(source, target, { recursive: true });
}
// Stable README image follows the latest published run, including future runs.
cpSync(
  fileURLToPath(new URL(`../public${current.chart}`, import.meta.url)),
  fileURLToPath(new URL("../public/report-images/latest.png", import.meta.url)),
);
console.log(`Validated and copied ${runs.length} historical snapshots.`);
