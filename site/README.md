# Inspectable benchmark website

Astro 7 builds static pages from the committed `docs/history` snapshots. Run from
this directory with Node 24:

```sh
npm ci
npm test
npm run build
npm run test:browser
npm run dev
```

Vercel uses the repository-root `vercel.json` (project root must remain the repo
root, not `site/`). It needs no PostgreSQL credential. The public site is
https://speed-comparison.vercel.app. Original `/history/...` download paths are
preserved. Published Git snapshots remain the portable source of truth.

## Editing the journal

Add a Markdown file to `src/content/journal/` with `title`, ISO `date`, `category`
and `summary` frontmatter. Filename becomes its URL. Record what changed, why,
validation and remaining limitations; do not describe a planned rollout as live.

## Private Postgres archive

The optional archive stores original artifacts as bytes and indexes raw target
JSON in JSONB. It never publishes benchmark results and never promotes partial
runs. Database failure does not prevent the site from serving existing snapshots.

```sh
npm run archive -- plan
# Inside the homelab or over an operator port-forward, using 1Password-backed
# PGHOST, PGPORT, PGDATABASE, PGUSER and PGPASSWORD environment variables:
npm run archive -- import
```

`archive.sql` creates three application-owned tables. Import is transactional per
snapshot, serialized with an advisory lock and idempotent. An existing run ID with
different artifacts is rejected instead of overwritten. Repeated import checks
stored artifact hashes against original bytes. The private database lives in the
shared CNPG cluster and uses its existing backup policy. Git remains an independent
recovery source. Never put database credentials into Vercel or client JavaScript.

## Evidence and limitations

The latest 75-target baseline is source c71e4cd, measured on September 5 with the
native adapter, 1b terms and its historical six-execution protocol. New Dagger
protocols do not rewrite it. Missing legacy target IDs cannot be safely inferred
from display names: those snapshots retain their original downloadable summaries.
Raw times are seconds; old summary times are milliseconds. Pages use raw precision
where available, and download checksums refer to the original files.

## Source, duration and share images

New Dagger results include `SourceFiles` (pre-compilation UTF-8 source and SHA-256),
`SourceFile`, the measurement profile, explicit timeout setting, tooling and SDK/engine
versions. Directory-based inputs include the copied source directory. Environment
collection records OS/libc, CPU information, cgroup limits and process limits where
exposed. Empty historical observations remain unknown.

`run.json` records UTC start/end and `elapsed_seconds` across the Dagger invocation,
including connection, catalog resolution and teardown. This is different from
`total_wall_seconds`, the preparation/measurement phases. The analyzer binds the
record to the exact suite IDs, source and target set before putting it under
`run_metadata.json.execution`. Analyze a bundle's `targets/` directory to discover
its sibling `run.json` automatically. Publication retains the record and updates
the README's marked latest-run block. Scheduler queueing/publication remain outside
that Dagger clock and must be recorded separately by the future Argo adapter.

Historical recovery is separate from immutable `docs/history` artifacts:

```sh
python scripts/export_source.py 2026-09-05T193245
uv run --locked python scripts/render_share_image.py 2026-09-05T193245
```

Run these from the repository root. Source export reads the measured full Git SHA
and uses AST declarations; it does not execute historical Python. Supplements in
`docs/report-evidence/RUN/` carry provenance, source checksums and retained scheduler
timestamps/settings. The site checks revision and source hashes before rendering.
Unsupported highlighting grammars retain readable plain text. Original archived
snapshots and their database digests are unchanged; supplements are retained in Git
and downloadable separately. Future embedded evidence is included in normal archives.

The refreshed PNG in `docs/report-images/RUN.png` is presentation derived from raw
medians, observed min/max and math/SIMD labels. Original historical images remain
available. `/report-images/latest.png` follows the latest published snapshot at each
site build, falling back to its original chart when no refreshed image exists.
