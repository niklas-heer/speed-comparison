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
