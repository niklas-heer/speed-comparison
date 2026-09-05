-- Private archive; the public Astro build reads immutable Git snapshots.
CREATE TABLE IF NOT EXISTS benchmark_runs (
  id text PRIMARY KEY,
  source_revision text,
  snapshot_sha256 text NOT NULL,
  metadata jsonb NOT NULL,
  imported_at timestamptz NOT NULL DEFAULT now()
);
CREATE TABLE IF NOT EXISTS benchmark_artifacts (
  run_id text NOT NULL REFERENCES benchmark_runs(id),
  path text NOT NULL,
  sha256 text NOT NULL,
  contents bytea NOT NULL,
  PRIMARY KEY (run_id, path)
);
CREATE TABLE IF NOT EXISTS benchmark_results (
  run_id text NOT NULL REFERENCES benchmark_runs(id),
  target text NOT NULL,
  language text NOT NULL,
  version text,
  median_seconds double precision NOT NULL CHECK (median_seconds > 0),
  rounds bigint,
  raw jsonb NOT NULL,
  PRIMARY KEY (run_id, target)
);
CREATE INDEX IF NOT EXISTS benchmark_results_target_idx ON benchmark_results(target, run_id);
CREATE INDEX IF NOT EXISTS benchmark_results_raw_idx ON benchmark_results USING gin(raw jsonb_path_ops);
