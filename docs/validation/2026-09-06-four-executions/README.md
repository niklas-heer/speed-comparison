# Four-execution smoke evidence

Local ARM Dagger engine, Go and Python, 10,000 terms. Both targets passed with
one warmup and three measured runs under `leibniz-1w-3m-v1`. Raw evidence records
cgroup limits and process limits. This verifies pipeline behavior, not x86_64
ranking stability or calibration of a shorter reporting workload.

Preparation 25.03 seconds; preparation plus measurement 27.45 seconds. Existing
engine caches were present. Driver hashes in run.json identify the tested code;
this check preceded the optional reporting environment contract integration.
