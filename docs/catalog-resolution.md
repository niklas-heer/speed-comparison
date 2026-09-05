# Resolving language declarations

Contributors keep editing `dagger-poc/languages.py`. A trusted driver can now resolve
that Python file inside a Dagger container and consume a validated JSON manifest.
There is no second manually maintained catalog and no change to the current
benchmark execution path. This is preparation for authorized selective PR execution;
automatic dispatch and the isolated persistent homelab engine remain rollout gates.

## Boundary and identity

The local CLI reads a regular UTF-8 file beneath an explicit source root. Directory
handles and no-follow opens reject symlinked files/parents before uploading data,
including links that could otherwise expose files outside an unreviewed checkout.

`catalog_resolver.py` uploads only the requested catalog file and the trusted
`catalog_export.py` helper. It uses a digest-pinned Python 3.12 image, Python isolated
mode, a 30-second export-process limit and a 120-second operation deadline. It does
not mount a host directory, Docker socket, caller environment or secret into the
export container. Python source is evaluated there; the calling process reads the
output file as data. Catalog stdout is not used as the JSON transport.
Both uppercase and lowercase standard proxy variables are explicitly set empty
before execution: Dagger otherwise inherits proxy settings from its engine, which
may include credentials. Explicit values suppress that inheritance in
[Dagger 0.19.8](https://github.com/dagger/dagger/blob/v0.19.8/engine/buildkit/executor_spec.go#L851).

`catalog_manifest.py` checks the returned identity against the caller's source SHA
and the independently computed hash of the supplied catalog. It rejects duplicate
JSON keys, unknown/missing fields, unsupported schemas, incorrect types, path
traversal, unpinned tooling and malformed package arguments. Source and output sizes
are limited to 4 MiB, with at most 512 targets. It reconstructs the trusted driver's
`Language` objects, preserving every field, compiler command and shared value.

The manifest contains a schema version, source revision, catalog SHA-256, resolved
tooling and all normalized language definitions. It is **not an authorization token**
or proof that an arbitrary checkout matches its declared revision. The dispatcher
must verify the exact commit snapshot, authorize that head revision, recompute the
affected-target plan and retain the resolved manifest with the run evidence.
Execution order belongs to the authorized run plan, not JSON object-key order.
A Python catalog can be dynamic; recording only its source hash is insufficient.

Use the driver and its dependencies from a trusted checkout, with the proposed
catalog supplied as a separate input file. Running a PR's replacement resolver on
a credentialed client would defeat this boundary. The current resolver supports the
single catalog file's standard-library imports, as used by all existing definitions;
additional local helper modules would require an explicit input contract.

Commands in the manifest remain executable build/run instructions for the isolated
engine. The engine isolation and revision-authorization requirements still apply.
The benchmark runner is not yet wired to consume this manifest, so tooling overrides
and selected-source binding must be implemented before that cutover.

## Local use and checks

From a trusted checkout, with an existing local Dagger-compatible runtime:

```sh
uv run --locked --project dagger-poc python dagger-poc/catalog_resolver.py \
  --source-root /path/to/verified-source \
  --catalog dagger-poc/languages.py \
  --source-revision FULL_VERIFIED_COMMIT_SHA \
  --output /tmp/catalog.json

uv run --locked --project dagger-poc --extra dev pytest dagger-poc -q
uv run --locked --project dagger-poc python dagger-poc/check_catalog_resolver.py
```

The unit suite checks lossless parity for all 75 targets, image fingerprints and
math/SIMD labels, plus invalid identities, schemas, paths and pins. It does not need
Docker. The optional real-Dagger check verifies the full catalog and uses an authored
fixture with a file side effect, client-environment probe and non-JSON stdout. It
checks that the side effect stays off the client filesystem, the client value is not
forwarded, and invalid exported paths are rejected. A second container starts with
fake credentialed proxy values and verifies that the resolver clears every standard
proxy variable. This checks explicit overrides without modifying the shared engine's
configuration; it does not simulate a separately configured proxy engine. This exercises the implemented
boundary; it does not validate the future homelab deployment or run any benchmarks.

The [recorded integration evidence](validation/2026-09-05-catalog-resolution.json)
includes the source/catalog identity, driver hashes, resolver image, SDK version
and observed checks. The complete Python suite has 129 passing tests.
