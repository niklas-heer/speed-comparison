#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

norm_bool() {
  local v
  v="$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')"
  case "$v" in
    1|true|yes|on) echo "true" ;;
    *) echo "false" ;;
  esac
}

retry() {
  local attempts="$1"
  shift
  local n=1
  until "$@"; do
    if [[ "$n" -ge "$attempts" ]]; then
      return 1
    fi
    echo "Command failed (attempt $n/$attempts): $*"
    sleep $((n * 10))
    n=$((n + 1))
  done
}

LANGUAGES="${LANGUAGES:-rust bun deno lua ocaml racket sbcl julia}"
REGISTRY="${REGISTRY:-ghcr.io/niklas-heer/speed-comparison}"
BUILD_ONLY="$(norm_bool "${BUILD_ONLY:-false}")"
BENCHMARK_ONLY="$(norm_bool "${BENCHMARK_ONLY:-false}")"
PUSH_IMAGES="$(norm_bool "${PUSH_IMAGES:-true}")"
DRY_RUN="$(norm_bool "${DRY_RUN:-false}")"

if [[ "$BUILD_ONLY" == "true" && "$BENCHMARK_ONLY" == "true" ]]; then
  echo "BUILD_ONLY=true and BENCHMARK_ONLY=true cannot both be set."
  exit 1
fi

IFS=' ' read -r -a LANG_ARRAY <<< "$LANGUAGES"
if [[ "${#LANG_ARRAY[@]}" -eq 0 ]]; then
  echo "No languages specified."
  exit 1
fi

echo "Buildkite Dagger benchmark config:"
echo "  REGISTRY=$REGISTRY"
echo "  LANGUAGES=${LANG_ARRAY[*]}"
echo "  BUILD_ONLY=$BUILD_ONLY"
echo "  BENCHMARK_ONLY=$BENCHMARK_ONLY"
echo "  PUSH_IMAGES=$PUSH_IMAGES"
echo "  DRY_RUN=$DRY_RUN"

"$ROOT_DIR/.buildkite/scripts/bootstrap.sh"

if [[ -n "${GHCR_TOKEN:-}" ]]; then
  GHCR_USER="${GHCR_USER:-buildkite}"
  echo "$GHCR_TOKEN" | docker login ghcr.io -u "$GHCR_USER" --password-stdin
elif [[ -n "${GITHUB_TOKEN:-}" ]]; then
  GHCR_USER="${GHCR_USER:-buildkite}"
  echo "$GITHUB_TOKEN" | docker login ghcr.io -u "$GHCR_USER" --password-stdin
else
  echo "GHCR_TOKEN/GITHUB_TOKEN not set; continuing without docker login."
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "DRY_RUN=true, skipping build and benchmark commands."
  exit 0
fi

cd "$ROOT_DIR/dagger-poc"
uv sync --quiet

if [[ "$BENCHMARK_ONLY" != "true" ]]; then
  build_cmd=(uv run dagger run python build_images.py "${LANG_ARRAY[@]}")
  if [[ "$PUSH_IMAGES" == "true" ]]; then
    build_cmd+=(--push)
  fi
  echo "Building images..."
  retry 3 "${build_cmd[@]}"
fi

if [[ "$BUILD_ONLY" != "true" ]]; then
  failed=()
  for lang in "${LANG_ARRAY[@]}"; do
    echo "Benchmarking: $lang"
    if ! retry 3 uv run dagger run python benchmark.py "$lang"; then
      failed+=("$lang")
    fi
  done

  if [[ "${#failed[@]}" -gt 0 ]]; then
    echo "Benchmarks failed: ${failed[*]}"
    exit 1
  fi
fi

echo "Buildkite Dagger benchmark step complete."
