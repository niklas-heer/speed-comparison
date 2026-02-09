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

docker_login_with_token() {
  local user="$1"
  local token="$2"
  local attempts="${3:-2}"
  local n=1
  until printf '%s' "$token" | docker login ghcr.io -u "$user" --password-stdin; do
    if [[ "$n" -ge "$attempts" ]]; then
      return 1
    fi
    echo "Docker login failed (attempt $n/$attempts)."
    sleep $((n * 10))
    n=$((n + 1))
  done
}

detect_missing_targets() {
  local missing_output
  local check_log
  check_log="$(mktemp)"

  # check_images.py reads GITHUB_TOKEN for GHCR auth.
  if [[ "$REGISTRY" == ghcr.io/* && -n "${GHCR_TOKEN:-}" && -z "${GITHUB_TOKEN:-}" ]]; then
    export GITHUB_TOKEN="$GHCR_TOKEN"
  fi

  if ! missing_output="$(env REGISTRY="$REGISTRY" uv run python check_images.py "${LANG_ARRAY[@]}" 2>"$check_log")"; then
    echo "Unable to pre-check registry images; continuing without local fallback planning."
    tail -n 20 "$check_log" || true
    rm -f "$check_log"
    return 1
  fi
  rm -f "$check_log"

  if [[ -z "${missing_output//[[:space:]]/}" ]]; then
    echo "All selected targets have registry images."
    return 0
  fi

  for target in $missing_output; do
    MISSING_TARGETS["$target"]=1
  done
  echo "Missing registry images for selected targets: ${missing_output}"
}

LANGUAGES="${LANGUAGES:-all}"
REGISTRY_REPOSITORY="${REGISTRY_REPOSITORY:-${BUILDKITE_PIPELINE_SLUG:-speed-comparison}}"
if [[ -z "${REGISTRY:-}" ]]; then
  if [[ -n "${BUILDKITE_HOSTED_REGISTRY_URL:-}" ]]; then
    REGISTRY="${BUILDKITE_HOSTED_REGISTRY_URL%/}/${REGISTRY_REPOSITORY}"
  else
    REGISTRY="ghcr.io/niklas-heer/speed-comparison"
  fi
fi
BUILD_ONLY="$(norm_bool "${BUILD_ONLY:-false}")"
BENCHMARK_ONLY="$(norm_bool "${BENCHMARK_ONLY:-false}")"
PUSH_IMAGES="$(norm_bool "${PUSH_IMAGES:-false}")"
DRY_RUN="$(norm_bool "${DRY_RUN:-false}")"

if [[ "$BUILD_ONLY" == "true" && "$BENCHMARK_ONLY" == "true" ]]; then
  echo "BUILD_ONLY=true and BENCHMARK_ONLY=true cannot both be set."
  exit 1
fi

. "$ROOT_DIR/.buildkite/scripts/bootstrap.sh"

login_failed="false"
if [[ "$REGISTRY" == ghcr.io/* ]]; then
  if [[ -n "${GHCR_TOKEN:-}" ]]; then
    GHCR_USER="${GHCR_USER:-buildkite}"
    if ! docker_login_with_token "$GHCR_USER" "$GHCR_TOKEN" 2; then
      login_failed="true"
    fi
  elif [[ -n "${GITHUB_TOKEN:-}" ]]; then
    GHCR_USER="${GHCR_USER:-buildkite}"
    if ! docker_login_with_token "$GHCR_USER" "$GITHUB_TOKEN" 2; then
      login_failed="true"
    fi
  else
    echo "GHCR_TOKEN/GITHUB_TOKEN not set; continuing without docker login."
  fi
else
  echo "Registry is not GHCR; skipping docker login."
fi

if [[ "$login_failed" == "true" ]]; then
  if [[ "$PUSH_IMAGES" == "true" || "$BUILD_ONLY" == "true" ]]; then
    echo "Docker login to ghcr.io failed and this run needs authenticated image operations."
    exit 1
  fi
  echo "Docker login to ghcr.io failed; continuing without login for benchmark-only/no-push run."
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "DRY_RUN=true, skipping build and benchmark commands."
  exit 0
fi

cd "$ROOT_DIR/dagger-poc"
uv sync --quiet

if [[ "${LANGUAGES,,}" == "all" ]]; then
  mapfile -t LANG_ARRAY < <(uv run python - <<'PY'
from languages import LANGUAGES
for key in LANGUAGES:
    print(key)
PY
)
else
  IFS=' ' read -r -a LANG_ARRAY <<< "$LANGUAGES"
fi

if [[ "${#LANG_ARRAY[@]}" -eq 0 ]]; then
  echo "No languages specified."
  exit 1
fi

echo "Buildkite Dagger benchmark config:"
echo "  REGISTRY=$REGISTRY"
echo "  LANGUAGES=${LANG_ARRAY[*]}"
echo "  LANG_COUNT=${#LANG_ARRAY[@]}"
echo "  BUILD_ONLY=$BUILD_ONLY"
echo "  BENCHMARK_ONLY=$BENCHMARK_ONLY"
echo "  PUSH_IMAGES=$PUSH_IMAGES"
echo "  DRY_RUN=$DRY_RUN"

if [[ "$BENCHMARK_ONLY" != "true" ]]; then
  if [[ "$PUSH_IMAGES" == "true" || "$BUILD_ONLY" == "true" ]]; then
    build_cmd=(uv run dagger run python build_images.py "${LANG_ARRAY[@]}")
    if [[ "$PUSH_IMAGES" == "true" ]]; then
      build_cmd+=(--push)
    fi
    echo "Building images..."
    retry 3 "${build_cmd[@]}"
  else
    echo "Skipping image prebuild (PUSH_IMAGES=false and BUILD_ONLY=false)."
  fi
fi

if [[ "$BUILD_ONLY" != "true" ]]; then
  declare -A MISSING_TARGETS=()
  if [[ "$PUSH_IMAGES" != "true" ]]; then
    detect_missing_targets || true
  fi

  failed=()
  for lang in "${LANG_ARRAY[@]}"; do
    used_local_fallback="false"
    if [[ "${MISSING_TARGETS[$lang]:-0}" == "1" ]]; then
      echo "Benchmarking: $lang (local image fallback)"
      bench_cmd=(env REGISTRY="$REGISTRY" USE_LOCAL_IMAGES=1 uv run dagger run python benchmark.py "$lang")
      used_local_fallback="true"
    else
      echo "Benchmarking: $lang"
      bench_cmd=(env REGISTRY="$REGISTRY" uv run dagger run python benchmark.py "$lang")
    fi

    if ! retry 3 "${bench_cmd[@]}"; then
      if [[ "$used_local_fallback" != "true" ]]; then
        echo "Registry benchmark failed for $lang; retrying with local image fallback."
        fallback_cmd=(env REGISTRY="$REGISTRY" USE_LOCAL_IMAGES=1 uv run dagger run python benchmark.py "$lang")
        if retry 2 "${fallback_cmd[@]}"; then
          continue
        fi
      fi
      failed+=("$lang")
    fi
  done

  if [[ "${#failed[@]}" -gt 0 ]]; then
    echo "Benchmarks failed: ${failed[*]}"
    exit 1
  fi
fi

echo "Buildkite Dagger benchmark step complete."
