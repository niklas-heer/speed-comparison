#!/usr/bin/env bash
set -euo pipefail

export PATH="$HOME/.local/bin:$PATH"
DAGGER_VERSION="${DAGGER_VERSION:-0.19.8}"

retry() {
  local attempts="$1"
  shift
  local n=1
  until "$@"; do
    if [[ "$n" -ge "$attempts" ]]; then
      return 1
    fi
    echo "Command failed (attempt $n/$attempts): $*"
    sleep $((n * 5))
    n=$((n + 1))
  done
}

ensure_uv() {
  if command -v uv >/dev/null 2>&1; then
    return 0
  fi
  retry 3 bash -lc "curl -LsSf https://astral.sh/uv/install.sh | sh"
}

ensure_dagger() {
  if command -v dagger >/dev/null 2>&1; then
    local ver
    ver="$(dagger version 2>/dev/null || true)"
    if [[ "$ver" == *"$DAGGER_VERSION"* ]]; then
      return 0
    fi
  fi
  retry 3 bash -lc "curl -fsSL https://dl.dagger.io/dagger/install.sh | DAGGER_VERSION=$DAGGER_VERSION BIN_DIR=$HOME/.local/bin sh"
}

ensure_docker() {
  if ! command -v docker >/dev/null 2>&1; then
    echo "docker is not installed on this agent."
    exit 1
  fi
  retry 3 docker version >/dev/null
}

ensure_uv
ensure_dagger
ensure_docker

echo "bootstrap complete: uv=$(uv --version) dagger=$(dagger version | head -n1)"
