#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/gh-run.sh <run-id-or-url> [--repo OWNER/REPO] [--logs] [--all-logs] [--artifacts]

Examples:
  scripts/gh-run.sh 20418837558
  scripts/gh-run.sh https://github.com/niklas-heer/speed-comparison/actions/runs/20418837558 --logs
  scripts/gh-run.sh 20418837558 --artifacts

Options:
  --repo OWNER/REPO   Override repo (default: inferred from git/gh)
  --logs              Save logs for failed jobs to test_output/gh-logs/<run-id>/
  --all-logs          Save logs for all jobs (can be large)
  --artifacts         Download workflow artifacts to test_output/gh-logs/<run-id>/artifacts/
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" || "${1:-}" == "" ]]; then
  usage
  exit 0
fi

run_input="$1"
shift

repo=""
save_failed_logs=0
save_all_logs=0
download_artifacts=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo)
      repo="${2:-}"
      shift 2
      ;;
    --logs)
      save_failed_logs=1
      shift
      ;;
    --all-logs)
      save_all_logs=1
      shift
      ;;
    --artifacts)
      download_artifacts=1
      shift
      ;;
    *)
      echo "Unknown arg: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

run_id="$run_input"
if [[ "$run_input" =~ /actions/runs/([0-9]+) ]]; then
  run_id="${BASH_REMATCH[1]}"
fi

gh_repo_args=()
if [[ -n "$repo" ]]; then
  gh_repo_args+=(--repo "$repo")
fi

gh run view "$run_id" "${gh_repo_args[@]}" \
  --json url,status,conclusion,workflowName,headBranch,headSha,createdAt,updatedAt \
  --jq '"\(.workflowName)  status=\(.status)  conclusion=\(.conclusion)\n\(.url)\nbranch=\(.headBranch)  sha=\(.headSha)\ncreated=\(.createdAt)  updated=\(.updatedAt)"'

failed_jobs="$(gh run view "$run_id" "${gh_repo_args[@]}" --json jobs --jq '.jobs[] | select(.conclusion=="failure") | "\(.databaseId)\t\(.name)"')"

if [[ -n "$failed_jobs" ]]; then
  echo ""
  echo "Failed jobs:"
  echo "$failed_jobs" | sed 's/\t/  /'
else
  echo ""
  echo "No failed jobs."
fi

out_dir="test_output/gh-logs/$run_id"

if [[ "$download_artifacts" -eq 1 ]]; then
  mkdir -p "$out_dir/artifacts"
  gh run download "$run_id" "${gh_repo_args[@]}" --dir "$out_dir/artifacts"
  echo ""
  echo "Artifacts downloaded to: $out_dir/artifacts"
fi

if [[ "$save_all_logs" -eq 1 ]]; then
  mkdir -p "$out_dir/logs"
  gh run view "$run_id" "${gh_repo_args[@]}" --json jobs --jq '.jobs[] | "\(.databaseId)\t\(.name)"' | while IFS=$'\t' read -r job_id job_name; do
    safe_name="$(echo "$job_name" | tr ' /' '__')"
    gh run view "$run_id" "${gh_repo_args[@]}" --job "$job_id" --log > "$out_dir/logs/${job_id}-${safe_name}.log"
    echo "Saved: $out_dir/logs/${job_id}-${safe_name}.log"
  done
fi

if [[ "$save_failed_logs" -eq 1 ]]; then
  mkdir -p "$out_dir/logs"
  if [[ -z "$failed_jobs" ]]; then
    echo ""
    echo "No failed job logs to save."
    exit 0
  fi
  echo "$failed_jobs" | while IFS=$'\t' read -r job_id job_name; do
    safe_name="$(echo "$job_name" | tr ' /' '__')"
    gh run view "$run_id" "${gh_repo_args[@]}" --job "$job_id" --log > "$out_dir/logs/${job_id}-${safe_name}.log"
    echo "Saved: $out_dir/logs/${job_id}-${safe_name}.log"
  done
fi
