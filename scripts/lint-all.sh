#!/usr/bin/env bash
# ############################################################################
# Copyright IBM Corporation 2026
#
# lint-all.sh — Local equivalent of the ansible-lint GitHub Actions workflow.
# Mirrors: .github/workflows/ansible-lint.yml
#
# Compatible with bash 3.2+ (macOS default shell).
#
# Usage:
#   bash scripts/lint-all.sh [output-file]
#
#   output-file  Optional path. When supplied, all stdout and stderr from this
#                script is written to that file (and the parent directory is
#                created if it does not exist).  The console still shows
#                progress when a tty is attached; pass /dev/null to suppress.
#
# Examples:
#   bash scripts/lint-all.sh                      # output to terminal only
#   bash scripts/lint-all.sh /tmp/lint-all.txt    # output to file only
#   bash scripts/lint-all.sh /logs/ci/lint.log    # parent dir created if needed
#
# Prerequisites:
#   pip install ansible-lint
#   ansible-galaxy collection install -r collections/requirements.yml
# ############################################################################

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/.." && pwd)"
export ANSIBLE_LIBRARY="${REPO_DIR}/zos_subsystems/ims/ims_provisioning/library"

# ── optional output file ──────────────────────────────────────────────────────
if [ "${1:-}" != "" ]; then
    OUTPUT_FILE="$1"
    # Create parent directory if it does not exist
    OUTPUT_DIR="$(dirname "$OUTPUT_FILE")"
    [ -d "$OUTPUT_DIR" ] || mkdir -p "$OUTPUT_DIR"
    # Redirect both stdout and stderr of the entire script to the file
    exec > "$OUTPUT_FILE" 2>&1
fi

# ── run lint across every top-level directory ─────────────────────────────────
failed=""   # space-separated list of failed dir paths (no arrays needed)

for dir in "${REPO_DIR}"/*/; do
    [ -d "$dir" ] || continue

    echo ""
    echo "==============================================="
    echo "  Entering: $dir"
    echo "==============================================="
    echo ""

    set +e
    (cd "$dir" && ansible-lint *)
    exit_code=$?
    set -e

    if [ $exit_code -eq 0 ]; then
        echo "[PASS] $dir"
    else
        echo "[FAIL] $dir"
        failed="${failed} ${dir}"
    fi
done

# ── summary ───────────────────────────────────────────────────────────────────
if [ -n "$failed" ]; then
    echo ""
    echo "The following directories failed:"
    for d in $failed; do
        echo "  - $d"
    done
    exit 1
fi

echo ""
echo "All directories passed."
