#!/usr/bin/env bash
# ############################################################################
# Copyright IBM Corporation 2026
#
# lint-report.sh — Run ansible-lint across every top-level directory and
# produce a self-contained simple HTML report.
#
# Compatible with bash 3.2+ (macOS default shell).
#
# Usage:
#   bash scripts/lint-report.sh [output.html]
#
# The report is written to the path given as the first argument, or to
# scripts/lint-report.html if omitted.
#
# Prerequisites:
#   pip install ansible-lint
#   ansible-galaxy collection install -r collections/requirements.yml

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/.." && pwd)"
export ANSIBLE_LIBRARY="${REPO_DIR}/zos_subsystems/ims/ims_provisioning/library"

REPORT="${1:-/tmp/lint-report.html}"
TIMESTAMP="$(date '+%Y-%m-%d %H:%M:%S %Z')"

# ── temp directory holds per-dir state files ──────────────────────────────────
# Each directory "name" gets two files:
#   $TMPDIR_STATE/name.status  — PASS | FAIL | SKIP
#   $TMPDIR_STATE/name.out     — raw ansible-lint output
TMPDIR_STATE="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_STATE"' EXIT

# Ordered list of directory names (plain indexed array, works on bash 3.2)
ALL_DIRS=()

# ── helpers ───────────────────────────────────────────────────────────────────

# Sanitise a directory name so it is safe as a filename.
safe_name() {
    printf '%s' "$1" | tr -c 'a-zA-Z0-9_-' '_'
}

# Escape text for safe embedding in HTML.
html_escape() {
    local s="$1"
    s="${s//&/&amp;}"
    s="${s//</&lt;}"
    s="${s//>/&gt;}"
    s="${s//\"/&quot;}"
    printf '%s' "$s"
}

# Read the status file for a directory.
dir_status() { cat "${TMPDIR_STATE}/$(safe_name "$1").status" 2>/dev/null || echo SKIP; }

# Count violation pairs in a directory's output file.
dir_vcount() {
    grep -c '^[a-zA-Z0-9_/\-]*\[[a-zA-Z0-9_\-]*\]:' \
        "${TMPDIR_STATE}/$(safe_name "$1").out" 2>/dev/null || echo 0
}

# ── run lint ──────────────────────────────────────────────────────────────────
for dir in "${REPO_DIR}"/*/; do
    [ -d "$dir" ] || continue
    name="$(basename "$dir")"
    sname="$(safe_name "$name")"

    ALL_DIRS+=("$name")

    # Skip directories that contain no YAML files.
    # Avoid any pipe here: "find | grep -q" and "find | head -1" both trigger
    # SIGPIPE under set -o pipefail, causing false negatives.
    # Write find output to a temp file and count lines instead.
    _yaml_check="$(mktemp)"
    find "$dir" -maxdepth 4 \( -name '*.yml' -o -name '*.yaml' \) \
        2>/dev/null > "$_yaml_check" || true
    _yaml_found="$(wc -l < "$_yaml_check" | tr -d ' ')"
    rm -f "$_yaml_check"
    if [ "$_yaml_found" -eq 0 ]; then
        printf 'SKIP' > "${TMPDIR_STATE}/${sname}.status"
        printf 'No YAML files found.' > "${TMPDIR_STATE}/${sname}.out"
        continue
    fi

    echo ""
    echo "==============================================="
    echo "  Entering: $dir"
    echo "==============================================="

    set +e
    (cd "$dir" && ansible-lint --nocolor * 2>&1) \
        > "${TMPDIR_STATE}/${sname}.out"
    exit_code=$?
    set -e

    if [ $exit_code -eq 0 ]; then
        printf 'PASS' > "${TMPDIR_STATE}/${sname}.status"
        echo "[PASS] $name"
    else
        printf 'FAIL' > "${TMPDIR_STATE}/${sname}.status"
        echo "[FAIL] $name"
    fi
done

# ── count totals ──────────────────────────────────────────────────────────────
pass_count=0; fail_count=0; skip_count=0
for name in "${ALL_DIRS[@]}"; do
    case "$(dir_status "$name")" in
        PASS) pass_count=$(( pass_count + 1 )) ;;
        FAIL) fail_count=$(( fail_count + 1 )) ;;
        SKIP) skip_count=$(( skip_count + 1 )) ;;
    esac
done
total=${#ALL_DIRS[@]}

# ── violation table emitter ───────────────────────────────────────────────────
# Reads the output file for $1 (directory name); writes an HTML table to stdout.
#
# ansible-lint emits violations as consecutive line pairs:
#   rule[tag]: Description of the problem
#   path/to/file.yml:LINE[:COL]  optional task context
violations_table() {
    local outfile="${TMPDIR_STATE}/$(safe_name "$1").out"
    local count=0
    local prev_rule="" prev_desc=""
    local rows="" file lineno ctx ctx_html

    while IFS= read -r line; do
        if echo "$line" | grep -qE '^[a-zA-Z0-9_/\-]+\[[a-zA-Z0-9_\-]+\]:'; then
            prev_rule="${line%%:*}"
            prev_desc="${line#*: }"
        elif [ -n "$prev_rule" ] && echo "$line" | grep -qE '^[^[:space:]]+\.ya?ml'; then
            file="${line%%:*}"
            local rest="${line#*:}"
            lineno="${rest%%[^0-9]*}"
            lineno="${lineno%%:*}"
            ctx="$(echo "$line" | sed 's/^[^[:space:]]*:[0-9]*:*[0-9]*[[:space:]]*//')"
            ctx_html=""
            [ -n "$ctx" ] && ctx_html="<span class=\"ctx\">$(html_escape "$ctx")</span>"
            rows="${rows}<tr>"
            rows="${rows}<td>$(html_escape "$file")</td>"
            rows="${rows}<td class=\"center\">$(html_escape "$lineno")</td>"
            rows="${rows}<td><code>$(html_escape "$prev_rule")</code></td>"
            rows="${rows}<td>$(html_escape "$prev_desc")${ctx_html}</td>"
            rows="${rows}</tr>"
            count=$(( count + 1 ))
            prev_rule=""; prev_desc=""
        else
            prev_rule=""; prev_desc=""
        fi
    done < "$outfile"

    if [ $count -eq 0 ]; then
        echo "<p class=\"muted\">No structured violations captured.</p>"
    else
        echo "<table class=\"violations\">"
        echo "  <thead><tr><th>File</th><th>Line</th><th>Rule</th><th>Description</th></tr></thead>"
        echo "  <tbody>${rows}</tbody>"
        echo "</table>"
    fi
}

# ── total violations across all failing dirs ──────────────────────────────────
total_violations=0
for name in "${ALL_DIRS[@]}"; do
    [ "$(dir_status "$name")" = "FAIL" ] || continue
    total_violations=$(( total_violations + $(dir_vcount "$name") ))
done

# ── build HTML ────────────────────────────────────────────────────────────────
{

cat <<'HTMLHEAD'
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Ansible Lint Report</title>
<style>
*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
body { font-family: -apple-system, "Segoe UI", system-ui, sans-serif; font-size: 14px; line-height: 1.6; background: #f7f8fa; color: #1f2328; }
.page { max-width: 900px; margin: 0 auto; padding: 32px 16px 64px; }
.report-header { border-bottom: 2px solid #e5e7eb; padding-bottom: 20px; margin-bottom: 28px; }
.report-header h1 { font-size: 22px; font-weight: 600; }
.report-header .meta { color: #57606a; font-size: 12px; margin-top: 4px; }
.summary-bar { display: flex; gap: 14px; margin-bottom: 24px; }
.summary-card { flex: 1; border: 1px solid #e5e7eb; border-radius: 6px; padding: 16px 12px; background: #fff; text-align: center; }
.summary-card .label { font-size: 11px; text-transform: uppercase; letter-spacing: .05em; color: #57606a; }
.summary-card .value { font-size: 36px; font-weight: 700; margin-top: 2px; }
.card-total .value { color: #1f2328; }
.card-pass  .value { color: #1a7f37; }
.card-fail  .value { color: #cf222e; }
.card-skip  .value { color: #9a6700; }
.progress-wrap { background: #e5e7eb; border-radius: 4px; height: 12px; overflow: hidden; margin-bottom: 24px; display: flex; }
.progress-pass { height: 100%; background: #1a7f37; }
.progress-fail { height: 100%; background: #cf222e; }
.progress-skip { height: 100%; background: #d4a017; }
h2 { font-size: 16px; font-weight: 600; margin: 36px 0 14px; border-bottom: 1px solid #e5e7eb; padding-bottom: 6px; }
.legend { display: flex; gap: 20px; flex-wrap: wrap; margin-bottom: 24px; padding: 12px 16px; background: #fff; border: 1px solid #e5e7eb; border-radius: 6px; }
.legend-item { display: flex; align-items: center; gap: 8px; font-size: 13px; color: #1f2328; }
.legend-swatch { width: 14px; height: 14px; border-radius: 3px; border: 1px solid transparent; flex-shrink: 0; }
.swatch-pass { background: #dafbe1; border-color: #82e0a0; }
.swatch-fail { background: #ffebe9; border-color: #f5b8ba; }
.swatch-skip { background: #fff8c5; border-color: #e6c84a; }
.legend-item strong { font-weight: 600; }
.legend-item span { color: #57606a; }
.total-violations { background: #fff; border: 1px solid #e5e7eb; border-left: 4px solid #cf222e; border-radius: 4px; padding: 10px 16px; margin-bottom: 32px; display: flex; align-items: baseline; gap: 10px; }
.total-violations .tv-count { font-size: 28px; font-weight: 700; color: #cf222e; line-height: 1; }
.total-violations .tv-label { font-size: 13px; color: #57606a; }
.dir-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 10px; margin-bottom: 40px; }
.dir-pill { border-radius: 6px; padding: 9px 13px; font-size: 13px; font-weight: 500; border: 1px solid transparent; display: flex; align-items: center; gap: 8px; }
.pill-pass { background: #dafbe1; border-color: #82e0a0; color: #1a7f37; }
.pill-fail { background: #ffebe9; border-color: #f5b8ba; color: #cf222e; }
.pill-skip { background: #fff8c5; border-color: #e6c84a; color: #7d5a00; }
.pill-count { margin-left: auto; background: rgba(0,0,0,.08); border-radius: 10px; padding: 1px 7px; font-size: 11px; }
.dir-block { margin-bottom: 28px; border-radius: 6px; overflow: hidden; border: 1px solid #f5b8ba; }
.dir-block h3 { font-size: 14px; font-weight: 600; background: #ffebe9; padding: 10px 14px; color: #cf222e; display: flex; align-items: center; gap: 8px; }
.dir-block h3 .badge { margin-left: auto; background: #cf222e; color: #fff; border-radius: 10px; padding: 1px 8px; font-size: 11px; font-weight: 600; }
.dir-block-body { padding: 14px; background: #fff; }
table.violations { width: 100%; border-collapse: collapse; font-size: 12px; }
table.violations th, table.violations td { text-align: left; padding: 6px 10px; border-bottom: 1px solid #e5e7eb; vertical-align: top; }
table.violations th { background: #f7f8fa; font-weight: 600; color: #57606a; text-transform: uppercase; font-size: 11px; letter-spacing: .04em; }
table.violations tbody tr:last-child td { border-bottom: none; }
table.violations tbody tr:nth-child(even) td { background: #fafbfc; }
table.violations .center { text-align: center; white-space: nowrap; }
table.violations code { background: #f0f6ff; border: 1px solid #c8dffe; border-radius: 3px; padding: 1px 5px; white-space: nowrap; font-size: 11px; color: #0969da; }
table.violations .ctx { color: #57606a; font-size: 11px; display: block; margin-top: 2px; }
.muted { color: #57606a; font-size: 12px; }
footer { text-align: center; font-size: 12px; color: #57606a; border-top: 1px solid #e5e7eb; padding-top: 16px; margin-top: 48px; }
</style>
</head>
<body>
<div class="page">
HTMLHEAD

# ── header ────────────────────────────────────────────────────────────────────
echo "<header class=\"report-header\">"
echo "  <h1>Ansible Lint Report</h1>"
echo "  <p class=\"meta\">Generated: $(html_escape "${TIMESTAMP}") &nbsp;&middot;&nbsp; Repository: $(html_escape "${REPO_DIR}") &nbsp;&middot;&nbsp; Directories: ${total}</p>"
echo "</header>"

# ── summary cards ─────────────────────────────────────────────────────────────
echo "<div class=\"summary-bar\">"
echo "  <div class=\"summary-card card-total\"><div class=\"label\">Total</div><div class=\"value\">${total}</div></div>"
echo "  <div class=\"summary-card card-pass\"><div class=\"label\">Pass</div><div class=\"value\">${pass_count}</div></div>"
echo "  <div class=\"summary-card card-fail\"><div class=\"label\">Fail</div><div class=\"value\">${fail_count}</div></div>"
echo "  <div class=\"summary-card card-skip\"><div class=\"label\">Skip</div><div class=\"value\">${skip_count}</div></div>"
echo "</div>"

# ── progress bar ──────────────────────────────────────────────────────────────
if [ "$total" -gt 0 ]; then
    pct_pass=$(( pass_count * 100 / total ))
    pct_fail=$(( fail_count * 100 / total ))
    pct_skip=$(( skip_count * 100 / total ))
else
    pct_pass=0; pct_fail=0; pct_skip=0
fi
echo "<div class=\"progress-wrap\">"
echo "  <div class=\"progress-pass\" style=\"width:${pct_pass}%\" title=\"Pass: ${pass_count}\"></div>"
echo "  <div class=\"progress-fail\" style=\"width:${pct_fail}%\" title=\"Fail: ${fail_count}\"></div>"
echo "  <div class=\"progress-skip\" style=\"width:${pct_skip}%\" title=\"Skip: ${skip_count}\"></div>"
echo "</div>"

# ── legend ────────────────────────────────────────────────────────────────────
cat <<'LEGEND'
<div class="legend">
  <div class="legend-item"><span class="legend-swatch swatch-pass"></span><strong>Green &mdash; Pass</strong>&nbsp;<span>All lint rules passed; no violations found.</span></div>
  <div class="legend-item"><span class="legend-swatch swatch-fail"></span><strong>Red &mdash; Fail</strong>&nbsp;<span>One or more lint violations were found. See the table below.</span></div>
  <div class="legend-item"><span class="legend-swatch swatch-skip"></span><strong>Yellow &mdash; Skip</strong>&nbsp;<span>Directory contains no YAML files and was not linted.</span></div>
</div>
LEGEND

# ── total violation count banner ──────────────────────────────────────────────
dirs_word="directories"
[ "$fail_count" -eq 1 ] && dirs_word="directory"
echo "<div class=\"total-violations\">"
echo "  <span class=\"tv-count\">${total_violations}</span>"
echo "  <span class=\"tv-label\">total violation(s) across ${fail_count} failing ${dirs_word}</span>"
echo "</div>"

# ── directory pill grid ───────────────────────────────────────────────────────
echo "<h2>Directories</h2>"
echo "<div class=\"dir-grid\">"
for name in "${ALL_DIRS[@]}"; do
    status="$(dir_status "$name")"
    case "$status" in
        PASS)
            echo "  <div class=\"dir-pill pill-pass\">&#10003; $(html_escape "$name")</div>"
            ;;
        FAIL)
            vcount="$(dir_vcount "$name")"
            echo "  <div class=\"dir-pill pill-fail\">&#10007; $(html_escape "$name")<span class=\"pill-count\">${vcount}</span></div>"
            ;;
        SKIP)
            echo "  <div class=\"dir-pill pill-skip\">&#8212; $(html_escape "$name")</div>"
            ;;
    esac
done
echo "</div>"

# ── per-directory violation tables ────────────────────────────────────────────
has_failures=false
for name in "${ALL_DIRS[@]}"; do
    [ "$(dir_status "$name")" = "FAIL" ] && { has_failures=true; break; }
done

if $has_failures; then
    echo "<h2>Violations by Directory</h2>"
    for name in "${ALL_DIRS[@]}"; do
        [ "$(dir_status "$name")" = "FAIL" ] || continue
        vcount="$(dir_vcount "$name")"
        echo "<div class=\"dir-block\">"
        echo "  <h3>&#10007; $(html_escape "$name")<span class=\"badge\">${vcount} violation(s)</span></h3>"
        echo "  <div class=\"dir-block-body\">"
        violations_table "$name"
        echo "  </div>"
        echo "</div>"
    done
fi

# ── footer ────────────────────────────────────────────────────────────────────
echo "</div>"
echo "<footer>Made with IBM Bob</footer>"
echo "</body>"
echo "</html>"

} > "${REPORT}"

# ── console summary ───────────────────────────────────────────────────────────
echo ""
echo "============================================="
echo "  Lint complete"
echo "  Pass: ${pass_count}  Fail: ${fail_count}  Skip: ${skip_count}  Total: ${total}"
echo "  Violations: ${total_violations}"
echo "  Report: ${REPORT}"
echo "============================================="

[ "$fail_count" -eq 0 ]
