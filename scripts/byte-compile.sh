#!/bin/bash

set -euo pipefail

. "$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"/include.sh

CHECK=

usage() {
    echo "usage: $0 [--check]" >&2
    echo "  --check:    Clean up byte-compiled files after running" >&2
    exit 1
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --check)
            CHECK=true; shift ;;
        *)
            echo "Error: Unknown option: $1" >&2
            echo >&2
            usage ;;
    esac
done

CLEANUP_DONE=
OUTPUT_FILE=

cleanup() {
    if [[ "$CLEANUP_DONE" = true ]]; then
        return
    fi
    CLEANUP_DONE=true

    if [[ -n "$OUTPUT_FILE" ]]; then
        rm -f "$OUTPUT_FILE"
    fi
    if [[ "$CHECK" = "true" ]]; then
        rm -f *.elc
    fi
}

trap cleanup EXIT INT TERM

OUTPUT_FILE=$(mktemp)
mod=eglot-python-preset

byte_compile "$mod" "$mod".el 2>&1 | tee "$OUTPUT_FILE"

output=$(< "$OUTPUT_FILE")
if [[ -z "$output" ]]; then
    exit 1
fi

non_compiling_lines=$(echo "$output" | grep -v "^Compiling " || true)

if [[ -n "$non_compiling_lines" ]]; then
    # bun has a bug where given exit code 2 it passes instead of failing
    exit 3
fi

exit 0
