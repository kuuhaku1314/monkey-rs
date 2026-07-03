#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CODE_BIN="${CODE_BIN:-code}"

cd "$ROOT"

if ! command -v "$CODE_BIN" >/dev/null 2>&1; then
  echo "VS Code command '$CODE_BIN' was not found." >&2
  echo "Install the 'code' shell command from VS Code, or run with CODE_BIN=/path/to/code." >&2
  exit 1
fi

cargo build

mkdir -p "$ROOT/.vscode"
if [ ! -f "$ROOT/.vscode/settings.json" ]; then
  cat > "$ROOT/.vscode/settings.json" <<EOF
{
  "monkey.executablePath": "target/debug/monkey-rs",
  "monkey.cargoRoot": "$ROOT"
}
EOF
fi

"$CODE_BIN" \
  --extensionDevelopmentPath="$ROOT/editors/vscode" \
  "$ROOT"
