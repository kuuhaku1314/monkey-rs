#!/usr/bin/env sh
set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT_DIR"

VERSION=$(cargo metadata --no-deps --format-version 1 | python3 -c 'import json,sys; print(json.load(sys.stdin)["packages"][0]["version"])')
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)
DIST_DIR="$ROOT_DIR/dist"
BIN_NAME="monkey-rs"

mkdir -p "$DIST_DIR"
cargo build --release

TARGET_NAME="monkey-rs-${VERSION}-${OS}-${ARCH}"
cp "$ROOT_DIR/target/release/$BIN_NAME" "$DIST_DIR/$TARGET_NAME"
chmod +x "$DIST_DIR/$TARGET_NAME"
echo "$DIST_DIR/$TARGET_NAME"
