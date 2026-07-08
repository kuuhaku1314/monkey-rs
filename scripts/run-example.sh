#!/usr/bin/env sh
set -eu

EXAMPLE=${1:-examples/vm_demo.monkey}

cargo run -- "$EXAMPLE"
