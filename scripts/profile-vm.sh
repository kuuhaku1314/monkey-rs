#!/usr/bin/env sh
set -eu

FILE=${1:-examples/vm_demo.monkey}

cargo run -- --profile-vm "$FILE"
