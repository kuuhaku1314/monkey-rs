#!/usr/bin/env sh
set -eu

KIND=${1:-bytecode}
FILE=${2:-examples/vm_demo.monkey}

case "$KIND" in
    ir)
        cargo run -- --dump-vm-ir "$FILE"
        ;;
    bytecode)
        cargo run -- --dump-bytecode "$FILE"
        ;;
    *)
        echo "Usage: $0 [ir|bytecode] [file]" >&2
        exit 2
        ;;
esac
