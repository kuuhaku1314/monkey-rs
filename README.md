# Monkey-RS

Monkey-RS is a small dynamic scripting language implemented in Rust. The main
runtime is a register VM, with a formatter, standard library, language-service
features, VS Code extension, examples, VM inspection tools, and release scripts.

## Quick Start

```sh
cargo run -- examples/vm_demo.monkey
cargo run -- --version
```

Run a larger terminal game demo:

```sh
cargo run -- examples/lanternbound/main.monkey
```

## Install

```sh
cargo build
cargo build --release
cargo install --path .
monkey-rs --version
```

Release artifact helpers:

```sh
./scripts/release-build.sh
./scripts/release-build.ps1
```

## Common Commands

```sh
./scripts/run-example.sh examples/vm_demo.monkey
./scripts/bench-vm.sh
./scripts/profile-vm.sh examples/vm_demo.monkey
./scripts/dump-vm.sh ir examples/vm_demo.monkey
./scripts/dump-vm.sh bytecode examples/vm_demo.monkey

cargo run -- --format examples/stdlib_demo.monkey
cargo run -- --format-check examples/stdlib_demo.monkey
```

## Documentation

- [Language specification](docs/LANGUAGE.md)
- [Standard library](docs/STDLIB.md)
- [VM benchmark baseline](benchmarks/baseline.md)
- [VS Code extension guide](editors/vscode/README.md)

## Examples

- [examples/vm_demo.monkey](examples/vm_demo.monkey)
- [examples/stdlib_demo.monkey](examples/stdlib_demo.monkey)
- [examples/json_parse_demo.monkey](examples/json_parse_demo.monkey)
- [examples/lanternbound/main.monkey](examples/lanternbound/main.monkey)

## Developer Workflow

```sh
./scripts/check.sh
./scripts/test.sh
./scripts/ci.sh
./scripts/generate-stdlib-docs.sh
```

The GitHub Actions workflow in [.github/workflows/ci.yml](.github/workflows/ci.yml)
runs Rust formatting, checks, clippy, tests, and VS Code extension packaging
checks across Linux, macOS, and Windows.

## Editor Support

The VS Code extension lives in [editors/vscode](editors/vscode). On Unix/macOS,
launch the extension development host with:

```sh
./scripts/run-vscode-extension.sh
```

The extension provides syntax highlighting, diagnostics, formatting, snippets,
Rust-backed completion, hover, go to definition, current-file references, and a
command for running the current Monkey file.

## Status

This is an experimental language/toolchain. It is usable for local scripting,
stdlib experiments, terminal demos, and VM work, but it is not yet a stable
ecosystem with package management or compatibility guarantees.
