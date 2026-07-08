# Monkey RS VS Code Extension

This folder contains the local VS Code extension for Monkey RS.

## Development

From the repository root:

```sh
./scripts/run-vscode-extension.sh
```

On Windows, build the interpreter first:

```powershell
cargo build
```

Then open `editors/vscode` in VS Code, set `monkey.executablePath` to the built `monkey-rs.exe`, and run the Extension launch configuration.

## Package a VSIX

Install the VS Code extension packaging tool if needed:

```sh
npm install -g @vscode/vsce
```

Package from this directory:

```sh
cd editors/vscode
vsce package
```

The generated `.vsix` can be installed with:

```sh
code --install-extension monkey-rs-vscode-*.vsix
```

For local development, prefer `monkey.executablePath` pointing at `target/debug/monkey-rs` or `target/debug/monkey-rs.exe`.
