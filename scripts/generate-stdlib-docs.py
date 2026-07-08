#!/usr/bin/env python3
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
STDLIB = ROOT / "stdlib"
OUT = ROOT / "docs" / "STDLIB.md"

EXPORT_RE = re.compile(r"^\s*export\s+([A-Za-z_][A-Za-z0-9_]*)\s*;")
IMPORT_RE = re.compile(r'^\s*import\s+(?:[A-Za-z_][A-Za-z0-9_]*\s+from\s+)?"([^"]+)"\s*;')
FN_RE = re.compile(r"^\s*fn\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)")


def parse_module(path: Path):
    imports = []
    exports = []
    functions = {}
    lines = path.read_text(encoding="utf-8").splitlines()
    for line in lines:
        if match := IMPORT_RE.match(line):
            imports.append(match.group(1))
        if match := FN_RE.match(line):
            functions[match.group(1)] = match.group(2).strip()
        if match := EXPORT_RE.match(line):
            exports.append(match.group(1))
    return imports, exports, functions


def main():
    OUT.parent.mkdir(parents=True, exist_ok=True)
    modules = sorted(STDLIB.glob("*.monkey"))
    chunks = [
        "# Standard Library",
        "",
        "Generated from `stdlib/*.monkey` by `scripts/generate-stdlib-docs.py`.",
        "",
    ]
    for module in modules:
        imports, exports, functions = parse_module(module)
        chunks.append(f"## {module.name}")
        chunks.append("")
        if imports:
            chunks.append("Imports:")
            for item in imports:
                chunks.append(f"- `{item}`")
            chunks.append("")
        if exports:
            chunks.append("Exports:")
            for name in exports:
                params = functions.get(name)
                if params is None:
                    chunks.append(f"- `{name}`")
                else:
                    chunks.append(f"- `{name}({params})`")
            chunks.append("")
        else:
            chunks.append("No explicit exports.")
            chunks.append("")
    OUT.write_text("\n".join(chunks).rstrip() + "\n", encoding="utf-8")
    print(OUT)


if __name__ == "__main__":
    main()
