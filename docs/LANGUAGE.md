# Monkey-RS Language Specification

This document describes the implemented Monkey-RS language. It is intended to be the user-facing language contract for syntax and runtime behavior, not an implementation design document.

## Source Files

Monkey-RS source files conventionally use the `.monkey` extension.

Programs are parsed as a sequence of statements. A semicolon is required after most simple statements. Block statements use braces.

Line comments start with `//` and continue to the end of the line.

```monkey
// comment
let value = 1;
```

## Values

Monkey-RS is dynamically typed. Variables can be reassigned to values of different runtime types.

Supported user-visible value categories:

- `int`
- `float`
- `bool`
- `string`
- `null`
- `list`
- `map`
- function / closure
- struct type
- struct instance
- module namespace

`null` is the user-visible absence value. Missing map keys and no-result calls evaluate to `null`.

The VM also has an internal empty/no-result marker for statement execution. It is not a normal value users construct.

## Literals

```monkey
let i = 12;
let f = 1.25;
let b = true;
let n = null;
let s = "hello";
let xs = [1, 2, 3];
let user = {"name": "tom", "age": 18};
```

Map keys may also use record-style shorthand syntax:

```monkey
let user = {name: "tom", age: 18};
```

Record-style maps are maps, not a separate runtime type.

## Variables

Variables are declared with `let`.

```monkey
let value = 1;
value = "one";
```

Redeclaring the same name in the same scope is a name error. Inner scopes may shadow outer names.

```monkey
let value = 1;
{
    let value = 2;
}
```

Assignment updates the nearest existing binding.

## Blocks and Scopes

Blocks are surrounded by `{}`. Blocks introduce lexical scopes, including standalone blocks, `if`, `while`, and `for` bodies.

```monkey
let value = 1;
{
    let local = 2;
}
```

Names declared inside a block are not visible after the block exits.

## Expression Results

If the final top-level statement or final statement in a block is an expression without a semicolon, that expression becomes the result.

```monkey
fn answer() {
    42
}
```

If an expression statement ends with a semicolon, its value is discarded.

```monkey
1;
2
```

The program result is `2`.

## Functions and Closures

Functions are values.

```monkey
fn add(x, y) {
    x + y
}

let f = add;
f(1, 2);
```

Functions can capture outer variables.

```monkey
fn make_adder(x) {
    fn add(y) {
        x + y
    }
    add
}
```

`return` exits the current function.

```monkey
fn first(value) {
    return value;
}
```

When there is no explicit `return`, the final un-terminated expression is the function result.

## Conditionals

`if` is an expression.

```monkey
let value = if score > 10 {
    "high"
} else {
    "low"
};
```

`else if` chains are supported.

```monkey
if value < 0 {
    "negative"
} else if value == 0 {
    "zero"
} else {
    "positive"
}
```

Boolean operators require boolean operands.

## Loops

`while` loops execute while the condition is true.

```monkey
let i = 0;
while i < 10 {
    i = i + 1;
}
```

`for in` iterates lists and maps.

```monkey
for i, value in [10, 20] {
    print(i, ":", value);
}

for key, value in {name: "tom"} {
    print(key, value);
}
```

The first loop variable is the list index or map key. The optional second variable is the value.

Loop variables are scoped to each iteration body. `break;` exits the nearest loop. `continue;` advances the nearest loop.

## Operators

Supported infix operators:

- assignment: `=`
- boolean: `&&`, `||`
- equality: `==`, `!=`
- comparison: `<`, `<=`, `>`, `>=`
- arithmetic: `+`, `-`, `*`, `/`

Supported prefix operators:

- `!`
- unary `-`

Typical precedence, from low to high:

1. assignment
2. `||`
3. `&&`
4. equality
5. comparison
6. addition/subtraction
7. multiplication/division
8. prefix
9. call, index, member access, struct literal suffix

Parentheses group expressions.

```monkey
let value = (1 + 2) * 3;
```

`int` and `float` mixed arithmetic is supported for `+`, `-`, `*`, and `/`. Division by zero is a runtime error.

String concatenation uses `+`. Other string conversions should use `str(value)`.

## Lists

Lists are mutable.

```monkey
let values = [1, 2];
append(values, 3);
values[1] = 20;
```

List indexing is zero-based. Out-of-range indexes are index errors.

## Maps and Record-Style Access

Maps are mutable key-value collections.

```monkey
let user = {name: "tom", age: 18};
user.age = user.age + 1;
user["name"] = "ann";
```

Map keys can be integers, booleans, or strings. Dot access uses string keys.

Missing map keys evaluate to `null`.

## Structs

Structs declare a fixed set of fields.

```monkey
struct User {
    name;
    age;
}
```

Named initialization:

```monkey
let user = User{name: "tom", age: 18};
```

Positional initialization:

```monkey
let user = User{"tom", 18};
```

Struct fields are mutable and may hold values of any runtime type.

```monkey
user.age = "18";
```

Missing fields default to `null`. Unknown field reads/writes are name errors. Duplicate named fields are name errors. Named and positional fields cannot be mixed in one struct literal.

Struct types can be imported through module namespaces:

```monkey
import models from "models.monkey";
let user = models.User{name: "tom", age: 18};
```

## Imports and Exports

Monkey-RS supports explicit exports.

```monkey
fn add_one(x) {
    x + 1
}
export add_one;
```

Namespace import:

```monkey
import math from "math.monkey";
math.add_one(1);
```

Bare import:

```monkey
import "math.monkey";
add_one(1);
```

Only explicitly exported bindings are visible to importers.

Import paths are string literals. Relative imports are resolved relative to the importing file. Imported modules are cached, and cyclic imports are import errors.

Standard library modules live under `stdlib/`.

```monkey
import json from "stdlib/json.monkey";
let result = json.json_parse("{\"ok\":true}");
```

## Errors

Program errors stop evaluation and include an error kind, source span, and call stack when available.

Common error categories include:

- syntax
- type
- name
- index
- runtime
- import
- assert
- panic

Expected external or data failures should use standard result maps instead of throwing runtime errors.

Standard result success:

```monkey
{ok: true, value: value, error: null}
```

Standard result failure:

```monkey
{ok: false, value: null, error: {kind: "io", code: "not_found", message: "..."}}
```

Examples of result-returning operations include file I/O, parsing helpers, HTTP/client failures, decode failures, process spawn failures, and terminal read failures. Invalid program usage, such as wrong argument types, remains a runtime error.

## Builtins

Global builtins include:

```text
print, len, byte_len, str, int, float, parse_int, parse_float,
append, delete, join, type,
is_null, is_bool, is_int, is_float, is_string, is_list, is_map,
args, read_line, panic, assert,
substr, find, replace, char_code, from_char_code
```

System, filesystem, path, time, math, HTTP, JSON, process, encoding, random, sort, and terminal helpers are exported through standard library modules rather than all being global names.

See `docs/STDLIB.md` for generated standard library module exports.

## Printing

`print` prints its arguments without inserting implicit spaces.

```monkey
print("name=", user.name, "\n");
```

Add spaces explicitly when needed.

## Formatting and Tooling

The implementation provides:

- source formatting
- format checks
- diagnostics JSON for editor integration
- symbol, completion, definition, and reference queries
- VM IR dump
- VM bytecode dump
- VM profiling
- VM benchmark summaries

Common commands:

```sh
cargo run -- examples/vm_demo.monkey
cargo run -- --format examples/stdlib_demo.monkey
cargo run -- --dump-bytecode examples/vm_demo.monkey
./scripts/bench-vm.sh
```

## Non-Goals in Current Version

The current language does not specify:

- static type checking
- classes or inheritance
- methods attached to structs
- package manager semantics
- concurrency primitives
- stable binary bytecode format

These may be added later, but they are not part of the current language contract.
