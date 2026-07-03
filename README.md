# Monkey-RS

Monkey programming language

Feature
* Supports closures, while loops, for in loops, break, continue, boolean operators (&&, ||, !), and if else expressions
* Values are dynamically typed, and variables can be reassigned to values of different runtime types
* Supports map, list, and usage similar to Python
* Supports Boolean, null, string, decimal, and integer types
* Supports import statements for loading Monkey standard library files, with relative path resolution, import cache, cycle detection, namespace import, and explicit exports: import json from "stdlib/json.monkey"; export json_parse;
* Supports lightweight record-style map syntax through map dot access and shorthand fields: {name: "tom"}, user.name, user.name = "tom". Records are maps, not a separate runtime type.
* Supports simple structs without inheritance or methods: struct User { name; age; }, User{name: "tom", age: 18}, User{"tom", 18}, and namespace-qualified literals such as models.User{name: "tom", age: 18}
* Runtime errors include structured ErrorKind, source spans, and a simple call stack
* Core global builtins include print, len, byte_len, append, delete, join, type, is_null, is_bool, is_int, is_float, is_string, is_list, is_map, args, read_line, panic, assert, substr, find, replace, char_code, from_char_code, parse_int, and parse_float
* System, filesystem, path, time, math, HTTP, JSON, process, encoding, random, and sort helpers are exported through stdlib modules instead of being global names
* Supports simple repl mode with multiline input for open (), {}, [], and strings
* Supports pretty-printing source with stable indentation: cargo run -- --format examples/stdlib_demo.monkey
* Supports format checks for CI: cargo run -- --format-check examples/stdlib_demo.monkey
* Includes a lightweight VS Code extension in editors/vscode for highlighting, diagnostics, formatting, snippets, completion, hover, go to definition, current-file references, and running the current file
* If there is no semicolon in the last statement within a block or at the top level, the value of the last expression will be used as the result
* print does not insert spaces between arguments; add spaces explicitly when needed
* null is the user-visible absence value; missing map keys and no-result calls evaluate to null
* Empty is an interpreter-internal no-result marker for statements, not a value users normally construct
* len(string) counts Unicode chars; byte_len(string) counts UTF-8 bytes
* int/float mixed arithmetic supports +, -, *, /, with division by zero errors
* Program errors such as syntax, type, name, index, assert, panic, and division errors stop evaluation with a structured runtime error
* Recoverable external/data failures return a standard result map: success is {ok: true, value: value, error: null}; failure is {ok: false, value: null, error: {kind, code, message}}

File helpers (stdlib/fs.monkey)
* read_file(path): return a result containing a UTF-8 file string
* write_file(path, content): return a result containing written byte count
* append_file(path, content): return a result containing written byte count
* file_exists(path): return whether a path exists
* read_dir(path): return a result containing sorted entry names
* mkdir(path): return a result after creating a directory and missing parents
* remove_file(path), remove_dir(path), copy_file(from, to), rename(from, to), metadata(path): return standard result maps

Path, env, math, time, terminal, HTTP, process, and encoding helpers
* path_join(...parts), path_dirname(path), path_basename(path), path_ext(path), path_exists(path), path_is_file(path), path_is_dir(path)
* env_get(name), env_set(name, value), cwd(), set_cwd(path), args(), exit(code). cwd(), set_cwd(path), and read_line() return standard result maps
* abs(value), floor(value), ceil(value), round(value), sqrt(value), pow(base, exponent), min(...values), max(...values)
* parse_int(value), parse_float(value): return standard result maps for string-to-number parsing; int(value) and float(value) remain direct conversions that report program errors on invalid input
* time_ms(), now_ms(), sleep(seconds), sleep_ms(milliseconds)
* terminal.clear(), terminal.home(), terminal.move(row, col), terminal.clear_line(), terminal.hide_cursor(), terminal.show_cursor(), terminal.enter_alt_screen(), terminal.leave_alt_screen(): cross-platform terminal display helpers returning null. row and col are zero-based.
* terminal.fg(color), terminal.bg(color), terminal.bold(), terminal.reset_style(): terminal style helpers. Colors include black, red, green, yellow, blue, magenta, cyan, white, gray, dark_gray, and bright_* variants such as bright_red.
* terminal.enable_raw_mode(), terminal.disable_raw_mode(), terminal.read_key(), terminal.read_key_timeout(ms), terminal.read_key_latest_timeout(ms), terminal.size(): interactive terminal helpers. read_key returns Result<{kind, key, ctrl, alt, shift}>, read_key_timeout returns value null on timeout, read_key_latest_timeout drains currently queued key events and returns the latest one, and size returns Result<{cols, rows}>.
* http_get(url, headers?), http_post(url, body, headers?), http_request(method, url, body, headers?): return a standard result. On success, value is {status, status_ok, headers, body}; HTTP 4xx/5xx responses keep ok=true and set value.status_ok=false
* exec(command, args?): return a standard result. On success, value is {success, status, stdout, stderr}; non-zero process exits keep ok=true and set value.success=false
* url_encode/url_decode, base64_encode/base64_decode, sha256. Decode helpers return standard result maps

Standard library
* import "stdlib/prelude.monkey"; provides range, sum, contains, sort, keys, values, get_or, and split
* stdlib/list.monkey provides range, sum, contains, and sort
* stdlib/map.monkey provides keys, values, and get_or
* stdlib/string.monkey provides split
* stdlib/io.monkey provides read_lines, returning a standard result map
* stdlib/fs.monkey exports file and directory builtins
* stdlib/path.monkey exports path helpers
* stdlib/env.monkey exports environment and process helpers
* stdlib/math.monkey exports numeric helpers
* stdlib/time.monkey exports clock and sleep helpers
* stdlib/terminal.monkey exports terminal display helpers
* stdlib/http.monkey exports the simple HTTP client
* stdlib/process.monkey exports process helpers
* stdlib/encoding.monkey exports URL, base64, and hash helpers
* stdlib/random.monkey exports random_int and random_float
* stdlib/json.monkey implements json_parse and json_stringify for objects, arrays, strings, numbers, booleans, structs, and null. json_parse returns a standard result map
* Run demos with:
  cargo run -- examples/stdlib_demo.monkey
  cargo run -- examples/json_parse_demo.monkey
  cargo run -- examples/lantern_rogue.monkey
  cargo run -- examples/heart_box_battle.monkey
  cargo run -- examples/lanternbound/main.monkey

Editor support
* On Unix/macOS, launch the VS Code extension development host with: ./scripts/run-vscode-extension.sh
* On Windows, build with cargo build, open editors/vscode in VS Code, set monkey.executablePath to the built monkey-rs.exe path, and run the Extension launch configuration
* The Unix/macOS launch script builds the interpreter and creates .vscode/settings.json on first run
* For best latency, set monkey.executablePath to target/debug/monkey-rs, or target/debug/monkey-rs.exe on Windows. If it is empty, the extension falls back to cargo run --quiet --
* The extension currently provides parse diagnostics, formatting, snippets, Rust-backed import path completion, module export completion, imported/local struct field completion, builtin/user symbol completion, hover details, go to definition, current-file references, syntax highlighting, and "Monkey: Run Current File"
* Run the current file from the command palette, editor title button, editor context menu, or Cmd+Alt+R on macOS / Ctrl+Alt+R on Windows and Linux

参考用Go语言自制解释器完成
