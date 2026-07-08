# Standard Library

Generated from `stdlib/*.monkey` by `scripts/generate-stdlib-docs.py`.

## encoding.monkey

Exports:
- `url_encode`
- `url_decode`
- `base64_encode`
- `base64_decode`
- `sha256`

## env.monkey

Exports:
- `env_get`
- `env_set`
- `cwd`
- `set_cwd`
- `args`
- `exit`

## fs.monkey

Exports:
- `read_file`
- `write_file`
- `append_file`
- `file_exists`
- `read_dir`
- `mkdir`
- `remove_file`
- `remove_dir`
- `copy_file`
- `rename`
- `metadata`

## http.monkey

Exports:
- `http_get`
- `http_post`
- `http_request`

## io.monkey

Imports:
- `string.monkey`
- `fs.monkey`

Exports:
- `read_lines(path)`

## json.monkey

Exports:
- `json_parse`
- `json_stringify`

## list.monkey

Exports:
- `range(start, end)`
- `sum(values)`
- `contains(values, expected)`
- `sort`

## map.monkey

Exports:
- `keys(map)`
- `values(map)`
- `get_or(map, key, default_value)`

## math.monkey

Exports:
- `abs`
- `floor`
- `ceil`
- `round`
- `sqrt`
- `pow`
- `min`
- `max`

## path.monkey

Exports:
- `path_join`
- `path_dirname`
- `path_basename`
- `path_ext`
- `path_exists`
- `path_is_file`
- `path_is_dir`

## prelude.monkey

Imports:
- `list.monkey`
- `map.monkey`
- `string.monkey`

Exports:
- `range`
- `sum`
- `contains`
- `sort`
- `keys`
- `values`
- `get_or`
- `split`

## process.monkey

Exports:
- `exec`

## random.monkey

Exports:
- `random_int`
- `random_float`

## string.monkey

Exports:
- `split(text, separator)`

## terminal.monkey

Exports:
- `clear`
- `home`
- `hide_cursor`
- `show_cursor`
- `enable_raw_mode`
- `disable_raw_mode`
- `read_key`
- `read_key_timeout`
- `read_key_latest_timeout`
- `move`
- `clear_line`
- `size`
- `enter_alt_screen`
- `leave_alt_screen`
- `fg`
- `bg`
- `bold`
- `reset_style`
- `paint`
- `paint_runs`

## time.monkey

Exports:
- `time_ms`
- `now_ms`
- `sleep(seconds)`
- `sleep_ms`
