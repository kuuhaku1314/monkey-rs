import terminal from "stdlib/terminal.monkey";
import env from "stdlib/env.monkey";
let ESC = from_char_code(27);
fn println(text) {
    print(text + "\r\n")
}
fn configured_width() {
    let override = env.env_get("LANTERNBOUND_WIDTH");
    if !is_null(override) {
        let parsed = parse_int(override);
        if parsed.ok {
            return parsed.value;
        }
    }
    let size = terminal.size();
    if size.ok && !is_null(size.value) {
        return size.value.cols;
    }
    96
}
fn configured_height() {
    let override = env.env_get("LANTERNBOUND_HEIGHT");
    if !is_null(override) {
        let parsed = parse_int(override);
        if parsed.ok {
            return parsed.value;
        }
    }
    let size = terminal.size();
    if size.ok && !is_null(size.value) {
        return size.value.rows;
    }
    30
}
fn width() {
    let cols = configured_width() - 4;
    if cols < 30 {
        30
    } else if cols > 92 {
        92
    } else {
        cols
    }
}
fn height() {
    let rows = configured_height();
    if rows < 12 {
        12
    } else if rows > 40 {
        40
    } else {
        rows
    }
}
fn repeat(mark, count) {
    let text = "";
    let i = 0;
    while i < count {
        text = text + mark;
        i = i + 1;
    }
    text
}
fn pad_right(text, size) {
    let out = text;
    if len(out) > size {
        return substr(out, 0, size);
    }
    while len(out) < size {
        out = out + " ";
    }
    out
}
fn line(text) {
    println(pad_right(text, width()))
}
fn blank() {
    line("")
}
fn color_line(color, text) {
    terminal.fg(color);
    line(text);
    terminal.reset_style()
}
fn title(text) {
    terminal.bold();
    terminal.fg("bright_yellow");
    line(text);
    terminal.reset_style()
}
fn at(row, col, text) {
    terminal.move(row, col);
    print(text)
}
fn color_at(row, col, color, text) {
    terminal.move(row, col);
    terminal.fg(color);
    print(text);
    terminal.reset_style()
}
fn setup() {
    terminal.enter_alt_screen();
    print(ESC + "[?7l");
    terminal.enable_raw_mode();
    terminal.hide_cursor();
    terminal.clear();
    terminal.bg("black")
}
fn restore() {
    terminal.reset_style();
    print(ESC + "[?7h");
    terminal.show_cursor();
    terminal.disable_raw_mode();
    terminal.leave_alt_screen()
}
fn wait_key() {
    terminal.read_key()
}
fn key_name(key_result) {
    if key_result.ok && !is_null(key_result.value) {
        key_result.value.key
    } else {
        ""
    }
}
fn clear_tail(lines) {
    let i = 0;
    while i < lines {
        line("");
        i = i + 1;
    }
}
export println;
export width;
export height;
export repeat;
export pad_right;
export line;
export blank;
export color_line;
export title;
export at;
export color_at;
export setup;
export restore;
export wait_key;
export key_name;
export clear_tail;
