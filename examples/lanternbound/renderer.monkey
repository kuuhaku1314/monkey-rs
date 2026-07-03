import terminal from "stdlib/terminal.monkey";
import env from "stdlib/env.monkey";
import ui from "ui.monkey";
import maps from "maps.monkey";
import tiles from "tiles.monkey";
import items from "items.monkey";
import story from "story.monkey";
import goals from "goals.monkey";
let ESC = from_char_code(27);
let RESET = ESC + "[0m";
fn ansi(color) {
    if color == "black" {
        ESC + "[30m"
    } else if color == "red" {
        ESC + "[31m"
    } else if color == "green" {
        ESC + "[32m"
    } else if color == "yellow" {
        ESC + "[33m"
    } else if color == "blue" {
        ESC + "[34m"
    } else if color == "magenta" {
        ESC + "[35m"
    } else if color == "cyan" {
        ESC + "[36m"
    } else if color == "dark_gray" {
        ESC + "[90m"
    } else if color == "bright_red" {
        ESC + "[91m"
    } else if color == "bright_green" {
        ESC + "[92m"
    } else if color == "bright_yellow" {
        ESC + "[93m"
    } else if color == "bright_blue" {
        ESC + "[94m"
    } else if color == "bright_magenta" {
        ESC + "[95m"
    } else if color == "bright_cyan" {
        ESC + "[96m"
    } else if color == "bright_white" {
        ESC + "[97m"
    } else {
        ESC + "[37m"
    }
}
fn paint(color, text) {
    ansi(color) + text + RESET
}
fn view_w() {
    let side = 0;
    if ui.width() >= 61 {
        side = 31;
    }
    let cells = (ui.width() - side) / tiles.width();
    if cells > 13 {
        13
    } else if cells < 6 {
        6
    } else {
        cells
    }
}
fn view_h() {
    6
}
fn map_width() {
    view_w() * tiles.width()
}
fn clamp(value, low, high) {
    if value < low {
        low
    } else if value > high {
        high
    } else {
        value
    }
}
fn camera_x(game) {
    clamp(game.x - view_w() / 2, 0, maps.area_w() - view_w())
}
fn camera_y(game) {
    clamp(game.y - view_h() / 2, 0, maps.area_h() - view_h())
}
fn ensure_cache(game) {
    if is_null(game.render_area) || game.render_area != game.area {
        game.render_area = game.area;
        game.render_entities = maps.entities(game.area);
        game.render_features = maps.features(game.area);
    }
}
fn seen(game, entity) {
    let value = game.flags[maps.key(game.area, entity.x, entity.y)];
    !is_null(value) && value == true
}
fn item_taken(game, entity) {
    let value = game.flags["item:" + entity.id];
    !is_null(value) && value == true
}
fn player_sprite(game) {
    if is_null(game.facing) {
        return "player_down";
    }
    if game.facing == "up" {
        "player_up"
    } else if game.facing == "left" {
        "player_left"
    } else if game.facing == "right" {
        "player_right"
    } else {
        "player_down"
    }
}
fn sprite_at(game, entities, features, x, y) {
    if game.x == x && game.y == y {
        return player_sprite(game);
    }
    cell_sprite_at(game, entities, features, x, y)
}
fn cell_sprite_at(game, entities, features, x, y) {
    let entity = maps.entity_in(entities, x, y);
    if !is_null(entity) {
        if entity.kind == "item" && item_taken(game, entity) {
            return maps.terrain_in(features, x, y);
        }
        if entity.kind == "foe" && seen(game, entity) {
            return "trace";
        }
        return entity.sprite;
    }
    maps.terrain_in(features, x, y)
}
fn target_hint(game, entity) {
    if is_null(entity) {
        return false;
    }
    if entity.kind == "npc" {
        return story.reward_name(entity.event) != "" && !story.has_flag(game, entity.event);
    }
    if entity.kind == "foe" {
        return !seen(game, entity);
    }
    if entity.kind == "item" {
        return !item_taken(game, entity);
    }
    if entity.kind == "gate" {
        return true;
    }
    false
}
fn cell_color_at(game, entities, features, x, y, id) {
    if game.x == x && game.y == y {
        return tiles.color(id);
    }
    let entity = maps.entity_in(entities, x, y);
    if target_hint(game, entity) {
        return "bright_yellow";
    }
    tiles.color(id)
}
fn portrait_id_at(game, entities, x, y) {
    let entity = maps.entity_in(entities, x, y);
    if is_null(entity) {
        return "";
    }
    if entity.kind == "item" && item_taken(game, entity) {
        return "";
    }
    if entity.kind == "foe" && seen(game, entity) {
        "trace"
    } else {
        entity.sprite
    }
}
fn portrait_kind(game, entities) {
    let here = portrait_id_at(game, entities, game.x, game.y);
    if here != "" {
        return here;
    }
    let up = portrait_id_at(game, entities, game.x, game.y - 1);
    if up != "" {
        return up;
    }
    let down = portrait_id_at(game, entities, game.x, game.y + 1);
    if down != "" {
        return down;
    }
    let left = portrait_id_at(game, entities, game.x - 1, game.y);
    if left != "" {
        return left;
    }
    let right = portrait_id_at(game, entities, game.x + 1, game.y);
    if right != "" {
        return right;
    }
    "player"
}
fn styled_line(color, text) {
    paint(color, ui.pad_right(text, ui.width())) + "\r\n"
}
fn plain_line(text) {
    ui.pad_right(text, ui.width()) + "\r\n"
}
fn draw_map_line(game, entities, features, y, start_x, part, side) {
    let parts = [];
    let current_color = "";
    let current_text = "";
    let x = start_x;
    while x < start_x + view_w() {
        let id = sprite_at(game, entities, features, x, y);
        let color = cell_color_at(game, entities, features, x, y, id);
        let text = tiles.row(id, part);
        if current_color == "" {
            current_color = color;
            current_text = text;
        } else if current_color == color {
            current_text = current_text + text;
        } else {
            append(parts, paint(current_color, current_text));
            current_color = color;
            current_text = text;
        }
        x = x + 1;
    }
    if current_color != "" {
        append(parts, paint(current_color, current_text));
    }
    append(parts, ui.pad_right("  " + side, ui.width() - map_width()) + "\r\n");
    join(parts, "")
}
fn chunk(text, start, size) {
    if len(text) <= start {
        ""
    } else {
        substr(text, start, size)
    }
}
fn box_line(text) {
    plain_line("| " + ui.pad_right(text, ui.width() - 4) + " |")
}
fn box_border() {
    styled_line("bright_yellow", "+" + ui.repeat("-", ui.width() - 2) + "+")
}
fn draw_dialog(dialog) {
    join([
        box_border(),
        box_line(dialog.speaker),
        styled_line("gray", "+" + ui.repeat("-", ui.width() - 2) + "+"),
        box_line(chunk(dialog.text, 0, ui.width() - 4)),
        box_line(chunk(dialog.text, ui.width() - 4, ui.width() - 4)),
        box_border()
    ], "")
}
fn pad_tail(count) {
    let parts = [];
    let i = 0;
    while i < count {
        append(parts, plain_line(""));
        i = i + 1;
    }
    join(parts, "")
}
fn status_line(game, facing) {
    styled_line("bright_green", "HP " + str(game.hp) + "/" + str(game.max_hp) + "   " + goals.progress(game) + "   " + items.inventory_summary(game) + "   Facing " + facing)
}
fn objective_line(game) {
    styled_line("bright_cyan", goals.objective(game))
}
fn map_row() {
    4
}
fn message_row() {
    map_row() + view_h() * tiles.height() + 1
}
fn windows_terminal() {
    let os = env.env_get("OS");
    !is_null(os) && os == "Windows_NT" || !is_null(env.env_get("WT_SESSION"))
}
fn incremental_enabled() {
    let override = env.env_get("LANTERNBOUND_INCREMENTAL");
    if !is_null(override) && override == "1" {
        return true;
    }
    if !is_null(override) && override == "0" {
        return false;
    }
    !windows_terminal()
}
fn draw_tile_at(game, entities, features, x, y, screen_row, screen_col) {
    let id = sprite_at(game, entities, features, x, y);
    let part = 0;
    while part < tiles.height() {
        terminal.move(screen_row + part, screen_col);
        print(paint(cell_color_at(game, entities, features, x, y, id), tiles.row(id, part)));
        part = part + 1;
    }
}
fn draw_cell_at(game, entities, features, x, y, screen_row, screen_col) {
    let id = cell_sprite_at(game, entities, features, x, y);
    let part = 0;
    while part < tiles.height() {
        terminal.move(screen_row + part, screen_col);
        print(paint(cell_color_at(game, entities, features, x, y, id), tiles.row(id, part)));
        part = part + 1;
    }
}
fn remember_frame(game, start_x, start_y, portrait) {
    game.last_draw_area = game.area;
    game.last_camera_x = start_x;
    game.last_camera_y = start_y;
    game.last_x = game.x;
    game.last_y = game.y;
    game.last_portrait = portrait;
    game.last_dialog_open = !is_null(game.dialog);
    game.last_ui_width = ui.width();
    game.last_view_w = view_w();
    game.render_dirty = false;
}
fn can_incremental(game, start_x, start_y, portrait) {
    incremental_enabled() && game.render_dirty != true && is_null(game.dialog) && game.last_dialog_open == false && game.last_portrait == portrait && game.last_draw_area == game.area && game.last_camera_x == start_x && game.last_camera_y == start_y && game.last_ui_width == ui.width() && game.last_view_w == view_w() && !is_null(game.last_x) && !is_null(game.last_y)
}
fn incremental_draw(game, entities, features, start_x, start_y, facing, portrait) {
    let old_x = game.last_x;
    let old_y = game.last_y;
    if old_x != game.x || old_y != game.y {
        draw_cell_at(game, entities, features, old_x, old_y, map_row() + (old_y - start_y) * tiles.height(), (old_x - start_x) * tiles.width());
        draw_tile_at(game, entities, features, game.x, game.y, map_row() + (game.y - start_y) * tiles.height(), (game.x - start_x) * tiles.width());
    } else {
        draw_tile_at(game, entities, features, game.x, game.y, map_row() + (game.y - start_y) * tiles.height(), (game.x - start_x) * tiles.width());
    }
    terminal.move(1, 0);
    print(status_line(game, facing));
    terminal.move(2, 0);
    print(objective_line(game));
    terminal.move(message_row(), 0);
    print(plain_line(game.message));
    remember_frame(game, start_x, start_y, portrait)
}
fn full_draw(game, entities, features, start_x, start_y, facing, portrait) {
    let frame = [];
    append(frame, styled_line("bright_yellow", "LANTERNBOUND    " + maps.title(game.area)));
    append(frame, status_line(game, facing));
    append(frame, objective_line(game));
    append(frame, plain_line(""));
    let art = tiles.portrait(portrait);
    let y = start_y;
    let art_line = 0;
    while y < start_y + view_h() {
        let part = 0;
        while part < tiles.height() {
            let side = "";
            if art_line < len(art) {
                side = art[art_line];
            }
            append(frame, draw_map_line(game, entities, features, y, start_x, part, side));
            art_line = art_line + 1;
            part = part + 1;
        }
        y = y + 1;
    }
    append(frame, plain_line(""));
    if !is_null(game.dialog) {
        if !is_null(game.area_intro) && game.area_intro != "" {
            append(frame, styled_line("bright_yellow", game.area_intro));
            game.area_intro = null;
        }
        append(frame, draw_dialog(game.dialog));
    } else if !is_null(game.area_intro) && game.area_intro != "" {
        append(frame, styled_line("bright_yellow", "== " + maps.title(game.area) + " =="));
        append(frame, plain_line(game.area_intro));
        append(frame, plain_line(goals.area_tip(game.area)));
        game.area_intro = null;
    } else {
        append(frame, plain_line(game.message));
        append(frame, plain_line(""));
        append(frame, styled_line("gray", "Yellow targets matter. Face one, press Z/Enter/Space. Follow Quest above."));
    }
    append(frame, pad_tail(8));
    terminal.home();
    print(join(frame, ""));
    remember_frame(game, start_x, start_y, portrait)
}
fn draw(game) {
    ensure_cache(game);
    let facing = game.facing;
    if is_null(facing) {
        facing = "down";
    }
    let entities = game.render_entities;
    let features = game.render_features;
    let start_x = camera_x(game);
    let start_y = camera_y(game);
    let portrait = portrait_kind(game, entities);
    if can_incremental(game, start_x, start_y, portrait) {
        incremental_draw(game, entities, features, start_x, start_y, facing, portrait);
    } else {
        full_draw(game, entities, features, start_x, start_y, facing, portrait);
    }
}
export draw;
