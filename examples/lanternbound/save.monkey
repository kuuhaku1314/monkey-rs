import fs from "stdlib/fs.monkey";
import json from "stdlib/json.monkey";
fn path() {
    "lanternbound_save.json"
}
fn save_game(game) {
    game.dialog = null;
    game.dialog_opened_at = null;
    game.dialog_cooldown_until = null;
    game.render_area = null;
    game.render_entities = null;
    game.render_features = null;
    game.area_intro = null;
    game.last_draw_area = null;
    game.last_camera_x = null;
    game.last_camera_y = null;
    game.last_x = null;
    game.last_y = null;
    game.last_portrait = null;
    game.last_dialog_open = null;
    game.last_ui_width = null;
    game.last_view_w = null;
    game.render_dirty = true;
    let text = json.json_stringify(game);
    fs.write_file(path(), text)
}
fn normalize(game) {
    if is_null(game.facing) {
        game.facing = "down";
    }
    if is_null(game.flags) {
        game.flags = {};
    }
    if is_null(game.items) {
        game.items = {};
    }
    game.dialog = null;
    game.dialog_opened_at = null;
    game.dialog_cooldown_until = null;
    game.render_area = null;
    game.render_entities = null;
    game.render_features = null;
    game.area_intro = null;
    game.last_draw_area = null;
    game.last_camera_x = null;
    game.last_camera_y = null;
    game.last_x = null;
    game.last_y = null;
    game.last_portrait = null;
    game.last_dialog_open = null;
    game.last_ui_width = null;
    game.last_view_w = null;
    game.render_dirty = true;
    game
}
fn load_game(default_game) {
    let read = fs.read_file(path());
    if !read.ok {
        return {ok: false, value: default_game, error: read.error};
    }
    let parsed = json.json_parse(read.value);
    if parsed.ok {
        {ok: true, value: normalize(parsed.value), error: null}
    } else {
        {ok: false, value: default_game, error: parsed.error}
    }
}
export path;
export save_game;
export load_game;
