import renderer from "renderer.monkey";
import maps from "maps.monkey";
import tiles from "tiles.monkey";
import story from "story.monkey";
import save from "save.monkey";
import items from "items.monkey";
import goals from "goals.monkey";
import time from "stdlib/time.monkey";
fn new_game() {
    {
        area: "home",
        x: 6,
        y: 20,
        hp: 32,
        max_hp: 32,
        glow: 0,
        items: {},
        enemy_states: {},
        flags: {},
        message: "Find four lantern names. Start with Caretaker Nara, then follow the east roads.",
        area_intro: goals.area_intro("home"),
        facing: "down",
        dialog: null,
        dialog_opened_at: null,
        dialog_cooldown_until: null,
        render_dirty: true,
        running: true,
        ending: "playing"
    }
}
fn draw(game) {
    renderer.draw(game)
}
fn area_title(area) {
    maps.title(area)
}
fn dialog_hold_ms() {
    700
}
fn dialog_cooldown_ms() {
    250
}
fn confirm_key(key) {
    key.key == "enter" || key.key == "z" || key.key == " "
}
fn open_dialog(game, speaker, text) {
    game.dialog = {speaker: speaker, text: text};
    game.dialog_opened_at = time.time_ms();
    game.render_dirty = true;
}
fn clear_dialog(game) {
    game.dialog = null;
    game.dialog_opened_at = null;
}
fn close_dialog(game) {
    clear_dialog(game);
    game.dialog_cooldown_until = time.time_ms() + dialog_cooldown_ms();
    game.render_dirty = true;
}
fn dialog_ready(game) {
    is_null(game.dialog_opened_at) || time.time_ms() - game.dialog_opened_at >= dialog_hold_ms()
}
fn interact_ready(game) {
    is_null(game.dialog_cooldown_until) || time.time_ms() >= game.dialog_cooldown_until
}
fn event_seen(game, entity) {
    let value = game.flags[maps.key(game.area, entity.x, entity.y)];
    !is_null(value) && value == true
}
fn item_taken(game, entity) {
    let value = game.flags["item:" + entity.id];
    !is_null(value) && value == true
}
fn mark_event_seen(game, x, y) {
    game.flags[maps.key(game.area, x, y)] = true;
}
fn mark_item_taken(game, entity) {
    game.flags["item:" + entity.id] = true;
}
fn blocked(game, x, y) {
    if !tiles.passable(maps.terrain(game.area, x, y)) {
        return true;
    }
    let entity = maps.entity_at(game.area, x, y);
    if is_null(entity) {
        return false;
    }
    if entity.kind == "foe" && event_seen(game, entity) {
        return false;
    }
    if entity.kind == "item" && item_taken(game, entity) {
        return false;
    }
    entity.blocks
}
fn transition(game, entity) {
    if entity.event == "home" {
        game.area = "home";
        game.x = 83;
        game.y = 13;
        game.message = goals.area_tip(game.area);
        game.area_intro = goals.area_intro(game.area);
        game.render_dirty = true;
    } else if entity.event == "grove" {
        game.area = "grove";
        game.x = 2;
        game.y = 13;
        game.message = goals.area_tip(game.area);
        game.area_intro = goals.area_intro(game.area);
        game.render_dirty = true;
    } else if entity.event == "ruins" {
        game.area = "ruins";
        game.x = 2;
        game.y = 13;
        game.message = goals.area_tip(game.area);
        game.area_intro = goals.area_intro(game.area);
        game.render_dirty = true;
    } else if entity.event == "span" {
        game.area = "span";
        game.x = 2;
        game.y = 13;
        game.message = goals.area_tip(game.area);
        game.area_intro = goals.area_intro(game.area);
        game.render_dirty = true;
    } else if entity.event == "clear" {
        if game.glow >= 4 && items.count(game, "glass_key") > 0 {
            game.running = false;
            game.ending = "clear";
        } else {
            game.message = "The morning gate needs four names and a Glass Key.";
        }
    }
}
fn face_x(game) {
    if game.facing == "left" {
        -1
    } else if game.facing == "right" {
        1
    } else {
        0
    }
}
fn face_y(game) {
    if game.facing == "up" {
        -1
    } else if game.facing == "down" {
        1
    } else {
        0
    }
}
fn move_player(game, dx, dy, facing) {
    game.facing = facing;
    clear_dialog(game);
    let nx = game.x + dx;
    let ny = game.y + dy;
    if blocked(game, nx, ny) {
        game.message = "Something solid holds the path. Try speaking from beside it.";
        return null;
    }
    let entity = maps.entity_at(game.area, nx, ny);
    if !is_null(entity) && entity.kind == "gate" {
        transition(game, entity);
    } else {
        game.x = nx;
        game.y = ny;
        game.message = "The floor hums under your steps.";
    }
}
fn target_at(game, x, y) {
    let entity = maps.entity_at(game.area, x, y);
    if is_null(entity) {
        return null;
    }
    if entity.kind == "gate" {
        return null;
    }
    if entity.kind == "item" && item_taken(game, entity) {
        return null;
    }
    entity
}
fn interaction_target(game) {
    target_at(game, game.x + face_x(game), game.y + face_y(game))
}
fn interact(game) {
    let target = interaction_target(game);
    if is_null(target) {
        game.message = "There is nothing close enough to answer here.";
        clear_dialog(game);
        return {kind: "none"};
    }
    if target.kind == "npc" {
        let before_glow = game.glow;
        let text = story.talk(target, game);
        if game.glow > before_glow {
            game.message = goals.reward_message(story.reward_name(target.event));
            game.area_intro = game.message;
            game.render_dirty = true;
        } else {
            game.message = "You speak with " + target.name + ".";
        }
        open_dialog(game, target.name, text);
        return {kind: "none"};
    }
    if target.kind == "save" {
        let saved = save.save_game(game);
        if saved.ok {
            let text = "Saved beside " + target.name + " at " + save.path() + ".";
            game.message = text;
            open_dialog(game, target.name, text);
        } else {
            game.message = "Save failed: " + saved.error.message;
            open_dialog(game, target.name, game.message);
        }
        return {kind: "none"};
    }
    if target.kind == "sign" {
        let text = story.dialogue(target, game);
        game.message = "You read " + target.name + ".";
        open_dialog(game, target.name, text);
        return {kind: "none"};
    }
    if target.kind == "item" {
        items.add(game, target.event, 1);
        mark_item_taken(game, target);
        game.render_dirty = true;
        let text = items.pickup_text(target.event);
        game.message = text;
        let note = goals.item_note(target.event);
        if note != "" {
            game.area_intro = note;
        }
        open_dialog(game, target.name, text);
        return {kind: "none"};
    }
    if target.kind == "foe" {
        if event_seen(game, target) {
            game.message = "Only a warm footprint remains.";
            open_dialog(game, target.name, game.message);
            return {kind: "none"};
        }
        clear_dialog(game);
        return {
            kind: "battle",
            enemy: story.enemy_for(target),
            battle_key: maps.key(game.area, target.x, target.y),
            x: target.x,
            y: target.y
        };
    }
    game.message = "There is nothing close enough to answer here.";
    clear_dialog(game);
    {kind: "none"}
}
fn handle_key(game, key) {
    if !is_null(game.dialog) {
        if key.key == "q" || key.key == "esc" {
            close_dialog(game);
            return {kind: "none"};
        }
        if confirm_key(key) {
            if dialog_ready(game) {
                close_dialog(game);
            }
            return {kind: "none"};
        }
        close_dialog(game);
    }
    if key.key == "q" || key.key == "esc" {
        game.running = false;
        game.ending = "quit";
        return {kind: "none"};
    }
    if key.key == "left" || key.key == "a" {
        move_player(game, -1, 0, "left");
    } else if key.key == "right" || key.key == "d" {
        move_player(game, 1, 0, "right");
    } else if key.key == "up" || key.key == "w" {
        move_player(game, 0, -1, "up");
    } else if key.key == "down" || key.key == "s" {
        move_player(game, 0, 1, "down");
    } else if confirm_key(key) {
        if !interact_ready(game) {
            return {kind: "none"};
        }
        return interact(game);
    }
    {kind: "none"}
}
fn finish_battle(game, event, result) {
    game.render_dirty = true;
    if result == "dead" {
        game.running = false;
        game.ending = "dead";
    } else if result == "quit" {
        game.running = false;
        game.ending = "quit";
    } else if result == "fled" {
        game.message = "You step back from the encounter. It waits where you left it.";
        game.area_intro = null;
    } else {
        let target = maps.entity_at(game.area, event.x, event.y);
        story.record_battle(game, target, result);
        mark_event_seen(game, event.x, event.y);
        if result == "spared" {
            game.message = "You spare the encounter. The road remembers your mercy.";
        } else {
            game.message = "The encounter fades. The road opens.";
        }
    }
}
export new_game;
export draw;
export handle_key;
export finish_battle;
export area_title;
