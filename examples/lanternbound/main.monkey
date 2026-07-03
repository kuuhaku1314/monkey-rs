import terminal from "stdlib/terminal.monkey";
import ui from "ui.monkey";
import world from "world.monkey";
import battle from "battle.monkey";
import save from "save.monkey";
import story from "story.monkey";
fn frame_line(text) {
    ui.pad_right(text, ui.width()) + "\r\n"
}
fn color_frame_line(color, text) {
    terminal.paint(color, ui.pad_right(text, ui.width())) + "\r\n"
}
fn color_frame_cell(color, text) {
    terminal.paint(color, ui.pad_right(text, ui.width()))
}
fn title_line_limit() {
    let limit = ui.height() - 2;
    if limit < 10 {
        10
    } else {
        limit
    }
}
fn add_title_line(state, color, text) {
    if state.used < state.limit {
        if color == "" {
            state.frame = state.frame + frame_line(text);
        } else {
            state.frame = state.frame + color_frame_line(color, text);
        }
        state.used = state.used + 1;
    }
}
fn logo() {
    [
        " __        ___    _   _ _____ _____ ____  _   _ ____   ___  _   _ _   _ ____  ",
        " \\ \\      / / \\  | \\ | |_   _| ____|  _ \\| \\ | | __ ) / _ \\| | | | \\ | |  _ \\ ",
        "  \\ \\ /\\ / / _ \\ |  \\| | | | |  _| | |_) |  \\| |  _ \\| | | | | | |  \\| | | | |",
        "   \\ V  V / ___ \\| |\\  | | | | |___|  _ <| |\\  | |_) | |_| | |_| | |\\  | |_| |",
        "    \\_/\\_/_/   \\_\\_| \\_| |_| |_____|_| \\_\\_| \\_|____/ \\___/ \\___/|_| \\_|____/ "
    ]
}
fn hero_art() {
    [
        "        LANTERN KEEPER              SAVE LAMP                LOST SHADE",
        "             .-.                       .-.                     .-''''-.",
        "            (o o)                    _/ S \\_                  /  .--.  \\",
        "           /|_-_|\\                  /  ___  \\                |  / !! \\  |",
        "            /| |\\                   |__| |__|                 \\  '--'  /",
        "           /_| |_\\                    _| |_                    '-.__.-'"
    ]
}
fn menu_text(choice, index, label) {
    if choice == index {
        "        > " + label + " <"
    } else {
        "          " + label
    }
}
fn title_menu_row() {
    if ui.height() >= 28 {
        16
    } else {
        9
    }
}
fn title_notice_row() {
    title_menu_row() + 4
}
fn draw_title_menu(choice, notice) {
    let row = title_menu_row();
    if row + 2 < title_line_limit() {
        terminal.move(row, 0);
        print(color_frame_cell("bright_yellow", menu_text(choice, 0, "NEW GAME")));
        terminal.move(row + 1, 0);
        print(color_frame_cell("bright_yellow", menu_text(choice, 1, "CONTINUE  " + save.path())));
        terminal.move(row + 2, 0);
        print(color_frame_cell("bright_yellow", menu_text(choice, 2, "QUIT")));
    }
    if title_notice_row() < title_line_limit() {
        terminal.move(title_notice_row(), 0);
        if notice != "" {
            print(color_frame_cell("bright_red", "        " + notice));
        } else {
            print(color_frame_cell("gray", "        Up/Down choose. Enter/Z confirms. N/C/Q shortcuts also work."));
        }
    }
}
fn title_screen(choice, notice) {
    terminal.home();
    let state = {frame: "", used: 0, limit: title_line_limit()};
    add_title_line(state, "dark_gray", "================================================================================");
    let lines = logo();
    for i, line in lines {
        add_title_line(state, "bright_yellow", line);
    }
    add_title_line(state, "dark_gray", "================================================================================");
    add_title_line(state, "bright_cyan", "            A terminal RPG about answering monsters instead of erasing them.");
    add_title_line(state, "", "");
    if ui.height() >= 28 {
        let art = hero_art();
        for i, line in art {
            add_title_line(state, "bright_green", line);
        }
        add_title_line(state, "", "");
    }
    add_title_line(state, "bright_yellow", menu_text(choice, 0, "NEW GAME"));
    add_title_line(state, "bright_yellow", menu_text(choice, 1, "CONTINUE  " + save.path()));
    add_title_line(state, "bright_yellow", menu_text(choice, 2, "QUIT"));
    add_title_line(state, "", "");
    if notice != "" {
        add_title_line(state, "bright_red", "        " + notice);
    } else {
        add_title_line(state, "gray", "        Up/Down choose. Enter/Z confirms. N/C/Q shortcuts also work.");
    }
    add_title_line(state, "gray", "        Goal: four lantern names, one Glass Key, then Morning Gate.");
    add_title_line(state, "gray", "        ACT, use items, or spare monsters. Fighting is only one answer.");
    add_title_line(state, "gray", "        Move with WASD or arrow keys. Face a target, then press Z/Enter/Space.");
    while state.used < state.limit {
        add_title_line(state, "", "");
    }
    print(state.frame)
}
fn choose_option(choice) {
    if choice < 0 {
        2
    } else if choice > 2 {
        0
    } else {
        choice
    }
}
fn choose_game() {
    let game = world.new_game();
    let choice = 0;
    let notice = "";
    let title_drawn = false;
    let menu_dirty = false;
    while true {
        if !title_drawn {
            title_screen(choice, notice);
            title_drawn = true;
        } else if menu_dirty {
            draw_title_menu(choice, notice);
            menu_dirty = false;
        }
        let key = terminal.read_key_latest_timeout(80);
        let name = ui.key_name(key);
        if name != "" {
            if name == "up" || name == "w" {
                choice = choose_option(choice - 1);
                notice = "";
                menu_dirty = true;
            } else if name == "down" || name == "s" {
                choice = choose_option(choice + 1);
                notice = "";
                menu_dirty = true;
            } else if name == "n" {
                choice = 0;
                return game;
            } else if name == "c" {
                choice = 1;
                menu_dirty = true;
            } else if name == "q" || name == "esc" {
                choice = 2;
                menu_dirty = true;
            }
            if name == "n" {
                return game;
            }
            if choice == 1 && (name == "c" || name == "enter" || name == "z" || name == " ") {
                let loaded = save.load_game(game);
                if loaded.ok {
                    loaded.value.message = "Loaded " + save.path() + ".";
                    return loaded.value;
                }
                notice = "No readable save yet.";
                menu_dirty = true;
            } else if choice == 0 && (name == "enter" || name == "z" || name == " ") {
                return game;
            } else if choice == 2 && (name == "q" || name == "esc" || name == "enter" || name == "z" || name == " ") {
                game.running = false;
                game.ending = "quit";
                return game;
            }
        }
    }
}
fn ending(game) {
    terminal.home();
    ui.title("LANTERNBOUND");
    ui.blank();
    if game.ending == "clear" {
        ui.color_line("bright_green", "The bridge opens. You carry four lantern names into morning.");
        if story.has_flag(game, "mason_key_story") {
            ui.color_line("bright_yellow", "The Glass Key breaks into sunlight, exactly as the Old Mason remembered.");
        } else {
            ui.color_line("bright_yellow", "The Glass Key turns once, then becomes a small bright window.");
        }
    } else if game.ending == "dead" {
        ui.color_line("bright_red", "Your light goes out, but the save file still remembers.");
    } else {
        ui.color_line("gray", "You close the journal for now.");
    }
    ui.blank();
    ui.line("Press any key to leave.");
    ui.clear_tail(16);
    terminal.read_key()
}
ui.setup();
let game = choose_game();
let needs_draw = true;
while game.running {
    if needs_draw || game.render_dirty {
        world.draw(game);
        needs_draw = false;
    }
    let key = terminal.read_key_latest_timeout(35);
    if key.ok && !is_null(key.value) {
        let event = world.handle_key(game, key.value);
        needs_draw = true;
        if event.kind == "battle" {
            let result = battle.run(game, event);
            world.finish_battle(game, event, result);
            needs_draw = true;
        }
    }
}
ending(game);
ui.restore();
