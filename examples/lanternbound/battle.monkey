import ui from "ui.monkey";
import terminal from "stdlib/terminal.monkey";
import random from "stdlib/random.monkey";
import time from "stdlib/time.monkey";
import items from "items.monkey";
import enemies from "enemies.monkey";
import patterns from "patterns.monkey";
fn clamp(value, low, high) {
    if value < low {
        low
    } else if value > high {
        high
    } else {
        value
    }
}
fn box_w() {
    patterns.box_w()
}
fn box_h() {
    patterns.box_h()
}
fn dodge_frames() {
    phase_one_frames() + phase_two_frames() + phase_three_frames()
}
fn phase_one_frames() {
    120
}
fn phase_two_frames() {
    360
}
fn phase_three_frames() {
    90
}
fn warning_settle_frames() {
    10
}
fn hit_feedback_ms() {
    260
}
fn hp_bar(hp, max_hp) {
    let full = hp * 18 / max_hp;
    ui.repeat("#", full) + ui.repeat("-", 18 - full)
}
fn frame_inner_w() {
    ui.width() - 4
}
fn frame_border() {
    "+" + ui.repeat("-", ui.width() - 2) + "+"
}
fn frame_text(text) {
    "| " + ui.pad_right(text, frame_inner_w()) + " |"
}
fn box_indent() {
    let indent = (ui.width() - box_w()) / 2;
    if indent < 0 {
        0
    } else {
        indent
    }
}
fn box_tail() {
    let tail = ui.width() - box_indent() - box_w();
    if tail < 0 {
        0
    } else {
        tail
    }
}
fn portrait(e) {
    let lines = enemies.portrait_lines(e);
    let i = 0;
    while i < len(lines) {
        ui.color_line(enemies.stage_color(e), lines[i]);
        i = i + 1;
    }
}
fn draw_turn_line(game, e, line, log, can_flee) {
    terminal.home();
    ui.title("LANTERNBOUND - turn");
    ui.blank();
    portrait(e);
    ui.blank();
    ui.color_line("bright_green", "HP [" + hp_bar(game.hp, game.max_hp) + "] " + str(game.hp) + "/" + str(game.max_hp) + "     " + e.name + " HP " + str(e.hp) + "  Mercy " + str(e.mercy) + "%");
    ui.blank();
    ui.line(frame_border());
    ui.line(frame_text(line));
    if log != "" && log != line {
        ui.line(frame_text(log));
    } else {
        ui.line(frame_text(""));
    }
    ui.line(frame_border());
    ui.blank();
    if can_flee {
        ui.line("Press any key to continue. Q returns to the road.");
    } else {
        ui.line("Press any key to continue. The reply must finish first.");
    }
    ui.clear_tail(8)
}
fn show_turn_line_may_flee(game, e, line, log, can_flee) {
    terminal.read_key_latest_timeout(0);
    draw_turn_line(game, e, line, log, can_flee);
    while true {
        let key = terminal.read_key();
        let name = ui.key_name(key);
        if can_flee && (name == "q" || name == "esc") {
            return "fled";
        }
        if name != "" {
            return "ok";
        }
    }
}
fn show_turn_line(game, e, line, log) {
    show_turn_line_may_flee(game, e, line, log, true)
}
fn draw_strike_frame(game, e, damage, color, shift, caption) {
    terminal.home();
    ui.title("LANTERNBOUND - strike");
    ui.blank();
    let lines = enemies.portrait_lines(e);
    let i = 0;
    while i < len(lines) {
        ui.color_line(color, ui.repeat(" ", shift) + lines[i]);
        i = i + 1;
    }
    ui.blank();
    let shown_hp = e.hp;
    if shown_hp < 0 {
        shown_hp = 0;
    }
    ui.color_line("bright_red", "STRIKE -" + str(damage) + "    " + caption);
    ui.color_line("bright_green", "HP [" + hp_bar(game.hp, game.max_hp) + "] " + str(game.hp) + "/" + str(game.max_hp) + "     " + e.name + " HP " + str(shown_hp) + "/" + str(e.max_hp));
    ui.clear_tail(10)
}
fn strike_animation(game, e, damage) {
    draw_strike_frame(game, e, damage, "bright_white", 0, "impact");
    time.sleep_ms(55);
    draw_strike_frame(game, e, damage, "bright_red", 2, "the outline buckles");
    time.sleep_ms(65);
    draw_strike_frame(game, e, damage, enemies.stage_color(e), 0, "the encounter steadies");
    time.sleep_ms(55)
}
fn draw_menu(game, e, choice, log) {
    terminal.home();
    ui.title("LANTERNBOUND - A quiet battle");
    ui.blank();
    portrait(e);
    ui.blank();
    ui.color_line("bright_green", "HP [" + hp_bar(game.hp, game.max_hp) + "] " + str(game.hp) + "/" + str(game.max_hp) + "     " + e.name + " HP " + str(e.hp) + "  Mercy " + str(e.mercy) + "%");
    ui.blank();
    ui.line(frame_border());
    ui.line(frame_text(log));
    ui.line(frame_border());
    ui.blank();
    let text = "";
    let i = 0;
    while i < 4 {
        let label = "";
        if i == 0 {
            label = "STRIKE";
        } else if i == 1 {
            label = "ACT";
        } else if i == 2 {
            label = "ITEM";
        } else {
            label = "SPARE";
        }
        if i == choice {
            text = text + " > " + label + " < ";
        } else {
            text = text + "   " + label + "   ";
        }
        i = i + 1;
    }
    terminal.fg("bright_yellow");
    ui.line(text);
    terminal.reset_style();
    ui.line("Left/Right or A/D choose. Enter/Z confirms. Q returns to the road.");
    ui.clear_tail(8)
}
fn menu_choice(game, e, log) {
    let choice = 0;
    while true {
        draw_menu(game, e, choice, log);
        let key = terminal.read_key();
        let name = ui.key_name(key);
        if name != "" {
            if name == "left" || name == "a" {
                choice = choice - 1;
                if choice < 0 {
                    choice = 3;
                }
            } else if name == "right" || name == "d" {
                choice = choice + 1;
                if choice > 3 {
                    choice = 0;
                }
            } else if name == "q" || name == "esc" {
                return -1;
            } else if name == "enter" || name == "z" || name == " " {
                return choice;
            }
        }
    }
}
fn draw_item_menu(game, choice) {
    terminal.home();
    ui.title("LANTERNBOUND - items");
    ui.blank();
    ui.color_line("bright_green", "HP " + str(game.hp) + "/" + str(game.max_hp) + "     " + items.inventory_summary(game));
    ui.blank();
    let list = items.item_list(game);
    if len(list) == 0 {
        ui.line("Your pack is empty.");
        ui.blank();
        ui.line("Press any key.");
        ui.clear_tail(14);
        terminal.read_key();
        return "";
    }
    let i = 0;
    while i < len(list) {
        let label = items.name(list[i]) + " x" + str(items.count(game, list[i]));
        if i == choice {
            ui.color_line("bright_yellow", " > " + label + " <");
        } else {
            ui.line("   " + label);
        }
        i = i + 1;
    }
    ui.blank();
    ui.line("Up/Down choose. Enter/Z uses. Q cancels.");
    ui.clear_tail(10);
    "open"
}
fn choose_item(game) {
    let list = items.item_list(game);
    if len(list) == 0 {
        draw_item_menu(game, 0);
        return "";
    }
    let choice = 0;
    while true {
        draw_item_menu(game, choice);
        let key = terminal.read_key();
        let name = ui.key_name(key);
        if name != "" {
            if name == "up" || name == "w" || name == "left" || name == "a" {
                choice = choice - 1;
                if choice < 0 {
                    choice = len(list) - 1;
                }
            } else if name == "down" || name == "s" || name == "right" || name == "d" {
                choice = choice + 1;
                if choice >= len(list) {
                    choice = 0;
                }
            } else if name == "q" || name == "esc" {
                return "";
            } else if name == "enter" || name == "z" || name == " " {
                return list[choice];
            }
        }
    }
}
fn draw_act_menu(game, e, choice, log) {
    terminal.home();
    ui.title("LANTERNBOUND - act");
    ui.blank();
    portrait(e);
    ui.blank();
    ui.color_line("bright_green", "HP " + str(game.hp) + "/" + str(game.max_hp) + "     " + e.name + " HP " + str(e.hp) + "  Mercy " + str(e.mercy) + "%");
    ui.blank();
    ui.line(frame_text(log));
    ui.blank();
    let list = enemies.act_options(e);
    let i = 0;
    while i < len(list) {
        let label = list[i].label;
        if i == choice {
            ui.color_line("bright_yellow", " > " + label + " <");
        } else {
            ui.line("   " + label);
        }
        i = i + 1;
    }
    ui.blank();
    ui.line("Up/Down choose. Enter/Z acts. Q cancels.");
    ui.clear_tail(8)
}
fn choose_act(game, e, log) {
    let list = enemies.act_options(e);
    let choice = 0;
    while true {
        draw_act_menu(game, e, choice, log);
        let key = terminal.read_key();
        let name = ui.key_name(key);
        if name != "" {
            if name == "up" || name == "w" || name == "left" || name == "a" {
                choice = choice - 1;
                if choice < 0 {
                    choice = len(list) - 1;
                }
            } else if name == "down" || name == "s" || name == "right" || name == "d" {
                choice = choice + 1;
                if choice >= len(list) {
                    choice = 0;
                }
            } else if name == "q" || name == "esc" {
                return "";
            } else if name == "enter" || name == "z" || name == " " {
                return list[choice].id;
            }
        }
    }
}
fn bullet_index(bullets, x, y) {
    let warning = -1;
    for i, b in bullets {
        if b.x == x && b.y == y {
            if b.warning == true {
                if warning < 0 {
                    warning = i;
                }
            } else {
                return i;
            }
        }
    }
    warning
}
fn trail_hit(bullets, x, y) {
    for i, b in bullets {
        if b.warning != true {
            let step = 1;
            while step <= 2 {
                if x == b.x - b.dx * step && y == b.y - b.dy * step {
                    return {index: i, step: step};
                }
                step = step + 1;
            }
        }
    }
    {index: -1, step: 0}
}
fn trail_mark(b, step) {
    if b.dx != 0 && b.dy == 0 {
        if step == 1 {
            "-"
        } else {
            "."
        }
    } else if b.dy != 0 && b.dx == 0 {
        if step == 1 {
            "."
        } else {
            " "
        }
    } else if step == 1 {
        "."
    } else {
        " "
    }
}
fn phase_index(frame) {
    if frame < phase_one_frames() {
        0
    } else if frame < phase_one_frames() + phase_two_frames() {
        1
    } else {
        2
    }
}
fn phase_start_frame(phase) {
    if phase == 0 {
        0
    } else if phase == 1 {
        phase_one_frames()
    } else {
        phase_one_frames() + phase_two_frames()
    }
}
fn phase_frame(frame) {
    frame - phase_start_frame(phase_index(frame))
}
fn phase_len(phase) {
    if phase == 0 {
        phase_one_frames()
    } else if phase == 1 {
        phase_two_frames()
    } else {
        phase_three_frames()
    }
}
fn phase_remaining(frame) {
    let phase = phase_index(frame);
    phase_len(phase) - phase_frame(frame)
}
fn calm_wave(wave) {
    wave == "calm" || wave == "quiet" || wave == "lowered" || wave == "clear"
}
fn phase_wave(e, phase) {
    let wave = pressured_wave(e);
    if phase == 0 || phase == 2 || calm_wave(wave) || enemies.hp_stage(e) > 0 {
        return wave;
    }
    if e.shape == "moth" {
        "soft"
    } else if e.shape == "knight" {
        "formal"
    } else if e.shape == "oracle" {
        "mirror"
    } else {
        "quiet"
    }
}
fn pressured_wave(e) {
    if enemies.hp_stage(e) == 0 || calm_wave(e.wave) {
        return e.wave;
    }
    if e.shape == "moth" {
        "wild"
    } else if e.shape == "knight" {
        "sharp"
    } else if e.shape == "oracle" {
        "crowded"
    } else {
        "wild"
    }
}
fn phase_intensity(intensity, phase, e) {
    let power = intensity;
    if !calm_wave(e.wave) {
        power = power + enemies.hp_stage(e);
    }
    if phase == 2 {
        power = power + 1;
    }
    clamp(power, 1, 3)
}
fn feedback_color(feedback) {
    if feedback == "hurt" {
        "bright_red"
    } else if feedback == "guard" {
        "bright_cyan"
    } else {
        "bright_green"
    }
}
fn battle_key(event) {
    if !is_null(event.battle_key) {
        event.battle_key
    } else {
        str(event.x) + ":" + str(event.y) + ":" + event.enemy
    }
}
fn load_enemy(game, event) {
    let key = battle_key(event);
    let e = game.enemy_states[key];
    if is_null(e) {
        let created = enemies.enemy(event.enemy);
        game.enemy_states[key] = created;
        created
    } else {
        e
    }
}
fn save_enemy(game, event, e) {
    game.enemy_states[battle_key(event)] = e;
}
fn phase_line(e, phase) {
    if phase == 0 {
        e.name + " tests the edge of the box."
    } else if phase == 1 {
        e.name + " settles into its true rhythm."
    } else {
        e.name + " gives the pattern one last push."
    }
}
fn draw_box(game, e, soul_x, soul_y, bullets, frame, shield, phase, feedback) {
    terminal.home();
    if feedback == "hurt" {
        ui.color_line("bright_red", "LANTERNBOUND - HIT");
    } else if feedback == "guard" {
        ui.color_line("bright_cyan", "LANTERNBOUND - GUARD");
    } else {
        ui.title("LANTERNBOUND - dodge");
    }
    ui.color_line(enemies.stage_color(e), "Phase " + str(phase + 1) + "/3  " + phase_line(e, phase) + "  Enemy " + enemies.stage_name(e));
    ui.color_line(feedback_color(feedback), "HP [" + hp_bar(game.hp, game.max_hp) + "] " + str(game.hp) + "/" + str(game.max_hp) + "     Shield " + str(shield));
    ui.blank();
    let indent = ui.repeat(" ", box_indent());
    let line_tail = ui.repeat(" ", box_tail());
    let y = 0;
    while y < box_h() {
        print(indent);
        let x = 0;
        while x < box_w() {
            if x == 0 || y == 0 || x == box_w() - 1 || y == box_h() - 1 {
                terminal.fg("gray");
                print("#");
            } else if x == soul_x && y == soul_y {
                if feedback == "hurt" {
                    terminal.fg("bright_red");
                    print("X");
                } else if feedback == "guard" {
                    terminal.fg("bright_cyan");
                    print("O");
                } else {
                    terminal.fg("bright_red");
                    print("@");
                }
            } else {
                let bi = bullet_index(bullets, x, y);
                if bi >= 0 {
                    terminal.fg(bullets[bi].color);
                    print(bullets[bi].mark);
                } else {
                    let trail = trail_hit(bullets, x, y);
                    if trail.index >= 0 {
                        terminal.fg("dark_gray");
                        print(trail_mark(bullets[trail.index], trail.step));
                    } else {
                        print(" ");
                    }
                }
            }
            x = x + 1;
        }
        terminal.reset_style();
        print(line_tail);
        ui.println("");
        y = y + 1;
    }
    ui.blank();
    if feedback == "hurt" {
        ui.color_line("bright_red", "Frame " + str(frame) + "/" + str(dodge_frames()) + "    HIT -2 HP. Keep moving.");
    } else if feedback == "guard" {
        ui.color_line("bright_cyan", "Frame " + str(frame) + "/" + str(dodge_frames()) + "    Shield cracked, no HP lost.");
    } else {
        ui.line("Frame " + str(frame) + "/" + str(dodge_frames()) + "    Gray : warns. Bright marks hurt. WASD/arrows move.");
    }
    ui.clear_tail(10)
}
fn move_soul(key, soul) {
    if key.key == "left" || key.key == "a" {
        soul.x = soul.x - 1;
    } else if key.key == "right" || key.key == "d" {
        soul.x = soul.x + 1;
    } else if key.key == "up" || key.key == "w" {
        soul.y = soul.y - 1;
    } else if key.key == "down" || key.key == "s" {
        soul.y = soul.y + 1;
    }
    soul.x = clamp(soul.x, 1, box_w() - 2);
    soul.y = clamp(soul.y, 1, box_h() - 2);
}
fn dodge(game, e, intensity, shield) {
    let bullets = [];
    let soul = {x: box_w() / 2, y: box_h() / 2};
    let frame = 0;
    while frame < dodge_frames() {
        let phase = phase_index(frame);
        let feedback = "";
        patterns.spawn_phase_wave(bullets, phase_frame(frame), phase_intensity(intensity, phase, e), e.shape, phase_wave(e, phase), phase, phase_remaining(frame) > warning_settle_frames());
        let key = terminal.read_key_latest_timeout(35);
        if key.ok && !is_null(key.value) {
            let name = ui.key_name(key);
            if name != "q" && name != "esc" {
                move_soul(key.value, soul);
            }
        }
        let i = 0;
        while i < len(bullets) {
            let b = bullets[i];
            if b.warning == true {
                b.warmup = b.warmup - 1;
                if b.warmup <= 0 {
                    delete(bullets, i);
                } else {
                    i = i + 1;
                }
            } else {
                b.x = b.x + b.dx;
                b.y = b.y + b.dy;
                if b.x <= 0 || b.y <= 0 || b.x >= box_w() - 1 || b.y >= box_h() - 1 {
                    delete(bullets, i);
                } else if b.x == soul.x && b.y == soul.y {
                    if shield > 0 {
                        shield = shield - 1;
                        feedback = "guard";
                    } else {
                        game.hp = game.hp - 2;
                        feedback = "hurt";
                    }
                    delete(bullets, i);
                    if game.hp <= 0 {
                        return "dead";
                    }
                } else {
                    i = i + 1;
                }
            }
        }
        draw_box(game, e, soul.x, soul.y, bullets, frame, shield, phase, feedback);
        if feedback != "" {
            time.sleep_ms(hit_feedback_ms());
        }
        frame = frame + 1;
    }
    "survived"
}
fn run(game, event) {
    let e = load_enemy(game, event);
    let log = "";
    let intro = show_turn_line(game, e, enemies.enemy_line(e, "intro"), "");
    if intro == "fled" {
        save_enemy(game, event, e);
        return "fled";
    }
    log = "Choose an answer. ACT often changes how the next attack behaves.";
    while game.hp > 0 {
        let choice = menu_choice(game, e, log);
        if choice < 0 {
            save_enemy(game, event, e);
            return "fled";
        }
        let intensity = 1;
        let shield = 0;
        let line = "";
        if choice == 0 {
            let damage = random.random_int(7, 13);
            e.hp = e.hp - damage;
            intensity = 3;
            e.wave = "sharp";
            e.phase = "struck";
            log = "You strike for " + str(damage) + ".";
            line = e.name + " reels, then steadies itself.";
            strike_animation(game, e, damage);
            if e.hp <= 0 {
                save_enemy(game, event, e);
                return "won";
            }
            show_turn_line_may_flee(game, e, line, log, false);
            line = "\"Then we answer in the old language: impact.\"";
            log = "The reply is sharp.";
        } else if choice == 1 {
            let act = choose_act(game, e, log);
            if act == "" {
                continue;
            } else {
                let acted = enemies.act_result(e, act);
                log = acted.log;
                intensity = acted.intensity;
                shield = acted.shield;
                line = acted.line;
            }
        } else if choice == 2 {
            let item = choose_item(game);
            if item == "" {
                continue;
            } else {
                let used = enemies.item_result(game, e, item);
                log = used.log;
                shield = used.shield;
                intensity = used.intensity;
                line = used.line;
            }
        } else {
            if e.mercy >= 100 {
                save_enemy(game, event, e);
                return "spared";
            }
            log = "Mercy is not full yet. The enemy hesitates and lets you choose again.";
            intensity = 0;
            line = "\"Not yet. I still do not know what you mean by mercy.\"";
        }
        if line != "" {
            let said = show_turn_line_may_flee(game, e, line, log, intensity <= 0);
            if said == "fled" {
                save_enemy(game, event, e);
                return "fled";
            }
        }
        if intensity > 0 {
            let result = dodge(game, e, intensity, shield);
            if result == "dead" || result == "fled" {
                save_enemy(game, event, e);
                return result;
            }
            let after = show_turn_line(game, e, enemies.enemy_line(e, "after"), "");
            if after == "fled" {
                save_enemy(game, event, e);
                return "fled";
            }
        }
        save_enemy(game, event, e);
    }
    save_enemy(game, event, e);
    "dead"
}
export run;
