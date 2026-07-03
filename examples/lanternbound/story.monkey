import items from "items.monkey";
fn flag(id) {
    "story:" + id
}
fn battle_flag(id) {
    "battle:" + id
}
fn has_flag(game, id) {
    let value = game.flags[flag(id)];
    !is_null(value) && value == true
}
fn mark_flag(game, id) {
    game.flags[flag(id)] = true;
}
fn battle_result(game, id) {
    let value = game.flags[battle_flag(id)];
    if is_null(value) {
        ""
    } else {
        value
    }
}
fn record_battle(game, entity, result) {
    if !is_null(entity) {
        game.flags[battle_flag(entity.event)] = result;
    }
}
fn reward_name(event) {
    if event == "kindle" {
        "KINDLE"
    } else if event == "listen" {
        "LISTEN"
    } else if event == "remember" {
        "REMEMBER"
    } else if event == "return" {
        "RETURN"
    } else {
        ""
    }
}
fn dialogue(entity, game) {
    if entity.event == "kindle" {
        if has_flag(game, entity.event) {
            if has_flag(game, "river_child_helped") {
                "Nara smiles toward the river: You warmed the child before the road asked you to. KINDLE suits you."
            } else {
                "Nara lowers her lamp: KINDLE is already warm in your pocket. The river child still waits by cold water."
            }
        } else {
            "Nara gives you the first lantern name: KINDLE. A brave light starts small."
        }
    } else if entity.event == "home_hint" {
        if has_flag(game, "river_child_helped") {
            "The River Child waves a dry ribbon: The bridge listened. I think it knows your steps now."
        } else if items.count(game, "moon_tea") > 0 {
            "You share Moon Tea with the River Child. In return, they press a Spark Charm into your palm."
        } else {
            "The River Child points at the bridge: The water is deep, but bridges remember feet. A warm drink would help me wait."
        }
    } else if entity.event == "listen" {
        if has_flag(game, entity.event) {
            if !has_flag(game, "moth_charm_read") && items.count(game, "spark_charm") > 0 {
                "The Moth Scribe reads your Spark Charm, copies its tiny storm, and rewards you with Moon Tea."
            } else if battle_result(game, "moth") == "spared" {
                "The Moth Scribe shows a page with no ink: You spared a shaking wing. LISTEN has begun answering you."
            } else if has_flag(game, "moth_charm_read") {
                "The Moth Scribe folds a page: The charm's spark is copied here. Carry it when words run out."
            } else {
                "The Moth Scribe folds a page: LISTEN is not silence. It is room for another heart."
            }
        } else {
            "The Moth Scribe dusts your sleeve and gives the second name: LISTEN."
        }
    } else if entity.event == "grove_hint" {
        if has_flag(game, "river_child_helped") {
            "The root child says: The river child told me you shared warmth. Some doors open before they know they are doors."
        } else {
            "The root child says: Some battles end faster when you talk before you strike."
        }
    } else if entity.event == "remember" {
        if has_flag(game, entity.event) {
            if battle_result(game, "knight") == "spared" {
                "The Root Clock ticks gently: You let the Tin Knight keep its dented pride. REMEMBER can be merciful."
            } else {
                "The Root Clock ticks around you: REMEMBER means carrying the past without living inside it."
            }
        } else {
            "The Root Clock strikes thirteen and gives the third name: REMEMBER."
        }
    } else if entity.event == "ruins_hint" {
        if items.count(game, "glass_key") > 0 {
            "The Old Mason studies your Glass Key: That is not for a lock. It lets morning recognize the hand that carried night."
        } else if has_flag(game, "remember") {
            "The Old Mason taps a cracked pillar: The key is near the sign, where broken things still point somewhere."
        } else {
            "The Old Mason taps a cracked pillar: Save when the road feels too quiet."
        }
    } else if entity.event == "return" {
        if has_flag(game, entity.event) {
            if game.glow >= 4 && items.count(game, "glass_key") > 0 {
                "The Bridge Voice hums through the Glass Key: Four names, one morning. The gate will ask who you became."
            } else {
                "The Bridge Voice hums: RETURN is not retreat. It is choosing what you bring back."
            }
        } else {
            "The Bridge Voice opens like glass and gives the fourth name: RETURN."
        }
    } else if entity.event == "home_sign" {
        "The sign reads: East road to Bluebell Grove. Face what you want to answer before pressing Z."
    } else if entity.event == "grove_sign" {
        "The sign reads: Blue moths collect promises. Speak gently if wings begin to shake."
    } else if entity.event == "ruins_sign" {
        "The cracked sign reads: Ruins remember every shortcut. They also remember every mistake."
    } else if entity.event == "span_sign" {
        "The glass sign reads: Four names open morning. Fewer than four open only your reflection."
    } else {
        entity.name + " has nothing more to say."
    }
}
fn side_quest(game, entity, was_known) {
    if entity.event == "home_hint" {
        if !has_flag(game, "river_child_helped") && items.count(game, "moon_tea") > 0 {
            items.remove(game, "moon_tea", 1);
            items.add(game, "spark_charm", 1);
            mark_flag(game, "river_child_helped");
            game.render_dirty = true;
        }
    } else if entity.event == "listen" {
        if was_known && !has_flag(game, "moth_charm_read") && items.count(game, "spark_charm") > 0 {
            items.add(game, "moon_tea", 1);
            mark_flag(game, "moth_charm_read");
            game.render_dirty = true;
        }
    } else if entity.event == "ruins_hint" {
        if items.count(game, "glass_key") > 0 && !has_flag(game, "mason_key_story") {
            mark_flag(game, "mason_key_story");
        }
    }
}
fn talk(entity, game) {
    let was_known = has_flag(game, entity.event);
    let text = dialogue(entity, game);
    receive_reward(game, entity);
    side_quest(game, entity, was_known);
    text
}
fn receive_reward(game, entity) {
    let name = reward_name(entity.event);
    if name == "" {
        return "";
    }
    if has_flag(game, entity.event) {
        return "";
    }
    mark_flag(game, entity.event);
    game.glow = game.glow + 1;
    if game.glow > 4 {
        game.glow = 4;
    }
    name
}
fn enemy_for(entity) {
    if entity.event == "moth" {
        "moth"
    } else if entity.event == "knight" {
        "knight"
    } else if entity.event == "oracle" {
        "oracle"
    } else {
        "shade"
    }
}
export flag;
export has_flag;
export mark_flag;
export battle_result;
export record_battle;
export reward_name;
export dialogue;
export talk;
export receive_reward;
export enemy_for;
