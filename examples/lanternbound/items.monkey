fn ensure(game) {
    if is_null(game.items) {
        game.items = {};
    }
}
fn count(game, id) {
    ensure(game);
    let value = game.items[id];
    if is_null(value) {
        0
    } else {
        value
    }
}
fn add(game, id, amount) {
    ensure(game);
    game.items[id] = count(game, id) + amount;
}
fn remove(game, id, amount) {
    ensure(game);
    let current = count(game, id);
    if current < amount {
        return false;
    }
    game.items[id] = current - amount;
    true
}
fn name(id) {
    if id == "moon_tea" {
        "Moon Tea"
    } else if id == "spark_charm" {
        "Spark Charm"
    } else if id == "glass_key" {
        "Glass Key"
    } else {
        id
    }
}
fn pickup_text(id) {
    if id == "moon_tea" {
        "You tuck Moon Tea into your pack. It restores HP in battle."
    } else if id == "spark_charm" {
        "You find a Spark Charm. It can soften one encounter."
    } else if id == "glass_key" {
        "You lift a Glass Key. It feels like a small door remembering your hand."
    } else {
        "You pick up " + name(id) + "."
    }
}
fn item_list(game) {
    ensure(game);
    let out = [];
    if count(game, "moon_tea") > 0 {
        append(out, "moon_tea");
    }
    if count(game, "spark_charm") > 0 {
        append(out, "spark_charm");
    }
    out
}
fn use_battle_item(game, enemy, id) {
    if id == "moon_tea" {
        if !remove(game, id, 1) {
            return "No Moon Tea remains.";
        }
        game.hp = game.hp + 12;
        if game.hp > game.max_hp {
            game.hp = game.max_hp;
        }
        return "You drink Moon Tea and recover 12 HP.";
    }
    if id == "spark_charm" {
        if !remove(game, id, 1) {
            return "No Spark Charm remains.";
        }
        enemy.mercy = enemy.mercy + 45;
        if enemy.mercy > 100 {
            enemy.mercy = 100;
        }
        return "The Spark Charm glows. Mercy rises sharply.";
    }
    "That item cannot be used here."
}
fn inventory_summary(game) {
    ensure(game);
    "Tea " + str(count(game, "moon_tea")) + "   Charm " + str(count(game, "spark_charm")) + "   Key " + str(count(game, "glass_key"))
}
export ensure;
export count;
export add;
export remove;
export name;
export pickup_text;
export item_list;
export use_battle_item;
export inventory_summary;
