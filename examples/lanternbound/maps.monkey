fn area_w() {
    86
}
fn area_h() {
    26
}
fn title(area) {
    if area == "home" {
        "Moonwell Home"
    } else if area == "grove" {
        "Bluebell Grove"
    } else if area == "ruins" {
        "Clockroot Ruins"
    } else {
        "The Glass Span"
    }
}
fn key(area, x, y) {
    area + ":" + str(x) + ":" + str(y)
}
fn entity(id, kind, x, y, sprite, name, event, blocks) {
    {
        id: id,
        kind: kind,
        x: x,
        y: y,
        sprite: sprite,
        name: name,
        event: event,
        blocks: blocks
    }
}
fn entities(area) {
    let out = [];
    if area == "home" {
        append(out, entity("caretaker", "npc", 16, 9, "caretaker", "Caretaker Nara", "kindle", true));
        append(out, entity("river_child", "npc", 62, 19, "child", "River Child", "home_hint", true));
        append(out, entity("home_lamp", "save", 68, 10, "save_lamp", "Moonwell Lamp", "save", true));
        append(out, entity("home_shade", "foe", 20, 18, "shade", "Small Shade", "shade", true));
        append(out, entity("home_sign", "sign", 30, 20, "sign", "River Sign", "home_sign", true));
        append(out, entity("home_tea", "item", 24, 20, "item_tea", "Moon Tea", "moon_tea", true));
        append(out, entity("home_exit", "gate", 84, 13, "gate_right", "Road to the grove", "grove", false));
    } else if area == "grove" {
        append(out, entity("grove_back", "gate", 1, 13, "gate_left", "Road to Moonwell", "home", false));
        append(out, entity("moth_scribe", "npc", 45, 5, "moth_scribe", "Moth Scribe", "listen", true));
        append(out, entity("root_child", "npc", 13, 18, "child", "Root Child", "grove_hint", true));
        append(out, entity("grove_moth", "foe", 70, 10, "moth_foe", "Moth Archivist", "moth", true));
        append(out, entity("grove_lamp", "save", 52, 21, "save_lamp", "Bluebell Lamp", "save", true));
        append(out, entity("grove_sign", "sign", 60, 17, "sign", "Bluebell Sign", "grove_sign", true));
        append(out, entity("grove_charm", "item", 44, 17, "item_charm", "Spark Charm", "spark_charm", true));
        append(out, entity("grove_exit", "gate", 84, 17, "gate_right", "Road to the ruins", "ruins", false));
    } else if area == "ruins" {
        append(out, entity("ruins_back", "gate", 1, 13, "gate_left", "Road to the grove", "grove", false));
        append(out, entity("root_clock", "npc", 13, 12, "root_clock", "Root Clock", "remember", true));
        append(out, entity("old_mason", "npc", 67, 12, "caretaker", "Old Mason", "ruins_hint", true));
        append(out, entity("ruins_knight", "foe", 36, 5, "knight_foe", "Tin Knight", "knight", true));
        append(out, entity("ruins_shade", "foe", 74, 20, "shade", "Stair Shade", "shade", true));
        append(out, entity("ruins_lamp", "save", 16, 22, "save_lamp", "Clockroot Lamp", "save", true));
        append(out, entity("ruins_sign", "sign", 58, 13, "sign", "Cracked Sign", "ruins_sign", true));
        append(out, entity("ruins_key", "item", 46, 13, "item_key", "Glass Key", "glass_key", true));
        append(out, entity("ruins_exit", "gate", 84, 13, "gate_right", "Road to the span", "span", false));
    } else {
        append(out, entity("span_back", "gate", 1, 13, "gate_left", "Road to the ruins", "ruins", false));
        append(out, entity("bridge_voice", "npc", 39, 7, "bridge_voice", "Bridge Voice", "return", true));
        append(out, entity("span_oracle", "foe", 43, 14, "oracle_foe", "Glass Oracle", "oracle", true));
        append(out, entity("span_lamp", "save", 55, 20, "save_lamp", "Glass Lamp", "save", true));
        append(out, entity("span_sign", "sign", 62, 13, "sign", "Glass Sign", "span_sign", true));
        append(out, entity("span_tea", "item", 34, 13, "item_tea", "Moon Tea", "moon_tea", true));
        append(out, entity("span_exit", "gate", 84, 13, "gate_right", "Morning Gate", "clear", false));
    }
    out
}
fn entity_at(area, x, y) {
    entity_in(entities(area), x, y)
}
fn entity_in(items, x, y) {
    for i, e in items {
        if e.x == x && e.y == y {
            return e;
        }
    }
    null
}
fn shape(kind, id, x1, y1, x2, y2, door_x, door_y) {
    {kind: kind, id: id, x1: x1, y1: y1, x2: x2, y2: y2, door_x: door_x, door_y: door_y}
}
fn rect(id, x1, y1, x2, y2) {
    shape("rect", id, x1, y1, x2, y2, -1, -1)
}
fn hline(id, y, x1, x2) {
    rect(id, x1, y, x2, y)
}
fn vline(id, x, y1, y2) {
    rect(id, x, y1, x, y2)
}
fn point(id, x, y) {
    rect(id, x, y, x, y)
}
fn hollow(id, x1, y1, x2, y2, door_x, door_y) {
    shape("hollow", id, x1, y1, x2, y2, door_x, door_y)
}
fn matches_shape(item, x, y) {
    if x < item.x1 || x > item.x2 || y < item.y1 || y > item.y2 {
        return false;
    }
    if item.kind == "hollow" {
        if x == item.door_x && y == item.door_y {
            return false;
        }
        y == item.y1 || y == item.y2 || x == item.x1 || x == item.x2
    } else {
        true
    }
}
fn feature_at(items, x, y) {
    let id = "";
    for i, item in items {
        if matches_shape(item, x, y) {
            id = item.id;
        }
    }
    id
}
fn home_features() {
    let out = [
        hline("path", 20, 5, 82),
        hline("path", 13, 36, 83),
        vline("path", 16, 9, 20),
        vline("path", 68, 10, 20),
        vline("wall", 34, 3, 6),
        vline("wall", 34, 8, 13),
        vline("wall", 34, 15, 22),
        hline("wall", 10, 9, 17),
        hline("wall", 10, 19, 29),
        hline("wall", 16, 46, 61),
        hline("wall", 16, 63, 75),
        rect("water", 42, 5, 43, 18),
        rect("bridge", 42, 13, 44, 13),
        hollow("house", 10, 2, 24, 8, 16, 8),
        hollow("house", 58, 3, 75, 9, 68, 9),
        point("door", 16, 8),
        point("door", 68, 9),
        point("window", 13, 4),
        point("window", 21, 4),
        point("window", 62, 5),
        point("window", 72, 5),
        hline("fence", 11, 10, 24),
        hline("fence", 12, 58, 75),
        point("sign", 30, 20),
        point("sign", 57, 20),
        point("flower", 15, 9),
        point("flower", 17, 9),
        point("flower", 67, 10),
        point("flower", 69, 10),
        point("tree", 7, 15),
        point("tree", 28, 20),
        point("tree", 51, 6),
        point("tree", 77, 18)
    ];
    out
}
fn grove_features() {
    let out = [
        hline("path", 17, 2, 83),
        vline("path", 45, 6, 17),
        vline("path", 70, 11, 17),
        vline("path", 52, 18, 21),
        vline("wall", 19, 2, 12),
        vline("wall", 19, 14, 18),
        vline("wall", 19, 20, 21),
        hline("wall", 8, 34, 44),
        hline("wall", 8, 46, 61),
        vline("wall", 63, 7, 16),
        vline("wall", 63, 18, 20),
        rect("water", 37, 13, 57, 14),
        rect("bridge", 46, 12, 49, 15),
        hline("fence", 4, 41, 49),
        point("flower", 43, 5),
        point("flower", 47, 5),
        point("sign", 30, 17),
        point("sign", 60, 17),
        point("tree", 9, 6),
        point("tree", 12, 9),
        point("tree", 32, 18),
        point("tree", 72, 5),
        point("tree", 76, 15)
    ];
    out
}
fn ruins_features() {
    let out = [
        hline("path", 13, 2, 83),
        vline("path", 36, 6, 13),
        vline("path", 74, 14, 20),
        vline("path", 16, 14, 22),
        vline("wall", 28, 3, 12),
        vline("wall", 28, 14, 22),
        vline("wall", 56, 3, 11),
        vline("wall", 56, 13, 18),
        vline("wall", 56, 20, 22),
        hline("wall", 9, 29, 41),
        hline("wall", 9, 43, 55),
        hline("wall", 17, 11, 15),
        hline("wall", 17, 17, 35),
        hline("rug", 11, 10, 17),
        point("sign", 12, 13),
        point("sign", 58, 13),
        vline("pillar", 20, 6, 11),
        vline("pillar", 44, 13, 19),
        vline("pillar", 69, 5, 9),
        point("tree", 8, 6),
        point("tree", 38, 21),
        point("tree", 78, 16)
    ];
    out
}
fn span_features() {
    let out = [
        hline("path", 13, 2, 83),
        vline("path", 39, 8, 13),
        vline("path", 55, 14, 20),
        hline("wall", 5, 13, 38),
        hline("wall", 5, 40, 72),
        hline("wall", 18, 13, 54),
        hline("wall", 18, 56, 72),
        vline("wall", 22, 6, 12),
        vline("wall", 22, 14, 17),
        vline("wall", 64, 6, 12),
        vline("wall", 64, 14, 17),
        rect("water", 31, 11, 55, 11),
        rect("bridge", 41, 10, 45, 12),
        point("rug", 39, 8),
        point("rug", 55, 19),
        point("sign", 28, 13),
        point("sign", 62, 13),
        point("pillar", 18, 8),
        point("pillar", 70, 20)
    ];
    out
}
fn features(area) {
    if area == "home" {
        home_features()
    } else if area == "grove" {
        grove_features()
    } else if area == "ruins" {
        ruins_features()
    } else {
        span_features()
    }
}
fn terrain(area, x, y) {
    terrain_in(features(area), x, y)
}
fn terrain_in(items, x, y) {
    if x == 0 || y == 0 || x == area_w() - 1 || y == area_h() - 1 {
        return "wall";
    }
    let id = feature_at(items, x, y);
    if id != "" {
        return id;
    }
    "floor"
}
export area_w;
export area_h;
export title;
export key;
export entities;
export entity_at;
export entity_in;
export features;
export terrain;
export terrain_in;
