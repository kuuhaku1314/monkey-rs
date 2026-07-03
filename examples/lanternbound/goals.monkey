import items from "items.monkey";
import story from "story.monkey";
fn area_intro(area) {
    if area == "home" {
        "Paper Moon Home is quiet, but Caretaker Nara waits by the road."
    } else if area == "grove" {
        "Bluebell Grove glows with moth dust. Listen before the wings scatter."
    } else if area == "ruins" {
        "Clockroot Ruins count every step. The Glass Key waits among broken stone."
    } else if area == "span" {
        "The Glass Span reflects every name you have chosen to carry."
    } else {
        "The road changes its breath."
    }
}
fn area_tip(area) {
    if area == "home" {
        "Talk to Nara, read signs, then follow the east road."
    } else if area == "grove" {
        "Seek LISTEN with the Moth Scribe and answer the archivist."
    } else if area == "ruins" {
        "Seek REMEMBER, take the Glass Key, then continue east."
    } else if area == "span" {
        "Seek RETURN, face the oracle, and reach Morning Gate."
    } else {
        "Follow yellow targets and keep your lantern lit."
    }
}
fn has_key(game) {
    items.count(game, "glass_key") > 0
}
fn objective(game) {
    if !story.has_flag(game, "kindle") {
        return "Quest: find KINDLE by speaking with Caretaker Nara.";
    }
    if !story.has_flag(game, "listen") {
        return "Quest: go east to Bluebell Grove and find LISTEN.";
    }
    if !story.has_flag(game, "remember") {
        return "Quest: cross to Clockroot Ruins and find REMEMBER.";
    }
    if !story.has_flag(game, "return") {
        return "Quest: reach The Glass Span and find RETURN.";
    }
    if !has_key(game) {
        return "Quest: take the Glass Key from Clockroot Ruins.";
    }
    "Quest: bring four names and the Glass Key to Morning Gate."
}
fn progress(game) {
    let key = "no";
    if has_key(game) {
        key = "yes";
    }
    "Names " + str(game.glow) + "/4 | Glass " + key
}
fn reward_message(name) {
    if name == "" {
        ""
    } else {
        "A lantern name settles into your light: " + name + "."
    }
}
fn item_note(id) {
    if id == "glass_key" {
        "The Glass Key is cold and bright. Morning Gate will know this shape."
    } else if id == "spark_charm" {
        "The Spark Charm crackles softly. Someone may be able to read it."
    } else if id == "moon_tea" {
        "Moon Tea steams in your pack. It might comfort someone cold."
    } else {
        ""
    }
}
export area_intro;
export area_tip;
export objective;
export progress;
export reward_message;
export item_note;
