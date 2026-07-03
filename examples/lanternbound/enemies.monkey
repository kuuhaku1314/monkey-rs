import items from "items.monkey";
fn clamp(value, low, high) {
    if value < low {
        low
    } else if value > high {
        high
    } else {
        value
    }
}
fn is_true(value) {
    !is_null(value) && value == true
}
fn hp_stage(e) {
    if e.hp * 100 <= e.max_hp * 30 {
        2
    } else if e.hp * 100 <= e.max_hp * 60 {
        1
    } else {
        0
    }
}
fn stage_name(e) {
    let stage = hp_stage(e);
    if stage == 2 {
        "critical"
    } else if stage == 1 {
        "pressured"
    } else {
        "steady"
    }
}
fn stage_color(e) {
    let stage = hp_stage(e);
    if stage == 2 {
        "bright_red"
    } else if stage == 1 {
        "bright_yellow"
    } else {
        e.color
    }
}
fn enemy(name) {
    if name == "moth" {
        {
            name: "Moth Archivist",
            hp: 24,
            max_hp: 24,
            mercy: 0,
            mood: "nervous",
            color: "bright_cyan",
            shape: "moth",
            phase: "guarded",
            wave: "normal"
        }
    } else if name == "knight" {
        {
            name: "Tin Knight",
            hp: 34,
            max_hp: 34,
            mercy: 0,
            mood: "proud",
            color: "bright_yellow",
            shape: "knight",
            phase: "guarded",
            wave: "normal"
        }
    } else if name == "oracle" {
        {
            name: "Glass Oracle",
            hp: 42,
            max_hp: 42,
            mercy: 0,
            mood: "lonely",
            color: "bright_magenta",
            shape: "oracle",
            phase: "guarded",
            wave: "normal"
        }
    } else {
        {
            name: "Small Shade",
            hp: 18,
            max_hp: 18,
            mercy: 0,
            mood: "lost",
            color: "gray",
            shape: "shade",
            phase: "guarded",
            wave: "normal"
        }
    }
}
fn portrait_lines(e) {
    if e.shape == "moth" {
        if hp_stage(e) == 2 {
            [
                "        \\  .|.  /     " + e.name,
                "     .--\\\\ | //--.    mood: " + e.mood + " / " + stage_name(e),
                "    /   . \\___/ . \\",
                "   |    . (o o) .  | torn pages whirl",
                "    \\__.._/---\\_.._/",
                "        ./| M |\\.",
                "       ./_|___|_\\."
            ]
        } else if hp_stage(e) == 1 {
            [
                "        \\  |  /      " + e.name,
                "     .---\\ | /---.   mood: " + e.mood + " / " + stage_name(e),
                "    /   . \\___/   \\",
                "   |      (o o)    | paper wings beat hard",
                "    \\_____/---\\___/",
                "        ./| M |\\",
                "       ./_|___|_\\"
            ]
        } else {
            [
                "        \\   |   /     " + e.name,
                "     .---\\  |  /---.  mood: " + e.mood + " / " + stage_name(e),
                "    /     \\___/     \\",
                "   |      (o o)      | paper wings rattle",
                "    \\_____/---\\_____/",
                "         /| M |\\",
                "        /_|___|_\\"
            ]
        }
    } else if e.shape == "knight" {
        if hp_stage(e) == 2 {
            [
                "          /^^^/\\      " + e.name,
                "         |[#//#]|     mood: " + e.mood + " / " + stage_name(e),
                "         | _//_ |",
                "      ==/|==//==|\\==  the sword rings loudly",
                "         |  ||  |",
                "        /__//|___\\",
                "          _/  \\_"
            ]
        } else if hp_stage(e) == 1 {
            [
                "          /^^^^\\      " + e.name,
                "         |[##/#]|     mood: " + e.mood + " / " + stage_name(e),
                "         |  __  |",
                "       =/|======|\\=   its tin sword shakes",
                "         |  ||  |",
                "        /___||___\\",
                "          _/  \\_"
            ]
        } else {
            [
                "          /^^^^\\      " + e.name,
                "         |[####]|     mood: " + e.mood + " / " + stage_name(e),
                "         |  __  |",
                "        /|======|\\    its tin sword shakes",
                "         |  ||  |",
                "        /___||___\\",
                "          _/  \\_"
            ]
        }
    } else if e.shape == "oracle" {
        if hp_stage(e) == 2 {
            [
                "           .-//-.     " + e.name,
                "        .-' O//O'-.   mood: " + e.mood + " / " + stage_name(e),
                "       /  . /__\\ . \\",
                "       |  ./\\__/\\. | every face shatters",
                "        '-. _//_ .-'",
                "           '--//'",
                "           /_//_\\"
            ]
        } else if hp_stage(e) == 1 {
            [
                "           .----.     " + e.name,
                "        .-'  OO '-.   mood: " + e.mood + " / " + stage_name(e),
                "       /  . /__\\ .  \\",
                "       |    \\__/    | reflections crowd close",
                "        '-. ____ .-'",
                "           '--.-'",
                "           /____\\"
            ]
        } else {
            [
                "           .----.     " + e.name,
                "        .-'  OO '-.   mood: " + e.mood + " / " + stage_name(e),
                "       /    /__\\    \\",
                "       |    \\__/    | every reflection waits",
                "        '-. ____ .-'",
                "           '----'",
                "           /____\\"
            ]
        }
    } else {
        if hp_stage(e) == 2 {
            [
                "       .-''''''-.     " + e.name,
                "      / .--..--. \\    mood: " + e.mood + " / " + stage_name(e),
                "     | / !!  !! \\ |",
                "      \\  '--''--' /   dark pulls inward",
                "       '-..__..-'",
                "        ./|  |\\.",
                "      _./ |__| \\._"
            ]
        } else if hp_stage(e) == 1 {
            [
                "        .-''''-.      " + e.name,
                "       / .----. \\     mood: " + e.mood + " / " + stage_name(e),
                "      |  / !! \\  |",
                "       \\  '--'  /     its outline jitters",
                "        '-.__.-'",
                "         ./| |\\",
                "       _/ |__| \\_"
            ]
        } else {
            [
                "         .-''''-.     " + e.name,
                "        /  .--.  \\    mood: " + e.mood + " / " + stage_name(e),
                "       |  / !! \\  |",
                "        \\  '--'  /    it studies your light",
                "         '-.__.-'",
                "          /|  |\\",
                "        _/ |__| \\_"
            ]
        }
    }
}
fn enemy_line(e, moment) {
    if moment == "after" && hp_stage(e) == 2 {
        if e.shape == "moth" {
            return "The Moth Archivist beats torn wings into a frantic storm.";
        } else if e.shape == "knight" {
            return "The Tin Knight locks its dented stance and raises every blade it remembers.";
        } else if e.shape == "oracle" {
            return "The Glass Oracle splinters the room into too many futures.";
        } else {
            return "The Small Shade pulls the light inward until the box feels smaller.";
        }
    }
    if moment == "after" && hp_stage(e) == 1 {
        if e.shape == "moth" {
            return "Loose paper circles faster around the Moth Archivist.";
        } else if e.shape == "knight" {
            return "The Knight's armor ticks like a clock under pressure.";
        } else if e.shape == "oracle" {
            return "A second reflection steps out of sync with the Oracle.";
        } else {
            return "The Shade redraws its outline with sharp, nervous corners.";
        }
    }
    if e.shape == "moth" {
        if moment == "intro" {
            "The Archivist beats dust from its wings: \"Do not turn the page so loudly.\""
        } else if moment == "after" {
            "Loose paper settles around the Moth Archivist."
        } else if e.phase == "resting" || e.phase == "spare" {
            "\"The lamp is quiet now. I can read the dark without fear.\""
        } else if e.phase == "heard" {
            "\"You heard it too? The shelves have been crying all night.\""
        } else if e.phase == "startled" {
            "\"No, no, not dark yet. I have not found my sentence.\""
        } else if e.phase == "struck" {
            "\"Ink spills when a page is hit instead of read.\""
        } else {
            "\"Every bright thing becomes a command if it stares long enough.\""
        }
    } else if e.shape == "knight" {
        if moment == "intro" {
            "The Tin Knight raises a dented blade: \"Name your banner.\""
        } else if moment == "after" {
            "The Knight counts your steps like a marching drum."
        } else if e.phase == "disarmed" || e.phase == "spare" {
            "\"A lowered sword is still a promise. I remember.\""
        } else if e.phase == "recognized" {
            "\"Protocol acknowledged. Continue with honor.\""
        } else if e.phase == "rattled" {
            "\"Respect first. Peace second. That is the old order.\""
        } else if e.phase == "struck" {
            "\"So. A duel, not a parley.\""
        } else {
            "\"The bridge fell because someone forgot the salute.\""
        }
    } else if e.shape == "oracle" {
        if moment == "intro" {
            "The Glass Oracle turns all its faces toward you."
        } else if moment == "after" {
            "A small future blinks, then hides behind the glass."
        } else if e.phase == "clear" || e.phase == "spare" {
            "\"At last, one reflection that does not interrupt me.\""
        } else if e.phase == "curious" {
            "\"Describe me again. This time, leave out the cracks.\""
        } else if e.phase == "crowded" {
            "\"Waiting is not silence when every mirror repeats it.\""
        } else if e.phase == "struck" {
            "\"Violence is also a reflection. An ugly one.\""
        } else {
            "\"I have seen you arrive. I have not seen why.\""
        }
    } else {
        if moment == "intro" {
            "The Small Shade lifts its head as if remembering a name."
        } else if moment == "after" {
            "The Shade redraws its outline with less fear."
        } else if e.phase == "quiet" || e.phase == "spare" {
            "\"If I stay small, will the room stay kind?\""
        } else if e.phase == "calmer" {
            "\"Your voice has corners. I can sit beside it.\""
        } else if e.phase == "struck" {
            "\"Small things bruise loudly.\""
        } else {
            "\"I was left where the lantern could not reach.\""
        }
    }
}
fn act_options(e) {
    if e.shape == "moth" {
        [
            {id: "check", label: "CHECK"},
            {id: "listen", label: "LISTEN"},
            {id: "dim", label: "DIM LIGHT"}
        ]
    } else if e.shape == "knight" {
        [
            {id: "check", label: "CHECK"},
            {id: "salute", label: "SALUTE"},
            {id: "lower", label: "LOWER SWORD"}
        ]
    } else if e.shape == "oracle" {
        [
            {id: "check", label: "CHECK"},
            {id: "reflect", label: "REFLECT"},
            {id: "wait", label: "WAIT"}
        ]
    } else {
        [
            {id: "check", label: "CHECK"},
            {id: "comfort", label: "COMFORT"},
            {id: "hum", label: "HUM"}
        ]
    }
}
fn add_mercy(e, amount) {
    e.mercy = clamp(e.mercy + amount, 0, 100);
}
fn settle_spare(e) {
    if e.mercy >= 100 {
        e.phase = "spare";
        if e.shape == "moth" {
            e.wave = "calm";
        } else if e.shape == "knight" {
            e.wave = "lowered";
        } else if e.shape == "oracle" {
            e.wave = "clear";
        } else {
            e.wave = "quiet";
        }
    }
}
fn act_result(e, id) {
    let intensity = 1;
    let shield = 0;
    let log = "";
    let line = "";
    if id == "check" {
        intensity = 0;
        e.wave = "normal";
        log = e.name + " - HP " + str(e.hp) + ". Mood: " + e.mood + ".";
        line = enemy_line(e, "before");
    } else if e.shape == "moth" {
        if id == "listen" {
            e.mood = "heard";
            e.phase = "heard";
            e.wave = "soft";
            e.listened = true;
            add_mercy(e, 42);
            intensity = 1;
            log = "You let the wingbeats become words. Mercy rises.";
            line = "\"Someone wrote my fear in the margins. You read it back softly.\"";
        } else if id == "dim" {
            if is_true(e.listened) {
                e.mood = "resting";
                e.phase = "resting";
                e.wave = "calm";
                add_mercy(e, 58);
                shield = 1;
                log = "You dim the lantern. The moth folds its panic away.";
                line = "\"Thank you. The dark is a blanket when it arrives slowly.\"";
            } else {
                e.phase = "startled";
                e.wave = "wild";
                add_mercy(e, 18);
                intensity = 2;
                log = "You dim the lantern too soon. The moth searches wildly.";
                line = "\"Do not close the book while I am still inside it.\"";
            }
        }
    } else if e.shape == "knight" {
        if id == "salute" {
            e.mood = "recognized";
            e.phase = "recognized";
            e.wave = "formal";
            e.saluted = true;
            add_mercy(e, 30);
            intensity = 1;
            log = "You salute the Tin Knight. Its visor dips back.";
            line = "\"A correct greeting. The old road still has witnesses.\"";
        } else if id == "lower" {
            if is_true(e.saluted) {
                e.mood = "disarmed";
                e.phase = "disarmed";
                e.wave = "lowered";
                add_mercy(e, 60);
                shield = 2;
                log = "You lower your hands first. The knight lowers its sword.";
                line = "\"Then I will guard the silence instead.\"";
            } else {
                e.phase = "rattled";
                e.wave = "sharp";
                add_mercy(e, 12);
                intensity = 2;
                log = "You ask for peace before respect. The sword shakes harder.";
                line = "\"A shortcut through ceremony cuts both hands.\"";
            }
        }
    } else if e.shape == "oracle" {
        if id == "reflect" {
            e.mood = "curious";
            e.phase = "curious";
            e.wave = "mirror";
            e.reflected = true;
            add_mercy(e, 34);
            intensity = 1;
            log = "You describe what the glass reflects: a lonely watcher.";
            line = "\"Lonely. Yes. A word with only one face.\"";
        } else if id == "wait" {
            if is_true(e.reflected) {
                e.mood = "clear";
                e.phase = "clear";
                e.wave = "clear";
                add_mercy(e, 55);
                shield = 1;
                log = "You wait without asking. The Oracle sees itself soften.";
                line = "\"You did not demand a prophecy. That is almost mercy.\"";
            } else {
                e.phase = "crowded";
                e.wave = "crowded";
                add_mercy(e, 22);
                intensity = 2;
                log = "You wait. The reflections multiply around you.";
                line = "\"Patience without seeing is only another mirror.\"";
            }
        }
    } else {
        if id == "comfort" {
            e.mood = "calmer";
            e.phase = "calmer";
            e.wave = "soft";
            e.comforted = true;
            add_mercy(e, 36);
            intensity = 1;
            log = "You tell the shade it does not have to be a shadow here.";
            line = "\"Then what shape should I be while I learn?\"";
        } else if id == "hum" {
            if is_true(e.comforted) {
                e.mood = "quiet";
                e.phase = "quiet";
                e.wave = "quiet";
                add_mercy(e, 58);
                shield = 1;
                log = "You hum a small tune. The shade remembers a softer room.";
                line = "\"I know this song. I was not always lost.\"";
            } else {
                e.wave = "soft";
                add_mercy(e, 24);
                intensity = 1;
                log = "You hum into the dark. Something inside it listens.";
                line = "\"Again. Not louder. Just again.\"";
            }
        }
    }
    if log == "" {
        log = "Nothing answers that act.";
        intensity = 0;
        line = enemy_line(e, "before");
    }
    settle_spare(e);
    {log: log, intensity: intensity, shield: shield, line: line}
}
fn item_result(game, e, item) {
    let log = items.use_battle_item(game, e, item);
    let line = "";
    if item == "spark_charm" {
        e.wave = "soft";
        settle_spare(e);
        line = "The Spark Charm bends the attack into a gentler rhythm.";
    } else if item == "moon_tea" {
        line = "The tea steadies your hands. The next pattern feels readable.";
    } else {
        line = enemy_line(e, "before");
    }
    {log: log, intensity: 1, shield: 2, line: line}
}
export enemy;
export hp_stage;
export stage_name;
export stage_color;
export portrait_lines;
export enemy_line;
export act_options;
export act_result;
export item_result;
export settle_spare;
