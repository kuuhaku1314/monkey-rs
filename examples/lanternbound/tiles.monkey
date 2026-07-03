fn width() {
    5
}
fn height() {
    3
}
fn passable(id) {
    id == "floor" || id == "path" || id == "bridge" || id == "flower" || id == "door" || id == "rug"
}
fn color(id) {
    if id == "player_up" || id == "player_down" || id == "player_left" || id == "player_right" {
        "bright_yellow"
    } else if id == "wall" {
        "dark_gray"
    } else if id == "house" {
        "bright_yellow"
    } else if id == "tree" {
        "bright_green"
    } else if id == "water" {
        "bright_blue"
    } else if id == "bridge" {
        "yellow"
    } else if id == "pillar" {
        "gray"
    } else if id == "path" {
        "yellow"
    } else if id == "flower" {
        "bright_magenta"
    } else if id == "fence" {
        "yellow"
    } else if id == "door" {
        "bright_yellow"
    } else if id == "window" {
        "bright_cyan"
    } else if id == "rug" {
        "bright_red"
    } else if id == "sign" {
        "bright_yellow"
    } else if id == "item_tea" {
        "bright_green"
    } else if id == "item_charm" {
        "bright_magenta"
    } else if id == "item_key" {
        "bright_cyan"
    } else if id == "caretaker" || id == "moth_scribe" || id == "root_clock" || id == "bridge_voice" || id == "child" {
        "bright_cyan"
    } else if id == "save_lamp" {
        "bright_green"
    } else if id == "trace" {
        "gray"
    } else if id == "shade" || id == "moth_foe" || id == "knight_foe" || id == "oracle_foe" {
        "bright_magenta"
    } else if id == "gate_right" || id == "gate_left" {
        "bright_white"
    } else {
        "gray"
    }
}
fn row(id, part) {
    if id == "player_up" {
        if part == 0 {
            "  o  "
        } else if part == 1 {
            " /|\\ "
        } else {
            " / \\ "
        }
    } else if id == "player_down" {
        if part == 0 {
            "  o  "
        } else if part == 1 {
            " /|\\ "
        } else {
            " / \\ "
        }
    } else if id == "player_left" {
        if part == 0 {
            "  o  "
        } else if part == 1 {
            "</|  "
        } else {
            " / \\ "
        }
    } else if id == "player_right" {
        if part == 0 {
            "  o  "
        } else if part == 1 {
            "  |\\>"
        } else {
            " / \\ "
        }
    } else if id == "wall" {
        if part == 1 {
            "#===#"
        } else {
            "#####"
        }
    } else if id == "house" {
        if part == 0 {
            "/---\\"
        } else if part == 1 {
            "|_#_|"
        } else {
            "|_|_|"
        }
    } else if id == "tree" {
        if part == 0 {
            " ^^^ "
        } else if part == 1 {
            "/|||\\"
        } else {
            "  |  "
        }
    } else if id == "water" {
        if part == 1 {
            "~ ~ ~"
        } else {
            "~~~~~"
        }
    } else if id == "bridge" {
        if part == 1 {
            "==.=="
        } else {
            "====="
        }
    } else if id == "pillar" {
        if part == 0 {
            " .-. "
        } else if part == 1 {
            " | | "
        } else {
            "_|_|_"
        }
    } else if id == "path" {
        if part == 0 {
            "....."
        } else if part == 1 {
            ".. .."
        } else {
            "....."
        }
    } else if id == "flower" {
        if part == 0 {
            " .*. "
        } else if part == 1 {
            ".*.*."
        } else {
            "  |  "
        }
    } else if id == "fence" {
        if part == 0 {
            "| | |"
        } else if part == 1 {
            "====="
        } else {
            "| | |"
        }
    } else if id == "door" {
        if part == 0 {
            " ___ "
        } else if part == 1 {
            "| . |"
        } else {
            "|___|"
        }
    } else if id == "window" {
        if part == 0 {
            ".---."
        } else if part == 1 {
            "|+ +|"
        } else {
            "'---'"
        }
    } else if id == "rug" {
        if part == 0 {
            "-----"
        } else if part == 1 {
            "-===-"
        } else {
            "-----"
        }
    } else if id == "sign" {
        if part == 0 {
            ".---."
        } else if part == 1 {
            "| ? |"
        } else {
            "  |  "
        }
    } else if id == "item_tea" {
        if part == 0 {
            " ___ "
        } else if part == 1 {
            "(tea)"
        } else {
            " '-' "
        }
    } else if id == "item_charm" {
        if part == 0 {
            " .*. "
        } else if part == 1 {
            "<***>"
        } else {
            "  '  "
        }
    } else if id == "item_key" {
        if part == 0 {
            "  o--"
        } else if part == 1 {
            " /|  "
        } else {
            "  '--"
        }
    } else if id == "caretaker" {
        if part == 0 {
            " .-. "
        } else if part == 1 {
            "|o o|"
        } else {
            "/|_|\\"
        }
    } else if id == "child" {
        if part == 0 {
            " .-. "
        } else if part == 1 {
            "(o_o)"
        } else {
            " /Y\\ "
        }
    } else if id == "moth_scribe" {
        if part == 0 {
            "\\o.o/"
        } else if part == 1 {
            " /M\\ "
        } else {
            "/| |\\"
        }
    } else if id == "root_clock" {
        if part == 0 {
            " .O. "
        } else if part == 1 {
            "-|R|-"
        } else {
            " / \\ "
        }
    } else if id == "bridge_voice" {
        if part == 0 {
            "~~~~~"
        } else if part == 1 {
            "< o >"
        } else {
            "~~~~~"
        }
    } else if id == "save_lamp" {
        if part == 0 {
            " ___ "
        } else if part == 1 {
            "[ S ]"
        } else {
            "|___|"
        }
    } else if id == "trace" {
        if part == 1 {
            "  *  "
        } else {
            "     "
        }
    } else if id == "shade" {
        if part == 0 {
            " .-. "
        } else if part == 1 {
            "(! !)"
        } else {
            "/___\\"
        }
    } else if id == "moth_foe" {
        if part == 0 {
            "\\M!//"
        } else if part == 1 {
            " /#\\ "
        } else {
            "/| |\\"
        }
    } else if id == "knight_foe" {
        if part == 0 {
            "/^^\\ "
        } else if part == 1 {
            "[##] "
        } else {
            "/||\\ "
        }
    } else if id == "oracle_foe" {
        if part == 0 {
            ".-O-."
        } else if part == 1 {
            "|OO| "
        } else {
            "'-=-'"
        }
    } else if id == "gate_right" {
        if part == 1 {
            " >>> "
        } else {
            "     "
        }
    } else if id == "gate_left" {
        if part == 1 {
            " <<< "
        } else {
            "     "
        }
    } else if part == 1 {
        "  .  "
    } else {
        "     "
    }
}
fn portrait_label(kind) {
    if kind == "caretaker" {
        "Nara: caretaker"
    } else if kind == "child" {
        "river child"
    } else if kind == "moth_scribe" {
        "moth scribe"
    } else if kind == "root_clock" {
        "root clock"
    } else if kind == "bridge_voice" {
        "bridge voice"
    } else if kind == "save_lamp" {
        "nearest: save lamp"
    } else if kind == "shade" {
        "small shade"
    } else if kind == "moth_foe" {
        "moth archivist"
    } else if kind == "knight_foe" {
        "tin knight"
    } else if kind == "oracle_foe" {
        "glass oracle"
    } else if kind == "trace" {
        "nearest: answered place"
    } else if kind == "gate_right" || kind == "gate_left" {
        "nearest: passage"
    } else if kind == "sign" {
        "nearest: readable sign"
    } else if kind == "item_tea" {
        "item: Moon Tea"
    } else if kind == "item_charm" {
        "item: Spark Charm"
    } else if kind == "item_key" {
        "item: Glass Key"
    } else {
        "you: lantern keeper"
    }
}
fn portrait(kind) {
    if kind == "caretaker" {
        [
            "       .-----.",
            "      /  . .  \\",
            "     |    _    |",
            "     |  /___\\  |",
            "      \\       /",
            "       '-...-'",
            "       /|===|\\",
            "      /_|___|_\\",
            "   " + portrait_label(kind)
        ]
    } else if kind == "child" {
        [
            "       .---.",
            "      / o o \\",
            "      |  _  |",
            "      | / \\ |",
            "       \\___/",
            "       /| |\\",
            "      /_| |_\\",
            "      small coat",
            "   " + portrait_label(kind)
        ]
    } else if kind == "moth_scribe" {
        [
            "       \\  |  /",
            "    .---\\ | /---.",
            "   /     \\_/     \\",
            "   \\    (o o)    /",
            "    '---/___\\---'",
            "        /|M|\\",
            "       /_| |_\\",
            "      winged scribe",
            "   " + portrait_label(kind)
        ]
    } else if kind == "root_clock" {
        [
            "       .-----.",
            "      /  13   \\",
            "     |   ||    |",
            "     |  /  \\   |",
            "      \\  --   /",
            "       '-----'",
            "       _/||\\_",
            "      rooted clock",
            "   " + portrait_label(kind)
        ]
    } else if kind == "bridge_voice" {
        [
            "      ~~~~~~~~~",
            "    ~~  .---.  ~~",
            "   ~~  / o o \\  ~~",
            "    ~~ \\_ ^ _/ ~~",
            "      ~~\\___/~~",
            "        _|||_",
            "      __|||||__",
            "      river voice",
            "   " + portrait_label(kind)
        ]
    } else if kind == "save_lamp" {
        [
            "        .-.",
            "      _/ S \\_",
            "     /  ___  \\",
            "     | |   | |",
            "     |_|___|_|",
            "       _| |_",
            "       SAVE",
            "    press interact",
            "   " + portrait_label(kind)
        ]
    } else if kind == "shade" {
        [
            "      .-''''-.",
            "     /  .--.  \\",
            "    |  / !! \\  |",
            "     \\  '--'  /",
            "      '-.__.-'",
            "       /|  |\\",
            "     _/ |__| \\_",
            "   answer or fight",
            "   " + portrait_label(kind)
        ]
    } else if kind == "moth_foe" {
        [
            "       \\  |  /",
            "    .---\\ | /---.",
            "   /    (M!)    \\",
            "   \\    /###\\   /",
            "    '---.   .---'",
            "        /| |\\",
            "       /_| |_\\",
            "      wings panic",
            "   " + portrait_label(kind)
        ]
    } else if kind == "knight_foe" {
        [
            "        /^^^\\",
            "       |[###]|",
            "       |  _  |",
            "      /|=====|\\",
            "       |  |  |",
            "       /__|__\\",
            "        _/ \\_",
            "     armor rattles",
            "   " + portrait_label(kind)
        ]
    } else if kind == "oracle_foe" {
        [
            "        .----.",
            "     .-'  OO '-.",
            "    /    /__\\    \\",
            "    |    \\__/    |",
            "     '-. ____ .-'",
            "        '----'",
            "        /____\\",
            "     glass watches",
            "   " + portrait_label(kind)
        ]
    } else if kind == "trace" {
        [
            "      .    .",
            "   .    ..    .",
            "     .-    -.",
            "    (  warm  )",
            "     '-.__.-'",
            "        ||",
            "      __||__",
            "   already answered",
            "   " + portrait_label(kind)
        ]
    } else if kind == "gate_right" || kind == "gate_left" {
        [
            "      ______",
            "     / ____ \\",
            "    / /    \\ \\",
            "   | |  <>  | |",
            "   | |      | |",
            "    \\ \\____/ /",
            "     \\______/ ",
            "   step through",
            "   " + portrait_label(kind)
        ]
    } else if kind == "sign" {
        [
            "       .---.",
            "      / ? /|",
            "     /___/ |",
            "     |   | |",
            "     |___|/",
            "       ||",
            "       ||",
            "   face it, then read",
            "   " + portrait_label(kind)
        ]
    } else if kind == "item_tea" {
        [
            "        ___",
            "       /___\\",
            "      ( tea )",
            "       \\___/",
            "        |_|",
            "      restores",
            "         HP",
            "   pick it up",
            "   " + portrait_label(kind)
        ]
    } else if kind == "item_charm" {
        [
            "        .*. ",
            "      .* * *.",
            "     <  ***  >",
            "      '* * *'",
            "        '*'",
            "      mercy",
            "      rises",
            "   pick it up",
            "   " + portrait_label(kind)
        ]
    } else if kind == "item_key" {
        [
            "        o====",
            "       /|",
            "      / |====",
            "        |",
            "        '====",
            "      morning",
            "       gate",
            "   pick it up",
            "   " + portrait_label(kind)
        ]
    } else {
        [
            "       .-.",
            "      (o o)",
            "     /|_-_|\\",
            "      /| |\\",
            "     /_| |_\\",
            "       / \\",
            "      /___\\",
            "   small but stubborn",
            "   " + portrait_label(kind)
        ]
    }
}
export width;
export height;
export passable;
export color;
export row;
export portrait;
