struct Bullet {
    x;
    y;
    dx;
    dy;
    mark;
    color;
    warning;
    warmup;
}
fn clamp(value, low, high) {
    if value < low {
        low
    } else if value > high {
        high
    } else {
        value
    }
}
fn multiple_of(value, step) {
    value / step * step == value
}
fn box_w() {
    36
}
fn box_h() {
    10
}
fn new_bullet(x, y, dx, dy, mark, color) {
    Bullet{
        x: x,
        y: y,
        dx: dx,
        dy: dy,
        mark: mark,
        color: color,
        warning: false,
        warmup: 0
    }
}
fn new_warning(x, y, mark, color, warmup) {
    Bullet{
        x: x,
        y: y,
        dx: 0,
        dy: 0,
        mark: mark,
        color: color,
        warning: true,
        warmup: warmup
    }
}
fn wrap(value, size) {
    let out = value;
    while out >= size {
        out = out - size;
    }
    out
}
fn delayed(frame, every, offset) {
    frame >= offset && multiple_of(frame - offset, every)
}
fn warn_row(bullets, y, color, warmup) {
    let x = 2;
    while x < box_w() - 2 {
        append(bullets, new_warning(x, y, ":", color, warmup));
        x = x + 4;
    }
}
fn warn_col(bullets, x, color, warmup) {
    let y = 1;
    while y < box_h() - 1 {
        append(bullets, new_warning(x, y, ":", color, warmup));
        y = y + 2;
    }
}
fn warn_diag_down(bullets, from_right, color, warmup) {
    let y = 1;
    while y < box_h() - 1 {
        let x = y;
        if from_right {
            x = box_w() - 1 - y;
        }
        if x > 0 && x < box_w() - 1 {
            append(bullets, new_warning(x, y, ":", color, warmup));
        }
        y = y + 2;
    }
}
fn warn_center_columns(bullets, color, warmup) {
    warn_col(bullets, box_w() / 2 - 4, color, warmup);
    warn_col(bullets, box_w() / 2 + 4, color, warmup);
}
fn warn_gate_rows(bullets, color, warmup) {
    warn_row(bullets, box_h() / 2 - 2, color, warmup);
    warn_row(bullets, box_h() / 2 + 2, color, warmup);
}
fn warn_ring(bullets, cx, cy, color, warmup) {
    append(bullets, new_warning(cx - 6, cy, ":", color, warmup));
    append(bullets, new_warning(cx - 3, cy - 2, ":", color, warmup));
    append(bullets, new_warning(cx, cy - 3, ":", color, warmup));
    append(bullets, new_warning(cx + 3, cy - 2, ":", color, warmup));
    append(bullets, new_warning(cx + 6, cy, ":", color, warmup));
    append(bullets, new_warning(cx + 3, cy + 2, ":", color, warmup));
    append(bullets, new_warning(cx, cy + 3, ":", color, warmup));
    append(bullets, new_warning(cx - 3, cy + 2, ":", color, warmup));
}
fn warn_laser_lattice(bullets, color, warmup) {
    let x = 4;
    while x < box_w() - 3 {
        warn_col(bullets, x, color, warmup);
        x = x + 9;
    }
    let y = 2;
    while y < box_h() - 1 {
        warn_row(bullets, y, color, warmup);
        y = y + 3;
    }
}
fn spawn_black_hole_burst(bullets, frame, allow_warnings) {
    let period = 90;
    let delay = 12;
    let cx = box_w() / 2;
    let cy = box_h() / 2;
    if allow_warnings && multiple_of(frame, period) {
        warn_ring(bullets, cx, cy, "gray", delay + 1);
        warn_col(bullets, cx, "gray", delay + 1);
        warn_row(bullets, cy, "gray", delay + 1);
    }
    if delayed(frame, period, delay) {
        append(bullets, new_bullet(1, cy, 1, 0, "=", "bright_magenta"));
        append(bullets, new_bullet(box_w() - 2, cy, -1, 0, "=", "bright_magenta"));
        append(bullets, new_bullet(cx, 1, 0, 1, "|", "bright_cyan"));
        append(bullets, new_bullet(cx, box_h() - 2, 0, -1, "|", "bright_cyan"));
        append(bullets, new_bullet(1, 1, 1, 1, "\\", "bright_magenta"));
        append(bullets, new_bullet(box_w() - 2, 1, -1, 1, "/", "bright_magenta"));
        append(bullets, new_bullet(1, box_h() - 2, 1, -1, "/", "bright_magenta"));
        append(bullets, new_bullet(box_w() - 2, box_h() - 2, -1, -1, "\\", "bright_magenta"));
    }
    if delayed(frame, period, delay + 8) {
        append(bullets, new_bullet(6, 1, 1, 1, "\\", "bright_cyan"));
        append(bullets, new_bullet(box_w() - 7, 1, -1, 1, "/", "bright_cyan"));
        append(bullets, new_bullet(6, box_h() - 2, 1, -1, "/", "bright_cyan"));
        append(bullets, new_bullet(box_w() - 7, box_h() - 2, -1, -1, "\\", "bright_cyan"));
    }
    if delayed(frame, period, delay + 18) {
        let y = 2;
        while y < box_h() - 2 {
            append(bullets, new_bullet(1, y, 1, 0, "=", "bright_yellow"));
            append(bullets, new_bullet(box_w() - 2, box_h() - y - 1, -1, 0, "=", "bright_yellow"));
            y = y + 2;
        }
    }
    if delayed(frame, period, delay + 30) {
        let x = 6;
        while x < box_w() - 5 {
            append(bullets, new_bullet(x, 1, 0, 1, "v", "bright_magenta"));
            append(bullets, new_bullet(box_w() - x - 1, box_h() - 2, 0, -1, "^", "bright_magenta"));
            x = x + 6;
        }
    }
}
fn spawn_sword_convergence(bullets, frame, allow_warnings) {
    let period = 84;
    let delay = 9;
    if allow_warnings && multiple_of(frame, period) {
        let x = 3;
        while x < box_w() - 2 {
            warn_col(bullets, x, "gray", delay + 1);
            x = x + 5;
        }
        warn_diag_down(bullets, false, "gray", delay + 1);
        warn_diag_down(bullets, true, "gray", delay + 1);
    }
    if delayed(frame, period, delay) {
        let x = 3;
        while x < box_w() - 2 {
            append(bullets, new_bullet(x, 1, 0, 1, "v", "bright_yellow"));
            x = x + 5;
        }
    }
    if delayed(frame, period, delay + 14) {
        append(bullets, new_bullet(1, 1, 1, 1, "\\", "bright_yellow"));
        append(bullets, new_bullet(box_w() - 2, 1, -1, 1, "/", "bright_yellow"));
        append(bullets, new_bullet(1, box_h() - 2, 1, -1, "/", "bright_yellow"));
        append(bullets, new_bullet(box_w() - 2, box_h() - 2, -1, -1, "\\", "bright_yellow"));
    }
    if delayed(frame, period, delay + 26) {
        let y = 2;
        while y < box_h() - 1 {
            append(bullets, new_bullet(1, y, 1, 0, ">", "bright_cyan"));
            append(bullets, new_bullet(box_w() - 2, y, -1, 0, "<", "bright_cyan"));
            y = y + 2;
        }
    }
}
fn spawn_laser_lattice(bullets, frame, allow_warnings) {
    let period = 90;
    let delay = 10;
    if allow_warnings && multiple_of(frame, period) {
        warn_laser_lattice(bullets, "gray", delay + 1);
    }
    if delayed(frame, period, delay) {
        let y = 2;
        while y < box_h() - 1 {
            append(bullets, new_bullet(1, y, 1, 0, "=", "bright_magenta"));
            append(bullets, new_bullet(box_w() - 2, y, -1, 0, "=", "bright_magenta"));
            y = y + 3;
        }
        let x = 4;
        while x < box_w() - 3 {
            append(bullets, new_bullet(x, 1, 0, 1, "|", "bright_cyan"));
            x = x + 9;
        }
    }
}
fn spawn_wing_swarm(bullets, frame, allow_warnings) {
    let period = 78;
    let delay = 8;
    if allow_warnings && multiple_of(frame, period) {
        warn_diag_down(bullets, false, "gray", delay + 1);
        warn_diag_down(bullets, true, "gray", delay + 1);
        warn_center_columns(bullets, "gray", delay + 1);
    }
    if delayed(frame, period, delay) {
        let x = 4;
        while x < box_w() - 3 {
            append(bullets, new_bullet(x, 1, 0, 1, "v", "bright_cyan"));
            append(bullets, new_bullet(box_w() - x - 1, box_h() - 2, 0, -1, "^", "bright_cyan"));
            x = x + 7;
        }
        append(bullets, new_bullet(1, 1, 1, 1, "\\", "bright_magenta"));
        append(bullets, new_bullet(box_w() - 2, 1, -1, 1, "/", "bright_magenta"));
    }
}
fn wave_intensity(intensity, wave) {
    if wave == "wild" || wave == "sharp" || wave == "crowded" {
        clamp(intensity + 1, 1, 3)
    } else if wave == "calm" || wave == "quiet" || wave == "lowered" || wave == "clear" {
        clamp(intensity - 1, 1, 3)
    } else {
        intensity
    }
}
fn wave_every(base, wave) {
    if wave == "wild" || wave == "sharp" || wave == "crowded" {
        clamp(base - 3, 8, 30)
    } else if wave == "soft" || wave == "formal" || wave == "mirror" {
        clamp(base + 2, 8, 30)
    } else if wave == "calm" || wave == "quiet" || wave == "lowered" || wave == "clear" {
        clamp(base + 5, 8, 34)
    } else {
        base
    }
}
fn spawn_shade_mode(bullets, frame, intensity, wave, allow_warnings) {
    let power = wave_intensity(intensity, wave);
    let every = wave_every(18 - power * 2, wave);
    let delay = 6;
    if allow_warnings && multiple_of(frame, every) {
        let lane = frame / every * 3;
        while lane >= box_h() - 2 {
            lane = lane - box_h() + 2;
        }
        warn_row(bullets, lane + 1, "gray", delay + 1);
        warn_row(bullets, box_h() - lane - 2, "gray", delay + 1);
    }
    if delayed(frame, every, delay) {
        let source = frame - delay;
        let lane = source / every * 3;
        while lane >= box_h() - 2 {
            lane = lane - box_h() + 2;
        }
        append(bullets, new_bullet(1, lane + 1, 1, 0, "=", "bright_yellow"));
        append(bullets, new_bullet(box_w() - 2, box_h() - lane - 2, -1, 0, "=", "bright_yellow"));
    }
    if allow_warnings && power > 1 && wave != "quiet" && multiple_of(frame, wave_every(24, wave)) {
        let x = frame / 2;
        while x >= box_w() - 2 {
            x = x - box_w() + 2;
        }
        warn_col(bullets, x + 1, "gray", delay + 1);
    }
    if power > 1 && wave != "quiet" && delayed(frame, wave_every(24, wave), delay) {
        let x = (frame - delay) / 2;
        while x >= box_w() - 2 {
            x = x - box_w() + 2;
        }
        append(bullets, new_bullet(x + 1, 1, 0, 1, "|", "bright_cyan"));
    }
}
fn spawn_moth_mode(bullets, frame, intensity, wave, allow_warnings) {
    let power = wave_intensity(intensity, wave);
    let every = wave_every(14, wave);
    let delay = 6;
    if allow_warnings && multiple_of(frame, every) {
        let x = wrap(frame / 2, box_w() - 2) + 1;
        warn_col(bullets, x, "gray", delay + 1);
        warn_col(bullets, box_w() - x - 1, "gray", delay + 1);
    }
    if delayed(frame, every, delay) {
        let x = wrap((frame - delay) / 2, box_w() - 2) + 1;
        append(bullets, new_bullet(x, 1, 0, 1, "v", "bright_cyan"));
        append(bullets, new_bullet(box_w() - x - 1, box_h() - 2, 0, -1, "^", "bright_cyan"));
    }
    if allow_warnings && power > 1 && wave != "calm" && multiple_of(frame, wave_every(24, wave)) {
        warn_diag_down(bullets, false, "gray", delay + 1);
        warn_diag_down(bullets, true, "gray", delay + 1);
    }
    if power > 1 && wave != "calm" && delayed(frame, wave_every(24, wave), delay) {
        append(bullets, new_bullet(1, 1, 1, 1, "\\", "bright_magenta"));
        append(bullets, new_bullet(box_w() - 2, 1, -1, 1, "/", "bright_magenta"));
    }
}
fn spawn_knight_mode(bullets, frame, intensity, wave, allow_warnings) {
    let power = wave_intensity(intensity, wave);
    let every = wave_every(16, wave);
    let delay = 7;
    if allow_warnings && multiple_of(frame, every) {
        let x = wrap(frame / 2, box_w() - 2) + 1;
        warn_col(bullets, x, "gray", delay + 1);
        warn_col(bullets, box_w() - x - 1, "gray", delay + 1);
    }
    if delayed(frame, every, delay) {
        let x = wrap((frame - delay) / 2, box_w() - 2) + 1;
        append(bullets, new_bullet(x, 1, 0, 1, "|", "bright_yellow"));
        append(bullets, new_bullet(box_w() - x - 1, box_h() - 2, 0, -1, "|", "bright_yellow"));
    }
    if allow_warnings && power > 1 && wave != "lowered" && multiple_of(frame, wave_every(26, wave)) {
        let y = wrap(frame / 3, box_h() - 2) + 1;
        warn_row(bullets, y, "gray", delay + 1);
        warn_row(bullets, box_h() - y - 1, "gray", delay + 1);
    }
    if power > 1 && wave != "lowered" && delayed(frame, wave_every(26, wave), delay) {
        let y = wrap((frame - delay) / 3, box_h() - 2) + 1;
        append(bullets, new_bullet(1, y, 1, 0, "=", "bright_yellow"));
        append(bullets, new_bullet(box_w() - 2, box_h() - y - 1, -1, 0, "=", "bright_yellow"));
    }
}
fn spawn_oracle_mode(bullets, frame, intensity, wave, allow_warnings) {
    let power = wave_intensity(intensity, wave);
    let every = wave_every(18, wave);
    let delay = 6;
    if allow_warnings && multiple_of(frame, every) {
        let y = wrap(frame / 4, box_h() - 2) + 1;
        warn_row(bullets, y, "gray", delay + 1);
        warn_row(bullets, box_h() - y - 1, "gray", delay + 1);
    }
    if delayed(frame, every, delay) {
        let y = wrap((frame - delay) / 4, box_h() - 2) + 1;
        append(bullets, new_bullet(1, y, 1, 0, "*", "bright_magenta"));
        append(bullets, new_bullet(box_w() - 2, box_h() - y - 1, -1, 0, "*", "bright_magenta"));
    }
    if allow_warnings && power > 1 && wave != "clear" && multiple_of(frame, wave_every(22, wave)) {
        let x = wrap(frame / 2, box_w() - 2) + 1;
        warn_col(bullets, x, "gray", delay + 1);
    }
    if power > 1 && wave != "clear" && delayed(frame, wave_every(22, wave), delay) {
        let x = wrap((frame - delay) / 2, box_w() - 2) + 1;
        append(bullets, new_bullet(x, 1, 0, 1, "o", "bright_cyan"));
    }
    if wave == "mirror" && delayed(frame, wave_every(22, wave), delay) {
        let x = wrap((frame - delay) / 2 + 8, box_w() - 2) + 1;
        append(bullets, new_bullet(x, box_h() - 2, 0, -1, "o", "bright_cyan"));
    }
}
fn spawn_phase_two(bullets, frame, intensity, pattern, wave, allow_warnings) {
    if pattern == "moth" {
        spawn_wing_swarm(bullets, frame, allow_warnings);
    } else if pattern == "knight" {
        spawn_sword_convergence(bullets, frame, allow_warnings);
    } else if pattern == "oracle" {
        spawn_laser_lattice(bullets, frame, allow_warnings);
    } else {
        spawn_black_hole_burst(bullets, frame, allow_warnings);
    }
}
fn spawn_phase_three(bullets, frame, intensity, pattern, wave, allow_warnings) {
    let power = clamp(wave_intensity(intensity, wave) + 1, 1, 3);
    let every = wave_every(20 - power * 2, wave);
    let delay = 5;
    if allow_warnings && multiple_of(frame, every) {
        warn_row(bullets, wrap(frame / every * 2, box_h() - 2) + 1, "gray", delay + 1);
        warn_col(bullets, wrap(frame / every * 5, box_w() - 2) + 1, "gray", delay + 1);
    }
    if delayed(frame, every, delay) {
        let y = wrap((frame - delay) / every * 2, box_h() - 2) + 1;
        let x = wrap((frame - delay) / every * 5, box_w() - 2) + 1;
        append(bullets, new_bullet(1, y, 1, 0, "=", "bright_yellow"));
        append(bullets, new_bullet(x, 1, 0, 1, "|", "bright_cyan"));
    }
}
fn spawn_phase_wave(bullets, frame, intensity, pattern, wave, phase, allow_warnings) {
    if phase == 1 {
        spawn_phase_two(bullets, frame, intensity, pattern, wave, allow_warnings);
    } else if phase == 2 {
        spawn_phase_three(bullets, frame, intensity, pattern, wave, allow_warnings);
    } else {
        spawn_wave_with_warnings(bullets, frame, intensity, pattern, wave, allow_warnings);
    }
}
fn spawn_wave_with_warnings(bullets, frame, intensity, pattern, wave, allow_warnings) {
    if pattern == "moth" {
        spawn_moth_mode(bullets, frame, intensity, wave, allow_warnings);
    } else if pattern == "knight" {
        spawn_knight_mode(bullets, frame, intensity, wave, allow_warnings);
    } else if pattern == "oracle" {
        spawn_oracle_mode(bullets, frame, intensity, wave, allow_warnings);
    } else {
        spawn_shade_mode(bullets, frame, intensity, wave, allow_warnings);
    }
}
fn spawn_wave(bullets, frame, intensity, pattern, wave) {
    spawn_wave_with_warnings(bullets, frame, intensity, pattern, wave, true)
}
export box_w;
export box_h;
export spawn_wave;
export spawn_wave_with_warnings;
export spawn_phase_wave;
