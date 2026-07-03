fn keys(map) {
    let result = [];
    for key, value in map {
        append(result, key);
    }
    result
}
fn values(map) {
    let result = [];
    for key, value in map {
        append(result, value);
    }
    result
}
fn get_or(map, key, default_value) {
    let value = map[key];
    if value == null {
        default_value
    } else {
        value
    }
}
export keys;
export values;
export get_or;
