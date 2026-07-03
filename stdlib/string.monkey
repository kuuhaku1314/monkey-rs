fn split(text, separator) {
    let result = [];
    let part = "";
    let i = 0;
    while i < len(text) {
        let ch = text[i];
        if ch == separator {
            append(result, part);
            part = "";
        } else {
            part = part + ch;
        }
        i = i + 1;
    }
    append(result, part);
    result
}
export split;
