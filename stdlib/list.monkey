fn range(start, end) {
    let values = [];
    let i = start;
    while i < end {
        append(values, i);
        i = i + 1;
    }
    values
}
fn sum(values) {
    let total = 0;
    for i, value in values {
        total = total + value;
    }
    total
}
fn contains(values, expected) {
    for i, value in values {
        if value == expected {
            return true;
        }
    }
    false
}
export range;
export sum;
export contains;
export sort;
