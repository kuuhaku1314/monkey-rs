fn make_counter(step) {
    let value = 0;
    fn next() {
        value = value + step;
        value
    }
    next
}

let counter = make_counter(3);
let i = 0;
let total = 0;
while i < 100000 {
    total = total + counter();
    i = i + 1;
}
total
