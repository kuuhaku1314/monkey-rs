fn sum_loop(n) {
    let i = 0;
    let total = 0;
    while i < n {
        total = total + i;
        i = i + 1;
    }
    total
}

let i = 0;
let total = 0;
while i < 5000 {
    total = total + sum_loop(200);
    i = i + 1;
}
total
