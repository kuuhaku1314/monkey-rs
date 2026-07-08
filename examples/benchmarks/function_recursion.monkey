fn sum_to(n, acc) {
    if n == 0 {
        acc
    } else {
        sum_to(n - 1, acc + n)
    }
}

let i = 0;
let total = 0;
while i < 2000 {
    total = total + sum_to(50, 0);
    i = i + 1;
}
total
