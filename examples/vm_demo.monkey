fn make_score(seed) {
    fn score(value) {
        seed + value * 2
    }
    score
}
let score = make_score(5);
let values = [1, 2, 3, 4, 5];
let total = 0;
for i, value in values {
    if value == 2 {
        continue;
    }
    if value > 4 {
        break;
    }
    total = total + score(value);
}
let result = {ok: true, value: total, error: null};
print("vm score: " + str(result.value));
