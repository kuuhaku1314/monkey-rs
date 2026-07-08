let values = [];
let i = 0;
while i < 30000 {
    append(values, i);
    i = i + 1;
}

let total = 0;
for index, value in values {
    total = total + value;
}
total
