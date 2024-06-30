let prefix = "running fib fn times = ";
let times = 1;
fn fib(n) {
    print(prefix + times + "\n");
    times = times + 1; // outer variable closure
    return if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    };
}

let fib_map = {};
let n = 5;
let key = n - 2;
while n > 0 {
    fib_map[n] = fib(n);
    n = n - 1;
}
print("map size = " + len(fib_map) + "\n");
let vec = [];
print("remove map key " + key + " to vec\n");
append(vec, delete(fib_map, key));
print("now map size = " + len(fib_map) + ", vec size = " + len(vec) + "\n");
print("vec index 0 value = ", vec[0] + "\n");
print("invoke fib times = " + (times - 1) + "\n");

print("test assign\n");
let k = n = len(vec);
print(k, n, k == len(vec));

print("\n");
print("test for in collection\n");
for index, element in vec {
    print("vec index:" + index + ",value:" + element +"\n");
}
for k, v in fib_map {
    print("map key:" + k + ",value:" + v +"\n");
}

print("\n");
print("test type converter");
let str_number = "123.2";
let number = 123;
print(float(str_number) + 10);
print("\n");
print(str_number + str(number));

print("\n");
print("test embed block");
let block_fn = fn(n) {
    {{{{n}}}}
};
let block_inner_value = block_fn(5);
print("\n");
print(block_inner_value == 5);
