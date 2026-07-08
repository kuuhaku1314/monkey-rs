fn min(a, b) {
    if a < b {
        a
    } else {
        b
    }
}
fn pop(stack) {
    delete(stack, len(stack) - 1)
}
fn top(stack) {
    stack[len(stack) - 1]
}
fn trap_rainwater(heights) {
    let stack = [];
    let water = 0;
    let i = 0;
    while i < len(heights) {
        while len(stack) > 0 && heights[i] > heights[top(stack)] {
            let bottom = pop(stack);
            if len(stack) == 0 {
                break;
            }
            let left = top(stack);
            let width = i - left - 1;
            let bounded_height = min(heights[left], heights[i]) - heights[bottom];
            if bounded_height > 0 {
                water = water + width * bounded_height;
            }
        }
        append(stack, i);
        i = i + 1;
    }
    water
}
fn check(name, heights, expected) {
    let actual = trap_rainwater(heights);
    assert(actual == expected, name + " expected " + str(expected) + ", got " + str(actual));
    print(name + " -> " + str(actual) + "\n");
}
check("classic", [0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1], 6);
check("flat", [1, 1, 1], 0);
check("valley", [4, 2, 0, 3, 2, 5], 9);
check("small bowl", [2, 0, 2], 2);
print("rainwater stack tests passed\n");
