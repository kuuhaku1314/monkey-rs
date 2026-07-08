import json from "../../stdlib/json.monkey";

let text = "{\"name\":\"tom\",\"age\":18,\"active\":true,\"tags\":[\"a\",null,3],\"nested\":{\"x\":1,\"y\":2}}";
let i = 0;
let total = 0;
while i < 1000 {
    let parsed = json.json_parse(text);
    if parsed.ok {
        total = total + parsed.value.age + parsed.value.nested.x;
    }
    i = i + 1;
}
total
