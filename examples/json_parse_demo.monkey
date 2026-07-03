import json from "stdlib/json.monkey";
let data = json.json_parse("{\"name\":\"tom\",\"age\":12,\"active\":true,\"tags\":[\"a\",null,3]}");
data = data.value;
print("name=", data.name, "\n");
print("age=", data.age, "\n");
print("active=", data.active, "\n");
print("tag1_is_null=", data.tags[1] == null, "\n");
print("json=", json.json_stringify(data), "\n");
