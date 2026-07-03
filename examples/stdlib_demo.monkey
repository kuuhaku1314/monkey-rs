import "stdlib/prelude.monkey";
let user = {name: "tom", scores: range(1, 5)};
user.total = sum(user.scores);
user.lines = split("a,b,c", ",");
print(user.name, " total=", user.total, "\n");
print("has 3=", contains(user.scores, 3), "\n");
print("second line=", user.lines[1], "\n");
