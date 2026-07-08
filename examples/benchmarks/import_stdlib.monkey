import list from "../../stdlib/list.monkey";
import string from "../../stdlib/string.monkey";
import math from "../../stdlib/math.monkey";

let values = list.range(0, 2000);
let parts = string.split("a,b,c,d,e,f,g,h", ",");
list.sum(values) + len(parts) + math.floor(3.9)
