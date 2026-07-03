import "string.monkey";
import "fs.monkey";
fn read_lines(path) {
    let result = read_file(path);
    if !result.ok {
        return result;
    }
    {ok: true, value: split(result.value, "\n"), error: null}
}
export read_lines;
