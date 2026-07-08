use super::register::RegisterFunction;
use super::symbols::SymbolId;

macro_rules! define_builtin_ops {
    ($(($variant:ident, $name:literal, $global:literal),)*) => {
        #[derive(Clone, Copy, Debug)]
        pub(super) enum BuiltinOp {
            $($variant,)*
        }

        impl BuiltinOp {
            const ALL: &'static [Self] = &[
                $(Self::$variant,)*
            ];

            pub(super) fn all() -> &'static [Self] {
                Self::ALL
            }

            pub(super) fn from_global_name(name: &str) -> Option<Self> {
                let op = Self::from_name(name)?;
                op.is_global().then_some(op)
            }

            pub(super) fn is_global(self) -> bool {
                match self {
                    $(Self::$variant => $global,)*
                }
            }

            pub(super) fn from_name(name: &str) -> Option<Self> {
                match name {
                    $($name => Some(Self::$variant),)*
                    _ => None,
                }
            }

            pub(super) fn name(self) -> &'static str {
                match self {
                    $(Self::$variant => $name,)*
                }
            }
        }
    };
}

define_builtin_ops! {
    (Print, "print", true),
    (Len, "len", true),
    (ByteLen, "byte_len", true),
    (Str, "str", true),
    (Int, "int", true),
    (Float, "float", true),
    (ParseInt, "parse_int", true),
    (ParseFloat, "parse_float", true),
    (Append, "append", true),
    (Delete, "delete", true),
    (Join, "join", true),
    (Args, "args", true),
    (ReadLine, "read_line", true),
    (Panic, "panic", true),
    (Assert, "assert", true),
    (Substr, "substr", true),
    (Find, "find", true),
    (Replace, "replace", true),
    (CharCode, "char_code", true),
    (FromCharCode, "from_char_code", true),
    (JsonParse, "json_parse", false),
    (JsonStringify, "json_stringify", false),
    (Type, "type", true),
    (IsNull, "is_null", true),
    (IsBool, "is_bool", true),
    (IsInt, "is_int", true),
    (IsFloat, "is_float", true),
    (IsString, "is_string", true),
    (IsList, "is_list", true),
    (IsMap, "is_map", true),
    (ReadFile, "read_file", false),
    (WriteFile, "write_file", false),
    (AppendFile, "append_file", false),
    (FileExists, "file_exists", false),
    (ReadDir, "read_dir", false),
    (Mkdir, "mkdir", false),
    (RemoveFile, "remove_file", false),
    (RemoveDir, "remove_dir", false),
    (CopyFile, "copy_file", false),
    (Rename, "rename", false),
    (Metadata, "metadata", false),
    (PathJoin, "path_join", false),
    (PathDirname, "path_dirname", false),
    (PathBasename, "path_basename", false),
    (PathExt, "path_ext", false),
    (PathExists, "path_exists", false),
    (PathIsFile, "path_is_file", false),
    (PathIsDir, "path_is_dir", false),
    (EnvGet, "env_get", false),
    (EnvSet, "env_set", false),
    (Cwd, "cwd", false),
    (SetCwd, "set_cwd", false),
    (Exit, "exit", false),
    (TimeMs, "time_ms", false),
    (NowMs, "now_ms", false),
    (SleepMs, "sleep_ms", false),
    (Clear, "clear", false),
    (Home, "home", false),
    (HideCursor, "hide_cursor", false),
    (ShowCursor, "show_cursor", false),
    (EnableRawMode, "enable_raw_mode", false),
    (DisableRawMode, "disable_raw_mode", false),
    (ReadKey, "read_key", false),
    (ReadKeyTimeout, "read_key_timeout", false),
    (ReadKeyLatestTimeout, "read_key_latest_timeout", false),
    (Move, "move", false),
    (ClearLine, "clear_line", false),
    (Size, "size", false),
    (EnterAltScreen, "enter_alt_screen", false),
    (LeaveAltScreen, "leave_alt_screen", false),
    (Fg, "fg", false),
    (Bg, "bg", false),
    (Bold, "bold", false),
    (ResetStyle, "reset_style", false),
    (Paint, "paint", false),
    (PaintRuns, "paint_runs", false),
    (Abs, "abs", false),
    (Floor, "floor", false),
    (Ceil, "ceil", false),
    (Round, "round", false),
    (Sqrt, "sqrt", false),
    (Pow, "pow", false),
    (Min, "min", false),
    (Max, "max", false),
    (RandomInt, "random_int", false),
    (RandomFloat, "random_float", false),
    (Sort, "sort", false),
    (HttpGet, "http_get", false),
    (HttpPost, "http_post", false),
    (HttpRequest, "http_request", false),
    (Exec, "exec", false),
    (UrlEncode, "url_encode", false),
    (UrlDecode, "url_decode", false),
    (Base64Encode, "base64_encode", false),
    (Base64Decode, "base64_decode", false),
    (Sha256, "sha256", false),
}

#[derive(Clone, Debug)]
pub(super) struct CompiledFunction {
    pub(super) name: Option<String>,
    pub(super) parameters: Vec<SymbolId>,
    pub(super) parameter_slots: Vec<usize>,
    pub(super) upvalues: Vec<UpvalueSpec>,
    pub(super) local_count: usize,
    pub(super) constants: Vec<String>,
    pub(super) registers: RegisterFunction,
}

#[derive(Clone, Debug)]
pub(super) struct UpvalueSpec {
    pub(super) depth: usize,
    pub(super) slot: usize,
}
