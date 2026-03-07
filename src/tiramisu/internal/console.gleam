@external(javascript, "./console.ffi.mjs", "log")
pub fn log(value: a) -> Nil

@external(javascript, "./console.ffi.mjs", "warn")
pub fn warn(value: a) -> Nil

@external(javascript, "./console.ffi.mjs", "error")
pub fn error(value: a) -> Nil

@external(javascript, "./console.ffi.mjs", "info")
pub fn info(value: a) -> Nil

@external(javascript, "./console.ffi.mjs", "debug")
pub fn debug(value: a) -> Nil

@external(javascript, "./console.ffi.mjs", "group")
pub fn group(label: String) -> Nil

@external(javascript, "./console.ffi.mjs", "group_collapsed")
pub fn group_collapsed(label: String) -> Nil

@external(javascript, "./console.ffi.mjs", "group_end")
pub fn group_end() -> Nil
