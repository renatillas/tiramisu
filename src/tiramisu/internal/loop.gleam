pub type Loop

@external(javascript, "./render_loop.ffi.mjs", "start")
pub fn start(on_tick: fn(Float, Int) -> Nil) -> Loop

@external(javascript, "./render_loop.ffi.mjs", "stop")
pub fn stop(loop: Loop) -> Nil
