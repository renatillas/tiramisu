import gleam/dynamic.{type Dynamic}

pub type Exception {
  /// An error was raised.
  /// On Erlang this would be caused by calling the `erlang:error/1` function,
  /// or some other runtime error.
  /// On JavaScript this would be caused by throwing an `Error` object.
  Errored(Dynamic)
  /// A value was thrown.
  /// On Erlang this would be caused by calling the `erlang:throw/1` function.
  /// On JavaScript this would be caused by throwing any non-`Error` value.
  Thrown(Dynamic)
  /// A process exited.
  /// On Erlang this would be caused by calling the `erlang:exit/1` function.
  /// On JavaScript this variant is not used.
  Exited(Dynamic)
}

/// This function will catch any crash and convert it into a result rather than
/// crashing the process.
///
/// You should ideally never use this function! Exceptions are not flow control
/// in Gleam, a result type should be used instead. This function is only if you
/// need to perform some cleanup when a crash occurs, and then you should favour
/// `defer` if possible.
///
@external(erlang, "exception_ffi", "rescue")
@external(javascript, "./exception_ffi.mjs", "rescue")
pub fn rescue(body: fn() -> a) -> Result(a, Exception)

/// This function will run a cleanup function after the given body function, even
/// if the body function crashes.
///
/// You should ideally never use this function! Exceptions are not flow control
/// in Gleam, a result type should be used instead. This function is only if you
/// need to perform some cleanup when a crash occurs.
///
/// # Examples
/// 
/// ```gleam
/// pub fn run_with_lock(f: fn() -> a) -> a {
///   let lock = acquire()
///   use <- defer(fn() { release(lock) })
///   f()
/// }
/// ```
/// 
///
@external(erlang, "exception_ffi", "defer")
@external(javascript, "./exception_ffi.mjs", "defer")
pub fn defer(cleanup: fn() -> b, body: fn() -> a) -> a

/// This function will run a cleanup function after the given body function,
/// but only if the body function crashes.
///
/// You should ideally never use this function! Exceptions are not flow control
/// in Gleam, a result type should be used instead. This function is only if you
/// need to perform some cleanup when a crash occurs.
///
@external(erlang, "exception_ffi", "on_crash")
@external(javascript, "./exception_ffi.mjs", "on_crash")
pub fn on_crash(cleanup: fn() -> b, body: fn() -> a) -> a
