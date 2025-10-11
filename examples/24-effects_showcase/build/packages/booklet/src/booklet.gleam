/// A Booklet is an opaque, typed reference to a concurrent, atomic, global cache.
///
/// Booklets are designed for caching values that persist throughout your program's
/// lifetime. Booklets can never be deleted, but offer thread-safe, atomic updates
/// and fast concurrent reads.
///
/// Booklet is implemented using atomic compare-and-swap operations on top of a
/// managed ETS table on Erlang, or a simple mutable reference on Javascript.
///
/// See the README for additional usage examples.
pub type Booklet(value)

/// Create a new Booklet, storing an initial value in it.
@external(erlang, "booklet_ffi", "make")
@external(javascript, "./booklet_ffi.mjs", "make")
pub fn new(initial_value: value) -> Booklet(value)

/// Get the current value stored in the Booklet.
@external(erlang, "booklet_ffi", "get")
@external(javascript, "./booklet_ffi.mjs", "get")
pub fn get(from booklet: Booklet(value)) -> value

/// Atomically update the value stored in the booklet, and return the final value.
///
/// Updates are guaranteed to be atomic and serialisable. If multiple processes
/// try to update the value at the same time, all but one process  have to retry
/// their update operation (by calling the updater again) until the update succeeds.
///
/// Note that Javascript is single-threaded, so no extra guards are in place.
@external(erlang, "booklet_ffi", "update")
@external(javascript, "./booklet_ffi.mjs", "update")
pub fn update(
  in booklet: Booklet(value),
  with updater: fn(value) -> value,
) -> value

/// Atomically replace the value stored in the booklet.
pub fn set(in booklet: Booklet(value), to new_value: value) -> Nil {
  update(booklet, fn(_) { new_value })
  Nil
}
