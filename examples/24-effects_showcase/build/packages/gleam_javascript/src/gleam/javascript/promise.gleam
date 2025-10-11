import gleam/dynamic.{type Dynamic}
import gleam/javascript/array.{type Array}

/// JavaScript promises represent the result of an asynchronous operation which
/// returns a value, either now or at some point in the future. In practice
/// they are the foundation of concurrency in JavaScript.
///
/// This library assumes you have some familiarity with JavaScript promises. If
/// you are not then you may want to take the time to learn about them outside of
/// Gleam. 
///
/// The Gleam promise type is generic over the type of value it resolves. It is
/// not generic over the error type as any Gleam panic or JavaScript exception
/// could alter the error value in an way that undermines the type, making it 
/// unsound and untypable.
/// If you want to represent success and failure with promises use a Gleam
/// `Result` inside of a promise.
///
/// For further information view the MDN documentation: 
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise>
///
pub type Promise(value)

/// Create a new promise from a callback function. The callback function itself
/// takes a second function as an argument, and when that second function is
/// called with a value then the promise resolves with that value.
///
/// This function is useful for converting code that uses callbacks into code
/// that uses promises.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "newPromise")
pub fn new(a: fn(fn(value) -> Nil) -> Nil) -> Promise(value)

/// Create a new promise and resolve function. The first time the resolve function
/// is called the promise resolves with that value.
///
/// This function is useful in cases where a reference to the promise and resolver
/// are needed.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "start_promise")
pub fn start() -> #(Promise(a), fn(a) -> Nil)

/// Create a promise that resolves immediately.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "resolve")
pub fn resolve(a: value) -> Promise(value)

/// If the promise is in an error state then apply a function to convert the
/// error value back into valid value, making the promise healthy again.
///
/// This is the equivilent of the `promise.catch` JavaScript method.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "rescue")
pub fn rescue(a: Promise(value), b: fn(Dynamic) -> value) -> Promise(value)

/// Chain a second asynchronous operation onto a promise, so it runs after the
/// promise has resolved.
///
/// This is the equivilent of the `promise.then` JavaScript method.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "then_await")
pub fn await(a: Promise(a), b: fn(a) -> Promise(b)) -> Promise(b)

/// Run a function on the value a promise resolves to, after it has resolved.
/// The value returned becomes the new value contained by the promise.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "map_promise")
pub fn map(a: Promise(a), b: fn(a) -> b) -> Promise(b)

/// Run a function on the value a promise resolves to, after it has resolved.
/// The value returned is discarded.
///
pub fn tap(promise: Promise(a), callback: fn(a) -> b) -> Promise(a) {
  promise
  |> map(fn(a) {
    callback(a)
    a
  })
}

/// Run a function on the value a promise resolves to, after it has resolved.
///
/// The function is only called if the value is `Ok`, and the returned becomes
/// the new value contained by the promise.
///
/// This is a convenience functin that combines the `map` function with `result.try`.
///
pub fn map_try(
  promise: Promise(Result(a, e)),
  callback: fn(a) -> Result(b, e),
) -> Promise(Result(b, e)) {
  promise
  |> map(fn(result) {
    case result {
      Ok(a) -> callback(a)
      Error(e) -> Error(e)
    }
  })
}

/// Run a promise returning function on the value a promise resolves to, after
/// it has resolved.
///
/// The function is only called if the value is `Ok`, and the returned becomes
/// the new value contained by the promise.
///
/// This is a convenience functin that combines the `await` function with
/// `result.try`.
///
pub fn try_await(
  promise: Promise(Result(a, e)),
  callback: fn(a) -> Promise(Result(b, e)),
) -> Promise(Result(b, e)) {
  promise
  |> await(fn(result) {
    case result {
      Ok(a) -> callback(a)
      Error(e) -> resolve(Error(e))
    }
  })
}

/// Chain an asynchronous operation onto an array of promises, so it runs after the
/// promises have resolved.
///
/// This is the equivilent of the `Promise.all` JavaScript static method.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "all_promises")
pub fn await_array(a: Array(Promise(a))) -> Promise(Array(a))

/// Chain an asynchronous operation onto an list of promises, so it runs after the
/// promises have resolved.
///
/// This is the equivilent of the `Promise.all` JavaScript static method.
///
pub fn await_list(xs: List(Promise(a))) -> Promise(List(a)) {
  xs
  |> do_await_list
  |> map(array.to_list)
}

@external(javascript, "../../gleam_javascript_ffi.mjs", "all_promises")
fn do_await_list(a: List(Promise(a))) -> Promise(Array(a))

@external(javascript, "../../gleam_javascript_ffi.mjs", "race_promises")
pub fn race_list(a: List(Promise(a))) -> Promise(a)

@external(javascript, "../../gleam_javascript_ffi.mjs", "race_promises")
pub fn race_array(a: Array(Promise(a))) -> Promise(a)

/// Create a promise that will resolve after a delay.
/// The delay is specified in milliseconds
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "wait")
pub fn wait(delay: Int) -> Promise(Nil)
