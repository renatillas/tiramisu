import gleam/javascript/promise.{type Promise}
import gleam/list

/// Effect system for managing side effects in Tiramisu
/// Inspired by Lustre's effect architecture
///
/// Effects represent side effects as data, allowing the runtime to
/// manage them in a controlled, testable way.
/// Opaque effect type that can dispatch messages back to the application
pub opaque type Effect(msg) {
  Effect(perform: fn(fn(msg) -> Nil) -> Nil)
}

/// Create an effect that performs no side effects
pub fn none() -> Effect(msg) {
  Effect(perform: fn(_dispatch) { Nil })
}

/// Create a custom effect from a function
/// The function receives a dispatch callback to send messages back
pub fn from(effect: fn(fn(msg) -> Nil) -> Nil) -> Effect(msg) {
  Effect(perform: effect)
}

/// Batch multiple effects to run them together
pub fn batch(effects: List(Effect(msg))) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    effects
    |> list.each(fn(effect) {
      let Effect(perform) = effect
      perform(dispatch)
    })
  })
}

/// Map effect messages to a different type
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b) {
  Effect(perform: fn(dispatch) {
    let Effect(perform) = effect
    perform(fn(msg) { dispatch(f(msg)) })
  })
}

/// Create an effect from a Promise
/// When the promise resolves, it will dispatch the resulting message
pub fn from_promise(p: Promise(msg)) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    promise.tap(p, dispatch)
    Nil
  })
}

@internal
pub fn run(effect: Effect(msg), dispatch: fn(msg) -> Nil) -> Nil {
  let Effect(perform) = effect
  perform(dispatch)
}

/// Request the next animation frame and dispatch a message
/// This is the primary way to create frame-based updates in games
@external(javascript, "./ffi/effects.mjs", "requestAnimationFrame")
pub fn tick(msg: msg) -> Effect(msg)
