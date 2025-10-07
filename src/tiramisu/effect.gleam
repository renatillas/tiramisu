//// Effect system for managing side effects in Tiramisu.
////
//// Effects represent side effects as immutable data, following The Elm Architecture.
//// Your `update` function returns effects that the runtime executes for you.
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/effect
////
//// type Msg {
////   Tick
////   PlayerMoved(Vec3(Float))
//// }
////
//// fn update(model: Model, msg: Msg, ctx: Context) {
////   case msg {
////     Tick -> #(
////       update_physics(model),
////       effect.batch([
////         effect.tick(Tick),  // Request next frame
////         effect.from(fn(dispatch) {
////           // Custom side effect
////           log_position(model.player_pos)
////           dispatch(PlayerMoved(model.player_pos))
////         }),
////       ]),
////     )
////     PlayerMoved(_) -> #(model, effect.none())
////   }
//// }
//// ```

import gleam/javascript/promise.{type Promise}
import gleam/list

/// Opaque effect type that can dispatch messages back to the application.
///
/// Effects are data descriptions of side effects to perform. The runtime
/// executes them after your `update` function returns.
pub opaque type Effect(msg) {
  Effect(perform: fn(fn(msg) -> Nil) -> Nil)
}

/// Create an effect that performs no side effects.
///
/// Use when you want to update state without triggering any effects.
///
/// ## Example
///
/// ```gleam
/// fn update(model, msg, ctx) {
///   case msg {
///     Idle -> #(model, effect.none())
///   }
/// }
/// ```
pub fn none() -> Effect(msg) {
  Effect(perform: fn(_dispatch) { Nil })
}

/// Create a custom effect from a function.
///
/// The function receives a `dispatch` callback to send messages back to your `update` function.
///
/// ## Example
///
/// ```gleam
/// effect.from(fn(dispatch) {
///   log("Player score: " <> int.to_string(score))
///   dispatch(ScoreLogged)
/// })
/// ```
pub fn from(effect: fn(fn(msg) -> Nil) -> Nil) -> Effect(msg) {
  Effect(perform: effect)
}

/// Batch multiple effects to run them together.
///
/// All effects execute in order during the same frame.
///
/// ## Example
///
/// ```gleam
/// effect.batch([
///   effect.tick(NextFrame),
///   play_sound_effect("jump.wav"),
///   update_scoreboard(score),
/// ])
/// ```
pub fn batch(effects: List(Effect(msg))) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    effects
    |> list.each(fn(effect) {
      let Effect(perform) = effect
      perform(dispatch)
    })
  })
}

/// Map effect messages to a different type.
///
/// Useful when composing effects from subcomponents.
///
/// ## Example
///
/// ```gleam
/// let player_effect = player.update(player_model, player_msg)
/// effect.map(player_effect, PlayerMsg)
/// ```
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b) {
  Effect(perform: fn(dispatch) {
    let Effect(perform) = effect
    perform(fn(msg) { dispatch(f(msg)) })
  })
}

/// Create an effect from a JavaScript Promise.
///
/// When the promise resolves, it dispatches the resulting message.
///
/// ## Example
///
/// ```gleam
/// let fetch_promise = fetch_data()
/// effect.from_promise(promise.map(fetch_promise, DataLoaded))
/// ```
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

/// Request the next animation frame and dispatch a message.
///
/// This is the primary way to create frame-based game loops. Call this in your
/// `update` function to receive a message on the next frame.
///
/// ## Example
///
/// ```gleam
/// type Msg {
///   Tick
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     Tick -> #(
///       Model(..model, time: model.time +. ctx.delta_time),
///       effect.tick(Tick),  // Request next frame
///     )
///   }
/// }
/// ```
@external(javascript, "./ffi/effects.mjs", "requestAnimationFrame")
pub fn tick(msg: msg) -> Effect(msg)
