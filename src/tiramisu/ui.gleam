////
//// Tiramisu-Lustre integration module.
////
//// This module provides bidirectional message passing between Tiramisu (game engine)
//// and Lustre (web UI framework) without using global state. Both systems communicate
//// through a shared Bridge instance.
////
//// ## Overview
////
//// The Bridge is an opaque type that holds dispatch functions for both systems.
//// You create it once, include it in your Lustre flags (alongside any other data
//// you need), and pass it to Tiramisu via `tiramisu.run_with_ui()`.
////
//// ## Example
////
//// ```gleam
//// import tiramisu
//// import tiramisu/ui
//// import lustre
////
//// // Define your Lustre flags - include bridge plus any other initial data
//// pub type Flags {
////   Flags(bridge: ui.Bridge(UIMsg, GameMsg), username: String, difficulty: Int)
//// }
////
//// pub type Model {
////   Model(bridge: ui.Bridge(UIMsg, GameMsg), score: Int, health: Float)
//// }
////
//// pub fn main() {
////   // 1. Create a bridge instance
////   let bridge = ui.new_bridge()
////
////   // 2. Start Lustre with bridge in your flags
////   let assert Ok(_) =
////     lustre.application(init, update, view)
////     |> lustre.start("#app", Flags(bridge, "Player1", 2))
////
////   // 3. Start Tiramisu with the same bridge
////   tiramisu.run_with_ui(
////     bridge: bridge,
////     selector: "#game",
////     dimensions: None,
////     init: game_init,
////     update: game_update,
////     view: game_view,
////   )
//// }
////
//// fn init(flags: Flags) {
////   // Store bridge in model and register Lustre's dispatch
////   #(
////     Model(bridge: flags.bridge, score: 0, health: 100.0),
////     ui.register_lustre(flags.bridge),
////   )
//// }
////
//// fn update(model: Model, msg: UIMsg) {
////   case msg {
////     StartGame -> #(model, ui.to_tiramisu(model.bridge, Resume))
////     UpdateScore(score) -> #(Model(..model, score: score), lustre_effect.none())
////   }
//// }
//// ```
////

import lustre/effect as lustre_effect
import tiramisu/effect as game_effect

/// Opaque type representing a communication bridge between Tiramisu and Lustre.
///
/// The bridge holds dispatch functions for both systems, allowing bidirectional
/// message passing without global state.
///
/// Type parameters:
/// - `lustre_msg`: The message type used by your Lustre application
/// - `game_msg`: The message type used by your Tiramisu game
///
pub opaque type Bridge(lustre_msg, game_msg) {
  Bridge(bridge: BridgeInternal)
}

// Internal JavaScript bridge object
@internal
pub type BridgeInternal

/// Create a new bridge for Tiramisu-Lustre communication.
///
/// The bridge is an instance (not global state) that should be:
/// 1. Included in your Lustre flags
/// 2. Passed to `tiramisu.run_with_ui()`
///
/// ## Example
///
/// ```gleam
/// let bridge = ui.new_bridge()
/// ```
///
pub fn new_bridge() -> Bridge(lustre_msg, game_msg) {
  Bridge(bridge: create_bridge_ffi())
}

/// Get the internal bridge object (for use by tiramisu.run_with_ui).
@internal
pub fn get_internal(bridge: Bridge(lustre_msg, game_msg)) -> BridgeInternal {
  bridge.bridge
}

// ============================================================================
// LUSTRE SIDE - Effects for Lustre applications
// ============================================================================

/// Register Lustre's dispatch function with the bridge.
///
/// Call this effect in your Lustre `init` function to enable bidirectional
/// communication. This allows Tiramisu to send messages to Lustre.
///
/// ## Example
///
/// ```gleam
/// fn init(flags: MyFlags) {
///   #(
///     Model(bridge: flags.bridge, ...),
///     ui.register_lustre(flags.bridge),
///   )
/// }
/// ```
///
pub fn register_lustre(
  bridge: Bridge(lustre_msg, game_msg),
) -> lustre_effect.Effect(lustre_msg) {
  lustre_effect.from(fn(dispatch) {
    register_lustre_dispatch_ffi(bridge.bridge, dispatch)
  })
}

/// Dispatch a message to Tiramisu from Lustre.
///
/// Use this effect in your Lustre `update` function to send messages to
/// the Tiramisu game.
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: UIMsg) {
///   case msg {
///     StartGame -> #(
///       Model(..model, state: Playing),
///       ui.to_tiramisu(model.bridge, Resume),
///     )
///     PauseGame -> #(
///       Model(..model, state: Paused),
///       ui.to_tiramisu(model.bridge, Pause),
///     )
///   }
/// }
/// ```
///
pub fn to_tiramisu(
  bridge: Bridge(lustre_msg, game_msg),
  msg: game_msg,
) -> lustre_effect.Effect(lustre_msg) {
  lustre_effect.from(fn(_dispatch) {
    dispatch_to_tiramisu_ffi(bridge.bridge, msg)
  })
}

// ============================================================================
// TIRAMISU SIDE - Effects for Tiramisu games
// ============================================================================

/// Dispatch a message to Lustre from Tiramisu.
///
/// Use this effect in your Tiramisu `update` function to send messages to
/// the Lustre UI.
///
/// ## Example
///
/// ```gleam
/// fn update(model: GameModel, msg: GameMsg, ctx: Context) {
///   case msg {
///     Tick -> {
///       let new_score = model.score + 1
///       #(
///         GameModel(..model, score: new_score),
///         game_effect.batch([
///           game_effect.tick(Tick),
///           ui.to_lustre(model.bridge, UpdateScore(new_score)),
///         ]),
///         None,
///       )
///     }
///   }
/// }
/// ```
///
pub fn to_lustre(
  bridge: Bridge(lustre_msg, game_msg),
  msg: lustre_msg,
) -> game_effect.Effect(game_msg) {
  game_effect.from(fn(_dispatch) {
    dispatch_to_lustre_ffi(bridge.bridge, msg)
  })
}

// ============================================================================
// FFI Declarations
// ============================================================================

@external(javascript, "../ui.ffi.mjs", "createBridge")
fn create_bridge_ffi() -> BridgeInternal

@external(javascript, "../ui.ffi.mjs", "registerLustreDispatch")
fn register_lustre_dispatch_ffi(
  bridge: BridgeInternal,
  dispatch: fn(lustre_msg) -> Nil,
) -> Nil

@external(javascript, "../ui.ffi.mjs", "dispatchToTiramisu")
fn dispatch_to_tiramisu_ffi(bridge: BridgeInternal, msg: game_msg) -> Nil

@external(javascript, "../ui.ffi.mjs", "dispatchToLustre")
fn dispatch_to_lustre_ffi(bridge: BridgeInternal, msg: lustre_msg) -> Nil
