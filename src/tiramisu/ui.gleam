////
//// Tiramisu-Lustre integration module.
////
//// This module provides bidirectional message passing between Tiramisu (game engine)
//// and Lustre (web UI framework). Both systems communicate through a shared Bridge
//// with a single message type.
////
//// ## Design
////
//// The bridge has ONE message type that both sides understand. Each side provides
//// a wrapper function to convert bridge messages into their internal message type.
//// This is similar to frontend/backend communication over websockets.
////
//// ## Example
////
//// ```gleam
//// // 1. Define a shared bridge message type
//// pub type BridgeMsg {
////   // Game → UI
////   UpdateScore(Int)
////   UpdateHealth(Float)
////   // UI → Game
////   SelectSlot(Int)
////   StartGame
//// }
////
//// // 2. Lustre side wraps bridge messages
//// pub type UIMsg {
////   FromBridge(BridgeMsg)
////   ButtonClicked
//// }
////
//// fn init(bridge) {
////   #(Model(bridge: bridge), ui.register_lustre(bridge, FromBridge))
//// }
////
//// fn update(model, msg) {
////   case msg {
////     FromBridge(UpdateScore(s)) -> #(Model(..model, score: s), effect.none())
////     ButtonClicked -> #(model, ui.send(model.bridge, SelectSlot(0)))
////   }
//// }
////
//// // 3. Game side wraps bridge messages
//// pub type GameMsg {
////   FromBridge(BridgeMsg)
////   Tick
//// }
////
//// fn update(model, msg, ctx) {
////   case msg {
////     FromBridge(SelectSlot(i)) -> // handle slot selection
////     Tick -> #(model, ui.send_to_ui(bridge, UpdateScore(10)), ...)
////   }
//// }
//// ```
////

import lustre/effect as lustre_effect
import tiramisu/effect as game_effect

/// Bridge for bidirectional Tiramisu-Lustre communication.
///
/// The bridge has a single type parameter: the shared message type that both
/// sides understand. Each side provides a wrapper function to convert bridge
/// messages into their internal message type.
///
pub opaque type Bridge(bridge_msg) {
  Bridge(bridge: BridgeInternal)
}

// Internal JavaScript bridge object
@internal
pub type BridgeInternal

/// Create a new bridge for Tiramisu-Lustre communication.
///
/// ## Example
///
/// ```gleam
/// let bridge = ui.new_bridge()
/// ```
///
pub fn new_bridge() -> Bridge(bridge_msg) {
  Bridge(bridge: create_bridge_ffi())
}

/// Get the internal bridge object (for use by tiramisu.run).
@internal
pub fn get_internal(bridge: Bridge(bridge_msg)) -> BridgeInternal {
  bridge.bridge
}

// ============================================================================
// LUSTRE SIDE
// ============================================================================

/// Register Lustre's dispatch with a wrapper function.
///
/// The wrapper converts bridge messages to Lustre's internal message type.
/// Call this in your Lustre `init` function.
///
/// ## Example
///
/// ```gleam
/// pub type Msg {
///   FromBridge(BridgeMsg)
///   // ... other messages
/// }
///
/// fn init(bridge) {
///   #(Model(bridge: bridge), ui.register_lustre(bridge, FromBridge))
/// }
/// ```
///
pub fn register_lustre(
  bridge: Bridge(bridge_msg),
  wrapper: fn(bridge_msg) -> lustre_msg,
) -> lustre_effect.Effect(lustre_msg) {
  lustre_effect.from(fn(dispatch) {
    // Register a composed dispatch: bridge_msg -> lustre_msg -> dispatch
    register_lustre_ffi(bridge.bridge, fn(msg) { dispatch(wrapper(msg)) })
  })
}

/// Send a message across the bridge to the game.
///
/// ## Example
///
/// ```gleam
/// fn update(model, msg) {
///   case msg {
///     ButtonClicked -> #(model, ui.send(model.bridge, SelectSlot(0)))
///   }
/// }
/// ```
///
pub fn send(
  bridge: Bridge(bridge_msg),
  msg: bridge_msg,
) -> lustre_effect.Effect(lustre_msg) {
  lustre_effect.from(fn(_dispatch) { send_to_game_ffi(bridge.bridge, msg) })
}

// ============================================================================
// GAME SIDE
// ============================================================================

/// Send a message across the bridge to the UI.
///
/// ## Example
///
/// ```gleam
/// fn update(model, msg, ctx) {
///   case msg {
///     Tick -> #(model, ui.send_to_ui(bridge, UpdateScore(10)), ...)
///   }
/// }
/// ```
///
pub fn send_to_ui(
  bridge: Bridge(bridge_msg),
  msg: bridge_msg,
) -> game_effect.Effect(game_msg) {
  game_effect.from(fn(_dispatch) { send_to_lustre_ffi(bridge.bridge, msg) })
}

// ============================================================================
// FFI Declarations
// ============================================================================

@external(javascript, "../ui.ffi.mjs", "createBridge")
fn create_bridge_ffi() -> BridgeInternal

@external(javascript, "../ui.ffi.mjs", "registerLustre")
fn register_lustre_ffi(
  bridge: BridgeInternal,
  dispatch: fn(bridge_msg) -> Nil,
) -> Nil

@external(javascript, "../ui.ffi.mjs", "sendToGame")
fn send_to_game_ffi(bridge: BridgeInternal, msg: bridge_msg) -> Nil

@external(javascript, "../ui.ffi.mjs", "sendToLustre")
fn send_to_lustre_ffi(bridge: BridgeInternal, msg: bridge_msg) -> Nil
