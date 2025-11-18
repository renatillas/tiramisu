//// <script>
//// const docs = [
////   {
////     header: "Lustre integration",
////     functions: [
////       "register_lustre",
////       "dispatch_to_lustre",
////       "dispatch_to_tiramisu"
////     ]
////   }
//// ]
////
//// const callback = () => {
////   const list = document.querySelector(".sidebar > ul:last-of-type")
////   const sortedLists = document.createDocumentFragment()
////   const sortedMembers = document.createDocumentFragment()
////
////   for (const section of docs) {
////     sortedLists.append((() => {
////       const node = document.createElement("h3")
////       node.append(section.header)
////       return node
////     })())
////     sortedMembers.append((() => {
////       const node = document.createElement("h2")
////       node.append(section.header)
////       return node
////     })())
////
////     const sortedList = document.createElement("ul")
////     sortedLists.append(sortedList)
////
////
////     for (const funcName of section.functions) {
////       const href = `#${funcName}`
////       const member = document.querySelector(
////         `.member:has(h2 > a[href="${href}"])`
////       )
////       const sidebar = list.querySelector(`li:has(a[href="${href}"])`)
////       sortedList.append(sidebar)
////       sortedMembers.append(member)
////     }
////   }
////
////   document.querySelector(".sidebar").insertBefore(sortedLists, list)
////   document
////     .querySelector(".module-members:has(#module-values)")
////     .insertBefore(
////       sortedMembers,
////       document.querySelector("#module-values").nextSibling
////     )
//// }
////
//// document.readyState !== "loading"
////   ? callback()
////   : document.addEventListener(
////     "DOMContentLoaded",
////     callback,
////     { once: true }
////   )
//// </script>
//// Lustre UI integration for Tiramisu.
////
//// This module provides bidirectional message passing between
//// Tiramisu games and Lustre UI overlays.
////
//// ## Setup
////
//// 1. In your Lustre init, register to receive messages from the game:
////
//// ```gleam
//// import tiramisu/ui
////
//// fn init(_) {
////   #(model, ui.register())
//// }
//// ```
////
//// 2. Send messages from game to UI:
////
//// ```gleam
//// // In Tiramisu update
//// ui.dispatch_to_lustre(UpdateScore(new_score))
//// ```
////
//// 3. Send messages from UI to game:
////
//// ```gleam
//// // In Lustre update
//// StartGame -> #(Model(..model, playing: True), ui.send_to_game(Resume))
//// ```

import lustre/effect
import tiramisu/effect as game_effect

/// Register your Lustre app to receive messages from Tiramisu.
///
/// Call this in your Lustre app's init function.
pub fn register_lustre() -> effect.Effect(a) {
  effect.from(fn(dispatch) { do_register_lustre(dispatch) })
}

/// Dispatch a message from Tiramisu game to Lustre UI.
///
/// Use this in your Tiramisu update function to send state updates to the UI.
///
/// ## Example
///
/// ```gleam
/// // In Tiramisu update
/// fn update(model, msg, ctx) {
///   #(
///     new_model,
///     effect.batch([
///       effect.tick(Tick),
///       ui.dispatch_to_lustre(UpdateScore(model.score)),
///     ])
///   )
/// }
/// ```
pub fn dispatch_to_lustre(msg: ui_msg) -> game_effect.Effect(game_msg) {
  game_effect.from(fn(_dispatch) { do_dispatch_to_lustre(msg) })
}

/// Send a message from Lustre UI to Tiramisu game.
///
/// Use this in your Lustre update function to control the game.
///
/// ## Example
///
/// ```gleam
/// // In Lustre update
/// fn update(model, msg) {
///   case msg {
///     StartGame -> #(
///       Model(..model, playing: True),
///       ui.dispatch_to_tiramisu(Resume)
///     )
///     PauseGame -> #(
///       Model(..model, playing: False),
///       ui.dispatch_to_tiramisu(Pause)
///     )
///   }
/// }
/// ```
pub fn dispatch_to_tiramisu(msg: game_msg) -> effect.Effect(ui_msg) {
  effect.from(fn(_dispatch) { do_dispatch_to_game(msg) })
}

@external(javascript, "../tiramisu.ffi.mjs", "registerUI")
fn do_register_lustre(dispatch: fn(msg) -> Nil) -> Nil

@external(javascript, "../tiramisu.ffi.mjs", "dispatchToUI")
fn do_dispatch_to_lustre(msg: ui_msg) -> Nil

@external(javascript, "../tiramisu.ffi.mjs", "dispatchToGame")
fn do_dispatch_to_game(msg: game_msg) -> Nil
