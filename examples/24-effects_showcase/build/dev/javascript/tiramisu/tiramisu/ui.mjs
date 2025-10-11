import * as $effect from "../../lustre/lustre/effect.mjs";
import {
  registerUI as do_register_lustre,
  dispatchToUI as do_dispatch_to_lustre,
  dispatchToGame as do_dispatch_to_game,
} from "../tiramisu.ffi.mjs";
import * as $game_effect from "../tiramisu/effect.mjs";

/**
 * Register your Lustre app to receive messages from Tiramisu.
 *
 * Call this in your Lustre app's init function.
 */
export function register_lustre() {
  return $effect.from((dispatch) => { return do_register_lustre(dispatch); });
}

/**
 * Dispatch a message from Tiramisu game to Lustre UI.
 *
 * Use this in your Tiramisu update function to send state updates to the UI.
 *
 * ## Example
 *
 * ```gleam
 * // In Tiramisu update
 * fn update(model, msg, ctx) {
 *   #(
 *     new_model,
 *     effect.batch([
 *       effect.tick(Tick),
 *       ui.dispatch_to_lustre(UpdateScore(model.score)),
 *     ])
 *   )
 * }
 * ```
 */
export function dispatch_to_lustre(msg) {
  return $game_effect.from((_) => { return do_dispatch_to_lustre(msg); });
}

/**
 * Send a message from Lustre UI to Tiramisu game.
 *
 * Use this in your Lustre update function to control the game.
 *
 * ## Example
 *
 * ```gleam
 * // In Lustre update
 * fn update(model, msg) {
 *   case msg {
 *     StartGame -> #(
 *       Model(..model, playing: True),
 *       ui.dispatch_to_tiramisu(Resume)
 *     )
 *     PauseGame -> #(
 *       Model(..model, playing: False),
 *       ui.dispatch_to_tiramisu(Pause)
 *     )
 *   }
 * }
 * ```
 */
export function dispatch_to_tiramisu(msg) {
  return $effect.from((_) => { return do_dispatch_to_game(msg); });
}
