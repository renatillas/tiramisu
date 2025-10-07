// Lustre UI integration FFI - Bidirectional messaging

/**
 * Reference to Lustre app's dispatch function.
 */
let uiDispatch = null;

/**
 * Reference to Tiramisu game's dispatch function.
 */
let gameDispatch = null;

/**
 * Register Lustre UI to receive messages from Tiramisu game.
 *
 * @param {Function} dispatch - Lustre's dispatch function
 */
export function registerUI(dispatch) {
  uiDispatch = dispatch;
}

/**
 * Register Tiramisu game to receive messages from Lustre UI.
 * This is called automatically by Tiramisu's effect system.
 *
 * @param {Function} dispatch - Tiramisu's dispatch function
 */
export function registerGame(dispatch) {
  gameDispatch = dispatch;
}

/**
 * Dispatch a message from Tiramisu game to Lustre UI.
 *
 * @param {*} msg - The message to dispatch to UI
 */
export function dispatchToUI(msg) {
  if (uiDispatch) {
    uiDispatch(msg);
  } else {
    console.warn('[Tiramisu] UI dispatch not registered. Call ui.register() in your Lustre init.');
  }
}

/**
 * Dispatch a message from Lustre UI to Tiramisu game.
 *
 * @param {*} msg - The message to dispatch to game
 */
export function dispatchToGame(msg) {
  if (gameDispatch) {
    gameDispatch(msg);
  } else {
    console.warn('[Tiramisu] Game dispatch not registered yet.');
  }
}
