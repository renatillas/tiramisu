// UI Bridge - Enables bidirectional communication between Tiramisu and Lustre
//
// The Bridge holds dispatch functions for both systems, allowing them to send
// messages to each other using a shared message type.

/**
 * Bridge class - holds dispatch functions for both Tiramisu and Lustre
 */
class Bridge {
  constructor() {
    // Lustre's dispatch function (set via registerLustre)
    // This is already composed with the wrapper: bridge_msg -> lustre_msg -> dispatch
    this.lustreDispatch = null;

    // Tiramisu's dispatch function (set by the game runtime)
    // This is already composed with the wrapper: bridge_msg -> game_msg -> dispatch
    this.tiramisuDispatch = null;
  }

  /**
   * Set Lustre's dispatch function (already wrapped)
   */
  setLustreDispatch(dispatch) {
    this.lustreDispatch = dispatch;
  }

  /**
   * Set Tiramisu's dispatch function (already wrapped)
   */
  setTiramisuDispatch(dispatch) {
    this.tiramisuDispatch = dispatch;
  }
}

/**
 * Create a new bridge instance
 * Called from Gleam: ui.new_bridge()
 */
export function createBridge() {
  return new Bridge();
}

/**
 * Register Lustre's dispatch function with the bridge
 * Called from Gleam: ui.register_lustre(bridge, wrapper) effect
 * The dispatch function is already composed with the wrapper
 */
export function registerLustre(bridge, dispatch) {
  bridge.setLustreDispatch(dispatch);
}

/**
 * Send a message to the game (Tiramisu) from Lustre
 * Called from Gleam: ui.send(bridge, msg) effect
 */
export function sendToGame(bridge, msg) {
  if (bridge.tiramisuDispatch) {
    bridge.tiramisuDispatch(msg);
  } else {
    console.warn(
      "Bridge: Cannot send to game - Tiramisu dispatch not registered. " +
        "Make sure tiramisu.run() has started with the bridge."
    );
  }
}

/**
 * Send a message to the UI (Lustre) from the game
 * Called from Gleam: ui.send_to_ui(bridge, msg) effect
 */
export function sendToLustre(bridge, msg) {
  if (bridge.lustreDispatch) {
    bridge.lustreDispatch(msg);
  } else {
    console.warn(
      "Bridge: Cannot send to UI - Lustre dispatch not registered. " +
        "Make sure to call ui.register_lustre(bridge, wrapper) in your Lustre init function."
    );
  }
}
