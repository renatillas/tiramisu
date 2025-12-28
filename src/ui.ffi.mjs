// UI Bridge - Enables bidirectional communication between Tiramisu and Lustre
//
// The Bridge is an instance (not global state) that holds dispatch functions
// for both systems, allowing them to send messages to each other.

/**
 * Bridge class - holds dispatch functions for both Tiramisu and Lustre
 */
class Bridge {
  constructor() {
    // Lustre's dispatch function (set via registerLustreDispatch)
    this.lustreDispatch = null;

    // Tiramisu's dispatch function (set by the game runtime)
    this.tiramisuDispatch = null;
  }

  /**
   * Set Lustre's dispatch function
   */
  setLustreDispatch(dispatch) {
    this.lustreDispatch = dispatch;
  }

  /**
   * Set Tiramisu's dispatch function
   */
  setTiramisuDispatch(dispatch) {
    this.tiramisuDispatch = dispatch;
  }

  /**
   * Dispatch a message to Lustre
   */
  dispatchToLustre(msg) {
    if (this.lustreDispatch) {
      this.lustreDispatch(msg);
    } else {
      console.warn(
        "Bridge: Cannot dispatch to Lustre - Lustre dispatch not registered. " +
          "Make sure to call ui.register_lustre(bridge) in your Lustre init function."
      );
    }
  }

  /**
   * Dispatch a message to Tiramisu
   */
  dispatchToTiramisu(msg) {
    if (this.tiramisuDispatch) {
      this.tiramisuDispatch(msg);
    } else {
      console.warn(
        "Bridge: Cannot dispatch to Tiramisu - Tiramisu dispatch not registered. " +
          "Make sure tiramisu.run() has started with the bridge."
      );
    }
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
 * Called from Gleam: ui.register_lustre(bridge) effect
 */
export function registerLustreDispatch(bridge, dispatch) {
  bridge.setLustreDispatch(dispatch);
}

/**
 * Dispatch a message to Tiramisu from Lustre
 * Called from Gleam: ui.to_tiramisu(bridge, msg) effect
 */
export function dispatchToTiramisu(bridge, msg) {
  bridge.dispatchToTiramisu(msg);
}

/**
 * Dispatch a message to Lustre from Tiramisu
 * Called from Gleam: ui.to_lustre(bridge, msg) effect
 */
export function dispatchToLustre(bridge, msg) {
  bridge.dispatchToLustre(msg);
}
