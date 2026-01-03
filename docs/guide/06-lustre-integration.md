# Lustre Integration

Games need more than 3D graphics. Health bars, inventory screens, menus, dialog boxes—these are best built with HTML/CSS, not 3D elements.

Tiramisu integrates with [Lustre](https://lustre.build), Gleam's Elm-inspired web framework. Both frameworks follow MVU, so they speak the same language. The challenge is getting them to talk to each other.

## The Bridge

The `Bridge` creates a communication channel between Tiramisu and Lustre. Think of it like a websocket between two servers, except both "servers" are in your browser.

The key insight: **one shared message type** flows in both directions. Each side provides a wrapper function to convert bridge messages into their internal type.

```
┌──────────────┐              ┌──────────────┐
│   TIRAMISU   │              │    LUSTRE    │
│              │              │              │
│ FromBridge ◄─┼── BridgeMsg ─┼─► FromBridge │
│              │              │              │
└──────────────┘              └──────────────┘
```

## Project setup

### Configure gleam.toml

Lustre's dev tools can generate the HTML structure for you. Add this to your `gleam.toml`:

```toml
[tools.lustre.html]
scripts = [
  { type = "importmap", content = "{ \"imports\": { \"three\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\", \"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\", \"@dimforge/rapier3d-compat\": \"https://cdn.jsdelivr.net/npm/@dimforge/rapier3d-compat@0.11.2/+esm\" } }" }
]
body = "<div id=\"game\"></div><div id=\"ui\"></div>"
```

The `body` field creates two containers: one for the 3D canvas, one for the HTML UI. When you run `gleam run -m lustre/dev start`, this HTML is generated automatically.

### Add CSS for layering

You'll also want styles to layer the UI over the game:

```toml
stylesheets = [
  { content = "body { margin: 0; overflow: hidden; } #game { position: fixed; inset: 0; z-index: 0; } #ui { position: fixed; inset: 0; z-index: 1; pointer-events: none; } #ui button, #ui .clickable { pointer-events: auto; }" }
]
```

The `pointer-events: none` lets clicks pass through to the game canvas, except for elements that explicitly enable them.

## Define the shared message type

Create a module that both sides import. This is the "protocol" they speak:

```gleam
// src/my_game/bridge_msg.gleam

pub type BridgeMsg {
  // Game → UI
  HealthUpdated(current: Float, max: Float)
  ScoreUpdated(Int)

  // UI → Game
  PauseClicked
  RestartClicked
}
```

Keep messages simple—plain data, no functions or opaque types. Use comments to organize by direction.

## The Lustre side

Your Lustre app needs three things:

1. **A `FromBridge` wrapper** in your message type
2. **Register with the bridge** in init using `ui.register_lustre(bridge, FromBridge)`
3. **Send messages** with `ui.send(bridge, msg)`

```gleam
pub type Msg {
  FromBridge(BridgeMsg)  // Wrapper for incoming bridge messages
  // ... other UI-only messages
}

fn init(bridge, _flags) {
  #(Model(bridge: bridge, ..), ui.register_lustre(bridge, FromBridge))
}
```

When handling messages, pattern match on `FromBridge`:

```gleam
fn update(model, msg) {
  case msg {
    FromBridge(bridge_msg.HealthUpdated(current, max)) ->
      #(Model(..model, health: current, max_health: max), effect.none())

    FromBridge(bridge_msg.PauseClicked) ->
      // This is a message we SEND, not receive—ignore it
      #(model, effect.none())
  }
}
```

To send a message to the game:

```gleam
ui.send(model.bridge, bridge_msg.PauseClicked)
```

## The Tiramisu side

Your game needs the same three things:

1. **A `FromBridge` wrapper** in your message type
2. **Pass the bridge and wrapper** to `tiramisu.start`
3. **Send messages** with `ui.send_to_ui(bridge, msg)`

```gleam
pub type Msg {
  Tick
  FromBridge(BridgeMsg)  // Wrapper for incoming bridge messages
}

pub fn main() {
  let bridge = ui.new_bridge()
  let assert Ok(_) = game_ui.start(bridge)  // Start Lustre first

  tiramisu.application(init(bridge, _), update, view)
  |> tiramisu.start("#game", tiramisu.FullScreen, option.Some(#(bridge, FromBridge)))
}
```

Handle incoming UI messages:

```gleam
fn update(model, msg, ctx) {
  case msg {
    FromBridge(bridge_msg.PauseClicked) ->
      #(Model(..model, paused: True), effect.none(), ctx.physics_world)

    FromBridge(bridge_msg.HealthUpdated(..)) ->
      // This is a message we SEND, not receive—ignore it
      #(model, effect.none(), ctx.physics_world)
  }
}
```

To send a message to the UI:

```gleam
ui.send_to_ui(model.bridge, bridge_msg.HealthUpdated(new_health, max_health))
```

## The pattern summarized

| Direction | Lustre calls | Tiramisu calls |
|-----------|--------------|----------------|
| Register | `ui.register_lustre(bridge, FromBridge)` | Pass `#(bridge, FromBridge)` to start |
| Send → Game | `ui.send(bridge, msg)` | — |
| Send → UI | — | `ui.send_to_ui(bridge, msg)` |
| Receive | `FromBridge(msg)` in update | `FromBridge(msg)` in update |

## Best practices

**Send state, not deltas.** When syncing, send the full current value. `HealthUpdated(75.0, 100.0)` is safer than `HealthChanged(-25.0)` which could get out of sync.

**Ignore irrelevant messages.** Each side receives ALL bridge messages, including ones it sent. Pattern match and return unchanged model for messages meant for the other side.

**Keep the bridge message type flat.** Avoid nesting complex types. If you need to send rich data, consider sending IDs and looking up the data locally.

## Next steps

For more complex UIs—components, routing, animations—check out [Lustre's documentation](https://hexdocs.pm/lustre/).

The bridge pattern scales well. Add new message variants as your game grows, and both sides will get compile errors if they forget to handle them.
