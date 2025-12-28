# Tiramisu - Gleam Game Engine

A type-safe game engine for Gleam targeting JavaScript, using Three.js for rendering and Rapier for physics.

## Core Principles

1. **MVU Architecture**: Model-View-Update pattern like Lustre. Game state is immutable, updates return new state + effects.

2. **Type-Safe Wrappers**: Three.js/Rapier objects wrapped in opaque Gleam types with validated constructors.

3. **Declarative Scene Graph**: Return scene nodes from `view()`, runtime handles diffing/patching.

4. **Effects for Side Effects**: All side effects (timers, audio, physics) go through the Effect system.

5. **String IDs**: All scene nodes, physics bodies, and audio use plain `String` IDs.

## Project Setup

```toml
# gleam.toml
[tools.lustre.html]
scripts = [
  { type = "importmap", content = "{ \"imports\": { \"three\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\", \"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\", \"@dimforge/rapier3d-compat\": \"https://cdn.jsdelivr.net/npm/@dimforge/rapier3d-compat@0.11.2/+esm\" } }" }
]
stylesheets = [
  { content = "body { margin: 0; padding: 0; overflow: hidden; } canvas { display: block; }" }
]
```

## Basic Game Structure

```gleam
import tiramisu
import tiramisu/effect
import tiramisu/scene

pub fn main() {
  tiramisu.run(
    selector: "#game",
    dimensions: None,        // Fullscreen
    bridge: None,            // No Lustre integration
    init: init,
    update: update,
    view: view,
  )
}

fn init(ctx: tiramisu.Context) {
  #(Model(...), effect.none(), None)  // state, effect, physics_world
}

fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  #(new_model, effect.none(), None)
}

fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  scene.empty(id: "root", transform: transform.identity, children: [...])
}
```

## Lustre Integration

For bidirectional UI communication:

```gleam
import tiramisu/ui

pub fn main() {
  let bridge = ui.new_bridge()

  // Lustre receives bridge in flags, calls ui.register_lustre(bridge) in init
  let assert Ok(_) = lustre.application(init, update, view)
    |> lustre.start("#app", Flags(bridge:))

  // Tiramisu uses same bridge
  tiramisu.run(bridge: Some(bridge), ...)
}

// Lustre -> Tiramisu: ui.to_tiramisu(bridge, GameMsg)
// Tiramisu -> Lustre: ui.to_lustre(bridge, UIMsg)
```

## FFI Architecture

```
Gleam Modules (type-safe API)
    ↓
threejs.ffi.mjs / rapier.ffi.mjs (pure 1:1 bindings)
    ↓
Three.js / Rapier3D
```

- `threejs.ffi.mjs`: Pure Three.js bindings, no logic
- `rapier.ffi.mjs`: Pure Rapier bindings, no logic
- `tiramisu.ffi.mjs`: Business logic (game loop, rendering, audio)
- `ui.ffi.mjs`: Lustre bridge communication
- `game_runtime.ffi.mjs`: Runtime state management

## Key Modules

- `tiramisu`: Main entry point, `run()` function
- `tiramisu/scene`: Scene nodes (mesh, camera, light, empty, sprite, model)
- `tiramisu/geometry`: Box, sphere, plane, cylinder, etc.
- `tiramisu/material`: Standard material with builder pattern
- `tiramisu/camera`: Perspective and orthographic cameras
- `tiramisu/light`: Ambient, directional, point, spot lights
- `tiramisu/transform`: Position, rotation, scale
- `tiramisu/effect`: Side effects (tick, delay, interval, browser APIs)
- `tiramisu/input`: Keyboard, mouse, touch, gamepad state
- `tiramisu/physics`: Rapier integration (rigid bodies, colliders)
- `tiramisu/audio`: Positional audio, audio groups
- `tiramisu/ui`: Lustre integration bridge

## Context Fields

```gleam
Context(
  delta_time: Duration,           // Use duration.to_seconds()
  input: input.InputState,        // Current input state
  canvas_size: Vec2(Float),       // Canvas dimensions
  physics_world: Option(...),     // Physics world if enabled
  scene: Scene,                   // Three.js scene
  renderer: Renderer,             // WebGL renderer
)
```

## Notes

- Cannot run examples directly - user runs them separately
- Postprocessing is camera-based (attach to scene.camera node)
- Physics bodies defined inline on mesh nodes
- Use `let assert Ok(x) = ...` for validated constructors
