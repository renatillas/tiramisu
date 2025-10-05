# Tiramisu Immutable Architecture

## Overview

Tiramisu follows **Lustre's Model-View-Update (MVU) architecture** to ensure all public APIs are immutable and side effects are managed in a controlled, testable way.

## Core Principles

1. **Immutability**: All game state is immutable. Updates return new state instead of mutating.
2. **Pure Functions**: Game logic (`init`, `update`, `view`) are pure functions.
3. **Effects as Data**: Side effects are represented as data, not performed directly.
4. **Snapshot-Based Input**: Input state is captured once per frame as an immutable snapshot.

---

## Architecture Components

### 1. Effect System (`tiramisu/effect.gleam`)

Effects represent side effects as data that the runtime executes.

```gleam
import tiramisu/effect

// No side effect
effect.none()

// Custom effect with dispatch callback
effect.from(fn(dispatch) {
  // Perform side effect
  dispatch(MyMessage)
})

// Batch multiple effects
effect.batch([effect1, effect2, effect3])
```

**Key Functions:**
- `none()` - No-op effect
- `from(fn)` - Create custom effect
- `batch(List(Effect))` - Run multiple effects
- `map(effect, fn)` - Transform effect messages

---

### 2. Immutable Input (`tiramisu/input.gleam`)

Input state is captured once per frame as an immutable snapshot, replacing mutable global queries.

**Old (Mutable) API:**
```gleam
// ❌ Reads global mutable state
keyboard.is_pressed(keyboard.KeyW)
mouse.get_x()
```

**New (Immutable) API:**
```gleam
// ✅ Reads from immutable snapshot
input.is_key_pressed(ctx.input, keyboard.KeyW)
let #(x, y) = input.mouse_position(ctx.input)
```

**InputState Structure:**
```gleam
pub type InputState {
  InputState(
    keyboard: KeyboardState,
    mouse: MouseState,
    gamepad: List(GamepadState),  // Supports multiple gamepads
    touch: TouchState,
  )
}
```

**Available Helper Functions:**

**Keyboard:**
- `is_key_pressed(input, key)`
- `is_key_just_pressed(input, key)`
- `is_key_just_released(input, key)`

**Mouse:**
- `mouse_position(input)` → `#(Float, Float)`
- `mouse_delta(input)` → `#(Float, Float)`
- `is_left_button_pressed(input)`
- `is_right_button_pressed(input)`
- `mouse_wheel_delta(input)`

**Gamepad:**
- `is_gamepad_connected(input, index)` → `Bool`
- `gamepad_button(input, gamepad_index, button)` → `Float`
- `is_gamepad_button_pressed(input, gamepad_index, button)` → `Bool`
- `gamepad_axis(input, gamepad_index, axis)` → `Float`
- `get_axis_with_deadzone(input, gamepad_index, axis, deadzone)` → `Float`
- `is_left_stick_active(input, gamepad_index, threshold)` → `Bool`
- `is_right_stick_active(input, gamepad_index, threshold)` → `Bool`

**Gamepad (Primary - index 0 convenience functions):**
- `is_primary_connected(input)` → `Bool`
- `is_primary_gamepad_button_pressed(input, button)` → `Bool`
- `get_primary_button(input, button)` → `Float`
- `get_primary_axis(input, axis)` → `Float`

**Touch:**
- `touches(input)` → `List(Touch)`
- `touches_just_started(input)`
- `touches_just_ended(input)`
- `touch_count(input)`

---

### 3. Game Loop (`tiramisu/game.gleam`)

The game loop follows MVU architecture with automatic scene diffing.

**Function Signature:**
```gleam
game.run(
  width: Int,
  height: Int,
  background: Int,
  camera: camera.Camera,
  init: fn(GameContext) -> #(state, Effect(msg)),
  update: fn(state, msg, GameContext) -> #(state, Effect(msg)),
  view: fn(state) -> List(SceneNode),
  on_msg: fn(state, msg, GameContext) -> #(state, Effect(msg)),
)
```

**GameContext:**
```gleam
pub type GameContext {
  GameContext(
    camera: camera.Camera,
    delta_time: Float,
    input: InputState,  // Immutable snapshot
  )
}
```

---

## MVU Pattern in Tiramisu

### Model (State)

Define your game state as immutable data:

```gleam
pub type Model {
  Model(
    player_position: vec3.Vec3,
    player_rotation: vec3.Vec3,
    score: Int,
    enemies: List(Enemy),
  )
}
```

### Msg (Messages)

Define all events that can update your game:

```gleam
pub type Msg {
  Tick                        // Frame update
  PlayerMoved(vec3.Vec3)      // Player position changed
  EnemySpawned(Enemy)         // New enemy created
  ScoreIncreased(Int)         // Score changed
}
```

### Init

Initialize game state and optionally trigger effects:

```gleam
pub fn init(ctx: GameContext) -> #(Model, Effect(Msg)) {
  let model = Model(
    player_position: vec3.Vec3(0.0, 0.0, 0.0),
    player_rotation: vec3.Vec3(0.0, 0.0, 0.0),
    score: 0,
    enemies: [],
  )

  // Spawn first enemy after 1 second
  #(model, spawn_enemy_effect())
}
```

### Update

Handle messages and return new state + effects:

```gleam
pub fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      // Update based on delta time and input
      let new_position = update_player_position(model, ctx)

      #(Model(..model, player_position: new_position), effect.none())
    }

    PlayerMoved(position) -> {
      #(Model(..model, player_position: position), effect.none())
    }

    EnemySpawned(enemy) -> {
      let new_enemies = [enemy, ..model.enemies]
      #(Model(..model, enemies: new_enemies), effect.none())
    }

    ScoreIncreased(amount) -> {
      let new_score = model.score + amount
      #(Model(..model, score: new_score), play_sound_effect())
    }
  }
}
```

### View

Declare what the scene should look like based on current state:

```gleam
pub fn view(model: Model) -> List(SceneNode) {
  [
    // Player mesh
    scene.Mesh(
      id: "player",
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(
        color: 0x00ff00,
        metalness: 0.3,
        roughness: 0.7,
        map: option.None,
      ),
      transform: scene.Transform(
        position: model.player_position,
        rotation: model.player_rotation,
        scale: vec3.one(),
      ),
    ),

    // Enemy meshes
    ..render_enemies(model.enemies),

    // Lights
    scene.Light(
      id: "sun",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 1.0),
      transform: scene.transform_at(10.0, 20.0, 10.0),
    ),
  ]
}
```

---

## Example: Complete Immutable Game

```gleam
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/input
import tiramisu/input/keyboard
import tiramisu/math/vec3
import tiramisu/scene

// --- Model ---
pub type Model {
  Model(position: vec3.Vec3, velocity: vec3.Vec3)
}

// --- Msg ---
pub type Msg {
  Tick
}

// --- Init ---
pub fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  #(Model(position: vec3.zero(), velocity: vec3.zero()), effect.none())
}

// --- Update ---
pub fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      // Immutable input queries
      let accel_x = case
        input.is_key_pressed(ctx.input, keyboard.KeyD),
        input.is_key_pressed(ctx.input, keyboard.KeyA)
      {
        True, False -> 10.0
        False, True -> -10.0
        _, _ -> 0.0
      }

      // Physics update (immutable)
      let new_velocity = vec3.Vec3(
        model.velocity.x + accel_x * ctx.delta_time,
        model.velocity.y,
        model.velocity.z,
      )

      let new_position = vec3.add(
        model.position,
        vec3.scale(new_velocity, ctx.delta_time),
      )

      #(
        Model(position: new_position, velocity: new_velocity),
        effect.none(),
      )
    }
  }
}

// --- View ---
pub fn view(model: Model) -> List(SceneNode) {
  [
    scene.Mesh(
      id: "player",
      geometry: scene.SphereGeometry(1.0, 32, 16),
      material: scene.BasicMaterial(
        color: 0xff0000,
        transparent: False,
        opacity: 1.0,
        map: option.None,
      ),
      transform: scene.Transform(
        position: model.position,
        rotation: vec3.zero(),
        scale: vec3.one(),
      ),
    ),
  ]
}

// --- Main ---
pub fn main() {
  let assert Ok(camera) = camera.perspective(
    field_of_view: 75.0,
    aspect: 16.0 /. 9.0,
    near: 0.1,
    far: 1000.0,
  )

  game.run(
    width: 800,
    height: 600,
    background: 0x000000,
    camera: camera,
    init: init,
    update: update,
    view: view,
    on_msg: update,  // Messages handled by update
  )
}
```

---

## Benefits of Immutable Architecture

### 1. **Predictability**
- Game state never mutates unexpectedly
- Easy to reason about data flow
- No hidden side effects

### 2. **Testability**
```gleam
// Test update logic without side effects
let model = Model(position: vec3.zero(), velocity: vec3.zero())
let ctx = test_context()
let #(new_model, _effect) = update(model, Tick, ctx)

assert new_model.position == vec3.Vec3(1.0, 0.0, 0.0)
```

### 3. **Time-Travel Debugging**
- Record all messages
- Replay game state at any point
- Inspect state history

### 4. **Easier Concurrency**
- No race conditions (immutable data)
- Safe to snapshot state
- Parallel scene rendering

---

## Migration Guide

### Old Mutable API → New Immutable API

| Old (Mutable) | New (Immutable) |
|---------------|-----------------|
| `keyboard.is_pressed(key)` | `input.is_key_pressed(ctx.input, key)` |
| `mouse.get_x()` | `let #(x, _) = input.mouse_position(ctx.input)` |
| `gamepad.is_pressed(button)` | `input.is_gamepad_button_pressed(ctx.input, gamepad_index, button)` or `input.is_primary_gamepad_button_pressed(ctx.input, button)` |
| Mutate state directly | Return new state from `update()` |
| Perform side effects inline | Return `Effect` from `update()` |

---

## Best Practices

1. **Keep Update Pure**: Don't perform side effects in `update()`, return `Effect` instead
2. **Single Source of Truth**: All game state in `Model`
3. **Descriptive Messages**: Name messages after domain events, not implementation
4. **Small Update Functions**: Break complex logic into helper functions
5. **Test Without Effects**: Most logic can be tested without running effects

---

## See Also

- **Example**: `src/tiramisu/examples/immutable_demo.gleam`
- **Effect API**: `src/tiramisu/effect.gleam`
- **Input API**: `src/tiramisu/input.gleam`
- **Lustre Documentation**: https://hexdocs.pm/lustre/
