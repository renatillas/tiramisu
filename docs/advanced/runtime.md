# Tiramisu Runtime Cycle

This document explains how Tiramisu executes your game code - when each function is called and how effects are processed.

## Overview

Tiramisu runs a continuous game loop using the browser's `requestAnimationFrame`. Your code interacts with this loop through three functions:

### Initialization

```
  tiramisu.application(init, update, view)
  |> tiramisu.start("#app", dimensions, bridge) called
       ↓
  init(ctx) → #(Model, Effect, Option(PhysicsWorld))
       ↓
  view(model, ctx) → scene.Node  (initial render)
       ↓
  Process initial effects
       ↓
  Start game loop
```

### Game Loop

```
  1. Browser calls requestAnimationFrame callback
       ↓
  2. Calculate delta_time, capture input, get canvas size
       ↓
  3. Process ALL queued messages
     for each msg in queue
       update(model, msg, ctx) → #(Model, Effect, Physics)
       ↓
  4. view(model, ctx) → scene.Node
       ↓
  5. Diff scene, apply patches, render to canvas
       ↓
  6. Process ALL effects from this frame
       ↓
  7. Clear per-frame input state (just_pressed, etc.)
       ↓
  8. Schedule next frame (goto step 1)
```

## The Three Core Functions

### `init`

Called **once** at startup. Returns initial state, effects, and optionally a physics world.

```gleam
fn init(ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg), Option(physics.PhysicsWorld)) {
  let model = Model(position: Vec3(0.0, 0.0, 0.0))

  // Start the tick loop
  #(model, effect.dispatch(Tick), option.None)
}
```

**When called:** Immediately when `tiramisu.start()` executes, before the game loop starts.

**Context available:** `ctx.delta_time` is zero, `ctx.input` is empty, `ctx.physics_world` is `None`.

### `update`

Called **once per message** in the queue. Multiple messages can be processed in a single frame.

```gleam
fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), Option(physics.PhysicsWorld)) {
  case msg {
    Tick -> {
      let new_model = move_player(model, ctx)
      #(new_model, effect.dispatch(Tick), ctx.physics_world)
    }
    Jump -> {
      let new_model = Model(..model, velocity_y: 10.0)
      #(new_model, effect.none(), ctx.physics_world)
    }
  }
}
```

**When called:** At the start of each frame, for every message in the queue.

**Important:** If 3 messages are queued, `update` is called 3 times before `view` is called once.

### `view`

Called **once per frame** after all messages are processed. Returns the scene to render.

```gleam
fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  scene.mesh(
    id: "player",
    geometry: player_geo,
    material: player_mat,
    transform: transform.at(position: model.position),
    physics: option.None,
  )
}
```

**When called:** Once per frame, after all `update` calls complete.

## The Message Queue

Messages are stored in a queue and processed at the start of each frame.

```
Frame N                              Frame N+1
────────────────────────────────────────────────────────────────

  Queue: [Tick, Jump, Tick]          Queue: [Tick]
         ↓                                  ↓
  update(Tick)  → dispatches Tick    update(Tick) → dispatches Tick
  update(Jump)  → dispatches nothing         ↓
  update(Tick)  → dispatches nothing  view() called
         ↓                                  ↓
  view() called                       render
         ↓                                  ↓
  render                              process effects (Tick queued)
         ↓
  process effects (Tick queued for next frame)
```

### How messages get queued

**`effect.dispatch(msg)`** - Adds `msg` to the queue immediately:

```gleam
// When this effect is processed, Tick is added to the queue
effect.dispatch(Tick)
```

The message will be processed in the **next frame** (since effects are processed at the end of the current frame).

## Effect Processing

Effects are collected from all `update` calls and processed **after** rendering:

```
Frame Timeline:
───────────────────────────────────────────────────────────────────
  update()  update()  update()  view()  render  PROCESS EFFECTS
     │         │         │                          │
     └─────────┴─────────┴──────────────────────────┘
          effects batched together              processed here
```

### What "processing" means for each effect type

| Effect | What happens when processed |
|--------|----------------------------|
| `effect.dispatch(msg)` | Adds `msg` to the queue (processed next frame) |
| `effect.batch([...])` | Processes each effect in order |
| `effect.none()` | Nothing |
| `effect.delay(duration, msg)` | Schedules a `setTimeout` to queue `msg` later |
| `effect.from(fn)` | Executes the function (for custom side effects) |
| Browser effects | Calls browser APIs (fullscreen, clipboard, etc.) |

Most game code only uses `effect.dispatch` and `effect.batch`, which just queue messages - no actual "execution" happens beyond adding to the queue.

### Common Effects

```gleam
// Add message to queue (processed next frame)
effect.dispatch(Tick)

// Combine multiple effects
effect.batch([
  effect.dispatch(Tick),
  effect.dispatch(UpdateUI),
])

// Do nothing
effect.none()

// Delay a message (schedules setTimeout)
effect.delay(duration.seconds(1), DelayedMsg)

// Custom side effect
effect.from(fn(dispatch) {
  // This code runs immediately when effect is processed
  do_something()
  dispatch(SomeMsg)  // Queue a message
})
```

## The Tick Loop Pattern

Since effects are processed at the end of each frame, dispatching a message schedules it for the next frame. This creates a natural tick loop:

```gleam
pub type Msg {
  Tick
}

fn init(ctx) {
  // Start the loop
  #(Model(...), effect.dispatch(Tick), option.None)
}

fn update(model, msg, ctx) {
  case msg {
    Tick -> {
      // Do per-frame work
      let new_model = update_game(model, ctx.delta_time)

      // Continue the loop
      #(new_model, effect.dispatch(Tick), ctx.physics_world)
    }
  }
}
```

**Flow:**
1. `init` dispatches `Tick` → queued
2. Frame 1: `update(Tick)` runs, dispatches `Tick` → queued for next frame
3. Frame 2: `update(Tick)` runs, dispatches `Tick` → queued for next frame
4. ...continues forever

## Context Details

The `tiramisu.Context` is updated each frame:

```gleam
pub type Context {
  Context(
    delta_time: Duration,           // Time since last frame
    input: input.InputState,        // Current input state
    canvas_size: Vec2(Float),       // Canvas dimensions in pixels
    physics_world: Option(PhysicsWorld),  // Physics simulation
    scene: Scene,                   // Three.js scene
    renderer: Renderer,             // WebGL renderer
  )
}
```

### delta_time

Time elapsed since the previous frame. Use for frame-rate independent movement:

```gleam
let speed = 5.0  // units per second
let movement = speed *. duration.to_seconds(ctx.delta_time)
```

### input

Input state is captured at the start of each frame. Per-frame states (`is_key_just_pressed`, etc.) are cleared at the end of each frame:

```gleam
// Held down (true every frame while held)
input.is_key_pressed(ctx.input, input.KeyW)

// Just pressed this frame (true for one frame only)
input.is_key_just_pressed(ctx.input, input.KeySpace)

// Just released this frame (true for one frame only)
input.is_key_just_released(ctx.input, input.KeySpace)
```

## Physics World Flow

The physics world flows through the system:

```
init() → Option(PhysicsWorld) ──┐
                                ↓
                         stored in Context
                                ↓
update() receives ctx.physics_world
         │
         ↓
update() returns Option(PhysicsWorld)
         │
         ↓
   if Some(world): replaces ctx.physics_world for next update/view
   if None: keeps existing physics_world
```

**Typical pattern:**

```gleam
fn update(model, msg, ctx) {
  case msg {
    Tick -> {
      let assert option.Some(world) = ctx.physics_world

      // Step physics
      let new_world = physics.step(world, ctx.delta_time)

      // Return updated world
      #(model, effect.dispatch(Tick), option.Some(new_world))
    }
  }
}
```
