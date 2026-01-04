# Tiramisu Game Architecture Guide

This guide explains how to structure games using Tiramisu's Model-View-Update (MVU) architecture. By the end, you'll understand how to build scalable, maintainable games with clear separation of concerns.

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [The MVU Pattern](#the-mvu-pattern)
3. [Independent Tick Cycles](#independent-tick-cycles)
4. [Structuring a Multi-Module Game](#structuring-a-multi-module-game)
5. [Cross-Module Communication with Taggers](#cross-module-communication-with-taggers)

---

## Core Concepts

Tiramisu follows the **Elm Architecture** (Model-View-Update), adapted for game development:

| Concept | Description |
|---------|-------------|
| **Model** | Your game state - immutable data representing everything in your game |
| **Msg** | Messages that describe events (user input, timers, collisions) |
| **Update** | Pure function: `(Model, Msg, Context) -> (Model, Effect, PhysicsWorld)` |
| **View** | Pure function: `(Model, Context) -> scene.Node` |
| **Effect** | Side effects like scheduling the next frame, playing sounds, or dispatching messages |

### Why MVU for Games?

1. **Predictable state** - All state changes happen in `update`, making debugging easy
2. **Time travel** - You can replay states for debugging or replays
3. **Testable** - Pure functions are easy to unit test
4. **Composable** - Modules can be nested and combined

---

## The MVU Pattern

### Basic Game Structure

```gleam
import gleam/option
import tiramisu
import tiramisu/effect
import tiramisu/scene

// 1. DEFINE YOUR STATE
pub type Model {
  Model(
    position: Vec3(Float),
    health: Float,
    score: Int,
  )
}

// 2. DEFINE YOUR MESSAGES
pub type Msg {
  Tick                    // Frame update
  PlayerHit(damage: Float)
  ScorePoint
}

// 3. INITIALIZE STATE
pub fn init(ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg), option.Option(physics.PhysicsWorld)) {
  let model = Model(
    position: Vec3(0.0, 0.0, 0.0),
    health: 100.0,
    score: 0,
  )

  // Start the game loop by scheduling the first tick
  #(model, effect.dispatch(Tick), option.None)
}

// 4. UPDATE STATE BASED ON MESSAGES
pub fn update(model: Model, msg: Msg, ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg), option.Option(physics.PhysicsWorld)) {
  case msg {
    Tick -> {
      let new_model = update_game_logic(model, ctx)
      // Schedule next frame
      #(new_model, effect.dispatch(Tick), ctx.physics_world)
    }

    PlayerHit(damage) -> {
      let new_health = model.health -. damage
      #(Model(..model, health: new_health), effect.none(), ctx.physics_world)
    }

    ScorePoint -> {
      #(Model(..model, score: model.score + 1), effect.none(), ctx.physics_world)
    }
  }
}

// 5. RENDER THE SCENE
pub fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  scene.mesh(
    id: "player",
    geometry: player_geometry,
    material: player_material,
    transform: transform.at(position: model.position),
    physics: option.None,
  )
}

// 6. START THE GAME
pub fn main() {
  tiramisu.application(init, update, view)
  |> tiramisu.start("#game", tiramisu.FullScreen, option.None)
}
```

### The Context

Every `update` and `view` function receives a `Context` with frame information:

```gleam
pub type Context {
  Context(
    delta_time: Duration,                        // Time since last frame
    input: input.InputState,                     // Keyboard, mouse, gamepad state
    canvas_size: Vec2(Float),                    // Canvas dimensions
    physics_world: Option(physics.PhysicsWorld), // Physics simulation state
    scene: scene.Scene,                          // Three.js scene
    renderer: scene.Renderer,                    // WebGL renderer
  )
}
```

Use `delta_time` for frame-rate independent movement:

```gleam
let speed = 10.0
let movement = speed *. duration.to_seconds(ctx.delta_time)
```

---

## Independent Tick Cycles

In complex games, different systems update at different rates or independently. Each module manages its own tick cycle using `effect.dispatch(Tick)`.

### Why Independent Ticks?

- **Decoupling** - Systems don't depend on each other's update order
- **Easier debugging** - Each system's tick is isolated

### Pattern: Self-Scheduling Tick

```gleam
// Each module has its own Tick message
pub type Msg {
  Tick
  // ... other messages
}

pub fn init() -> #(Model, effect.Effect(Msg)) {
  // Schedule the first tick
  #(initial_model, effect.dispatch(Tick))
}

pub fn update(model: Model, msg: Msg, ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick -> {
      let new_model = process_tick(model, ctx)
      // Schedule next tick - creates continuous loop
      #(new_model, effect.dispatch(Tick))
    }
  }
}
```

---

## Structuring a Multi-Module Game

As games grow, split them into modules. Each module follows the same MVU pattern.

### Recommended Structure

```
src/
├── my_game.gleam              # Main module - routes messages
├── my_game/
│   ├── player.gleam           # Player movement, health, inventory
│   ├── player/
│   │   └── magic.gleam        # Nested: spells, projectiles
│   ├── enemy.gleam            # Enemy AI, spawning, attacks
│   ├── map.gleam              # Level geometry, loading
│   └── game_physics.gleam     # Physics coordination
└── my_game/
    └── ui.gleam               # Lustre UI integration
```

### Module Interface

Each module exports a consistent interface:

```gleam
// player.gleam

pub type Model { ... }
pub type Msg { ... }

pub fn init() -> #(Model, effect.Effect(Msg))
pub fn update(model: Model, msg: Msg, ctx: tiramisu.Context, ...) -> #(Model, effect.Effect(game_msg))
pub fn view(model: Model, ctx: tiramisu.Context) -> List(scene.Node)
```

### Message Tree

The main module wraps child messages:

```gleam
// main module
pub type Msg {
  PlayerMsg(player.Msg)
  EnemyMsg(enemy.Msg)
  PhysicsMsg(game_physics.Msg)
  MapMsg(map.Msg)
}
```

This creates a tree structure:

```
Msg (main)
├── PlayerMsg(player.Msg)
│   ├── Tick
│   ├── TakeDamage(Float)
│   └── MagicMsg(magic.Msg)
│       ├── Tick
│       ├── CastSpell
│       └── RemoveProjectile(Int)
├── EnemyMsg(enemy.Msg)
│   ├── Tick
│   └── TakeProjectileDamage(id, damage)
└── PhysicsMsg(game_physics.Msg)
    └── Tick
```

---

## Cross-Module Communication with Taggers

The key challenge: **How does the enemy module tell the player module about damage?**

Child modules can't import siblings (would create cycles). The solution: **Message Taggers**.

### What Are Taggers?

Taggers are functions passed from parent to child that wrap messages into the parent's message type:

```gleam
// Parent knows how to wrap messages
fn(damage) { PlayerMsg(player.TakeDamage(damage)) }
```

### The Pattern

**Parent (main module) passes taggers:**

```gleam
EnemyMsg(enemy_msg) -> {
  let #(new_enemy, enemy_effect) =
    enemy.update(
      model.enemy,
      enemy_msg,
      ctx,
      // Tagger for cross-module dispatch
      player_took_damage: fn(dmg) { PlayerMsg(player.TakeDamage(dmg)) },
      // effect_mapper: wraps child's own Tick message
      effect_mapper: EnemyMsg,
    )
  #(Model(..model, enemy: new_enemy), enemy_effect, ctx.physics_world)
}
```

**Child (enemy module) uses taggers:**

```gleam
pub fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
  // Accept taggers as parameters
  player_took_damage player_took_damage,
  effect_mapper effect_mapper,
) -> #(Model, effect.Effect(game_msg)) {
  case msg {
    Tick -> {
      let #(new_model, damage_dealt) = tick(model, ctx)

      // Use tagger to dispatch to sibling
      let damage_effect = case damage_dealt >. 0.0 {
        True -> effect.dispatch(player_took_damage(damage_dealt))
        False -> effect.none()
      }

      // Use effect_mapper to wrap own Tick
      #(new_model, effect.batch([
        effect.dispatch(effect_mapper(Tick)),
        damage_effect,
      ]))
    }
  }
}
```

### Key Principles

1. **Parent is the router** - Only the parent knows all sibling message types
2. **Taggers are functions** - `fn(args) -> ParentMsg`
3. **effect_mapper for self** - Wraps the module's own messages
4. **No sibling imports** - Children only know tagger function signatures
5. **Effects bubble up** - Child returns `effect.Effect(game_msg)` (parent's type)

### Async vs Sync Updates

**Prefer async dispatch** for cross-module updates:

```gleam
// Dispatch effect - processed next frame
effect.dispatch(player_took_damage(10.0))
```

**Use sync only when data is needed immediately** (same frame):

```gleam
// Sync: Get velocities for physics (needed THIS frame)
let #(updated_enemy, velocities) = enemy.update_for_physics(enemy_model, player_pos)

// Physics uses velocities immediately
let stepped_world = physics.step(set_velocities(world, velocities), dt)

// Async: Position updates can wait
effect.dispatch(update_enemy_positions(new_positions))
```
