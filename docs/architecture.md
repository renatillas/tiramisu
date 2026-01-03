# Tiramisu Game Architecture Guide

This guide explains how to structure games using Tiramisu's Model-View-Update (MVU) architecture. By the end, you'll understand how to build scalable, maintainable games with clear separation of concerns.

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [The MVU Pattern](#the-mvu-pattern)
3. [Independent Tick Cycles](#independent-tick-cycles)
4. [Structuring a Multi-Module Game](#structuring-a-multi-module-game)
5. [Cross-Module Communication with Taggers](#cross-module-communication-with-taggers)
6. [The Physics Coordination Pattern](#the-physics-coordination-pattern)
7. [UI Integration with Lustre](#ui-integration-with-lustre)
8. [Complete Example](#complete-example)

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
  tiramisu.run(
    selector: "#game",
    dimensions: tiramisu.FullScreen,
    bridge: option.None,
    init: init,
    update: update,
    view: view,
  )
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
- **Different rates** - UI can update at 60fps while AI updates at 10fps
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
      // Taggers for cross-module dispatch
      player_took_damage: fn(dmg) { PlayerMsg(player.TakeDamage(dmg)) },
      spawn_altar: fn(pos) { AltarMsg(altar.SpawnAltar(pos)) },
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
  spawn_altar spawn_altar,
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

    EnemyDied(position) -> {
      // Dispatch to altar module via tagger
      let spawn_effect = effect.dispatch(spawn_altar(position))
      #(remove_enemy(model), spawn_effect)
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

---

## The Physics Coordination Pattern

Physics requires special coordination because it needs data from multiple modules in the same frame.

### Physics Module Structure

```gleam
pub type TickResult {
  TickResult(
    physics: Model,
    enemy: enemy.Model,
    stepped_world: Option(physics.PhysicsWorld),
  )
}

pub fn update(
  msg msg: Msg,
  ctx ctx: tiramisu.Context,
  player_model player_model: player.Model,
  enemy_model enemy_model: enemy.Model,
  // Taggers
  enemy_took_damage enemy_took_damage,
  remove_projectile remove_projectile,
  effect_mapper effect_mapper,
) -> #(TickResult, effect.Effect(game_msg)) {
  case msg {
    Tick -> {
      // 1. PRE-STEP: Gather velocities from game state
      let projectile_velocities = get_projectile_velocities(player_model)
      let enemy_velocities = get_enemy_velocities(enemy_model)

      // 2. STEP: Run physics simulation
      let world = ctx.physics_world
        |> set_velocities(projectile_velocities)
        |> set_velocities(enemy_velocities)
      let stepped_world = physics.step(world, ctx.delta_time)

      // 3. POST-STEP: Process collisions, read positions
      let collisions = physics.get_collision_events(stepped_world)
      let collision_effects = process_collisions(collisions, enemy_took_damage, remove_projectile)

      let result = TickResult(
        physics: new_physics_model,
        enemy: updated_enemy_model,
        stepped_world: option.Some(stepped_world),
      )

      #(result, effect.batch([collision_effects, effect.dispatch(effect_mapper(Tick))]))
    }
  }
}
```

### Main Module Handling

```gleam
PhysicsMsg(physics_msg) -> {
  let #(result, physics_effect) =
    game_physics.update(
      msg: physics_msg,
      ctx: ctx,
      player_model: model.player,
      enemy_model: model.enemy,
      enemy_took_damage: fn(id, dmg) { EnemyMsg(enemy.TakeDamage(id, dmg)) },
      remove_projectile: fn(id) { PlayerMsg(player.RemoveProjectile(id)) },
      effect_mapper: PhysicsMsg,
    )

  #(
    Model(
      ..model,
      physics: result.physics,
      enemy: result.enemy,
    ),
    physics_effect,
    result.stepped_world,
  )
}
```

---

## UI Integration with Lustre

Tiramisu integrates with Lustre for HTML-based UI (menus, HUD, inventory screens).

### The Bridge Pattern

```gleam
import tiramisu/ui

pub fn main() {
  // 1. Create bridge for bidirectional communication
  let bridge = ui.new_bridge()

  // 2. Start Lustre UI
  lustre.application(ui_init, ui_update, ui_view)
  |> lustre.start("#ui", bridge)

  // 3. Start Tiramisu with the same bridge
  tiramisu.run(
    selector: "#game",
    dimensions: tiramisu.FullScreen,
    bridge: option.Some(bridge),
    init: init(bridge, _),
    update: update,
    view: view,
  )
}
```

### Sending Messages

**Tiramisu to Lustre:**

```gleam
// In game update function
let ui_effect = ui.to_lustre(bridge, UpdateScore(new_score))
```

**Lustre to Tiramisu:**

```gleam
// In Lustre update function
let game_effect = ui.to_tiramisu(bridge, StartGame)
```

### Message Types

Define separate message types for each system:

```gleam
// Game messages
pub type GameMsg {
  Tick
  PlayerMsg(player.Msg)
  // ...
}

// UI messages
pub type UIMsg {
  UpdateScore(Int)
  UpdateHealth(Float)
  ToggleInventory
  // ...
}
```

The bridge connects them: `ui.Bridge(UIMsg, GameMsg)`

---

## Complete Example

Here's how all the pieces fit together:

```gleam
// main.gleam
import gleam/option
import tiramisu
import tiramisu/effect
import tiramisu/physics
import tiramisu/ui
import my_game/player
import my_game/enemy
import my_game/game_physics
import my_game/ui as game_ui

pub type Msg {
  PlayerMsg(player.Msg)
  EnemyMsg(enemy.Msg)
  PhysicsMsg(game_physics.Msg)
}

pub type Model {
  Model(
    player: player.Model,
    enemy: enemy.Model,
    physics: game_physics.Model,
    bridge: ui.Bridge(game_ui.Msg, Msg),
  )
}

pub fn main() {
  let bridge = ui.new_bridge()

  // Start UI
  game_ui.start(bridge)

  // Start game
  tiramisu.run(
    selector: "#game",
    dimensions: tiramisu.FullScreen,
    bridge: option.Some(bridge),
    init: init(bridge, _),
    update: update,
    view: view,
  )
}

fn init(bridge, ctx) {
  let #(player_model, player_effect) = player.init()
  let #(enemy_model, enemy_effect) = enemy.init()
  let #(physics_model, _) = game_physics.init()

  let physics_world = physics.new_world(physics.WorldConfig(
    gravity: vec3.Vec3(0.0, -9.8, 0.0),
  ))

  let model = Model(
    player: player_model,
    enemy: enemy_model,
    physics: physics_model,
    bridge: bridge,
  )

  #(
    model,
    effect.batch([
      effect.map(player_effect, PlayerMsg),
      effect.map(enemy_effect, EnemyMsg),
      effect.dispatch(PhysicsMsg(game_physics.Tick)),
    ]),
    option.Some(physics_world),
  )
}

fn update(model, msg, ctx) {
  case msg {
    PlayerMsg(player_msg) -> {
      let #(new_player, player_effect) =
        player.update(
          model.player,
          player_msg,
          ctx,
          bridge: model.bridge,
          ui_msg: game_ui.PlayerStateUpdated,
          effect_mapper: PlayerMsg,
        )
      #(Model(..model, player: new_player), player_effect, ctx.physics_world)
    }

    EnemyMsg(enemy_msg) -> {
      let #(new_enemy, enemy_effect) =
        enemy.update(
          model.enemy,
          enemy_msg,
          ctx,
          player_took_damage: fn(dmg) { PlayerMsg(player.TakeDamage(dmg)) },
          effect_mapper: EnemyMsg,
        )
      #(Model(..model, enemy: new_enemy), enemy_effect, ctx.physics_world)
    }

    PhysicsMsg(physics_msg) -> {
      let #(result, physics_effect) =
        game_physics.update(
          msg: physics_msg,
          ctx: ctx,
          player_model: model.player,
          enemy_model: model.enemy,
          enemy_took_damage: fn(id, dmg) { EnemyMsg(enemy.TakeDamage(id, dmg)) },
          remove_projectile: fn(id) { PlayerMsg(player.RemoveProjectile(id)) },
          effect_mapper: PhysicsMsg,
        )
      #(
        Model(..model, physics: result.physics, enemy: result.enemy),
        physics_effect,
        result.stepped_world,
      )
    }
  }
}

fn view(model, ctx) {
  let player_nodes = player.view(model.player, ctx)
  let enemy_nodes = enemy.view(model.enemy, ctx)

  scene.empty(
    id: "root",
    transform: transform.identity,
    children: list.flatten([player_nodes, enemy_nodes]),
  )
}
```

---

## Summary

| Concept | Purpose |
|---------|---------|
| **MVU Pattern** | Predictable state management with pure functions |
| **Independent Ticks** | Each module manages its own update cycle |
| **Message Taggers** | Cross-module communication without import cycles |
| **Physics Coordination** | Special pattern for physics that needs multi-module data |
| **UI Bridge** | Bidirectional communication with Lustre |

### Best Practices

1. **Keep modules focused** - Each module owns one domain (player, enemies, physics)
2. **Use taggers everywhere** - Avoid module imports for cross-module dispatch
3. **Prefer async dispatch** - Sync only when same-frame data is required
4. **Parent routes, children act** - Main module doesn't contain game logic
5. **Test in isolation** - Pure functions make testing easy
