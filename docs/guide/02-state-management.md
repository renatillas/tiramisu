# State Management

If you're coming from Unity, Unreal, or Godot, the way Tiramisu handles state might feel unusual at first. There's no `GameObject` with mutable properties. No `MonoBehaviour` scripts modifying fields. Instead, all your game state lives in one place, changes happen through one mechanism, and the flow of data is always predictable.

This guide explains why that matters and how to structure your state as your game grows.

## The problem with mutable state

In traditional game engines, objects own their state and modify it directly:

```
// Pseudocode - typical mutable approach
class Player {
  var health = 100
  var position = Vector3(0, 0, 0)

  fun takeDamage(amount) {
    health -= amount
    if (health <= 0) die()
  }
}

class Enemy {
  fun attack(player) {
    player.takeDamage(10)  // Directly mutates player
  }
}
```

This works fine for small games. But as complexity grows, problems emerge:

- **Hidden dependencies**: Who modified the player's health? Could be anyone with a reference.
- **Order-dependent bugs**: The behavior changes depending on which update runs first.
- **Hard to test**: You need to set up entire object graphs to test one interaction.
- **Impossible to replay**: You can't "go back" to a previous state without saving everything.

The Elm Architecture solves these problems by making data flow explicit and unidirectional.

## Unidirectional data flow

Here's how Tiramisu manages state:

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                 │
│    ┌─────────┐         ┌─────────────┐         ┌─────────┐     │
│    │  Model  │ ──────► │    view     │ ──────► │  Scene  │     │
│    └─────────┘         └─────────────┘         └─────────┘     │
│         ▲                                           │           │
│         │                                           │           │
│         │                                           ▼           │
│    ┌─────────┐         ┌─────────────┐         ┌─────────┐     │
│    │ update  │ ◄────── │   Message   │ ◄────── │  Input  │     │
│    └─────────┘         └─────────────┘         └─────────┘     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

Data flows in one direction:

1. The **Model** contains all game state
2. The **view** function reads the Model and produces a Scene
3. User **Input** (or effects) generates Messages
4. The **update** function receives Messages and produces a new Model
5. Repeat

There's exactly one way for state to change: through `update`. This single constraint eliminates entire categories of bugs.

## Designing your Model

The Model is a Gleam type that holds everything about your game. Start simple:

```gleam
pub type Model {
  Model(
    player_position: Vec3(Float),
    player_health: Float,
    score: Int,
  )
}
```

As your game grows, you'll add more fields. But resist the urge to nest too deeply at first. Flat structures are easier to work with:

```gleam
// Good: flat and clear
pub type Model {
  Model(
    player_x: Float,
    player_y: Float,
    player_health: Float,
    player_facing: Direction,
    enemies: List(Enemy),
    bullets: List(Bullet),
    score: Int,
    game_state: GameState,
  )
}
```

### When to use nested types

Nest when a group of fields always change together:

```gleam
// The player's position, velocity, and facing are tightly coupled
pub type Player {
  Player(
    position: Vec3(Float),
    velocity: Vec3(Float),
    facing: Direction,
    health: Float,
  )
}

pub type Model {
  Model(
    player: Player,
    enemies: List(Enemy),
    // ...
  )
}
```

### Making impossible states impossible

One of Gleam's superpowers is preventing invalid states at compile time. Instead of:

```gleam
// Bad: Many ways to be in an invalid state
pub type Model {
  Model(
    is_game_over: Bool,
    is_paused: Bool,
    is_in_menu: Bool,
    score: Int,
    // What if is_game_over AND is_paused are both true?
  )
}
```

Use a custom type that encodes valid states:

```gleam
// Good: Only valid states are representable
pub type GameState {
  Playing(score: Int, lives: Int)
  Paused(score: Int, lives: Int)
  GameOver(final_score: Int)
  InMenu
}

pub type Model {
  Model(
    game_state: GameState,
    // ...
  )
}
```

Now it's impossible to be "game over" and "paused" simultaneously. The type system enforces your invariants.

## Designing your Messages

Messages describe **what happened**, not what should change. This distinction matters.

### Name messages as events

Bad message names describe actions:

```gleam
// Bad: Describes what to do
pub type Msg {
  SetPlayerHealth(Float)
  DecrementLives
  ResetGame
}
```

Good message names describe events:

```gleam
// Good: Describes what happened
pub type Msg {
  PlayerHitByEnemy(enemy_id: String, damage: Float)
  PlayerCollectedCoin(value: Int)
  PlayerRequestedRestart
  TimerExpired
}
```

The naming convention is **Subject-Verb-Object**: who did what to whom. This makes your code self-documenting.

### Why event names matter

Consider debugging. Which log is more helpful?

```
SetPlayerHealth(80)
SetPlayerHealth(70)
SetPlayerHealth(0)
```

vs

```
PlayerHitByEnemy("goblin-3", 20)
PlayerHitByEnemy("boss", 10)
PlayerFellInPit
```

Event names tell you *why* the state changed, not just *that* it changed.

### Group related messages

As your game grows, you'll have many messages. Group them logically:

```gleam
pub type Msg {
  // Game loop
  Tick

  // Player actions
  PlayerMoved(direction: Direction)
  PlayerJumped
  PlayerAttacked

  // Collisions
  PlayerHitEnemy(enemy_id: String)
  PlayerHitByEnemy(enemy_id: String, damage: Float)
  BulletHitEnemy(bullet_id: String, enemy_id: String)

  // Game flow
  GameStarted
  GamePaused
  GameResumed
  LevelCompleted(level: Int)
}
```

## Writing update functions

The update function is where state changes happen. It should be:

- **Pure**: Same inputs always produce same outputs
- **Total**: Handles every possible message
- **Focused**: Does one thing well

### Basic structure

```gleam
fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), Option(PhysicsWorld)) {
  case msg {
    Tick -> handle_tick(model, ctx)
    PlayerMoved(direction) -> handle_movement(model, direction, ctx)
    PlayerHitByEnemy(id, damage) -> handle_player_damage(model, id, damage)
    // ... handle all messages
  }
}
```

### Extracting handlers

Keep individual handlers small and focused:

```gleam
fn handle_player_damage(
  model: Model,
  enemy_id: String,
  damage: Float,
) -> #(Model, effect.Effect(Msg), Option(PhysicsWorld)) {
  let new_health = float.max(0.0, model.player.health -. damage)

  let new_player = Player(..model.player, health: new_health)
  let new_model = Model(..model, player: new_player)

  case new_health <=. 0.0 {
    True -> #(new_model, effect.dispatch(PlayerDied), None)
    False -> #(new_model, effect.none(), None)
  }
}
```

Notice how we:

1. Compute the new value
2. Create a new Player with the updated field (using `..model.player` spread syntax)
3. Create a new Model with the updated player
4. Dispatch a follow-up message if needed

### Updating nested state

Gleam's spread syntax makes nested updates readable:

```gleam
// Update a nested field
let new_model = Model(
  ..model,
  player: Player(
    ..model.player,
    position: new_position,
  ),
)
```

For deeper nesting, extract helper functions:

```gleam
fn update_player_position(model: Model, new_pos: Vec3(Float)) -> Model {
  Model(
    ..model,
    player: Player(..model.player, position: new_pos),
  )
}

fn update_player_health(model: Model, new_health: Float) -> Model {
  Model(
    ..model,
    player: Player(..model.player, health: new_health),
  )
}
```

## The tick pattern

Most games need a per-frame update. The standard pattern:

```gleam
pub type Msg {
  Tick
  // ... other messages
}

fn init(ctx: tiramisu.Context) {
  // Start the tick loop
  #(initial_model, effect.dispatch(Tick), None)
}

fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  case msg {
    Tick -> {
      let new_model =
        model
        |> update_physics(ctx.delta_time)
        |> update_animations(ctx.delta_time)
        |> check_collisions()

      // Continue the loop
      #(new_model, effect.dispatch(Tick), ctx.physics_world)
    }
    // ... other messages
  }
}
```

The `effect.dispatch(Tick)` at the end schedules another Tick for the next frame. This creates a continuous loop without any mutable state.

### Delta time

Always use `ctx.delta_time` for movement and animations:

```gleam
let delta_seconds = duration.to_seconds(ctx.delta_time)

// Movement at 5 units per second, regardless of frame rate
let movement = speed *. delta_seconds
```

This ensures your game runs at the same speed on 30fps and 144fps displays.

## Scaling to larger games

As your game grows, you'll want to split state management across modules. The pattern is straightforward: each module has its own Model, Msg, and update function.

### Module structure

```
src/
├── my_game.gleam           # Main module, combines everything
├── my_game/
│   ├── player.gleam        # Player state and logic
│   ├── enemies.gleam       # Enemy state and logic
│   ├── physics.gleam       # Physics integration
│   └── ui.gleam            # UI state (if using Lustre)
```

Each module exports its types and update function:

```gleam
// player.gleam
pub type Model {
  Model(position: Vec3(Float), health: Float, ...)
}

pub type Msg {
  Moved(Direction)
  TookDamage(Float)
  // ...
}

pub fn update(model: Model, msg: Msg, ctx: Context) -> #(Model, Effect(Msg)) {
  // ...
}
```

### Composing in the main module

The main module wraps child messages:

```gleam
// my_game.gleam
pub type Model {
  Model(
    player: player.Model,
    enemies: enemies.Model,
  )
}

pub type Msg {
  PlayerMsg(player.Msg)
  EnemyMsg(enemies.Msg)
  Tick
}

fn update(model: Model, msg: Msg, ctx: Context) {
  case msg {
    PlayerMsg(player_msg) -> {
      let #(new_player, player_effect) = player.update(model.player, player_msg, ctx)
      let new_model = Model(..model, player: new_player)
      #(new_model, effect.map(player_effect, PlayerMsg), ctx.physics_world)
    }

    EnemyMsg(enemy_msg) -> {
      // Similar pattern
    }

    Tick -> {
      // Update all systems
    }
  }
}
```

This pattern scales to any size. The [Architecture guide](07-architecture.md) covers advanced composition patterns including cross-module communication.

## Best practices

### Keep the Model serializable

Avoid putting functions or opaque runtime objects in your Model. This enables:

- Saving/loading game state
- Networking (send state over the wire)
- Time-travel debugging

```gleam
// Bad: Contains opaque types
pub type Model {
  Model(
    physics_world: physics.PhysicsWorld,  // Opaque, can't serialize
    render_texture: texture.Texture,       // Runtime object
  )
}

// Good: Only data
pub type Model {
  Model(
    player_position: Vec3(Float),
    enemy_positions: List(Vec3(Float)),
  )
}
```

Physics worlds are an exception—they're returned separately from update, not stored in Model.

### Don't over-engineer

Start with a flat Model. Extract modules when you feel pain, not before. A 200-line update function is fine. It's better to have clear, simple code than a complex architecture you don't need yet.

### Test your update function

Pure functions are trivial to test:

```gleam
pub fn player_takes_damage_test() {
  let model = Model(player: Player(health: 100.0, ..))
  let msg = PlayerHitByEnemy("goblin", 30.0)

  let #(new_model, _effect, _physics) = update(model, msg, mock_context())

  let assert 70.0 = new_model.player.health
}
```

No mocking frameworks needed. No setup/teardown. Just functions.

## Next steps

You now understand how Tiramisu manages state. The principles are simple:

1. All state lives in the Model
2. All changes happen through update
3. Messages describe events, not mutations
4. Data flows in one direction

Next, learn about [Side Effects](03-side-effects.md) to understand how Tiramisu handles timers, audio, and other interactions with the outside world.
