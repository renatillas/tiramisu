# Side Effects

So far, we've focused on pure functions: `update` takes a Model and Message, returns a new Model. No side effects, no external interactions, completely deterministic.

But games need to interact with the world. You need to:

- Schedule the next frame
- Play sound effects
- Load assets from URLs
- Read from local storage
- Request fullscreen mode

These are **side effects**—actions that reach outside your pure game logic. Tiramisu handles them through the `Effect` type, keeping your code testable while still letting you do everything a real game needs.

## Why managed effects?

You might wonder: why not just call functions directly? What's wrong with this?

```gleam
// Hypothetical unmanaged approach
fn update(model, msg, ctx) {
  case msg {
    PlaySound -> {
      audio.play("explosion.wav")  // Side effect!
      #(model, effect.none(), None)
    }
  }
}
```

It seems simpler. But there are problems:

1. **Testing becomes hard**: Your test now needs an audio system, or you need to mock it.

2. **Order matters**: If `audio.play` is slow, it blocks the update. If it's async, you lose control of when it completes.

3. **Composition breaks**: What if you want to play two sounds? Or play a sound *and* dispatch a message? You'd need to manage callbacks manually.

4. **No replay**: If you're recording a game for replay, the side effects happen immediately instead of being recorded.

The Effect system solves all of these. Effects are **data** describing what should happen. The runtime executes them after your pure functions return.

## The Effect type

An `Effect(Msg)` is a description of work to do. It doesn't do anything by itself—it's a value you return from `init` or `update`, and the runtime processes it.

```gleam
fn update(model, msg, ctx) -> #(Model, effect.Effect(Msg), Option(PhysicsWorld)) {
  case msg {
    Tick -> {
      // Return an effect that dispatches Tick next frame
      #(model, effect.dispatch(Tick), None)
    }
  }
}
```

## Core effects

### dispatch

The most common effect. Adds a message to the queue for the next frame:

```gleam
effect.dispatch(Tick)
effect.dispatch(PlayerJumped)
effect.dispatch(SpawnEnemy(position))
```

Messages dispatched this frame are processed next frame. This creates the game loop:

```
Frame 1: Process Tick → dispatch Tick
Frame 2: Process Tick → dispatch Tick
Frame 3: Process Tick → dispatch Tick
...
```

### none

When nothing should happen:

```gleam
fn update(model, msg, ctx) {
  case msg {
    SomethingTrivial -> {
      // No effect needed
      #(model, effect.none(), None)
    }
  }
}
```

This is the "do nothing" effect. Use it when you update state but don't need any side effects.

### batch

Combine multiple effects:

```gleam
effect.batch([
  effect.dispatch(Tick),
  effect.dispatch(UpdateUI),
  effect.dispatch(SaveProgress),
])
```

All effects in the batch are processed in order. Use this when one event should trigger multiple things.

### delay

Schedule a message for later:

```gleam
// Dispatch PowerUpExpired in 10 seconds
effect.delay(duration.seconds(10), PowerUpExpired)

// Or using milliseconds
effect.delay(duration.milliseconds(500), HalfSecondPassed)
```

The message won't fire until the duration elapses. Great for:

- Power-up timers
- Spawn delays
- Cooldowns
- Timed events

### from

Create custom effects from a callback:

```gleam
effect.from(fn(dispatch) {
  // Do something synchronously
  let result = compute_something()

  // Then dispatch a message with the result
  dispatch(GotResult(result))
})
```

The callback receives a `dispatch` function. Call it with messages to send back to your update function. You can dispatch zero, one, or many messages.

## Common patterns

### The tick loop

We've seen this, but let's be explicit about how it works:

```gleam
pub type Msg {
  Tick
}

fn init(ctx) {
  // Start the loop by dispatching the first Tick
  #(Model(..), effect.dispatch(Tick), None)
}

fn update(model, msg, ctx) {
  case msg {
    Tick -> {
      // Do per-frame work...
      let new_model = update_game(model, ctx.delta_time)

      // Continue the loop
      #(new_model, effect.dispatch(Tick), None)
    }
  }
}
```

The `effect.dispatch(Tick)` schedules Tick for the next frame. Since effects are processed at the end of each frame, this creates:

```
Frame 1: Tick queued from init
         → update(Tick) runs
         → dispatch(Tick) effect processed
         → Tick queued for next frame

Frame 2: Tick in queue
         → update(Tick) runs
         → dispatch(Tick) effect processed
         → Tick queued for next frame

...forever
```

### Chained effects

Sometimes one effect should trigger another after completion. Use `effect.from` with dispatch:

```gleam
fn load_level(level_number: Int) -> effect.Effect(Msg) {
  effect.from(fn(dispatch) {
    dispatch(LoadingStarted)

    // Simulate async work (in practice, this would be an actual load)
    // ...

    dispatch(LevelLoaded(level_number))
  })
}
```

### Conditional effects

Return different effects based on state:

```gleam
fn update(model, msg, ctx) {
  case msg {
    PlayerRequestedJump -> {
      case model.player.is_grounded {
        True -> {
          let new_player = Player(..model.player, velocity_y: jump_velocity)
          #(Model(..model, player: new_player), effect.dispatch(PlayJumpSound), None)
        }
        False -> {
          // Can't jump while airborne - no effect
          #(model, effect.none(), None)
        }
      }
    }
  }
}
```

### Effect composition

Build complex effects from simple ones:

```gleam
fn game_over_effects(score: Int) -> effect.Effect(Msg) {
  effect.batch([
    effect.dispatch(StopMusic),
    effect.dispatch(PlayGameOverSound),
    effect.dispatch(SaveHighScore(score)),
    effect.delay(duration.seconds(3), ShowRetryScreen),
  ])
}

fn update(model, msg, ctx) {
  case msg {
    PlayerDied -> {
      let new_model = Model(..model, game_state: GameOver)
      #(new_model, game_over_effects(model.score), None)
    }
  }
}
```

## Audio effects

Loading and playing audio uses the effect system:

### Loading audio

```gleam
pub type Msg {
  SoundLoaded(audio.Buffer)
  SoundFailed
  // ...
}

fn init(ctx) {
  let load_effect = audio.load_audio(
    url: "sounds/jump.wav",
    on_success: SoundLoaded,
    on_error: SoundFailed,
  )

  #(Model(sound: None), load_effect, None)
}

fn update(model, msg, ctx) {
  case msg {
    SoundLoaded(buffer) -> {
      // Store the loaded audio buffer
      #(Model(..model, sound: Some(buffer)), effect.none(), None)
    }
    SoundFailed -> {
      // Handle error (maybe log it, continue without sound)
      #(model, effect.none(), None)
    }
  }
}
```

### Playing audio

Audio is played through the scene graph, not effects. Add an `audio` node to your view:

```gleam
fn view(model, ctx) {
  scene.empty(
    id: "root",
    transform: transform.identity,
    children: [
      // ... other nodes
      case model.play_jump_sound, model.sound {
        True, Some(buffer) ->
          scene.audio(
            id: "jump-sound",
            audio: audio.GlobalAudio(
              buffer: buffer,
              config: audio.config()
                |> audio.with_state(audio.Playing)
                |> audio.with_volume(0.8),
            ),
          )
        _, _ -> scene.empty(id: "no-sound", transform: transform.identity, children: [])
      },
    ],
  )
}
```

See the [Audio guide](08-audio.md) for details on positional audio, looping, and audio groups.

## Background effects

The `background.set` effect changes the scene background:

```gleam
pub type Msg {
  BackgroundSet
  BackgroundFailed
}

fn init(ctx) {
  let bg_effect = background.set(
    ctx.scene,
    background.Color(0x1a1a2e),  // Dark blue
    on_success: BackgroundSet,
    on_error: BackgroundFailed,
  )

  #(Model(..), effect.batch([effect.dispatch(Tick), bg_effect]), None)
}
```

You can also use skyboxes:

```gleam
background.set(
  ctx.scene,
  background.Skybox(skybox_texture),
  on_success: BackgroundSet,
  on_error: BackgroundFailed,
)
```

## Browser effects

Tiramisu provides effects for common browser APIs:

### Fullscreen

```gleam
import tiramisu/browser

// Request fullscreen
browser.request_fullscreen(
  on_success: FullscreenEnabled,
  on_error: FullscreenFailed,
)

// Exit fullscreen
browser.exit_fullscreen(
  on_success: FullscreenExited,
  on_error: FullscreenExitFailed,
)
```

### Clipboard

```gleam
browser.copy_to_clipboard(
  text: "High score: 9999",
  on_success: CopiedToClipboard,
  on_error: ClipboardFailed,
)
```

### Local storage

```gleam
browser.save_to_storage(
  key: "save_data",
  value: json.to_string(save_data),
  on_success: SaveComplete,
  on_error: SaveFailed,
)

browser.load_from_storage(
  key: "save_data",
  on_success: SaveLoaded,  // Receives Option(String)
  on_error: LoadFailed,
)
```

## Effect timing

Understanding when effects execute is crucial:

```
Frame Timeline:
─────────────────────────────────────────────────────────────────────
  update()  update()  update()  view()  render  EFFECTS PROCESSED
     │         │         │                           │
     └─────────┴─────────┴───────────────────────────┘
          Effects collected here              Run here
```

All effects from all `update` calls in a frame are batched together and processed **after** rendering. This means:

1. **Dispatched messages queue for next frame**: `effect.dispatch(Msg)` doesn't run immediately.

2. **Multiple updates, one effect processing**: If three messages are processed this frame, their effects all run together at the end.

3. **Effects don't block updates**: Even slow effects won't delay your game logic.

## Custom effects

For game-specific side effects, use `effect.from`:

```gleam
fn log_event(event: String) -> effect.Effect(Msg) {
  effect.from(fn(_dispatch) {
    // Fire-and-forget logging
    io.println("[GAME] " <> event)
    // Don't dispatch anything
  })
}

fn fetch_leaderboard() -> effect.Effect(Msg) {
  effect.from(fn(dispatch) {
    // This would be actual HTTP in practice
    dispatch(LeaderboardLoading)

    // Simulate async fetch...
    // On completion:
    dispatch(LeaderboardLoaded(scores))
  })
}
```

The key insight: `effect.from` gives you an escape hatch to the imperative world when needed, while keeping the interface clean.

## Testing effects

Because effects are data, you can inspect them in tests:

```gleam
pub fn player_jump_dispatches_sound_test() {
  let model = Model(player: grounded_player())
  let msg = PlayerRequestedJump

  let #(_new_model, effect, _physics) = update(model, msg, mock_context())

  // You can pattern match on the effect to verify it
  let assert effect.Dispatch(PlayJumpSound) = effect
}
```

For complex effects, you might want to structure your code to return effect descriptions that are easy to verify.

## Best practices

### Keep effects small

Each effect should do one thing. Compose them with `batch` for complex behaviors:

```gleam
// Good: Small, focused effects
fn update(model, msg, ctx) {
  case msg {
    LevelComplete -> {
      #(model, effect.batch([
        effect.dispatch(SaveProgress),
        effect.dispatch(PlayVictorySound),
        effect.dispatch(ShowLevelCompleteUI),
        effect.delay(duration.seconds(2), LoadNextLevel),
      ]), None)
    }
  }
}
```

### Don't rely on effect order

While effects in a `batch` process in order, don't write code that depends on this. If order matters, use message chaining instead:

```gleam
// Instead of relying on batch order...
effect.batch([
  first_thing,   // Must complete before...
  second_thing,  // ...this runs
])

// ...use message chains:
case msg {
  DoFirstThing -> {
    // Do first thing, then trigger second
    #(model, effect.dispatch(DoSecondThing), None)
  }
  DoSecondThing -> {
    // Now we're sure first thing completed
    #(model, effect.none(), None)
  }
}
```

### Handle failure cases

Always provide error handlers for effects that can fail:

```gleam
// Good: Handles both outcomes
audio.load_audio(
  url: "sounds/music.mp3",
  on_success: MusicLoaded,
  on_error: MusicLoadFailed,  // Don't ignore this!
)
```

Your update function should handle both messages gracefully. Games should be resilient to missing assets.

## Next steps

You now understand how Tiramisu manages side effects while keeping your game logic pure and testable.

Next, learn about the [Scene Graph](04-scene-graph.md) to understand how Tiramisu renders your game world.
