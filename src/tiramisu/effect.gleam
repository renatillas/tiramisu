import gleam/javascript/array
import gleam/javascript/promise.{type Promise}
import gleam/list
import gleam/time/duration
import plinth/browser/document
import plinth/browser/window
import tiramisu/browser
import tiramisu/internal/timer

/// Opaque effect type that can dispatch messages back to the application.
///
/// Effects are data descriptions of side effects to perform. The runtime
/// executes them after your `update` function returns.
pub opaque type Effect(msg) {
  Effect(perform: fn(fn(msg) -> Nil) -> Nil)
}

/// Create an effect that performs no side effects.
///
/// Use when you want to update state without triggering any effects.
///
/// ## Example
///
/// ```gleam
/// fn update(model, msg, ctx) {
///   case msg {
///     Idle -> #(model, effect.none())
///   }
/// }
/// ```
pub fn none() -> Effect(msg) {
  Effect(perform: fn(_dispatch) { Nil })
}

/// Create a custom effect from a function.
///
/// The function receives a `dispatch` callback to send messages back to your `update` function.
///
/// ## Example
///
/// ```gleam
/// effect.from(fn(dispatch) {
///   log("Player score: " <> int.to_string(score))
///   dispatch(ScoreLogged)
/// })
/// ```
pub fn from(effect: fn(fn(msg) -> Nil) -> Nil) -> Effect(msg) {
  Effect(perform: effect)
}

/// Batch multiple effects to run them together.
///
/// All effects execute in order during the same frame.
///
/// ## Example
///
/// ```gleam
/// effect.batch([
///   effect.tick(NextFrame),
///   play_sound_effect("jump.wav"),
///   update_scoreboard(score),
/// ])
/// ```
pub fn batch(effects: List(Effect(msg))) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    effects
    |> list.each(fn(effect) { effect.perform(dispatch) })
  })
}

/// Map effect messages to a different type.
///
/// Useful when composing effects from subcomponents.
///
/// ## Example
///
/// ```gleam
/// let player_effect = player.update(player_model, player_msg)
/// effect.map(player_effect, PlayerMsg)
/// ```
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b) {
  Effect(perform: fn(dispatch) { effect.perform(fn(msg) { dispatch(f(msg)) }) })
}

/// Create an effect from a JavaScript Promise.
///
/// When the promise resolves, it dispatches the resulting message.
///
/// ## Example
///
/// ```gleam
/// let fetch_promise = fetch_data()
/// effect.from_promise(promise.map(fetch_promise, DataLoaded))
/// ```
pub fn from_promise(p: Promise(msg)) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    promise.tap(p, dispatch)
    Nil
  })
}

@internal
pub fn run(effect: Effect(msg), dispatch: fn(msg) -> Nil) -> Nil {
  effect.perform(dispatch)
}

/// Request the next animation frame and dispatch a message.
///
/// This is the primary way to create frame-based game loops. Call this in your
/// `update` function to receive a message on the next frame.
///
/// ## Example
///
/// ```gleam
/// type Msg {
///   Tick
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     Tick -> #(
///       Model(..model, time: model.time +. ctx.delta_time),
///       effect.tick(Tick),  // Request next frame
///     )
///   }
/// }
/// ```
pub fn tick(msg: msg) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    window.request_animation_frame(fn(_timestamp) { dispatch(msg) })
    Nil
  })
}

/// Delay dispatching a message by a specified duration.
///
/// Unlike `tick`, which waits for the next animation frame, `delay` waits
/// for a specific number of milliseconds using `setTimeout`.
///
/// ## Example
///
/// ```gleam
/// type Msg {
///   PlayerHit
///   ShowDamageEffect
///   HideDamageEffect
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     PlayerHit -> #(
///       Model(..model, health: model.health - 10),
///       effect.batch([
///         effect.from(fn(_) { show_damage_animation() }),
///         effect.delay(500, HideDamageEffect),  // Hide after 500ms
///       ]),
///     )
///     HideDamageEffect -> #(model, effect.none())
///     _ -> #(model, effect.none())
///   }
/// }
/// ```
pub fn delay(ms milliseconds: Int, msg msg: msg) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    timer.delay(milliseconds, fn() { dispatch(msg) })
    Nil
  })
}

/// Create a recurring interval that dispatches a message periodically.
///
/// Immediately dispatches `on_created` with the interval ID, which you should
/// store in your model. Use this ID with `cancel_interval` to stop it later.
///
/// **No global state** - the interval ID is managed by JavaScript's `setInterval`
/// and you store it in your model.
///
/// ## Example
///
/// ```gleam
/// type Model {
///   Model(spawn_interval: option.Option(timer.TimerId))
/// }
///
/// type Msg {
///   StartSpawning
///   IntervalCreated(timer.TimerId)
///   SpawnEnemy
///   StopSpawning
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     StartSpawning -> #(
///       model,
///       effect.interval(
///         ms: 2000,
///         msg: SpawnEnemy,
///         on_created: IntervalCreated,
///       ),
///     )
///     IntervalCreated(id) -> #(
///       Model(..model, spawn_interval: option.Some(id)),
///       effect.none(),
///     )
///     SpawnEnemy -> #(spawn_enemy(model), effect.none())
///     StopSpawning ->
///       case model.spawn_interval {
///         option.Some(id) -> #(
///           Model(..model, spawn_interval: option.None),
///           effect.cancel_interval(id),
///         )
///         option.None -> #(model, effect.none())
///       }
///   }
/// }
/// ```
pub fn interval(
  ms milliseconds: Int,
  msg msg: msg,
  on_created on_created: fn(timer.TimerId) -> msg,
) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    let id = timer.interval(milliseconds, fn() { dispatch(msg) })
    dispatch(on_created(id))
    Nil
  })
}

/// Cancel a recurring interval by its ID.
///
/// Pass the interval ID that was dispatched via `on_created` when you created the interval.
///
/// ## Example
///
/// ```gleam
/// case model.spawn_interval {
///   option.Some(id) -> effect.cancel_interval(id)
///   option.None -> effect.none()
/// }
/// ```
pub fn cancel_interval(id: timer.TimerId) -> Effect(msg) {
  Effect(perform: fn(_dispatch) {
    timer.cancel_interval(id)
    Nil
  })
}

// ============================================================================
// SYSTEM & BROWSER EFFECTS
// ============================================================================

/// Request fullscreen mode for the game canvas.
///
/// This must be called in response to a user interaction (click, key press, etc.)
/// due to browser security restrictions.
///
/// ## Example
///
/// ```gleam
/// type Msg {
///   FullscreenButtonClicked
///   FullscreenEntered
///   FullscreenFailed
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     FullscreenButtonClicked -> #(
///       model,
///       effect.request_fullscreen(
///         on_success: FullscreenEntered,
///         on_error: FullscreenEnteredFailed,
///       ),
///     )
///     _ -> #(model, effect.none())
///   }
/// }
/// ```
pub fn request_fullscreen(
  on_success on_success: msg,
  on_error on_error: msg,
) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    case document.query_selector("canvas") {
      Error(_) -> dispatch(on_error)
      Ok(canvas) -> {
        browser.request_fullscreen(canvas)
        |> promise.map(fn(result) {
          case result {
            Ok(_) -> dispatch(on_success)
            Error(_) -> dispatch(on_error)
          }
        })
        Nil
      }
    }
  })
}

/// Exit fullscreen mode.
///
/// ## Example
///
/// ```gleam
/// effect.exit_fullscreen(
///   on_success: FullScreenExited,
///   on_error: FullScreenExitedFailed
/// )
/// ```
pub fn exit_fullscreen(
  on_success on_success: msg,
  on_error on_error: msg,
) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    browser.exit_fullscreen()
    |> promise.map(fn(result) {
      case result {
        Ok(_) -> dispatch(on_success)
        Error(_) -> dispatch(on_error)
      }
    })
    Nil
  })
}

/// Request pointer lock for the game canvas.
///
/// This hides the cursor and provides unlimited mouse movement,
/// commonly used in first-person games. Must be called in response
/// to user interaction.
///
/// ## Example
///
/// ```gleam
/// type Msg {
///   StartFPSMode
///   PointerLocked
///   PointerLockFailed
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     StartFPSMode -> #(
///       model,
///       effect.request_pointer_lock(
///         on_success: PointerLocked,
///         on_error: PointerLockFailed,
///       ),
///     )
///     _ -> #(model, effect.none())
///   }
/// }
/// ```
pub fn request_pointer_lock(
  on_success on_success: msg,
  on_error on_error: msg,
) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    case document.query_selector("canvas") {
      Error(_) -> dispatch(on_error)
      Ok(canvas) -> {
        browser.request_pointer_lock(canvas)
        |> promise.map(fn(result) {
          case result {
            Ok(_) -> dispatch(on_success)
            Error(_) -> dispatch(on_error)
          }
        })
        Nil
      }
    }
  })
}

/// Exit pointer lock mode.
///
/// ## Example
///
/// ```gleam
/// effect.exit_pointer_lock()
/// ```
pub fn exit_pointer_lock() -> Effect(msg) {
  Effect(perform: fn(_dispatch) {
    browser.exit_pointer_lock()
    Nil
  })
}

/// Trigger haptic feedback on mobile devices.
///
/// The pattern is a list of vibration durations in milliseconds.
/// For example, `[200, 100, 200]` vibrates for 200ms, pauses 100ms, then vibrates 200ms.
///
/// ## Example
///
/// ```gleam
/// type Msg {
///   PlayerHit
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     PlayerHit -> #(
///       Model(..model, health: model.health - 10),
///       effect.vibrate([100, 50, 100]),  // Quick buzz pattern
///     )
///     _ -> #(model, effect.none())
///   }
/// }
/// ```
pub fn vibrate(pattern: List(Int)) -> Effect(msg) {
  Effect(perform: fn(_dispatch) { browser.vibrate(array.from_list(pattern)) })
}

/// Trigger haptic feedback on a gamepad.
///
/// Intensity ranges from 0.0 (no vibration) to 1.0 (maximum vibration).
/// Duration is in milliseconds.
///
/// ## Example
///
/// ```gleam
/// type Msg {
///   ExplosionNearPlayer
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     ExplosionNearPlayer -> #(
///       model,
///       effect.gamepad_vibrate(
///         gamepad: 0,
///         intensity: 0.7,
///         duration: duration.milliseconds(500),
///       ),
///     )
///     _ -> #(model, effect.none())
///   }
/// }
/// ```
pub fn gamepad_vibrate(
  gamepad gamepad: Int,
  intensity intensity: Float,
  duration duration: duration.Duration,
) -> Effect(msg) {
  let #(duration_seconds, duration_nanoseconds) =
    duration.to_seconds_and_nanoseconds(duration)
  let duration_milliseconds =
    duration_seconds * 1000 + duration_nanoseconds / 1_000_000
  Effect(perform: fn(_dispatch) {
    browser.gamepad_vibrate(gamepad, intensity, duration_milliseconds)
    Nil
  })
}

/// Write text to the system clipboard.
///
/// Must be called in response to user interaction due to browser security.
///
/// ## Example
///
/// ```gleam
/// type Msg {
///   CopyScoreClicked
///   CopiedToClipboard
///   CopyFailed
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     CopyScoreClicked -> #(
///       model,
///       effect.clipboard_write(
///         text: "High Score: " <> int.to_string(model.high_score),
///         on_success: CopiedToClipboard,
///         on_error: CopyFailed,
///       ),
///     )
///     _ -> #(model, effect.none())
///   }
/// }
/// ```
pub fn clipboard_write(
  text text: String,
  on_success on_success: msg,
  on_error on_error: msg,
) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    browser.clipboard_write(text)
    |> promise.map(fn(result) {
      case result {
        Ok(_) -> dispatch(on_success)
        Error(_) -> dispatch(on_error)
      }
    })
    Nil
  })
}

/// Read text from the system clipboard.
///
/// Must be called in response to user interaction due to browser security.
///
/// ## Example
///
/// ```gleam
/// type Msg {
///   PasteClicked
///   ClipboardData(String)
///   PasteFailed
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     PasteClicked -> #(
///       model,
///       effect.clipboard_read(
///         on_success: ClipboardData,
///         on_error: PasteFailed,
///       ),
///     )
///     ClipboardData(text) -> {
///       // Process clipboard text
///       #(model, effect.none())
///     }
///     _ -> #(model, effect.none())
///   }
/// }
/// ```
pub fn clipboard_read(
  on_success on_success: fn(String) -> msg,
  on_error on_error: msg,
) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    browser.clipboard_read()
    |> promise.map(fn(result) {
      case result {
        Ok(text) -> dispatch(on_success(text))
        Error(_) -> dispatch(on_error)
      }
    })
    Nil
  })
}
