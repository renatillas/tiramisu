//// <script>
//// const docs = [
////   {
////     header: "Creating effects",
////     functions: [
////       "none",
////       "from",
////       "from_promise",
////       "batch"
////     ]
////   },
////   {
////     header: "Transform effects",
////     functions: [
////       "map"
////     ]
////   },
////   {
////     header: "Timing",
////     functions: [
////       "tick",
////       "delay",
////       "interval",
////       "cancel_interval"
////     ]
////   },
////   {
////     header: "Scene effects",
////     functions: [
////       "set_background",
////       "tween"
////     ]
////   },
////   {
////     header: "Browser APIs",
////     functions: [
////       "request_fullscreen",
////       "exit_fullscreen",
////       "request_pointer_lock",
////       "exit_pointer_lock",
////       "vibrate",
////       "gamepad_vibrate",
////       "clipboard_write",
////       "clipboard_read"
////     ]
////   }
//// ]
////
//// const callback = () => {
////   const list = document.querySelector(".sidebar > ul:last-of-type")
////   const sortedLists = document.createDocumentFragment()
////   const sortedMembers = document.createDocumentFragment()
////
////   for (const section of docs) {
////     sortedLists.append((() => {
////       const node = document.createElement("h3")
////       node.append(section.header)
////       return node
////     })())
////     sortedMembers.append((() => {
////       const node = document.createElement("h2")
////       node.append(section.header)
////       return node
////     })())
////
////     const sortedList = document.createElement("ul")
////     sortedLists.append(sortedList)
////
////
////     for (const funcName of section.functions) {
////       const href = `#${funcName}`
////       const member = document.querySelector(
////         `.member:has(h2 > a[href="${href}"])`
////       )
////       const sidebar = list.querySelector(`li:has(a[href="${href}"])`)
////       sortedList.append(sidebar)
////       sortedMembers.append(member)
////     }
////   }
////
////   document.querySelector(".sidebar").insertBefore(sortedLists, list)
////   document
////     .querySelector(".module-members:has(#module-values)")
////     .insertBefore(
////       sortedMembers,
////       document.querySelector("#module-values").nextSibling
////     )
//// }
////
//// document.readyState !== "loading"
////   ? callback()
////   : document.addEventListener(
////     "DOMContentLoaded",
////     callback,
////     { once: true }
////   )
//// </script>
//// Effect system for managing side effects in Tiramisu.
////
//// Effects represent side effects as immutable data, following The Elm Architecture.
//// Your `update` function returns effects that the runtime executes for you.
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/effect
////
//// type Msg {
////   Tick
////   PlayerMoved(Vec3(Float))
//// }
////
//// fn update(model: Model, msg: Msg, ctx: Context) {
////   case msg {
////     Tick -> #(
////       update_physics(model),
////       effect.batch([
////         effect.tick(Tick),  // Request next frame
////         effect.from(fn(dispatch) {
////           // Custom side effect
////           log_position(model.player_pos)
////           dispatch(PlayerMoved(model.player_pos))
////         }),
////       ]),
////     )
////     PlayerMoved(_) -> #(model, effect.none())
////   }
//// }
//// ```

import gleam/javascript/promise.{type Promise}
import gleam/list
import plinth/browser/window
import tiramisu/background.{type Background}
import tiramisu/texture

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
    |> list.each(fn(effect) {
      let Effect(perform) = effect
      perform(dispatch)
    })
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
  Effect(perform: fn(dispatch) {
    let Effect(perform) = effect
    perform(fn(msg) { dispatch(f(msg)) })
  })
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
  let Effect(perform) = effect
  perform(dispatch)
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

/// Change the scene background dynamically.
///
/// Allows you to switch between color, texture, or cube texture backgrounds
/// at runtime from your update function.
///
/// ## Example
///
/// ```gleam
/// import tiramisu
/// import tiramisu/background
/// import tiramisu/effect
///
/// type Msg {
///   Tick
///   ChangeToNight
///   ChangeToDawn
/// }
///
/// fn update(model, msg, ctx) {
///   case msg {
///     ChangeToNight -> #(
///       model,
///       effect.set_background(background.Color(0x0a0a1e)),
///     )
///     ChangeToDawn -> #(
///       model,
///       effect.set_background(background.Texture("assets/dawn-sky.jpg")),
///     )
///     Tick -> #(model, effect.tick(Tick))
///   }
/// }
/// ```
pub fn set_background(background: Background) -> Effect(msg) {
  Effect(perform: fn(_dispatch) {
    let scene = get_game_scene_ffi()
    case background {
      background.Color(color) -> set_scene_background_color_ffi(scene, color)
      background.Texture(url) -> {
        // Load texture asynchronously
        load_texture_ffi(url)
        |> promise.tap(fn(texture) {
          set_scene_background_texture_ffi(scene, texture)
        })
        Nil
      }
      background.EquirectangularTexture(url) -> {
        // Load equirectangular texture asynchronously
        load_equirectangular_texture_ffi(url)
        |> promise.tap(fn(texture) {
          set_scene_background_texture_ffi(scene, texture)
        })
        Nil
      }
      background.CubeTexture(urls) -> {
        // Load cube texture asynchronously
        load_cube_texture_ffi(urls)
        |> promise.tap(fn(cube_texture) {
          set_scene_background_cube_texture_ffi(scene, cube_texture)
        })
        Nil
      }
    }
  })
}

type GameScene

// FFI bindings for background setting
@external(javascript, "../threejs.ffi.mjs", "getGameScene")
fn get_game_scene_ffi() -> GameScene

@external(javascript, "../threejs.ffi.mjs", "setSceneBackgroundColor")
fn set_scene_background_color_ffi(scene: GameScene, color: Int) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setSceneBackgroundTexture")
fn set_scene_background_texture_ffi(
  scene: GameScene,
  texture: texture.Texture,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setSceneBackgroundCubeTexture")
fn set_scene_background_cube_texture_ffi(
  scene: GameScene,
  cube_texture: texture.Texture,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "loadTexture")
fn load_texture_ffi(url: String) -> Promise(texture.Texture)

@external(javascript, "../threejs.ffi.mjs", "loadEquirectangularTexture")
fn load_equirectangular_texture_ffi(url: String) -> Promise(texture.Texture)

@external(javascript, "../threejs.ffi.mjs", "loadCubeTexture")
fn load_cube_texture_ffi(urls: List(String)) -> Promise(texture.Texture)

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
    delay_ffi(milliseconds, fn() { dispatch(msg) })
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
///   Model(spawn_interval: option.Option(Int))
/// }
///
/// type Msg {
///   StartSpawning
///   IntervalCreated(Int)
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
  on_created on_created: fn(Int) -> msg,
) -> Effect(msg) {
  Effect(perform: fn(dispatch) {
    let id = interval_ffi(milliseconds, fn() { dispatch(msg) })
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
pub fn cancel_interval(id: Int) -> Effect(msg) {
  Effect(perform: fn(_dispatch) {
    cancel_interval_ffi(id)
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
///         on_error: FullscreenFailed,
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
    request_fullscreen_ffi()
    |> promise.map(fn(result) {
      case result {
        Ok(_) -> dispatch(on_success)
        Error(_) -> dispatch(on_error)
      }
    })
    |> promise.tap(fn(_) { Nil })
    Nil
  })
}

/// Exit fullscreen mode.
///
/// ## Example
///
/// ```gleam
/// effect.exit_fullscreen()
/// ```
pub fn exit_fullscreen() -> Effect(msg) {
  Effect(perform: fn(_dispatch) {
    exit_fullscreen_ffi()
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
    request_pointer_lock_ffi()
    |> promise.map(fn(result) {
      case result {
        Ok(_) -> dispatch(on_success)
        Error(_) -> dispatch(on_error)
      }
    })
    |> promise.tap(fn(_) { Nil })
    Nil
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
    exit_pointer_lock_ffi()
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
  Effect(perform: fn(_dispatch) {
    vibrate_ffi(pattern)
    Nil
  })
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
///         duration_ms: 500,
///       ),
///     )
///     _ -> #(model, effect.none())
///   }
/// }
/// ```
pub fn gamepad_vibrate(
  gamepad gamepad: Int,
  intensity intensity: Float,
  duration_ms duration_ms: Int,
) -> Effect(msg) {
  Effect(perform: fn(_dispatch) {
    gamepad_vibrate_ffi(gamepad, intensity, duration_ms)
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
    clipboard_write_ffi(text)
    |> promise.map(fn(result) {
      case result {
        Ok(_) -> dispatch(on_success)
        Error(_) -> dispatch(on_error)
      }
    })
    |> promise.tap(fn(_) { Nil })
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
    clipboard_read_ffi()
    |> promise.map(fn(result) {
      case result {
        Ok(text) -> dispatch(on_success(text))
        Error(_) -> dispatch(on_error)
      }
    })
    |> promise.tap(fn(_) { Nil })
    Nil
  })
}

// ============================================================================
// FFI BINDINGS
// ============================================================================

// Time & Animation FFI
@external(javascript, "../tiramisu.ffi.mjs", "delay")
fn delay_ffi(milliseconds: Int, callback: fn() -> Nil) -> Nil

@external(javascript, "../tiramisu.ffi.mjs", "interval")
fn interval_ffi(milliseconds: Int, callback: fn() -> Nil) -> Int

@external(javascript, "../tiramisu.ffi.mjs", "cancelInterval")
fn cancel_interval_ffi(id: Int) -> Nil

// System & Browser FFI
@external(javascript, "../tiramisu.ffi.mjs", "requestFullscreen")
fn request_fullscreen_ffi() -> Promise(Result(Nil, String))

@external(javascript, "../tiramisu.ffi.mjs", "exitFullscreen")
fn exit_fullscreen_ffi() -> Nil

@external(javascript, "../tiramisu.ffi.mjs", "requestPointerLock")
fn request_pointer_lock_ffi() -> Promise(Result(Nil, String))

@external(javascript, "../tiramisu.ffi.mjs", "exitPointerLock")
fn exit_pointer_lock_ffi() -> Nil

@external(javascript, "../tiramisu.ffi.mjs", "vibrate")
fn vibrate_ffi(pattern: List(Int)) -> Nil

@external(javascript, "../tiramisu.ffi.mjs", "gamepadVibrate")
fn gamepad_vibrate_ffi(gamepad: Int, intensity: Float, duration_ms: Int) -> Nil

@external(javascript, "../tiramisu.ffi.mjs", "clipboardWrite")
fn clipboard_write_ffi(text: String) -> Promise(Result(Nil, String))

@external(javascript, "../tiramisu.ffi.mjs", "clipboardRead")
fn clipboard_read_ffi() -> Promise(Result(String, String))
