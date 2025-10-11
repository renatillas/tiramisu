import * as $promise from "../../gleam_javascript/gleam/javascript/promise.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $window from "../../plinth/plinth/browser/window.mjs";
import { Ok, CustomType as $CustomType } from "../gleam.mjs";
import {
  getGameScene as get_game_scene_ffi,
  setSceneBackgroundColor as set_scene_background_color_ffi,
  setSceneBackgroundTexture as set_scene_background_texture_ffi,
  setSceneBackgroundCubeTexture as set_scene_background_cube_texture_ffi,
  loadTexture as load_texture_ffi,
  loadCubeTexture as load_cube_texture_ffi,
} from "../threejs.ffi.mjs";
import {
  delay as delay_ffi,
  interval as interval_ffi,
  cancelInterval as cancel_interval_ffi,
  tween as tween_ffi,
  requestFullscreen as request_fullscreen_ffi,
  exitFullscreen as exit_fullscreen_ffi,
  requestPointerLock as request_pointer_lock_ffi,
  exitPointerLock as exit_pointer_lock_ffi,
  vibrate as vibrate_ffi,
  gamepadVibrate as gamepad_vibrate_ffi,
  clipboardWrite as clipboard_write_ffi,
  clipboardRead as clipboard_read_ffi,
} from "../tiramisu.ffi.mjs";
import * as $background from "../tiramisu/background.mjs";

class Effect extends $CustomType {
  constructor(perform) {
    super();
    this.perform = perform;
  }
}

export class Linear extends $CustomType {}

export class EaseInQuad extends $CustomType {}

export class EaseOutQuad extends $CustomType {}

export class EaseInOutQuad extends $CustomType {}

export class EaseInCubic extends $CustomType {}

export class EaseOutCubic extends $CustomType {}

export class EaseInOutCubic extends $CustomType {}

/**
 * Create an effect that performs no side effects.
 *
 * Use when you want to update state without triggering any effects.
 *
 * ## Example
 *
 * ```gleam
 * fn update(model, msg, ctx) {
 *   case msg {
 *     Idle -> #(model, effect.none())
 *   }
 * }
 * ```
 */
export function none() {
  return new Effect((_) => { return undefined; });
}

/**
 * Create a custom effect from a function.
 *
 * The function receives a `dispatch` callback to send messages back to your `update` function.
 *
 * ## Example
 *
 * ```gleam
 * effect.from(fn(dispatch) {
 *   log("Player score: " <> int.to_string(score))
 *   dispatch(ScoreLogged)
 * })
 * ```
 */
export function from(effect) {
  return new Effect(effect);
}

/**
 * Batch multiple effects to run them together.
 *
 * All effects execute in order during the same frame.
 *
 * ## Example
 *
 * ```gleam
 * effect.batch([
 *   effect.tick(NextFrame),
 *   play_sound_effect("jump.wav"),
 *   update_scoreboard(score),
 * ])
 * ```
 */
export function batch(effects) {
  return new Effect(
    (dispatch) => {
      let _pipe = effects;
      return $list.each(
        _pipe,
        (effect) => {
          let perform;
          perform = effect.perform;
          return perform(dispatch);
        },
      );
    },
  );
}

/**
 * Map effect messages to a different type.
 *
 * Useful when composing effects from subcomponents.
 *
 * ## Example
 *
 * ```gleam
 * let player_effect = player.update(player_model, player_msg)
 * effect.map(player_effect, PlayerMsg)
 * ```
 */
export function map(effect, f) {
  return new Effect(
    (dispatch) => {
      let perform;
      perform = effect.perform;
      return perform((msg) => { return dispatch(f(msg)); });
    },
  );
}

/**
 * Create an effect from a JavaScript Promise.
 *
 * When the promise resolves, it dispatches the resulting message.
 *
 * ## Example
 *
 * ```gleam
 * let fetch_promise = fetch_data()
 * effect.from_promise(promise.map(fetch_promise, DataLoaded))
 * ```
 */
export function from_promise(p) {
  return new Effect(
    (dispatch) => {
      $promise.tap(p, dispatch);
      return undefined;
    },
  );
}

export function run(effect, dispatch) {
  let perform;
  perform = effect.perform;
  return perform(dispatch);
}

/**
 * Request the next animation frame and dispatch a message.
 *
 * This is the primary way to create frame-based game loops. Call this in your
 * `update` function to receive a message on the next frame.
 *
 * ## Example
 *
 * ```gleam
 * type Msg {
 *   Tick
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     Tick -> #(
 *       Model(..model, time: model.time +. ctx.delta_time),
 *       effect.tick(Tick),  // Request next frame
 *     )
 *   }
 * }
 * ```
 */
export function tick(msg) {
  return new Effect(
    (dispatch) => {
      $window.request_animation_frame((_) => { return dispatch(msg); });
      return undefined;
    },
  );
}

/**
 * Change the scene background dynamically.
 *
 * Allows you to switch between color, texture, or cube texture backgrounds
 * at runtime from your update function.
 *
 * ## Example
 *
 * ```gleam
 * import tiramisu
 * import tiramisu/effect
 *
 * type Msg {
 *   Tick
 *   ChangeToNight
 *   ChangeToDawn
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     ChangeToNight -> #(
 *       model,
 *       effect.set_background(tiramisu.Color(0x0a0a1e)),
 *     )
 *     ChangeToDawn -> #(
 *       model,
 *       effect.set_background(tiramisu.Texture("assets/dawn-sky.jpg")),
 *     )
 *     Tick -> #(model, effect.tick(Tick))
 *   }
 * }
 * ```
 */
export function set_background(background) {
  return new Effect(
    (_) => {
      let scene = get_game_scene_ffi();
      if (background instanceof $background.Color) {
        let color = background[0];
        return set_scene_background_color_ffi(scene, color);
      } else if (background instanceof $background.Texture) {
        let url = background[0];
        let _pipe = load_texture_ffi(url);
        $promise.tap(
          _pipe,
          (texture) => {
            return set_scene_background_texture_ffi(scene, texture);
          },
        )
        return undefined;
      } else {
        let urls = background[0];
        let _pipe = load_cube_texture_ffi(urls);
        $promise.tap(
          _pipe,
          (cube_texture) => {
            return set_scene_background_cube_texture_ffi(scene, cube_texture);
          },
        )
        return undefined;
      }
    },
  );
}

function easing_to_string(easing) {
  if (easing instanceof Linear) {
    return "linear";
  } else if (easing instanceof EaseInQuad) {
    return "easeInQuad";
  } else if (easing instanceof EaseOutQuad) {
    return "easeOutQuad";
  } else if (easing instanceof EaseInOutQuad) {
    return "easeInOutQuad";
  } else if (easing instanceof EaseInCubic) {
    return "easeInCubic";
  } else if (easing instanceof EaseOutCubic) {
    return "easeOutCubic";
  } else {
    return "easeInOutCubic";
  }
}

/**
 * Delay dispatching a message by a specified duration.
 *
 * Unlike `tick`, which waits for the next animation frame, `delay` waits
 * for a specific number of milliseconds using `setTimeout`.
 *
 * ## Example
 *
 * ```gleam
 * type Msg {
 *   PlayerHit
 *   ShowDamageEffect
 *   HideDamageEffect
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     PlayerHit -> #(
 *       Model(..model, health: model.health - 10),
 *       effect.batch([
 *         effect.from(fn(_) { show_damage_animation() }),
 *         effect.delay(500, HideDamageEffect),  // Hide after 500ms
 *       ]),
 *     )
 *     HideDamageEffect -> #(model, effect.none())
 *     _ -> #(model, effect.none())
 *   }
 * }
 * ```
 */
export function delay(milliseconds, msg) {
  return new Effect(
    (dispatch) => {
      delay_ffi(milliseconds, () => { return dispatch(msg); });
      return undefined;
    },
  );
}

/**
 * Create a recurring interval that dispatches a message periodically.
 *
 * Immediately dispatches `on_created` with the interval ID, which you should
 * store in your model. Use this ID with `cancel_interval` to stop it later.
 *
 * **No global state** - the interval ID is managed by JavaScript's `setInterval`
 * and you store it in your model.
 *
 * ## Example
 *
 * ```gleam
 * type Model {
 *   Model(spawn_interval: option.Option(Int))
 * }
 *
 * type Msg {
 *   StartSpawning
 *   IntervalCreated(Int)
 *   SpawnEnemy
 *   StopSpawning
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     StartSpawning -> #(
 *       model,
 *       effect.interval(
 *         ms: 2000,
 *         msg: SpawnEnemy,
 *         on_created: IntervalCreated,
 *       ),
 *     )
 *     IntervalCreated(id) -> #(
 *       Model(..model, spawn_interval: option.Some(id)),
 *       effect.none(),
 *     )
 *     SpawnEnemy -> #(spawn_enemy(model), effect.none())
 *     StopSpawning ->
 *       case model.spawn_interval {
 *         option.Some(id) -> #(
 *           Model(..model, spawn_interval: option.None),
 *           effect.cancel_interval(id),
 *         )
 *         option.None -> #(model, effect.none())
 *       }
 *   }
 * }
 * ```
 */
export function interval(milliseconds, msg, on_created) {
  return new Effect(
    (dispatch) => {
      let id = interval_ffi(milliseconds, () => { return dispatch(msg); });
      dispatch(on_created(id));
      return undefined;
    },
  );
}

/**
 * Cancel a recurring interval by its ID.
 *
 * Pass the interval ID that was dispatched via `on_created` when you created the interval.
 *
 * ## Example
 *
 * ```gleam
 * case model.spawn_interval {
 *   option.Some(id) -> effect.cancel_interval(id)
 *   option.None -> effect.none()
 * }
 * ```
 */
export function cancel_interval(id) {
  return new Effect(
    (_) => {
      cancel_interval_ffi(id);
      return undefined;
    },
  );
}

/**
 * Animate a value from start to end over a duration with easing.
 *
 * Dispatches `on_update` messages with interpolated values each frame,
 * and `on_complete` when the animation finishes.
 *
 * ## Example
 *
 * ```gleam
 * type Msg {
 *   FadeIn
 *   UpdateOpacity(Float)
 *   FadeComplete
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     FadeIn -> #(
 *       model,
 *       effect.tween(
 *         from: 0.0,
 *         to: 1.0,
 *         duration_ms: 1000,
 *         easing: effect.EaseInOutQuad,
 *         on_update: UpdateOpacity,
 *         on_complete: FadeComplete,
 *       ),
 *     )
 *     UpdateOpacity(opacity) -> #(
 *       Model(..model, opacity: opacity),
 *       effect.none(),
 *     )
 *     FadeComplete -> #(model, effect.none())
 *     _ -> #(model, effect.none())
 *   }
 * }
 * ```
 */
export function tween(start, end, duration_ms, easing, on_update, on_complete) {
  return new Effect(
    (dispatch) => {
      tween_ffi(
        start,
        end,
        duration_ms,
        easing_to_string(easing),
        (value) => { return dispatch(on_update(value)); },
        () => { return dispatch(on_complete); },
      );
      return undefined;
    },
  );
}

/**
 * Request fullscreen mode for the game canvas.
 *
 * This must be called in response to a user interaction (click, key press, etc.)
 * due to browser security restrictions.
 *
 * ## Example
 *
 * ```gleam
 * type Msg {
 *   FullscreenButtonClicked
 *   FullscreenEntered
 *   FullscreenFailed
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     FullscreenButtonClicked -> #(
 *       model,
 *       effect.request_fullscreen(
 *         on_success: FullscreenEntered,
 *         on_error: FullscreenFailed,
 *       ),
 *     )
 *     _ -> #(model, effect.none())
 *   }
 * }
 * ```
 */
export function request_fullscreen(on_success, on_error) {
  return new Effect(
    (dispatch) => {
      let _pipe = request_fullscreen_ffi();
      let _pipe$1 = $promise.map(
        _pipe,
        (result) => {
          if (result instanceof Ok) {
            return dispatch(on_success);
          } else {
            return dispatch(on_error);
          }
        },
      );
      $promise.tap(_pipe$1, (_) => { return undefined; })
      return undefined;
    },
  );
}

/**
 * Exit fullscreen mode.
 *
 * ## Example
 *
 * ```gleam
 * effect.exit_fullscreen()
 * ```
 */
export function exit_fullscreen() {
  return new Effect(
    (_) => {
      exit_fullscreen_ffi();
      return undefined;
    },
  );
}

/**
 * Request pointer lock for the game canvas.
 *
 * This hides the cursor and provides unlimited mouse movement,
 * commonly used in first-person games. Must be called in response
 * to user interaction.
 *
 * ## Example
 *
 * ```gleam
 * type Msg {
 *   StartFPSMode
 *   PointerLocked
 *   PointerLockFailed
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     StartFPSMode -> #(
 *       model,
 *       effect.request_pointer_lock(
 *         on_success: PointerLocked,
 *         on_error: PointerLockFailed,
 *       ),
 *     )
 *     _ -> #(model, effect.none())
 *   }
 * }
 * ```
 */
export function request_pointer_lock(on_success, on_error) {
  return new Effect(
    (dispatch) => {
      let _pipe = request_pointer_lock_ffi();
      let _pipe$1 = $promise.map(
        _pipe,
        (result) => {
          if (result instanceof Ok) {
            return dispatch(on_success);
          } else {
            return dispatch(on_error);
          }
        },
      );
      $promise.tap(_pipe$1, (_) => { return undefined; })
      return undefined;
    },
  );
}

/**
 * Exit pointer lock mode.
 *
 * ## Example
 *
 * ```gleam
 * effect.exit_pointer_lock()
 * ```
 */
export function exit_pointer_lock() {
  return new Effect(
    (_) => {
      exit_pointer_lock_ffi();
      return undefined;
    },
  );
}

/**
 * Trigger haptic feedback on mobile devices.
 *
 * The pattern is a list of vibration durations in milliseconds.
 * For example, `[200, 100, 200]` vibrates for 200ms, pauses 100ms, then vibrates 200ms.
 *
 * ## Example
 *
 * ```gleam
 * type Msg {
 *   PlayerHit
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     PlayerHit -> #(
 *       Model(..model, health: model.health - 10),
 *       effect.vibrate([100, 50, 100]),  // Quick buzz pattern
 *     )
 *     _ -> #(model, effect.none())
 *   }
 * }
 * ```
 */
export function vibrate(pattern) {
  return new Effect(
    (_) => {
      vibrate_ffi(pattern);
      return undefined;
    },
  );
}

/**
 * Trigger haptic feedback on a gamepad.
 *
 * Intensity ranges from 0.0 (no vibration) to 1.0 (maximum vibration).
 * Duration is in milliseconds.
 *
 * ## Example
 *
 * ```gleam
 * type Msg {
 *   ExplosionNearPlayer
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     ExplosionNearPlayer -> #(
 *       model,
 *       effect.gamepad_vibrate(
 *         gamepad: 0,
 *         intensity: 0.7,
 *         duration_ms: 500,
 *       ),
 *     )
 *     _ -> #(model, effect.none())
 *   }
 * }
 * ```
 */
export function gamepad_vibrate(gamepad, intensity, duration_ms) {
  return new Effect(
    (_) => {
      gamepad_vibrate_ffi(gamepad, intensity, duration_ms);
      return undefined;
    },
  );
}

/**
 * Write text to the system clipboard.
 *
 * Must be called in response to user interaction due to browser security.
 *
 * ## Example
 *
 * ```gleam
 * type Msg {
 *   CopyScoreClicked
 *   CopiedToClipboard
 *   CopyFailed
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     CopyScoreClicked -> #(
 *       model,
 *       effect.clipboard_write(
 *         text: "High Score: " <> int.to_string(model.high_score),
 *         on_success: CopiedToClipboard,
 *         on_error: CopyFailed,
 *       ),
 *     )
 *     _ -> #(model, effect.none())
 *   }
 * }
 * ```
 */
export function clipboard_write(text, on_success, on_error) {
  return new Effect(
    (dispatch) => {
      let _pipe = clipboard_write_ffi(text);
      let _pipe$1 = $promise.map(
        _pipe,
        (result) => {
          if (result instanceof Ok) {
            return dispatch(on_success);
          } else {
            return dispatch(on_error);
          }
        },
      );
      $promise.tap(_pipe$1, (_) => { return undefined; })
      return undefined;
    },
  );
}

/**
 * Read text from the system clipboard.
 *
 * Must be called in response to user interaction due to browser security.
 *
 * ## Example
 *
 * ```gleam
 * type Msg {
 *   PasteClicked
 *   ClipboardData(String)
 *   PasteFailed
 * }
 *
 * fn update(model, msg, ctx) {
 *   case msg {
 *     PasteClicked -> #(
 *       model,
 *       effect.clipboard_read(
 *         on_success: ClipboardData,
 *         on_error: PasteFailed,
 *       ),
 *     )
 *     ClipboardData(text) -> {
 *       // Process clipboard text
 *       #(model, effect.none())
 *     }
 *     _ -> #(model, effect.none())
 *   }
 * }
 * ```
 */
export function clipboard_read(on_success, on_error) {
  return new Effect(
    (dispatch) => {
      let _pipe = clipboard_read_ffi();
      let _pipe$1 = $promise.map(
        _pipe,
        (result) => {
          if (result instanceof Ok) {
            let text = result[0];
            return dispatch(on_success(text));
          } else {
            return dispatch(on_error);
          }
        },
      );
      $promise.tap(_pipe$1, (_) => { return undefined; })
      return undefined;
    },
  );
}
