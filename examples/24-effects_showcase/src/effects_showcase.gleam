/// Effects Showcase Example
///
/// Demonstrates Time & Animation and System & Browser effects
///
/// Controls:
/// - Space: Start interval (cube color pulses)
/// - S: Stop interval
/// - 1-7: Start tween animations with different easings
/// - T: Trigger 3-second timeout
/// - F: Toggle fullscreen
/// - L: Toggle pointer lock
/// - Touch screen: Trigger vibration (mobile only)
/// - C: Copy to clipboard
/// - P: Paste from clipboard
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import tiramisu
import tiramisu/background
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub fn main() {
  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x1a1a2e),
    init: init,
    update: update,
    view: view,
  )
}

pub type Model {
  Model(
    // Cube animation
    rotation: Float,
    cube_scale: Float,
    cube_color: Int,
    // Tween animation
    tween_value: Float,
    tween_running: Bool,
    current_easing: effect.Easing,
    // Interval tracking (stores interval ID from JavaScript)
    interval_id: option.Option(Int),
    interval_count: Int,
    // Status messages
    status_message: String,
    // Browser state
    is_fullscreen: Bool,
    is_pointer_locked: Bool,
    clipboard_text: String,
    // Key press tracking (for single-press actions)
    last_keys: KeyPressState,
  )
}

pub type KeyPressState {
  KeyPressState(
    space: Bool,
    s: Bool,
    digit1: Bool,
    digit2: Bool,
    digit3: Bool,
    digit4: Bool,
    digit5: Bool,
    digit6: Bool,
    digit7: Bool,
    t: Bool,
    f: Bool,
    l: Bool,
    c: Bool,
    p: Bool,
    touch_screen_pressed: Bool,
  )
}

pub type Msg {
  Tick
  // Time & Animation Effects
  DelayedMessage
  TimeoutTriggered
  IntervalCreated(Int)
  IntervalTick
  TweenUpdate(Float)
  TweenComplete
  // System & Browser Effects
  FullscreenSuccess
  FullscreenError
  PointerLockSuccess
  PointerLockError
  ClipboardWriteSuccess
  ClipboardWriteError
  ClipboardRead(String)
  ClipboardReadError
}

fn init(
  _ctx: tiramisu.Context(String),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  let model =
    Model(
      rotation: 0.0,
      cube_scale: 1.0,
      cube_color: 0x3498db,
      tween_value: 0.0,
      tween_running: False,
      current_easing: effect.Linear,
      interval_id: option.None,
      interval_count: 0,
      status_message: "Press keys to trigger effects! [Space] Interval, [S] Stop, [1-7] Tweens, [F] Fullscreen, [L] Lock, [V] Vibrate, [C] Copy, [P] Paste",
      is_fullscreen: False,
      is_pointer_locked: False,
      clipboard_text: "Hello from Tiramisu!",
      last_keys: KeyPressState(
        space: False,
        s: False,
        digit1: False,
        digit2: False,
        digit3: False,
        digit4: False,
        digit5: False,
        digit6: False,
        digit7: False,
        t: False,
        f: False,
        l: False,
        touch_screen_pressed: False,
        c: False,
        p: False,
      ),
    )

  // Start with a delayed welcome message and tick
  let welcome_effect = effect.delay(ms: 2000, msg: DelayedMessage)
  #(model, effect.batch([effect.tick(Tick), welcome_effect]), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(String),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      // Auto rotation
      let new_rotation = model.rotation +. ctx.delta_time *. 0.5

      // Check for key presses (trigger only once per press)
      let #(new_model, key_effects) = handle_input(model, ctx.input)

      let updated_model = Model(..new_model, rotation: new_rotation)

      #(
        updated_model,
        effect.batch([effect.tick(Tick), key_effects]),
        option.None,
      )
    }

    // Time & Animation Effects
    DelayedMessage -> {
      #(
        set_status(model, "Delayed message received after 2 seconds!"),
        effect.none(),
        option.None,
      )
    }

    TimeoutTriggered -> {
      #(set_status(model, "Timeout triggered!"), effect.none(), option.None)
    }

    IntervalCreated(id) -> {
      #(
        Model(..model, interval_id: option.Some(id)),
        effect.none(),
        option.None,
      )
    }

    IntervalTick -> {
      let new_count = model.interval_count + 1
      let new_color = case new_count % 3 {
        0 -> 0x3498db
        1 -> 0xe74c3c
        _ -> 0x2ecc71
      }
      #(
        set_status(
          Model(..model, interval_count: new_count, cube_color: new_color),
          "Interval tick #" <> int.to_string(new_count),
        ),
        effect.none(),
        option.None,
      )
    }

    TweenUpdate(value) -> {
      #(
        Model(..model, tween_value: value, cube_scale: value),
        effect.none(),
        option.None,
      )
    }

    TweenComplete -> {
      // Tween back to original size
      let reverse_tween =
        effect.tween(
          from: 2.0,
          to: 1.0,
          duration_ms: 1000,
          easing: model.current_easing,
          on_update: TweenUpdate,
          on_complete: TweenComplete,
        )
      #(
        set_status(Model(..model, tween_running: False), "Tween complete!"),
        reverse_tween,
        option.None,
      )
    }

    // System & Browser Effects
    FullscreenSuccess -> {
      #(
        set_status(
          Model(..model, is_fullscreen: True),
          "Entered fullscreen mode!",
        ),
        effect.none(),
        option.None,
      )
    }

    FullscreenError -> {
      #(
        set_status(model, "Fullscreen request failed"),
        effect.none(),
        option.None,
      )
    }

    PointerLockSuccess -> {
      #(
        set_status(
          Model(..model, is_pointer_locked: True),
          "Pointer locked! Press ESC to exit.",
        ),
        effect.none(),
        option.None,
      )
    }

    PointerLockError -> {
      #(
        set_status(model, "Pointer lock request failed"),
        effect.none(),
        option.None,
      )
    }

    ClipboardWriteSuccess -> {
      #(
        set_status(
          model,
          "Copied to clipboard: \"" <> model.clipboard_text <> "\"",
        ),
        effect.none(),
        option.None,
      )
    }

    ClipboardWriteError -> {
      #(
        set_status(model, "Failed to copy to clipboard"),
        effect.none(),
        option.None,
      )
    }

    ClipboardRead(text) -> {
      #(
        set_status(
          Model(..model, clipboard_text: text),
          "Pasted from clipboard: \"" <> text <> "\"",
        ),
        effect.none(),
        option.None,
      )
    }

    ClipboardReadError -> {
      #(
        set_status(model, "Failed to read from clipboard"),
        effect.none(),
        option.None,
      )
    }
  }
}

fn handle_input(
  model: Model,
  input_state: input.InputState,
) -> #(Model, effect.Effect(Msg)) {
  // Check each key and track press state to only trigger once
  let space_pressed = input.is_key_pressed(input_state, input.Space)
  let s_pressed = input.is_key_pressed(input_state, input.KeyS)
  let digit1_pressed = input.is_key_pressed(input_state, input.Digit1)
  let digit2_pressed = input.is_key_pressed(input_state, input.Digit2)
  let digit3_pressed = input.is_key_pressed(input_state, input.Digit3)
  let digit4_pressed = input.is_key_pressed(input_state, input.Digit4)
  let digit5_pressed = input.is_key_pressed(input_state, input.Digit5)
  let digit6_pressed = input.is_key_pressed(input_state, input.Digit6)
  let digit7_pressed = input.is_key_pressed(input_state, input.Digit7)
  let t_pressed = input.is_key_pressed(input_state, input.KeyT)
  let f_pressed = input.is_key_pressed(input_state, input.KeyF)
  let l_pressed = input.is_key_pressed(input_state, input.KeyL)
  let touched = input.touches_just_started(input_state)
  let c_pressed = input.is_key_pressed(input_state, input.KeyC)
  let p_pressed = input.is_key_pressed(input_state, input.KeyP)

  // Space - Start interval
  let #(new_model, effects) = case
    space_pressed && !model.last_keys.space,
    model.interval_id
  {
    True, option.None -> {
      let interval_effect =
        effect.interval(
          ms: 1000,
          msg: IntervalTick,
          on_created: IntervalCreated,
        )
      let updated_model =
        set_status(
          Model(..model, interval_count: 0),
          "Interval started - cube will pulse every second",
        )
      #(updated_model, [interval_effect])
    }
    _, _ -> #(model, [])
  }

  // S - Stop interval
  let #(new_model, mut_effects) = case
    s_pressed && !new_model.last_keys.s,
    new_model.interval_id
  {
    True, option.Some(id) -> {
      let cancel_effect = effect.cancel_interval(id)
      let updated_model =
        set_status(
          Model(..new_model, interval_id: option.None),
          "Interval stopped",
        )
      #(updated_model, [cancel_effect, ..effects])
    }
    _, _ -> #(new_model, effects)
  }

  // 1-7 - Tween with different easings
  let #(new_model, mut_effects) = case
    digit1_pressed && !new_model.last_keys.digit1,
    new_model.tween_running
  {
    True, False -> start_tween(new_model, effect.Linear, mut_effects)
    _, _ -> #(new_model, mut_effects)
  }

  let #(new_model, mut_effects) = case
    digit2_pressed && !new_model.last_keys.digit2,
    new_model.tween_running
  {
    True, False -> start_tween(new_model, effect.EaseInQuad, mut_effects)
    _, _ -> #(new_model, mut_effects)
  }

  let #(new_model, mut_effects) = case
    digit3_pressed && !new_model.last_keys.digit3,
    new_model.tween_running
  {
    True, False -> start_tween(new_model, effect.EaseOutQuad, mut_effects)
    _, _ -> #(new_model, mut_effects)
  }

  let #(new_model, mut_effects) = case
    digit4_pressed && !new_model.last_keys.digit4,
    new_model.tween_running
  {
    True, False -> start_tween(new_model, effect.EaseInOutQuad, mut_effects)
    _, _ -> #(new_model, mut_effects)
  }

  let #(new_model, mut_effects) = case
    digit5_pressed && !new_model.last_keys.digit5,
    new_model.tween_running
  {
    True, False -> start_tween(new_model, effect.EaseInCubic, mut_effects)
    _, _ -> #(new_model, mut_effects)
  }

  let #(new_model, mut_effects) = case
    digit6_pressed && !new_model.last_keys.digit6,
    new_model.tween_running
  {
    True, False -> start_tween(new_model, effect.EaseOutCubic, mut_effects)
    _, _ -> #(new_model, mut_effects)
  }

  let #(new_model, mut_effects) = case
    digit7_pressed && !new_model.last_keys.digit7,
    new_model.tween_running
  {
    True, False -> start_tween(new_model, effect.EaseInOutCubic, mut_effects)
    _, _ -> #(new_model, mut_effects)
  }

  // T - Trigger timeout
  let #(new_model, mut_effects) = case t_pressed && !new_model.last_keys.t {
    True -> {
      let timeout_effect = effect.delay(ms: 3000, msg: TimeoutTriggered)
      let updated_model = set_status(new_model, "Timeout set")
      #(updated_model, [timeout_effect, ..mut_effects])
    }
    False -> #(new_model, mut_effects)
  }

  // F - Toggle fullscreen
  let #(new_model, mut_effects) = case f_pressed && !new_model.last_keys.f {
    True ->
      case new_model.is_fullscreen {
        False -> {
          let fullscreen_effect =
            effect.request_fullscreen(
              on_success: FullscreenSuccess,
              on_error: FullscreenError,
            )
          #(new_model, [fullscreen_effect, ..mut_effects])
        }
        True -> {
          let updated_model =
            set_status(
              Model(..new_model, is_fullscreen: False),
              "Exited fullscreen",
            )
          #(updated_model, [effect.exit_fullscreen(), ..mut_effects])
        }
      }
    False -> #(new_model, mut_effects)
  }

  // L - Toggle pointer lock
  let #(new_model, mut_effects) = case l_pressed && !new_model.last_keys.l {
    True ->
      case new_model.is_pointer_locked {
        False -> {
          let pointer_lock_effect =
            effect.request_pointer_lock(
              on_success: PointerLockSuccess,
              on_error: PointerLockError,
            )
          #(new_model, [pointer_lock_effect, ..mut_effects])
        }
        True -> {
          let updated_model =
            set_status(
              Model(..new_model, is_pointer_locked: False),
              "Exited pointer lock",
            )
          #(updated_model, [effect.exit_pointer_lock(), ..mut_effects])
        }
      }
    False -> #(new_model, mut_effects)
  }

  // V - Trigger vibration
  let #(new_model, mut_effects) = case
    !list.is_empty(touched) && !new_model.last_keys.touch_screen_pressed
  {
    True -> {
      let vibration_effect = effect.vibrate([200, 100, 200])
      let updated_model =
        set_status(new_model, "Vibration triggered (mobile only)")
      #(updated_model, [vibration_effect, ..mut_effects])
    }
    False -> #(new_model, mut_effects)
  }

  // C - Copy to clipboard
  let #(new_model, mut_effects) = case c_pressed && !new_model.last_keys.c {
    True -> {
      let copy_effect =
        effect.clipboard_write(
          text: new_model.clipboard_text,
          on_success: ClipboardWriteSuccess,
          on_error: ClipboardWriteError,
        )
      #(new_model, [copy_effect, ..mut_effects])
    }
    False -> #(new_model, mut_effects)
  }

  // P - Paste from clipboard
  let #(new_model, mut_effects) = case p_pressed && !new_model.last_keys.p {
    True -> {
      let paste_effect =
        effect.clipboard_read(
          on_success: ClipboardRead,
          on_error: ClipboardReadError,
        )
      #(new_model, [paste_effect, ..mut_effects])
    }
    False -> #(new_model, mut_effects)
  }

  // Update key press state
  let new_keys =
    KeyPressState(
      space: space_pressed,
      s: s_pressed,
      digit1: digit1_pressed,
      digit2: digit2_pressed,
      digit3: digit3_pressed,
      digit4: digit4_pressed,
      digit5: digit5_pressed,
      digit6: digit6_pressed,
      digit7: digit7_pressed,
      t: t_pressed,
      f: f_pressed,
      l: l_pressed,
      touch_screen_pressed: !list.is_empty(touched),
      c: c_pressed,
      p: p_pressed,
    )

  let final_model = Model(..new_model, last_keys: new_keys)
  #(final_model, effect.batch(mut_effects))
}

fn start_tween(
  model: Model,
  easing: effect.Easing,
  effects: List(effect.Effect(Msg)),
) -> #(Model, List(effect.Effect(Msg))) {
  let tween_effect =
    effect.tween(
      from: 0.5,
      to: 2.0,
      duration_ms: 2000,
      easing: easing,
      on_update: TweenUpdate,
      on_complete: TweenComplete,
    )
  let easing_name = easing_to_string(easing)
  let updated_model =
    set_status(
      Model(..model, tween_running: True, current_easing: easing),
      "Tween started with " <> easing_name <> " easing",
    )
  #(updated_model, [tween_effect, ..effects])
}

fn view(
  model: Model,
  _ctx: tiramisu.Context(String),
) -> List(scene.Node(String)) {
  // Camera
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  // Animated cube geometry and material
  let assert Ok(cube_geometry) =
    geometry.box(width: 1.0, height: 1.0, depth: 1.0)

  let assert Ok(cube_material) =
    material.new()
    |> material.with_color(model.cube_color)
    |> material.with_metalness(0.3)
    |> material.with_roughness(0.7)
    |> material.build()

  // Background plane
  let assert Ok(plane_geometry) = geometry.plane(width: 10.0, height: 10.0)

  let assert Ok(plane_material) =
    material.new()
    |> material.with_color(0x2c3e50)
    |> material.with_metalness(0.1)
    |> material.with_roughness(0.9)
    |> material.build()

  // Lights
  let assert Ok(ambient) = light.ambient(intensity: 0.3, color: 0xffffff)
  let assert Ok(directional) =
    light.directional(intensity: 0.8, color: 0xffffff)

  [
    scene.Camera(
      id: "main",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 2.0, 5.0)),
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      active: True,
      viewport: option.None,
    ),
    scene.Light(id: "ambient", light: ambient, transform: transform.identity),
    scene.Light(
      id: "sun",
      light: directional,
      transform: transform.at(position: vec3.Vec3(5.0, 5.0, 5.0)),
    ),
    // Animated cube
    scene.Mesh(
      id: "cube",
      geometry: cube_geometry,
      material: cube_material,
      transform: transform.identity
        |> transform.with_rotation(vec3.Vec3(
          x: model.rotation,
          y: model.rotation *. 1.3,
          z: 0.0,
        ))
        |> transform.scale_by(vec3.Vec3(
          x: model.cube_scale,
          y: model.cube_scale,
          z: model.cube_scale,
        )),
      physics: option.None,
    ),
    // Background plane
    scene.Mesh(
      id: "plane",
      geometry: plane_geometry,
      material: plane_material,
      transform: transform.at(position: vec3.Vec3(0.0, -2.0, -3.0)),
      physics: option.None,
    ),
  ]
}

fn easing_to_string(easing: effect.Easing) -> String {
  case easing {
    effect.Linear -> "Linear"
    effect.EaseInQuad -> "EaseInQuad"
    effect.EaseOutQuad -> "EaseOutQuad"
    effect.EaseInOutQuad -> "EaseInOutQuad"
    effect.EaseInCubic -> "EaseInCubic"
    effect.EaseOutCubic -> "EaseOutCubic"
    effect.EaseInOutCubic -> "EaseInOutCubic"
  }
}

/// Helper to set status message and log it to console
fn set_status(model: Model, message: String) -> Model {
  io.println("[Effects Demo] " <> message)
  Model(..model, status_message: message)
}
