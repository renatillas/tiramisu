//// Input handling for game-style keyboard and mouse polling.
////
//// `InputState` is pure Gleam data stored in your Model. Input is captured via
//// Lustre event attributes on the renderer element. The browser's focus system
//// naturally scopes input — clicking a renderer focuses it, and only that
//// renderer receives keyboard events. Multiple games on one page work
//// independently.
////
//// ## Usage
////
//// ```gleam
//// import tiramisu/input
////
//// type Model { Model(input: input.InputState, ...) }
//// type Msg {
////   Tick(tick.TickContext)
////   KeyDown(input.Key) KeyUp(input.Key)
////   MouseMove(Float, Float) MouseDown(input.MouseButton) MouseUp(input.MouseButton)
////   Wheel(Float)
//// }
////
//// fn init(_) {
////   #(Model(input: input.new()), tick.subscribe("", Tick))
//// }
////
//// fn update(model, msg) {
////   case msg {
////     KeyDown(key) -> #(
////       Model(..model, input: input.key_down(model.input, key)),
////       effect.none(),
////     )
////     KeyUp(key) -> #(
////       Model(..model, input: input.key_up(model.input, key)),
////       effect.none(),
////     )
////     MouseMove(x, y) -> #(
////       Model(..model, input: input.mouse_move(model.input, x, y)),
////       effect.none(),
////     )
////     MouseDown(btn) -> #(
////       Model(..model, input: input.mouse_down(model.input, btn)),
////       effect.none(),
////     )
////     MouseUp(btn) -> #(
////       Model(..model, input: input.mouse_up(model.input, btn)),
////       effect.none(),
////     )
////     Wheel(delta) -> #(
////       Model(..model, input: input.mouse_wheel_input(model.input, delta)),
////       effect.none(),
////     )
////     Tick(ctx) -> {
////       let moving = input.is_pressed(model.input, input.W)
////       let #(mx, my) = input.mouse_position(model.input)
////       // ... game logic ...
////       #(Model(..model, input: input.end_frame(model.input)), effects)
////     }
////   }
//// }
////
//// fn view(model) {
////   renderer.renderer([
////     renderer.width(800),
////     renderer.height(600),
////     attribute.attribute("tabindex", "0"),
////     input.on_keydown(KeyDown),
////     input.on_keyup(KeyUp),
////     input.on_mousemove(MouseMove),
////     input.on_mousedown(MouseDown),
////     input.on_mouseup(MouseUp),
////     input.on_wheel(Wheel),
////   ], [ ... ])
//// }
//// ```

import gleam/dynamic/decode
import gleam/set.{type Set}
import lustre/attribute.{type Attribute}
import lustre/event

// TYPES -----------------------------------------------------------------------

/// A keyboard key identified by its physical position (KeyboardEvent.key).
///
/// Uses `key` so that letter keys, arrows, function keys, and modifiers all
/// have distinct human-readable identifiers.
pub type Key {
  // Letters
  A
  B
  C
  D
  E
  F
  G
  H
  I
  J
  K
  L
  M
  N
  O
  P
  Q
  R
  S
  T
  U
  V
  W
  X
  Y
  Z

  // Shift + letter
  ShiftA
  ShiftB
  ShiftC
  ShiftD
  ShiftE
  ShiftF
  ShiftG
  ShiftH
  ShiftI
  ShiftJ
  ShiftK
  ShiftL
  ShiftM
  ShiftN
  ShiftO
  ShiftP
  ShiftQ
  ShiftR
  ShiftS
  ShiftT
  ShiftU
  ShiftV
  ShiftW
  ShiftX
  ShiftY
  ShiftZ

  // Arrow keys
  ArrowUp
  ArrowDown
  ArrowLeft
  ArrowRight

  // Digits
  Digit0
  Digit1
  Digit2
  Digit3
  Digit4
  Digit5
  Digit6
  Digit7
  Digit8
  Digit9

  // Function keys
  F1
  F2
  F3
  F4
  F5
  F6
  F7
  F8
  F9
  F10
  F11
  F12

  // Special keys
  Enter
  Escape
  Tab
  Backspace
  Delete
  Space

  // Modifiers
  Shift
  Control
  Alt
  Meta
  CapsLock

  // Fallback for unmapped keys
  Other(String)
}

/// A mouse button.
pub type MouseButton {
  LeftButton
  MiddleButton
  RightButton
}

/// Tracks which keys/buttons are currently held, just pressed, or just released,
/// plus mouse position and wheel state.
///
/// Store this in your Model and update it from input messages.
/// Call `end_frame` once per tick to clear the per-frame sets.
pub opaque type InputState {
  InputState(
    // Keyboard — keys currently held down
    pressed: Set(Key),
    // Keys pressed this frame (since last end_frame)
    just_pressed: Set(Key),
    // Keys released this frame (since last end_frame)
    just_released: Set(Key),
    // Mouse position (relative to renderer element via offsetX/offsetY)
    mouse_x: Float,
    mouse_y: Float,
    // Mouse movement delta since last mouse_move call
    mouse_dx: Float,
    mouse_dy: Float,
    // Mouse buttons currently held down
    mouse_buttons: Set(MouseButton),
    // Mouse buttons pressed this frame
    mouse_just_pressed: Set(MouseButton),
    // Mouse buttons released this frame
    mouse_just_released: Set(MouseButton),
    // Scroll wheel delta accumulated this frame (positive = scroll down)
    mouse_wheel: Float,
  )
}

// STATE MANAGEMENT ------------------------------------------------------------

/// Create an empty input state with no keys or buttons pressed.
pub fn new() -> InputState {
  InputState(
    pressed: set.new(),
    just_pressed: set.new(),
    just_released: set.new(),
    mouse_x: 0.0,
    mouse_y: 0.0,
    mouse_dx: 0.0,
    mouse_dy: 0.0,
    mouse_buttons: set.new(),
    mouse_just_pressed: set.new(),
    mouse_just_released: set.new(),
    mouse_wheel: 0.0,
  )
}

/// Record a key press. Call this from your `KeyDown` message handler.
///
/// Ignores browser key repeat — if the key is already held, this is a no-op.
pub fn key_down(state: InputState, key: Key) -> InputState {
  case set.contains(state.pressed, key) {
    // Already held — browser auto-repeat, ignore
    True -> state
    False ->
      InputState(
        ..state,
        pressed: set.insert(state.pressed, key),
        just_pressed: set.insert(state.just_pressed, key),
      )
  }
}

/// Record a key release. Call this from your `KeyUp` message handler.
pub fn key_up(state: InputState, key: Key) -> InputState {
  InputState(
    ..state,
    pressed: set.delete(state.pressed, key),
    just_released: set.insert(state.just_released, key),
  )
}

/// Record mouse movement. Call this from your `MouseMove` message handler.
///
/// `x` and `y` are the position relative to the renderer element (offsetX/offsetY).
/// Delta is computed from the difference to the previous position.
pub fn mouse_move(state: InputState, x: Float, y: Float) -> InputState {
  InputState(
    ..state,
    mouse_x: x,
    mouse_y: y,
    mouse_dx: x -. state.mouse_x,
    mouse_dy: y -. state.mouse_y,
  )
}

/// Record a mouse button press. Call this from your `MouseDown` message handler.
pub fn mouse_down(state: InputState, button: MouseButton) -> InputState {
  InputState(
    ..state,
    mouse_buttons: set.insert(state.mouse_buttons, button),
    mouse_just_pressed: set.insert(state.mouse_just_pressed, button),
  )
}

/// Record a mouse button release. Call this from your `MouseUp` message handler.
pub fn mouse_up(state: InputState, button: MouseButton) -> InputState {
  InputState(
    ..state,
    mouse_buttons: set.delete(state.mouse_buttons, button),
    mouse_just_released: set.insert(state.mouse_just_released, button),
  )
}

/// Record scroll wheel input. Call this from your `Wheel` message handler.
///
/// Delta values accumulate within a frame — if the user scrolls multiple times
/// before `end_frame`, the deltas add up.
pub fn mouse_wheel_input(state: InputState, delta: Float) -> InputState {
  InputState(..state, mouse_wheel: state.mouse_wheel +. delta)
}

/// Clear per-frame state (just_pressed, just_released, mouse delta, wheel).
///
/// Call this once per tick, after you've read the input state for game logic.
/// Typically at the end of your `Tick` message handler.
pub fn end_frame(state: InputState) -> InputState {
  InputState(
    ..state,
    just_pressed: set.new(),
    just_released: set.new(),
    mouse_dx: 0.0,
    mouse_dy: 0.0,
    mouse_just_pressed: set.new(),
    mouse_just_released: set.new(),
    mouse_wheel: 0.0,
  )
}

// KEYBOARD QUERIES ------------------------------------------------------------

/// Is the key currently held down?
pub fn is_pressed(state: InputState, key: Key) -> Bool {
  set.contains(state.pressed, key)
}

/// Was the key pressed this frame? (True only on the frame it was first pressed.)
pub fn is_just_pressed(state: InputState, key: Key) -> Bool {
  set.contains(state.just_pressed, key)
}

/// Was the key released this frame?
pub fn is_just_released(state: InputState, key: Key) -> Bool {
  set.contains(state.just_released, key)
}

// MOUSE QUERIES ---------------------------------------------------------------

/// Current mouse position relative to the renderer element.
pub fn mouse_position(state: InputState) -> #(Float, Float) {
  #(state.mouse_x, state.mouse_y)
}

/// Mouse movement delta since the last `end_frame`.
pub fn mouse_delta(state: InputState) -> #(Float, Float) {
  #(state.mouse_dx, state.mouse_dy)
}

/// Is the given mouse button currently held down?
pub fn is_mouse_pressed(state: InputState, button: MouseButton) -> Bool {
  set.contains(state.mouse_buttons, button)
}

/// Was the mouse button pressed this frame?
pub fn is_mouse_just_pressed(state: InputState, button: MouseButton) -> Bool {
  set.contains(state.mouse_just_pressed, button)
}

/// Was the mouse button released this frame?
pub fn is_mouse_just_released(state: InputState, button: MouseButton) -> Bool {
  set.contains(state.mouse_just_released, button)
}

/// Scroll wheel delta accumulated this frame. Positive = scroll down.
pub fn wheel_delta(state: InputState) -> Float {
  state.mouse_wheel
}

// KEYBOARD EVENT ATTRIBUTES ---------------------------------------------------

/// Lustre attribute that listens for keydown events and maps the key code to a `Key`.
pub fn on_keydown(msg: fn(Key) -> msg) -> Attribute(msg) {
  event.on_keydown(fn(code) { code |> code_to_key |> msg })
}

/// Lustre attribute that listens for keyup events and maps the key code to a `Key`.
pub fn on_keyup(msg: fn(Key) -> msg) -> Attribute(msg) {
  event.on_keyup(fn(code) { code |> code_to_key |> msg })
}

// MOUSE EVENT ATTRIBUTES ------------------------------------------------------

/// Lustre attribute that listens for mousemove events and provides the
/// position relative to the element (offsetX, offsetY).
pub fn on_mousemove(msg: fn(Float, Float) -> msg) -> Attribute(msg) {
  event.on("mousemove", {
    use x <- decode.field("offsetX", decode.float)
    use y <- decode.field("offsetY", decode.float)
    msg(x, y) |> decode.success
  })
}

/// Lustre attribute that listens for mousedown events and provides the button.
pub fn on_mousedown(msg: fn(MouseButton) -> msg) -> Attribute(msg) {
  event.on("mousedown", {
    use button_int <- decode.field("button", decode.int)
    button_int |> int_to_button |> msg |> decode.success
  })
}

/// Lustre attribute that listens for mouseup events and provides the button.
pub fn on_mouseup(msg: fn(MouseButton) -> msg) -> Attribute(msg) {
  event.on("mouseup", {
    use button_int <- decode.field("button", decode.int)
    button_int |> int_to_button |> msg |> decode.success
  })
}

/// Lustre attribute that listens for wheel events and provides the vertical
/// scroll delta (deltaY). Positive = scroll down.
pub fn on_wheel(msg: fn(Float) -> msg) -> Attribute(msg) {
  event.on("wheel", {
    use delta <- decode.field("deltaY", decode.float)
    msg(delta) |> decode.success
  })
}

// INTERNAL --------------------------------------------------------------------

/// Convert a JS mouse button integer to a MouseButton.
/// 0 = left, 1 = middle, 2 = right.
fn int_to_button(n: Int) -> MouseButton {
  case n {
    1 -> MiddleButton
    2 -> RightButton
    _ -> LeftButton
  }
}

/// Convert a KeyboardEvent.key string to a Key variant.
fn code_to_key(code: String) -> Key {
  case code {
    // Letters (lowercase from key property)
    "a" -> A
    "b" -> B
    "c" -> C
    "d" -> D
    "e" -> E
    "f" -> F
    "g" -> G
    "h" -> H
    "i" -> I
    "j" -> J
    "k" -> K
    "l" -> L
    "m" -> M
    "n" -> N
    "o" -> O
    "p" -> P
    "q" -> Q
    "r" -> R
    "s" -> S
    "t" -> T
    "u" -> U
    "v" -> V
    "w" -> W
    "x" -> X
    "y" -> Y
    "z" -> Z

    // Shift + letter (uppercase from key property)
    "A" -> ShiftA
    "B" -> ShiftB
    "C" -> ShiftC
    "D" -> ShiftD
    "E" -> ShiftE
    "F" -> ShiftF
    "G" -> ShiftG
    "H" -> ShiftH
    "I" -> ShiftI
    "J" -> ShiftJ
    "K" -> ShiftK
    "L" -> ShiftL
    "M" -> ShiftM
    "N" -> ShiftN
    "O" -> ShiftO
    "P" -> ShiftP
    "Q" -> ShiftQ
    "R" -> ShiftR
    "S" -> ShiftS
    "T" -> ShiftT
    "U" -> ShiftU
    "V" -> ShiftV
    "W" -> ShiftW
    "X" -> ShiftX
    "Y" -> ShiftY
    "Z" -> ShiftZ

    // Arrow keys
    "ArrowUp" -> ArrowUp
    "ArrowDown" -> ArrowDown
    "ArrowLeft" -> ArrowLeft
    "ArrowRight" -> ArrowRight

    // Digits
    "0" -> Digit0
    "1" -> Digit1
    "2" -> Digit2
    "3" -> Digit3
    "4" -> Digit4
    "5" -> Digit5
    "6" -> Digit6
    "7" -> Digit7
    "8" -> Digit8
    "9" -> Digit9

    // Function keys
    "F1" -> F1
    "F2" -> F2
    "F3" -> F3
    "F4" -> F4
    "F5" -> F5
    "F6" -> F6
    "F7" -> F7
    "F8" -> F8
    "F9" -> F9
    "F10" -> F10
    "F11" -> F11
    "F12" -> F12

    // Special keys
    "Enter" -> Enter
    "Escape" -> Escape
    "Tab" -> Tab
    "Backspace" -> Backspace
    "Delete" -> Delete
    " " -> Space

    // Modifiers
    "Shift" -> Shift
    "Control" -> Control
    "Alt" -> Alt
    "Meta" -> Meta
    "CapsLock" -> CapsLock

    // Fallback
    other -> Other(other)
  }
}
