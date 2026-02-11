//// Input handling for game-style keyboard polling.
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
//// type Msg { Tick(tick.TickContext) KeyDown(input.Key) KeyUp(input.Key) }
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
////     Tick(ctx) -> {
////       let moving = input.is_pressed(model.input, input.key_w)
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
////     input.on_key_down(KeyDown),
////     input.on_key_up(KeyUp),
////   ], [ ... ])
//// }
//// ```

import gleam/set.{type Set}
import lustre/attribute
import lustre/event

// TYPES -----------------------------------------------------------------------

/// A keyboard key identified by its physical position (KeyboardEvent.code).
///
/// Uses `code` rather than `key` so that WASD works regardless of keyboard
/// layout (QWERTY, AZERTY, Dvorak, etc.).
pub type Key {
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

  Shift
  Control
  Meta
  Space

  Other(String)
}

/// Tracks which keys are currently held, just pressed, or just released.
///
/// Store this in your Model and update it from `KeyDown`/`KeyUp` messages.
/// Call `end_frame` once per tick to clear the per-frame sets.
pub opaque type InputState {
  InputState(
    // Keys currently held down
    pressed: Set(Key),
    // Keys pressed this frame (since last end_frame)
    just_pressed: Set(Key),
    // Keys released this frame (since last end_frame)
    just_released: Set(Key),
  )
}

// STATE MANAGEMENT ------------------------------------------------------------

/// Create an empty input state with no keys pressed.
pub fn new() -> InputState {
  InputState(
    pressed: set.new(),
    just_pressed: set.new(),
    just_released: set.new(),
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
        pressed: set.insert(state.pressed, key),
        just_pressed: set.insert(state.just_pressed, key),
        just_released: state.just_released,
      )
  }
}

/// Record a key release. Call this from your `KeyUp` message handler.
pub fn key_up(state: InputState, key: Key) -> InputState {
  InputState(
    pressed: set.delete(state.pressed, key),
    just_pressed: state.just_pressed,
    just_released: set.insert(state.just_released, key),
  )
}

/// Clear per-frame state (just_pressed and just_released).
///
/// Call this once per tick, after you've read the input state for game logic.
/// Typically at the end of your `Tick` message handler.
pub fn end_frame(state: InputState) -> InputState {
  InputState(
    pressed: state.pressed,
    just_pressed: set.new(),
    just_released: set.new(),
  )
}

// QUERIES ---------------------------------------------------------------------

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

pub fn on_keydown(msg: fn(Key) -> msg) -> attribute.Attribute(msg) {
  event.on_keydown(fn(code) { code |> code_to_key |> msg })
}

pub fn on_keyup(msg: fn(Key) -> msg) -> attribute.Attribute(msg) {
  event.on_keyup(fn(code) { code |> code_to_key |> msg })
}

// INTERNAL --------------------------------------------------------------------

fn code_to_key(code: String) -> Key {
  case code {
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

    " " -> Space

    "Shift" -> Shift
    "Control" -> Control
    "Meta" -> Meta

    other -> Other(other)
  }
}
