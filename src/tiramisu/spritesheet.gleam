//// Spritesheet animation with state machine transitions.
////
//// This module provides a complete animation system for 2D sprites with:
//// - Multiple named animations (idle, walk, jump, etc.)
//// - Automatic frame advancement with configurable timing
//// - Loop modes (once, repeat, ping-pong)
//// - State machine transitions with conditions
//// - Smooth blending between animations
////
//// ## Creating an Animation Machine
////
//// ```gleam
//// import tiramisu/spritesheet
//// import gleam/time/duration
////
//// let assert Ok(machine) =
////   spritesheet.new(texture: player_tex, columns: 8, rows: 4)
////   |> result.map(spritesheet.with_animation(
////     _,
////     name: "idle",
////     frames: [0, 1, 2, 3],
////     frame_duration: duration.milliseconds(100),
////     loop: spritesheet.Repeat,
////   ))
////   |> result.map(spritesheet.with_animation(
////     _,
////     name: "walk",
////     frames: [8, 9, 10, 11, 12, 13],
////     frame_duration: duration.milliseconds(80),
////     loop: spritesheet.Repeat,
////   ))
////   |> result.map(spritesheet.with_transition(
////     _,
////     from: "idle",
////     to: "walk",
////     condition: spritesheet.custom(fn(ctx) { ctx.is_moving }),
////     blend_duration: duration.milliseconds(200),
////   ))
////   |> result.map(spritesheet.build)
//// ```
////
//// ## Updating and Rendering
////
//// ```gleam
//// fn update(model: Model, msg: Msg, ctx: Context) {
////   let #(new_machine, _transitioned) =
////     spritesheet.update(model.machine, model.game_state, ctx.delta_time)
////   Model(..model, machine: new_machine)
//// }
////
//// fn view(model: Model, _ctx: Context) -> scene.Node {
////   scene.animated_sprite(
////     id: "player",
////     sprite: spritesheet.to_sprite(model.machine),
////     size: vec2.Vec2(64.0, 64.0),
////     transform: transform.identity,
////     physics: option.None,
////   )
//// }
//// ```
////
//// ## Transition Conditions
////
//// - `always()`: Immediate transition
//// - `after_duration(duration)`: After time in current state
//// - `custom(fn(ctx) -> Bool)`: Based on game context
////

import gleam/bool
import gleam/dict
import gleam/int
import gleam/option
import gleam/order
import gleam/time/duration
import iv
import statemachine
import tiramisu/texture

// ============================================================================
// Types
// ============================================================================

/// Render state for an animated sprite.
///
/// This is a non-generic type that contains everything the scene needs
/// to render an animated sprite. Extract it from an AnimationMachine
/// using `to_sprite`.
///
/// ## Example
///
/// ```gleam
/// scene.animated_sprite(
///   id: "player",
///   sprite: spritesheet.to_sprite(model.machine),
///   size: vec2.Vec2(64.0, 64.0),
///   transform: transform.identity,
///   physics: option.None,
/// )
/// ```
pub opaque type Sprite {
  Sprite(
    texture: texture.Texture,
    frame_index: Int,
    columns: Int,
    rows: Int,
    pixel_art: Bool,
  )
}

/// How the animation should loop.
pub type LoopMode {
  /// Play once and stop on the last frame
  Once
  /// Loop continuously from start to finish
  Repeat
  /// Play forward then backward (ping-pong)
  PingPong
}

/// Errors that can occur when creating a spritesheet.
pub type SpritesheetError {
  /// Columns must be at least 1
  InvalidColumns
  /// Rows must be at least 1
  InvalidRows
  /// Frame count must be at least 1
  InvalidFrameCount
  /// Frame count exceeds grid capacity (columns * rows)
  FrameCountExceedsGrid
}

// ============================================================================
// Phantom Types for Builder
// ============================================================================

/// Phantom type indicating no animation has been added yet.
pub type NoAnimation

/// Phantom type indicating at least one animation has been added.
pub type HasAnimation

// ============================================================================
// Internal Types (not exposed)
// ============================================================================

/// Internal spritesheet configuration
type Spritesheet {
  Spritesheet(
    texture: texture.Texture,
    columns: Int,
    rows: Int,
    frame_count: Int,
  )
}

/// Internal animation sequence
type Animation {
  Animation(
    name: String,
    frames: iv.Array(Int),
    frame_duration: duration.Duration,
    loop: LoopMode,
  )
}

/// Internal frame tracking state for each animation.
type FrameState {
  FrameState(
    current_frame_index: Int,
    elapsed_time: duration.Duration,
    is_playing: Bool,
    ping_pong_forward: Bool,
  )
}

// ============================================================================
// Transition Conditions (re-exported from statemachine)
// ============================================================================

/// Condition for transitioning between animations.
///
/// ## Variants
///
/// - `Always`: Transition immediately when evaluated
/// - `AfterDuration(Duration)`: Transition after time in current state
/// - `Custom(fn(ctx) -> Bool)`: Transition based on custom context
///
/// ## Example
///
/// ```gleam
/// // Always transition
/// spritesheet.always()
///
/// // After 2 seconds in current animation
/// spritesheet.after_duration(duration.seconds(2))
///
/// // Based on game context
/// spritesheet.custom(fn(ctx) { ctx.velocity >. 0.1 })
/// ```
pub type Condition(ctx) =
  statemachine.Condition(ctx)

/// Always transition immediately when evaluated.
pub fn always() -> Condition(ctx) {
  statemachine.Always
}

/// Transition after spending the specified duration in the current state.
pub fn after_duration(time: duration.Duration) -> Condition(ctx) {
  statemachine.AfterDuration(time)
}

/// Transition based on a custom condition function.
pub fn custom(check: fn(ctx) -> Bool) -> Condition(ctx) {
  statemachine.Custom(check)
}

// ============================================================================
// Builder Type
// ============================================================================

/// Builder for creating an AnimationMachine.
///
/// The `has_animation` phantom type parameter tracks whether at least one
/// animation has been added. You can only call `build()` on a builder that
/// has at least one animation (`Builder(HasAnimation, ctx)`).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(machine) =
///   spritesheet.new(texture: tex, columns: 8, rows: 4)
///   |> result.map(spritesheet.with_animation(
///     _,
///     name: "idle",
///     frames: [0, 1, 2, 3],
///     frame_duration: duration.milliseconds(100),
///     loop: spritesheet.Repeat,
///   ))
///   |> result.map(spritesheet.with_pixel_art(_, True))
///   |> result.map(spritesheet.build)
/// ```
pub opaque type Builder(has_animation, ctx) {
  Builder(
    spritesheet: Spritesheet,
    animations: dict.Dict(String, Animation),
    frame_states: dict.Dict(String, FrameState),
    initial_animation: String,
    transitions: List(TransitionConfig(ctx)),
    default_blend: option.Option(duration.Duration),
    pixel_art: Bool,
  )
}

/// Internal transition configuration
type TransitionConfig(ctx) {
  TransitionConfig(
    from: String,
    to: String,
    condition: Condition(ctx),
    blend_duration: duration.Duration,
    easing: option.Option(fn(Float) -> Float),
    weight: Int,
  )
}

// ============================================================================
// Animation State Machine
// ============================================================================

/// A self-contained animation state machine for spritesheet animations.
///
/// Contains everything needed for animated sprites:
/// - The spritesheet texture and grid configuration
/// - All registered animations
/// - Current animation state and frame tracking
/// - Automatic transitions based on conditions
///
/// Create one using the builder pattern:
///
/// ## Example
///
/// ```gleam
/// let assert Ok(machine) =
///   spritesheet.new(texture: player_texture, columns: 8, rows: 4)
///   |> result.map(spritesheet.with_animation(
///     _,
///     name: "idle",
///     frames: [0, 1, 2, 3],
///     frame_duration: duration.milliseconds(100),
///     loop: spritesheet.Repeat,
///   ))
///   |> result.map(spritesheet.with_animation(
///     _,
///     name: "walk",
///     frames: [8, 9, 10, 11, 12, 13, 14, 15],
///     frame_duration: duration.milliseconds(80),
///     loop: spritesheet.Repeat,
///   ))
///   |> result.map(spritesheet.with_transition(
///     _,
///     from: "idle",
///     to: "walk",
///     condition: spritesheet.custom(fn(ctx) { ctx.is_moving }),
///     blend_duration: duration.milliseconds(200),
///   ))
///   |> result.map(spritesheet.build)
/// ```
pub opaque type AnimationMachine(ctx) {
  AnimationMachine(
    spritesheet: Spritesheet,
    state_machine: statemachine.StateMachine(Animation, ctx),
    frame_states: dict.Dict(String, FrameState),
    animations: dict.Dict(String, Animation),
    initial_animation: String,
    pixel_art: Bool,
  )
}

/// The current frame data from an animation machine.
///
/// Either a single animation's frame, or blending between two animations.
pub type FrameData {
  /// Playing a single animation
  SingleFrame(frame_index: Int)
  /// Blending between two animations with a factor (0.0 = from, 1.0 = to)
  BlendingFrames(
    from_frame_index: Int,
    to_frame_index: Int,
    blend_factor: Float,
  )
}

// ============================================================================
// Builder API
// ============================================================================

/// Create a new animation machine builder from a texture.
///
/// This creates the spritesheet configuration. You must add at least one
/// animation with `with_animation` before calling `build()`.
///
/// ## Parameters
///
/// - `texture`: The loaded texture containing the sprite frames
/// - `columns`: Number of frames horizontally
/// - `rows`: Number of frames vertically
///
/// ## Example
///
/// ```gleam
/// let assert Ok(machine) =
///   spritesheet.new(texture: tex, columns: 8, rows: 4)
///   |> result.map(spritesheet.with_animation(...))
///   |> result.map(spritesheet.build)
/// ```
pub fn new(
  texture texture: texture.Texture,
  columns columns: Int,
  rows rows: Int,
) -> Result(Builder(NoAnimation, ctx), SpritesheetError) {
  new_with_count(texture, columns, rows, columns * rows)
}

/// Create a new animation machine builder with a specific frame count.
///
/// Use this when your sprite atlas has empty cells at the end.
pub fn new_with_count(
  texture texture: texture.Texture,
  columns columns: Int,
  rows rows: Int,
  frame_count frame_count: Int,
) -> Result(Builder(NoAnimation, ctx), SpritesheetError) {
  use <- bool.guard(columns < 1, return: Error(InvalidColumns))
  use <- bool.guard(rows < 1, return: Error(InvalidRows))
  use <- bool.guard(frame_count < 1, return: Error(InvalidFrameCount))
  use <- bool.guard(
    frame_count > columns * rows,
    return: Error(FrameCountExceedsGrid),
  )
  let sheet = Spritesheet(texture, columns, rows, frame_count)
  Ok(Builder(
    spritesheet: sheet,
    animations: dict.new(),
    frame_states: dict.new(),
    initial_animation: "",
    transitions: [],
    default_blend: option.None,
    pixel_art: False,
  ))
}

fn new_frame_state() -> FrameState {
  FrameState(
    current_frame_index: 0,
    elapsed_time: duration.nanoseconds(0),
    is_playing: True,
    ping_pong_forward: True,
  )
}

/// Add an animation to the builder.
///
/// The first animation added becomes the initial/default animation.
/// Adding an animation transitions the builder from `NoAnimation` to `HasAnimation`.
///
/// ## Parameters
///
/// - `name`: Unique identifier for this animation (e.g., "idle", "walk", "jump")
/// - `frames`: List of frame indices from the spritesheet (0-indexed)
/// - `frame_duration`: How long to show each frame
/// - `loop`: How the animation should loop
///
/// ## Example
///
/// ```gleam
/// builder
/// |> spritesheet.with_animation(
///   name: "idle",
///   frames: [0, 1, 2, 3],
///   frame_duration: duration.milliseconds(100),
///   loop: spritesheet.Repeat,
/// )
/// |> spritesheet.with_animation(
///   name: "walk",
///   frames: [8, 9, 10, 11, 12, 13, 14, 15],
///   frame_duration: duration.milliseconds(80),
///   loop: spritesheet.Repeat,
/// )
/// ```
pub fn with_animation(
  builder: Builder(_, ctx),
  name name: String,
  frames frames: List(Int),
  frame_duration frame_duration: duration.Duration,
  loop loop: LoopMode,
) -> Builder(HasAnimation, ctx) {
  let anim = Animation(name, iv.from_list(frames), frame_duration, loop)
  let new_animations = dict.insert(builder.animations, name, anim)
  let new_frame_states =
    dict.insert(builder.frame_states, name, new_frame_state())

  // If this is the first animation, set it as initial
  let initial = case builder.initial_animation {
    "" -> name
    existing -> existing
  }

  Builder(
    spritesheet: builder.spritesheet,
    animations: new_animations,
    frame_states: new_frame_states,
    initial_animation: initial,
    transitions: builder.transitions,
    default_blend: builder.default_blend,
    pixel_art: builder.pixel_art,
  )
}

/// Enable pixel art rendering (nearest-neighbor filtering).
///
/// When enabled, the texture will use nearest-neighbor filtering instead of
/// linear filtering, preserving crisp pixel edges.
pub fn with_pixel_art(
  builder: Builder(has_animation, ctx),
  enabled: Bool,
) -> Builder(has_animation, ctx) {
  Builder(..builder, pixel_art: enabled)
}

/// Add a transition between two animations by name.
///
/// This can only be called on a builder that has at least one animation.
///
/// ## Parameters
///
/// - `from`: Name of the source animation (must be registered)
/// - `to`: Name of the target animation (must be registered)
/// - `condition`: When to trigger (`always()`, `after_duration()`, or `custom()`)
/// - `blend_duration`: Time to crossfade between animations
///
/// ## Example
///
/// ```gleam
/// builder
/// |> spritesheet.with_transition(
///   from: "idle",
///   to: "walk",
///   condition: spritesheet.custom(fn(ctx) { ctx.is_moving }),
///   blend_duration: duration.milliseconds(200),
/// )
/// ```
pub fn with_transition(
  builder: Builder(HasAnimation, ctx),
  from from: String,
  to to: String,
  condition condition: Condition(ctx),
  blend_duration blend_duration: duration.Duration,
) -> Builder(HasAnimation, ctx) {
  let config =
    TransitionConfig(
      from: from,
      to: to,
      condition: condition,
      blend_duration: blend_duration,
      easing: option.None,
      weight: 0,
    )
  Builder(..builder, transitions: [config, ..builder.transitions])
}

/// Add a transition with advanced options.
///
/// Like `with_transition` but allows specifying easing function and priority weight.
pub fn with_transition_advanced(
  builder: Builder(HasAnimation, ctx),
  from from: String,
  to to: String,
  condition condition: Condition(ctx),
  blend_duration blend_duration: duration.Duration,
  easing easing: option.Option(fn(Float) -> Float),
  weight weight: Int,
) -> Builder(HasAnimation, ctx) {
  let config =
    TransitionConfig(
      from: from,
      to: to,
      condition: condition,
      blend_duration: blend_duration,
      easing: easing,
      weight: weight,
    )
  Builder(..builder, transitions: [config, ..builder.transitions])
}

/// Set the default blend duration for manual transitions.
pub fn with_default_blend(
  builder: Builder(has_animation, ctx),
  blend_duration: duration.Duration,
) -> Builder(has_animation, ctx) {
  Builder(..builder, default_blend: option.Some(blend_duration))
}

/// Build the animation machine from the builder.
///
/// This can only be called on a builder that has at least one animation
/// (`Builder(HasAnimation, ctx)`).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(machine) =
///   spritesheet.new(texture: tex, columns: 8, rows: 4)
///   |> result.map(spritesheet.with_animation(...))
///   |> result.map(spritesheet.build)
/// ```
pub fn build(builder: Builder(HasAnimation, ctx)) -> AnimationMachine(ctx) {
  // Get the initial animation
  let assert Ok(initial_anim) =
    dict.get(builder.animations, builder.initial_animation)

  // Create the state machine with all animations
  let state_machine =
    dict.fold(
      builder.animations,
      statemachine.new(initial_anim),
      fn(sm, _, anim) {
        case anim.name == initial_anim.name {
          True -> sm
          False -> statemachine.with_state(sm, anim)
        }
      },
    )

  // Add all transitions
  let state_machine =
    builder.transitions
    |> list_reverse([])
    |> list_fold(state_machine, fn(sm, config) {
      case
        dict.get(builder.animations, config.from),
        dict.get(builder.animations, config.to)
      {
        Ok(from_anim), Ok(to_anim) ->
          statemachine.with_transition(
            sm,
            from: from_anim,
            to: to_anim,
            condition: config.condition,
            blend_duration: config.blend_duration,
            easing: config.easing,
            weight: config.weight,
          )
        _, _ -> sm
      }
    })

  // Apply default blend if set
  let state_machine = case builder.default_blend {
    option.Some(duration) ->
      statemachine.with_default_blend(state_machine, duration:)
    option.None -> state_machine
  }

  AnimationMachine(
    spritesheet: builder.spritesheet,
    state_machine: state_machine,
    frame_states: builder.frame_states,
    animations: builder.animations,
    initial_animation: builder.initial_animation,
    pixel_art: builder.pixel_art,
  )
}

// Simple list helpers to avoid importing gleam/list
fn list_reverse(list: List(a), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [first, ..rest] -> list_reverse(rest, [first, ..acc])
  }
}

fn list_fold(list: List(a), acc: b, f: fn(b, a) -> b) -> b {
  case list {
    [] -> acc
    [first, ..rest] -> list_fold(rest, f(acc, first), f)
  }
}

// ============================================================================
// Animation Machine API
// ============================================================================

/// Update the animation machine (call every frame).
///
/// This advances frame counters for active animations and evaluates
/// transition conditions.
///
/// Returns the updated machine and a boolean indicating if a transition
/// occurred this frame.
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: Context) {
///   case msg {
///     Tick -> {
///       let #(new_machine, _transitioned) =
///         spritesheet.update(model.machine, model.game_state, ctx.delta_time)
///       Model(..model, machine: new_machine)
///     }
///   }
/// }
/// ```
pub fn update(
  machine: AnimationMachine(ctx),
  context: ctx,
  delta_time: duration.Duration,
) -> #(AnimationMachine(ctx), Bool) {
  // Update the state machine
  let #(new_state_machine, transitioned) =
    statemachine.update(machine.state_machine, context, delta_time)

  // Get current animation state(s) and update their frame counters
  let new_frame_states = case statemachine.state_data(new_state_machine) {
    statemachine.Single(animation) -> {
      update_frame_state(machine.frame_states, animation, delta_time)
    }
    statemachine.BlendingData(from: from_anim, to: to_anim, factor: _) -> {
      machine.frame_states
      |> update_frame_state(from_anim, delta_time)
      |> update_frame_state(to_anim, delta_time)
    }
  }

  #(
    AnimationMachine(
      ..machine,
      state_machine: new_state_machine,
      frame_states: new_frame_states,
    ),
    transitioned,
  )
}

fn update_frame_state(
  frame_states: dict.Dict(String, FrameState),
  animation: Animation,
  delta_time: duration.Duration,
) -> dict.Dict(String, FrameState) {
  let state = case dict.get(frame_states, animation.name) {
    Ok(s) -> s
    Error(_) -> new_frame_state()
  }

  let new_state = advance_animation(state, animation, delta_time)
  dict.insert(frame_states, animation.name, new_state)
}

fn advance_animation(
  state: FrameState,
  animation: Animation,
  delta_time: duration.Duration,
) -> FrameState {
  case state.is_playing {
    False -> state
    True -> {
      let new_elapsed = duration.add(state.elapsed_time, delta_time)

      case duration.compare(new_elapsed, animation.frame_duration) {
        order.Lt -> FrameState(..state, elapsed_time: new_elapsed)
        order.Eq | order.Gt -> {
          let remaining =
            duration.difference(animation.frame_duration, new_elapsed)
          advance_frame(state, animation, remaining)
        }
      }
    }
  }
}

fn advance_frame(
  state: FrameState,
  animation: Animation,
  remaining_time: duration.Duration,
) -> FrameState {
  let frame_count = iv.size(animation.frames)

  let new_state = case animation.loop {
    Repeat -> {
      let next_index = { state.current_frame_index + 1 } % frame_count
      FrameState(
        ..state,
        current_frame_index: next_index,
        elapsed_time: remaining_time,
      )
    }

    Once -> {
      let next_index = state.current_frame_index + 1
      case next_index >= frame_count {
        True ->
          FrameState(
            ..state,
            current_frame_index: frame_count - 1,
            is_playing: False,
            elapsed_time: duration.nanoseconds(0),
          )
        False ->
          FrameState(
            ..state,
            current_frame_index: next_index,
            elapsed_time: remaining_time,
          )
      }
    }

    PingPong -> {
      case state.ping_pong_forward {
        True -> {
          let next_index = state.current_frame_index + 1
          case next_index >= frame_count {
            True ->
              FrameState(
                ..state,
                current_frame_index: frame_count - 2,
                ping_pong_forward: False,
                elapsed_time: remaining_time,
              )
            False ->
              FrameState(
                ..state,
                current_frame_index: next_index,
                elapsed_time: remaining_time,
              )
          }
        }
        False -> {
          let next_index = state.current_frame_index - 1
          case next_index < 0 {
            True ->
              FrameState(
                ..state,
                current_frame_index: 1,
                ping_pong_forward: True,
                elapsed_time: remaining_time,
              )
            False ->
              FrameState(
                ..state,
                current_frame_index: next_index,
                elapsed_time: remaining_time,
              )
          }
        }
      }
    }
  }

  // If we still have enough time for another frame, advance again
  case new_state.is_playing {
    False -> new_state
    True -> {
      case duration.compare(new_state.elapsed_time, animation.frame_duration) {
        order.Lt -> new_state
        order.Eq -> advance_frame(new_state, animation, duration.nanoseconds(0))
        order.Gt -> {
          let remaining =
            duration.difference(
              animation.frame_duration,
              new_state.elapsed_time,
            )
          advance_frame(new_state, animation, remaining)
        }
      }
    }
  }
}

/// Get the current frame data from the animation machine.
///
/// Returns either a single frame or blending frame data.
pub fn frame_data(machine: AnimationMachine(ctx)) -> FrameData {
  case statemachine.state_data(machine.state_machine) {
    statemachine.Single(animation) -> {
      let frame_idx = get_frame_index(machine.frame_states, animation)
      SingleFrame(frame_index: frame_idx)
    }
    statemachine.BlendingData(from: from_anim, to: to_anim, factor:) -> {
      let from_idx = get_frame_index(machine.frame_states, from_anim)
      let to_idx = get_frame_index(machine.frame_states, to_anim)
      BlendingFrames(
        from_frame_index: from_idx,
        to_frame_index: to_idx,
        blend_factor: factor,
      )
    }
  }
}

fn get_frame_index(
  frame_states: dict.Dict(String, FrameState),
  animation: Animation,
) -> Int {
  case dict.get(frame_states, animation.name) {
    Ok(state) ->
      case iv.get(animation.frames, state.current_frame_index) {
        Ok(frame) -> frame
        Error(_) -> 0
      }
    Error(_) ->
      case iv.get(animation.frames, 0) {
        Ok(frame) -> frame
        Error(_) -> 0
      }
  }
}

/// Force a transition to a specific animation by name.
///
/// Bypasses all transition conditions and forces an immediate state change.
/// Useful for external events like damage, death, or cutscenes.
///
/// ## Example
///
/// ```gleam
/// // Player got hit, immediately play hurt animation
/// let machine = spritesheet.transition_to(machine, "hurt")
/// ```
pub fn transition_to(
  machine: AnimationMachine(ctx),
  animation_name: String,
) -> AnimationMachine(ctx) {
  case dict.get(machine.animations, animation_name) {
    Ok(target) -> {
      AnimationMachine(
        ..machine,
        state_machine: statemachine.transition_to(
          machine.state_machine,
          target,
          blend_duration: option.None,
          easing: option.None,
        ),
      )
    }
    Error(_) -> machine
  }
}

/// Force a transition with custom blend duration.
pub fn transition_to_with_blend(
  machine: AnimationMachine(ctx),
  animation_name: String,
  blend_duration: duration.Duration,
) -> AnimationMachine(ctx) {
  case dict.get(machine.animations, animation_name) {
    Ok(target) -> {
      AnimationMachine(
        ..machine,
        state_machine: statemachine.transition_to(
          machine.state_machine,
          target,
          blend_duration: option.Some(blend_duration),
          easing: option.None,
        ),
      )
    }
    Error(_) -> machine
  }
}

/// Check if currently blending between animations.
pub fn is_blending(machine: AnimationMachine(ctx)) -> Bool {
  statemachine.is_blending(machine.state_machine)
}

/// Get blend progress as a normalized value (0.0 to 1.0).
///
/// Returns Error(Nil) if not currently blending.
pub fn blend_progress(machine: AnimationMachine(ctx)) -> Result(Float, Nil) {
  statemachine.blend_progress(machine.state_machine)
}

/// Get the name of the current animation.
pub fn current_animation_name(machine: AnimationMachine(ctx)) -> String {
  case statemachine.state_data(machine.state_machine) {
    statemachine.Single(animation) -> animation.name
    statemachine.BlendingData(to: to_anim, ..) -> to_anim.name
  }
}

/// Check if pixel art mode is enabled.
pub fn is_pixel_art(machine: AnimationMachine(ctx)) -> Bool {
  machine.pixel_art
}

/// Convert an animation machine to a sprite for rendering.
///
/// Call this in your view function to get the current sprite state
/// that can be passed to `scene.animated_sprite`.
///
/// ## Example
///
/// ```gleam
/// fn view(model: Model, _ctx: Context) -> scene.Node {
///   scene.animated_sprite(
///     id: "player",
///     sprite: spritesheet.to_sprite(model.machine),
///     size: vec2.Vec2(64.0, 64.0),
///     transform: transform.identity,
///     physics: option.None,
///   )
/// }
/// ```
pub fn to_sprite(machine: AnimationMachine(ctx)) -> Sprite {
  let frame_idx = case frame_data(machine) {
    SingleFrame(frame_index:) -> frame_index
    BlendingFrames(to_frame_index:, ..) -> to_frame_index
  }
  Sprite(
    texture: machine.spritesheet.texture,
    frame_index: frame_idx,
    columns: machine.spritesheet.columns,
    rows: machine.spritesheet.rows,
    pixel_art: machine.pixel_art,
  )
}

// ============================================================================
// Internal Functions (for scene.gleam to use)
// ============================================================================

/// Get the texture from a sprite.
/// @internal
pub fn sprite_texture(sprite: Sprite) -> texture.Texture {
  sprite.texture
}

/// Get the current frame index from a sprite.
/// @internal
pub fn sprite_frame_index(sprite: Sprite) -> Int {
  sprite.frame_index
}

/// Check if pixel art mode is enabled for a sprite.
/// @internal
pub fn sprite_pixel_art(sprite: Sprite) -> Bool {
  sprite.pixel_art
}

/// Calculate UV offset for a sprite's current frame.
/// @internal
pub fn sprite_frame_offset(sprite: Sprite) -> #(Float, Float) {
  let row = sprite.frame_index / sprite.columns
  let col = sprite.frame_index % sprite.columns

  let offset_x = int.to_float(col) /. int.to_float(sprite.columns)
  let offset_y = int.to_float(row) /. int.to_float(sprite.rows)

  #(offset_x, offset_y)
}

/// Calculate UV repeat values for a sprite.
/// @internal
pub fn sprite_frame_repeat(sprite: Sprite) -> #(Float, Float) {
  let repeat_x = 1.0 /. int.to_float(sprite.columns)
  let repeat_y = 1.0 /. int.to_float(sprite.rows)

  #(repeat_x, repeat_y)
}
