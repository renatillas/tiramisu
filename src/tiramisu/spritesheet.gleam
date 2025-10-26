//// Spritesheet animation system for texture-based sprites.
////
//// This module provides a functional approach to spritesheet animation,
//// allowing you to define sprite atlases and animate through frames over time.
////
//// ## Quick Start
////
//// ```gleam
//// import tiramisu/spritesheet
//// import tiramisu/asset
////
//// // 1. Define your spritesheet (8 frames in a horizontal strip)
//// let assert Ok(sheet) = spritesheet.from_grid(
////   texture: my_texture,
////   columns: 8,
////   rows: 1,
//// )
////
//// // 2. Define an animation sequence
//// let walk_animation = spritesheet.animation(
////   name: "walk",
////   frames: [0, 1, 2, 3, 4, 5, 6, 7],
////   frame_duration: 0.1,  // 10 FPS
////   loop: spritesheet.Repeat,
//// )
////
//// // 3. Create initial animation state
//// let state = spritesheet.initial_state("walk")
////
//// // 4. Update in your game loop
//// fn update(model, msg, ctx) {
////   let new_state = spritesheet.update(
////     state: model.anim_state,
////     animation: walk_animation,
////     delta_time: ctx.delta_time,
////   )
////   Model(..model, anim_state: new_state)
//// }
//// ```

import gleam/float
import gleam/int
import gleam/order
import iv.{type Array}
import tiramisu/asset
import tiramisu/texture

// ============================================================================
// Types
// ============================================================================

/// A spritesheet configuration defining a grid-based sprite atlas.
///
/// Spritesheets are textures containing multiple frames arranged in a grid.
/// Each frame has equal dimensions determined by dividing the texture
/// by the number of columns and rows.
pub opaque type Spritesheet {
  Spritesheet(texture: asset.Texture, columns: Int, rows: Int, frame_count: Int)
}

/// An animation sequence with frame indices and timing.
pub type Animation {
  Animation(
    name: String,
    frames: Array(Int),
    frame_duration: Float,
    loop: LoopMode,
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

/// Runtime animation state tracked in your model.
///
/// This contains the current playback state that you update each frame.
pub type AnimationState {
  AnimationState(
    current_animation: String,
    current_frame_index: Int,
    elapsed_time: Float,
    is_playing: Bool,
    ping_pong_forward: Bool,
  )
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
// Spritesheet Creation
// ============================================================================

/// Create a spritesheet from a grid-based texture atlas.
///
/// The texture is divided into equal-sized frames based on columns and rows.
///
/// ## Parameters
///
/// - `texture`: The loaded texture containing the sprite frames
/// - `columns`: Number of frames horizontally
/// - `rows`: Number of frames vertically
///
/// The `frame_count` defaults to `columns * rows`, but you can specify fewer
/// if your spritesheet has empty cells at the end.
///
/// ## Example
///
/// ```gleam
/// // 8 frames in a horizontal strip (8 columns, 1 row)
/// let assert Ok(sheet) = spritesheet.from_grid(
///   texture: player_texture,
///   columns: 8,
///   rows: 1,
/// )
/// ```
pub fn from_grid(
  texture texture: asset.Texture,
  columns columns: Int,
  rows rows: Int,
) -> Result(Spritesheet, SpritesheetError) {
  from_grid_with_count(texture, columns, rows, columns * rows)
}

/// Create a spritesheet with a specific frame count.
///
/// Use this when your sprite atlas has empty cells at the end.
///
/// ## Example
///
/// ```gleam
/// // 4x4 grid but only 12 frames used
/// let assert Ok(sheet) = spritesheet.from_grid_with_count(
///   texture: items_texture,
///   columns: 4,
///   rows: 4,
///   frame_count: 12,
/// )
/// ```
pub fn from_grid_with_count(
  texture texture: asset.Texture,
  columns columns: Int,
  rows rows: Int,
  frame_count frame_count: Int,
) -> Result(Spritesheet, SpritesheetError) {
  // Validation
  case columns < 1 {
    True -> Error(InvalidColumns)
    False ->
      case rows < 1 {
        True -> Error(InvalidRows)
        False ->
          case frame_count < 1 {
            True -> Error(InvalidFrameCount)
            False ->
              case frame_count > columns * rows {
                True -> Error(FrameCountExceedsGrid)
                False -> Ok(Spritesheet(texture, columns, rows, frame_count))
              }
          }
      }
  }
}

/// Get the base texture from a spritesheet.
///
/// **Note**: This returns the original texture. For animated sprites,
/// you should clone this texture so each sprite can animate independently.
pub fn texture(sheet: Spritesheet) -> asset.Texture {
  sheet.texture
}

/// Get the number of columns in the spritesheet.
pub fn columns(sheet: Spritesheet) -> Int {
  sheet.columns
}

/// Get the number of rows in the spritesheet.
pub fn rows(sheet: Spritesheet) -> Int {
  sheet.rows
}

/// Get the total number of frames in the spritesheet.
pub fn frame_count(sheet: Spritesheet) -> Int {
  sheet.frame_count
}

// ============================================================================
// Animation Creation
// ============================================================================

/// Create an animation sequence.
///
/// ## Parameters
///
/// - `name`: Identifier for this animation (e.g., "walk", "jump", "idle")
/// - `frames`: Array of frame indices to play (0-indexed)
/// - `frame_duration`: How long to show each frame in seconds
/// - `loop`: How the animation should loop
///
/// ## Example
///
/// ```gleam
/// let walk = spritesheet.animation(
///   name: "walk",
///   frames: iv.from_list([0, 1, 2, 3]),
///   frame_duration: 0.1,  // 10 FPS
///   loop: spritesheet.Repeat,
/// )
///
/// let jump = spritesheet.animation(
///   name: "jump",
///   frames: iv.from_list([8, 9, 10]),
///   frame_duration: 0.15,
///   loop: spritesheet.Once,
/// )
/// ```
pub fn animation(
  name name: String,
  frames frames: List(Int),
  frame_duration frame_duration: Float,
  loop loop: LoopMode,
) -> Animation {
  Animation(name, frames |> iv.from_list(), frame_duration, loop)
}

// ============================================================================
// Animation State Management
// ============================================================================

/// Create initial animation state.
///
/// Use this in your `init()` function to set up the starting state.
///
/// ## Example
///
/// ```gleam
/// fn init(_ctx) {
///   let model = Model(
///     anim_state: spritesheet.initial_state("idle"),
///     // ...
///   )
///   #(model, effect.none(), option.None)
/// }
/// ```
pub fn initial_state(animation_name: String) -> AnimationState {
  AnimationState(
    current_animation: animation_name,
    current_frame_index: 0,
    elapsed_time: 0.0,
    is_playing: True,
    ping_pong_forward: True,
  )
}

/// Update animation state based on delta time.
///
/// Call this in your `update()` function every frame to advance the animation.
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: Context) {
///   case msg {
///     Tick -> {
///       let new_state = spritesheet.update(
///         state: model.player_anim_state,
///         animation: model.walk_animation,
///         delta_time: ctx.delta_time,
///       )
///       #(Model(..model, player_anim_state: new_state), effect.tick(Tick), option.None)
///     }
///   }
/// }
/// ```
pub fn update(
  state state: AnimationState,
  animation animation: Animation,
  delta_time delta_time: Float,
) -> AnimationState {
  case state.is_playing {
    False -> state
    True -> {
      let new_elapsed = state.elapsed_time +. delta_time

      case
        float.loosely_compare(new_elapsed, animation.frame_duration, 0.0001)
      {
        order.Lt -> AnimationState(..state, elapsed_time: new_elapsed)
        order.Eq | order.Gt ->
          advance_frame(
            state,
            animation,
            new_elapsed -. animation.frame_duration,
          )
      }
    }
  }
}

fn advance_frame(
  state: AnimationState,
  animation: Animation,
  remaining_time: Float,
) -> AnimationState {
  let frame_count = iv.length(animation.frames)

  let new_state = case animation.loop {
    Repeat -> {
      let next_index = { state.current_frame_index + 1 } % frame_count
      AnimationState(
        ..state,
        current_frame_index: next_index,
        elapsed_time: remaining_time,
      )
    }

    Once -> {
      let next_index = state.current_frame_index + 1
      case next_index >= frame_count {
        True ->
          AnimationState(
            ..state,
            current_frame_index: frame_count - 1,
            is_playing: False,
            elapsed_time: 0.0,
          )
        False ->
          AnimationState(
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
              AnimationState(
                ..state,
                current_frame_index: frame_count - 2,
                ping_pong_forward: False,
                elapsed_time: remaining_time,
              )
            False ->
              AnimationState(
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
              AnimationState(
                ..state,
                current_frame_index: 1,
                ping_pong_forward: True,
                elapsed_time: remaining_time,
              )
            False ->
              AnimationState(
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
  case
    new_state.is_playing
    && {
      new_state.elapsed_time >=. animation.frame_duration
      || float.loosely_equals(
        new_state.elapsed_time,
        animation.frame_duration,
        0.0001,
      )
    }
  {
    True ->
      advance_frame(
        new_state,
        animation,
        new_state.elapsed_time -. animation.frame_duration,
      )
    False -> new_state
  }
}

// ============================================================================
// Animation Control
// ============================================================================

/// Play/resume the animation.
pub fn play(state: AnimationState) -> AnimationState {
  AnimationState(..state, is_playing: True)
}

/// Pause the animation.
pub fn pause(state: AnimationState) -> AnimationState {
  AnimationState(..state, is_playing: False)
}

/// Stop the animation and reset to first frame.
pub fn stop(state: AnimationState) -> AnimationState {
  AnimationState(
    ..state,
    is_playing: False,
    current_frame_index: 0,
    elapsed_time: 0.0,
  )
}

/// Change to a different animation.
///
/// This resets the state to the first frame of the new animation.
///
/// ## Example
///
/// ```gleam
/// PlayerJumped -> {
///   let new_state = spritesheet.change_animation(
///     model.player_anim_state,
///     "jump",
///   )
///   Model(..model, player_anim_state: new_state)
/// }
/// ```
pub fn change_animation(
  _state: AnimationState,
  animation_name: String,
) -> AnimationState {
  AnimationState(
    current_animation: animation_name,
    current_frame_index: 0,
    elapsed_time: 0.0,
    is_playing: True,
    ping_pong_forward: True,
  )
}

/// Check if the animation is currently playing.
pub fn is_playing(state: AnimationState) -> Bool {
  state.is_playing
}

/// Get the current animation name.
pub fn current_animation(state: AnimationState) -> String {
  state.current_animation
}

// ============================================================================
// Frame Calculation
// ============================================================================

/// Get the current frame index from the animation state.
///
/// This returns the index into the animation's frame list,
/// not the actual sprite frame number.
pub fn current_frame_index(state: AnimationState) -> Int {
  state.current_frame_index
}

/// Get the actual sprite frame number to display.
///
/// This looks up the frame index in the animation's frame array
/// to get the actual sprite frame number.
///
/// ## Example
///
/// ```gleam
/// let animation = spritesheet.animation(
///   name: "walk",
///   frames: iv.from_list([0, 1, 2, 3]),  // Frames to play
///   frame_duration: 0.1,
///   loop: spritesheet.Repeat,
/// )
///
/// // If current_frame_index is 2, this returns 2 (the 3rd frame in the array)
/// let frame = spritesheet.current_frame(state, animation)
/// ```
pub fn current_frame(
  state: AnimationState,
  animation: Animation,
) -> Result(Int, Nil) {
  iv.get(animation.frames, state.current_frame_index)
}

/// Calculate UV offset for a specific frame.
///
/// Returns (offset_x, offset_y) for use with texture.set_offset().
///
/// ## Example
///
/// ```gleam
/// let assert Ok(frame) = spritesheet.current_frame(state, animation)
/// let #(offset_x, offset_y) = spritesheet.frame_offset(sheet, frame)
///
/// my_texture
/// |> texture.set_offset(offset_x, offset_y)
/// ```
pub fn frame_offset(sheet: Spritesheet, frame_index: Int) -> #(Float, Float) {
  let row = frame_index / sheet.columns
  let col = frame_index % sheet.columns

  let offset_x = int.to_float(col) /. int.to_float(sheet.columns)
  let offset_y = int.to_float(row) /. int.to_float(sheet.rows)

  #(offset_x, offset_y)
}

/// Calculate UV repeat values for the spritesheet.
///
/// Returns (repeat_x, repeat_y) for use with texture.set_repeat().
///
/// ## Example
///
/// ```gleam
/// let #(repeat_x, repeat_y) = spritesheet.frame_repeat(sheet)
///
/// my_texture
/// |> texture.set_repeat(repeat_x, repeat_y)
/// ```
pub fn frame_repeat(sheet: Spritesheet) -> #(Float, Float) {
  let repeat_x = 1.0 /. int.to_float(sheet.columns)
  let repeat_y = 1.0 /. int.to_float(sheet.rows)

  #(repeat_x, repeat_y)
}

/// Apply spritesheet frame to a texture.
///
/// This is a convenience function that sets up the texture with the correct
/// UV offset, repeat, and wrap mode for the current frame.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(frame) = spritesheet.current_frame(state, animation)
/// let animated_texture = spritesheet.apply_frame(sheet, my_texture, frame)
/// ```
pub fn apply_frame(
  sheet: Spritesheet,
  tex: asset.Texture,
  frame_index: Int,
) -> asset.Texture {
  let #(offset_x, offset_y) = frame_offset(sheet, frame_index)
  let #(repeat_x, repeat_y) = frame_repeat(sheet)

  tex
  |> texture.set_repeat(repeat_x, repeat_y)
  |> texture.set_wrap_mode(texture.RepeatWrapping, texture.RepeatWrapping)
  |> texture.set_offset(offset_x, offset_y)
}
