/// Internal animation management
///
/// This module manages Three.js AnimationMixers and AnimationActions.
/// It provides functions to create mixers, setup animations (single or blended),
/// and update all active mixers each frame.
import gleam/dynamic.{type Dynamic}
import gleam/option
import tiramisu/internal/object_cache.{
  type AnimationAction, type AnimationActions, type AnimationMixer,
  type CacheState, type ThreeObject,
}
import tiramisu/object3d

/// Create an animation mixer for a Three.js object
pub fn create_mixer(object: ThreeObject) -> AnimationMixer {
  let obj = object_cache.unwrap_object(object)
  let mixer_dynamic = create_animation_mixer_ffi(obj)
  object_cache.wrap_mixer(mixer_dynamic)
}

/// Setup animation for a Model3D node
///
/// This handles both single animations and blended animations.
/// It stops any existing animations for this node and starts the new one(s).
pub fn setup_animation(
  cache: CacheState,
  id: id,
  mixer: AnimationMixer,
  animation: object3d.AnimationPlayback,
) -> CacheState {
  // Stop any existing animations
  let cache = case object_cache.get_actions(cache, id) {
    option.Some(actions) -> {
      stop_actions(actions)
      cache
    }
    option.None -> cache
  }

  // Setup new animation(s)
  case animation {
    object3d.SingleAnimation(anim) -> {
      let action = setup_single_animation(mixer, anim)
      object_cache.set_actions(cache, id, object_cache.SingleAction(action))
    }

    object3d.BlendedAnimations(from, to, blend_factor) -> {
      let from_action =
        setup_single_animation_with_weight(
          mixer,
          from,
          { 1.0 -. blend_factor } *. from.weight,
        )
      let to_action =
        setup_single_animation_with_weight(mixer, to, blend_factor *. to.weight)
      object_cache.set_actions(
        cache,
        id,
        object_cache.BlendedActions(from: from_action, to: to_action),
      )
    }
  }
}

/// Setup a single animation
fn setup_single_animation(
  mixer: AnimationMixer,
  anim: object3d.Animation,
) -> AnimationAction {
  let mixer_dynamic = object_cache.unwrap_mixer(mixer)
  let clip_dynamic = clip_to_dynamic(anim.clip)
  let action_dynamic = clip_action_ffi(mixer_dynamic, clip_dynamic)

  // Set loop mode
  let loop_mode = case anim.loop {
    object3d.LoopOnce -> get_loop_once_ffi()
    object3d.LoopRepeat -> get_loop_repeat_ffi()
  }
  set_action_loop_ffi(action_dynamic, loop_mode)

  // Set time scale (speed)
  set_action_time_scale_ffi(action_dynamic, anim.speed)

  // Set weight
  set_action_weight_ffi(action_dynamic, anim.weight)

  // Play the action
  play_action_ffi(action_dynamic)

  object_cache.wrap_action(action_dynamic)
}

/// Setup a single animation with custom weight
fn setup_single_animation_with_weight(
  mixer: AnimationMixer,
  anim: object3d.Animation,
  weight: Float,
) -> AnimationAction {
  let mixer_dynamic = object_cache.unwrap_mixer(mixer)
  let clip_dynamic = clip_to_dynamic(anim.clip)
  let action_dynamic = clip_action_ffi(mixer_dynamic, clip_dynamic)

  // Set loop mode
  let loop_mode = case anim.loop {
    object3d.LoopOnce -> get_loop_once_ffi()
    object3d.LoopRepeat -> get_loop_repeat_ffi()
  }
  set_action_loop_ffi(action_dynamic, loop_mode)

  // Set time scale (speed)
  set_action_time_scale_ffi(action_dynamic, anim.speed)

  // Set custom weight for blending
  set_action_weight_ffi(action_dynamic, weight)

  // Play the action
  play_action_ffi(action_dynamic)

  object_cache.wrap_action(action_dynamic)
}

/// Stop animation actions
fn stop_actions(actions: AnimationActions) -> Nil {
  case actions {
    object_cache.SingleAction(action) -> {
      let action_dynamic = object_cache.unwrap_action(action)
      stop_action_ffi(action_dynamic)
    }
    object_cache.BlendedActions(from, to) -> {
      let from_dynamic = object_cache.unwrap_action(from)
      let to_dynamic = object_cache.unwrap_action(to)
      stop_action_ffi(from_dynamic)
      stop_action_ffi(to_dynamic)
    }
  }
}

/// Update a single mixer with delta time
pub fn update_mixer(mixer: AnimationMixer, delta_time: Float) -> Nil {
  let mixer_dynamic = object_cache.unwrap_mixer(mixer)
  update_mixer_ffi(mixer_dynamic, delta_time)
}

/// Update all mixers in the cache with delta time
pub fn update_all_mixers(cache: CacheState, delta_time: Float) -> Nil {
  let mixers = object_cache.get_all_mixers(cache)
  case mixers {
    [] -> Nil
    _ -> {
      update_mixers_recursive(mixers, delta_time)
      Nil
    }
  }
}

fn update_mixers_recursive(
  mixers: List(#(String, AnimationMixer)),
  delta_time: Float,
) -> Nil {
  case mixers {
    [] -> Nil
    [#(_id, mixer), ..rest] -> {
      update_mixer(mixer, delta_time)
      update_mixers_recursive(rest, delta_time)
    }
  }
}

// ============================================================================
// FFI FUNCTIONS
// ============================================================================

/// Convert AnimationClip to Dynamic (identity function in JavaScript)
fn clip_to_dynamic(clip: object3d.AnimationClip) -> Dynamic {
  do_clip_to_dynamic(clip)
}

@external(javascript, "../../threejs.ffi.mjs", "identity")
fn do_clip_to_dynamic(clip: object3d.AnimationClip) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "createAnimationMixer")
fn create_animation_mixer_ffi(root: object3d.Object3D) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "clipAction")
fn clip_action_ffi(mixer: Dynamic, clip: Dynamic) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "updateMixer")
fn update_mixer_ffi(mixer: Dynamic, delta_time: Float) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "playAction")
fn play_action_ffi(action: Dynamic) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "stopAction")
fn stop_action_ffi(action: Dynamic) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setActionLoop")
fn set_action_loop_ffi(action: Dynamic, loop_mode: Int) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setActionTimeScale")
fn set_action_time_scale_ffi(action: Dynamic, time_scale: Float) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setActionWeight")
fn set_action_weight_ffi(action: Dynamic, weight: Float) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "getLoopOnce")
fn get_loop_once_ffi() -> Int

@external(javascript, "../../threejs.ffi.mjs", "getLoopRepeat")
fn get_loop_repeat_ffi() -> Int
