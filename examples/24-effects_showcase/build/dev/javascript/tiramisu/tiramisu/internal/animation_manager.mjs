import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Empty as $Empty } from "../../gleam.mjs";
import {
  identity as do_clip_to_dynamic,
  createAnimationMixer as create_animation_mixer_ffi,
  clipAction as clip_action_ffi,
  updateMixer as update_mixer_ffi,
  playAction as play_action_ffi,
  stopAction as stop_action_ffi,
  setActionLoop as set_action_loop_ffi,
  setActionTimeScale as set_action_time_scale_ffi,
  setActionWeight as set_action_weight_ffi,
  getLoopOnce as get_loop_once_ffi,
  getLoopRepeat as get_loop_repeat_ffi,
} from "../../threejs.ffi.mjs";
import * as $object_cache from "../../tiramisu/internal/object_cache.mjs";
import * as $object3d from "../../tiramisu/object3d.mjs";

/**
 * Convert AnimationClip to Dynamic (identity function in JavaScript)
 * 
 * @ignore
 */
function clip_to_dynamic(clip) {
  return do_clip_to_dynamic(clip);
}

/**
 * Create an animation mixer for a Three.js object
 */
export function create_mixer(object) {
  let obj = $object_cache.unwrap_object(object);
  let mixer_dynamic = create_animation_mixer_ffi(obj);
  return $object_cache.wrap_mixer(mixer_dynamic);
}

/**
 * Update a single mixer with delta time
 */
export function update_mixer(mixer, delta_time) {
  let mixer_dynamic = $object_cache.unwrap_mixer(mixer);
  return update_mixer_ffi(mixer_dynamic, delta_time);
}

function update_mixers_recursive(loop$mixers, loop$delta_time) {
  while (true) {
    let mixers = loop$mixers;
    let delta_time = loop$delta_time;
    if (mixers instanceof $Empty) {
      return undefined;
    } else {
      let rest = mixers.tail;
      let mixer = mixers.head[1];
      update_mixer(mixer, delta_time);
      loop$mixers = rest;
      loop$delta_time = delta_time;
    }
  }
}

/**
 * Update all mixers in the cache with delta time
 */
export function update_all_mixers(cache, delta_time) {
  let mixers = $object_cache.get_all_mixers(cache);
  if (mixers instanceof $Empty) {
    return undefined;
  } else {
    update_mixers_recursive(mixers, delta_time);
    return undefined;
  }
}

/**
 * Stop animation actions
 * 
 * @ignore
 */
function stop_actions(actions) {
  if (actions instanceof $object_cache.SingleAction) {
    let action = actions[0];
    let action_dynamic = $object_cache.unwrap_action(action);
    return stop_action_ffi(action_dynamic);
  } else {
    let from = actions.from;
    let to = actions.to;
    let from_dynamic = $object_cache.unwrap_action(from);
    let to_dynamic = $object_cache.unwrap_action(to);
    stop_action_ffi(from_dynamic);
    return stop_action_ffi(to_dynamic);
  }
}

/**
 * Setup a single animation
 * 
 * @ignore
 */
function setup_single_animation(mixer, anim) {
  let mixer_dynamic = $object_cache.unwrap_mixer(mixer);
  let clip_dynamic = clip_to_dynamic(anim.clip);
  let action_dynamic = clip_action_ffi(mixer_dynamic, clip_dynamic);
  let _block;
  let $ = anim.loop;
  if ($ instanceof $object3d.LoopOnce) {
    _block = get_loop_once_ffi();
  } else {
    _block = get_loop_repeat_ffi();
  }
  let loop_mode = _block;
  set_action_loop_ffi(action_dynamic, loop_mode);
  set_action_time_scale_ffi(action_dynamic, anim.speed);
  set_action_weight_ffi(action_dynamic, anim.weight);
  play_action_ffi(action_dynamic);
  return $object_cache.wrap_action(action_dynamic);
}

/**
 * Setup a single animation with custom weight
 * 
 * @ignore
 */
function setup_single_animation_with_weight(mixer, anim, weight) {
  let mixer_dynamic = $object_cache.unwrap_mixer(mixer);
  let clip_dynamic = clip_to_dynamic(anim.clip);
  let action_dynamic = clip_action_ffi(mixer_dynamic, clip_dynamic);
  let _block;
  let $ = anim.loop;
  if ($ instanceof $object3d.LoopOnce) {
    _block = get_loop_once_ffi();
  } else {
    _block = get_loop_repeat_ffi();
  }
  let loop_mode = _block;
  set_action_loop_ffi(action_dynamic, loop_mode);
  set_action_time_scale_ffi(action_dynamic, anim.speed);
  set_action_weight_ffi(action_dynamic, weight);
  play_action_ffi(action_dynamic);
  return $object_cache.wrap_action(action_dynamic);
}

/**
 * Setup animation for a Model3D node
 *
 * This handles both single animations and blended animations.
 * It stops any existing animations for this node and starts the new one(s).
 */
export function setup_animation(cache, id, mixer, animation) {
  let _block;
  let $ = $object_cache.get_actions(cache, id);
  if ($ instanceof $option.Some) {
    let actions = $[0];
    stop_actions(actions);
    _block = cache;
  } else {
    _block = cache;
  }
  let cache$1 = _block;
  if (animation instanceof $object3d.SingleAnimation) {
    let anim = animation[0];
    let action = setup_single_animation(mixer, anim);
    return $object_cache.set_actions(
      cache$1,
      id,
      new $object_cache.SingleAction(action),
    );
  } else {
    let from = animation.from;
    let to = animation.to;
    let blend_factor = animation.blend_factor;
    let from_action = setup_single_animation_with_weight(
      mixer,
      from,
      (1.0 - blend_factor) * from.weight,
    );
    let to_action = setup_single_animation_with_weight(
      mixer,
      to,
      blend_factor * to.weight,
    );
    return $object_cache.set_actions(
      cache$1,
      id,
      new $object_cache.BlendedActions(from_action, to_action),
    );
  }
}
