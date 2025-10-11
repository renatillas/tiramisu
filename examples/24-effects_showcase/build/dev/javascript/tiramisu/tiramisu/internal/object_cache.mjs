import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Ok, Error, CustomType as $CustomType } from "../../gleam.mjs";
import * as $id from "../../tiramisu/internal/id.mjs";
import * as $object3d from "../../tiramisu/object3d.mjs";

class ThreeObject extends $CustomType {
  constructor(object) {
    super();
    this.object = object;
  }
}

class AnimationMixer extends $CustomType {
  constructor(mixer) {
    super();
    this.mixer = mixer;
  }
}

class AnimationAction extends $CustomType {
  constructor(action) {
    super();
    this.action = action;
  }
}

export class SingleAction extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class BlendedActions extends $CustomType {
  constructor(from, to) {
    super();
    this.from = from;
    this.to = to;
  }
}

class ParticleSystem extends $CustomType {
  constructor(data) {
    super();
    this.data = data;
  }
}

export class Viewport extends $CustomType {
  constructor(x, y, width, height) {
    super();
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }
}

export class CacheState extends $CustomType {
  constructor(objects, mixers, actions, viewports, particles) {
    super();
    this.objects = objects;
    this.mixers = mixers;
    this.actions = actions;
    this.viewports = viewports;
    this.particles = particles;
  }
}

/**
 * Create an empty cache state
 */
export function init() {
  return new CacheState(
    $dict.new$(),
    $dict.new$(),
    $dict.new$(),
    $dict.new$(),
    $dict.new$(),
  );
}

/**
 * Add a Three.js object to the cache
 */
export function add_object(cache, id, object) {
  let string_id = $id.to_string(id);
  return new CacheState(
    $dict.insert(cache.objects, string_id, object),
    cache.mixers,
    cache.actions,
    cache.viewports,
    cache.particles,
  );
}

/**
 * Get a Three.js object from the cache
 */
export function get_object(cache, id) {
  let string_id = $id.to_string(id);
  let _pipe = $dict.get(cache.objects, string_id);
  return $option.from_result(_pipe);
}

/**
 * Remove a Three.js object from the cache
 */
export function remove_object(cache, id) {
  let string_id = $id.to_string(id);
  return new CacheState(
    $dict.delete$(cache.objects, string_id),
    cache.mixers,
    cache.actions,
    cache.viewports,
    cache.particles,
  );
}

/**
 * Get all cached objects as a list of (ID, Object) tuples
 */
export function get_all_objects(cache) {
  return $dict.to_list(cache.objects);
}

/**
 * Add an animation mixer to the cache
 */
export function add_mixer(cache, id, mixer) {
  let string_id = $id.to_string(id);
  return new CacheState(
    cache.objects,
    $dict.insert(cache.mixers, string_id, mixer),
    cache.actions,
    cache.viewports,
    cache.particles,
  );
}

/**
 * Get an animation mixer from the cache
 */
export function get_mixer(cache, id) {
  let string_id = $id.to_string(id);
  let _pipe = $dict.get(cache.mixers, string_id);
  return $option.from_result(_pipe);
}

/**
 * Remove an animation mixer from the cache
 */
export function remove_mixer(cache, id) {
  let string_id = $id.to_string(id);
  return new CacheState(
    cache.objects,
    $dict.delete$(cache.mixers, string_id),
    cache.actions,
    cache.viewports,
    cache.particles,
  );
}

/**
 * Get all mixers as a list
 */
export function get_all_mixers(cache) {
  return $dict.to_list(cache.mixers);
}

/**
 * Set the current animation actions for a node
 */
export function set_actions(cache, id, actions) {
  let string_id = $id.to_string(id);
  return new CacheState(
    cache.objects,
    cache.mixers,
    $dict.insert(cache.actions, string_id, actions),
    cache.viewports,
    cache.particles,
  );
}

/**
 * Get the current animation actions for a node
 */
export function get_actions(cache, id) {
  let string_id = $id.to_string(id);
  let _pipe = $dict.get(cache.actions, string_id);
  return $option.from_result(_pipe);
}

/**
 * Remove animation actions for a node
 */
export function remove_actions(cache, id) {
  let string_id = $id.to_string(id);
  return new CacheState(
    cache.objects,
    cache.mixers,
    $dict.delete$(cache.actions, string_id),
    cache.viewports,
    cache.particles,
  );
}

/**
 * Set viewport configuration for a camera
 */
export function set_viewport(cache, id, viewport) {
  let string_id = $id.to_string(id);
  return new CacheState(
    cache.objects,
    cache.mixers,
    cache.actions,
    $dict.insert(cache.viewports, string_id, viewport),
    cache.particles,
  );
}

/**
 * Get viewport configuration for a camera
 */
export function get_viewport(cache, id) {
  let string_id = $id.to_string(id);
  let _pipe = $dict.get(cache.viewports, string_id);
  return $option.from_result(_pipe);
}

/**
 * Remove viewport configuration for a camera
 */
export function remove_viewport(cache, id) {
  let string_id = $id.to_string(id);
  return new CacheState(
    cache.objects,
    cache.mixers,
    cache.actions,
    $dict.delete$(cache.viewports, string_id),
    cache.particles,
  );
}

/**
 * Get all cameras with viewports
 */
export function get_cameras_with_viewports(cache) {
  let _pipe = $dict.to_list(cache.viewports);
  return $list.filter_map(
    _pipe,
    (entry) => {
      let id;
      let viewport;
      id = entry[0];
      viewport = entry[1];
      let $ = $dict.get(cache.objects, id);
      if ($ instanceof Ok) {
        let camera_obj = $[0];
        return new Ok([camera_obj, viewport]);
      } else {
        return new Error(undefined);
      }
    },
  );
}

/**
 * Add a particle system to the cache
 */
export function add_particle_system(cache, id, system) {
  let string_id = $id.to_string(id);
  return new CacheState(
    cache.objects,
    cache.mixers,
    cache.actions,
    cache.viewports,
    $dict.insert(cache.particles, string_id, system),
  );
}

/**
 * Get a particle system from the cache
 */
export function get_particle_system(cache, id) {
  let string_id = $id.to_string(id);
  let _pipe = $dict.get(cache.particles, string_id);
  return $option.from_result(_pipe);
}

/**
 * Remove a particle system from the cache
 */
export function remove_particle_system(cache, id) {
  let string_id = $id.to_string(id);
  return new CacheState(
    cache.objects,
    cache.mixers,
    cache.actions,
    cache.viewports,
    $dict.delete$(cache.particles, string_id),
  );
}

/**
 * Get all particle systems
 */
export function get_all_particle_systems(cache) {
  return $dict.to_list(cache.particles);
}

/**
 * Remove all cached data for a given ID (object, mixer, actions, viewport, particles)
 * This is used when a node is removed from the scene
 */
export function remove_all(cache, id) {
  let _pipe = cache;
  let _pipe$1 = remove_object(_pipe, id);
  let _pipe$2 = remove_mixer(_pipe$1, id);
  let _pipe$3 = remove_actions(_pipe$2, id);
  let _pipe$4 = remove_viewport(_pipe$3, id);
  return remove_particle_system(_pipe$4, id);
}

/**
 * Clear all caches
 */
export function clear(_) {
  return init();
}

/**
 * Unwrap a ThreeObject to access the underlying Dynamic value
 * 
 * @ignore
 */
export function unwrap_object(object) {
  return object.object;
}

/**
 * Unwrap an AnimationMixer to access the underlying Dynamic value
 * 
 * @ignore
 */
export function unwrap_mixer(mixer) {
  return mixer.mixer;
}

/**
 * Unwrap an AnimationAction to access the underlying Dynamic value
 * 
 * @ignore
 */
export function unwrap_action(action) {
  return action.action;
}

/**
 * Unwrap a ParticleSystem to access the underlying Dynamic value
 * 
 * @ignore
 */
export function unwrap_particle_system(system) {
  return system.data;
}

/**
 * Wrap a Dynamic value as a ThreeObject
 * 
 * @ignore
 */
export function wrap_object(object) {
  return new ThreeObject(object);
}

/**
 * Wrap a Dynamic value as an AnimationMixer
 * 
 * @ignore
 */
export function wrap_mixer(mixer) {
  return new AnimationMixer(mixer);
}

/**
 * Wrap a Dynamic value as an AnimationAction
 * 
 * @ignore
 */
export function wrap_action(action) {
  return new AnimationAction(action);
}

/**
 * Wrap a Dynamic value as a ParticleSystem
 * 
 * @ignore
 */
export function wrap_particle_system(data) {
  return new ParticleSystem(data);
}
