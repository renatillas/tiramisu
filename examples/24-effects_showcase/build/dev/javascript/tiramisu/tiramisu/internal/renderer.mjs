import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $vec3 from "../../../vec/vec/vec3.mjs";
import { CustomType as $CustomType, divideFloat } from "../../gleam.mjs";
import {
  identity as to_dynamic,
  identity as points_to_object3d,
  createRenderer as create_renderer_ffi,
  getRendererDomElement as get_dom_element,
  setCanvas as set_canvas,
  createScene as create_scene_ffi,
  render,
  addToScene as add_to_scene_ffi,
  removeFromScene as remove_from_scene_ffi,
  addChild as add_child_ffi,
  applyTransform as apply_transform_ffi,
  applyTransformWithQuaternion as apply_transform_with_quaternion_ffi,
  updateMatrixWorld as update_matrix_world_ffi,
  applyCameraLookAt as apply_camera_look_at_ffi,
  applyCameraLookAtDynamic as apply_camera_look_at_dynamic_ffi,
  createMesh as create_mesh_ffi,
  createInstancedMesh as create_instanced_mesh_ffi,
  createGroup as create_group_ffi,
  createLOD as create_lod_ffi,
  cloneObject as clone_object_ffi,
  setObjectGeometry as set_object_geometry_ffi,
  setObjectMaterial as set_object_material_ffi,
  getObjectGeometry as get_object_geometry_ffi,
  getObjectMaterial as get_object_material_ffi,
  setShadowProperties as set_shadow_properties_ffi,
  isPerspectiveCamera as is_perspective_camera_ffi,
  isOrthographicCamera as is_orthographic_camera_ffi,
  updateCameraProjectionMatrix as update_camera_projection_matrix_ffi,
  setCameraUserData as set_camera_user_data_ffi,
  getCameraUserData as get_camera_user_data_ffi,
  hasCameraUserData as has_camera_user_data_ffi,
  deleteCameraUserData as delete_camera_user_data_ffi,
  createPerspectiveCamera as create_perspective_camera_ffi,
  createOrthographicCamera as create_orthographic_camera_ffi,
  setActiveCamera as set_active_camera_ffi,
  getCanvasClientWidth as get_canvas_client_width_ffi,
  getCanvasClientHeight as get_canvas_client_height_ffi,
  getWindowWidth as get_window_width_ffi,
  getWindowHeight as get_window_height_ffi,
  createAnimationMixer as create_animation_mixer_ffi,
  updateMixer as update_animation_mixer_ffi,
  isInstancedMesh as is_instanced_mesh_ffi,
  updateInstancedMeshTransforms as update_instanced_mesh_transforms_ffi,
  isLOD as is_lod_ffi,
  addLODLevel as add_lod_level_ffi,
  clearLODLevels as clear_lod_levels_ffi,
  getObjectPosition as get_object_position_ffi,
  getObjectRotation as get_object_rotation_ffi,
  getObjectScale as get_object_scale_ffi,
  setObjectPosition as set_object_position_ffi,
  setObjectRotation as set_object_rotation_ffi,
  setObjectScale as set_object_scale_ffi,
  clipAction as create_animation_action_ffi,
  setActionLoop as set_animation_loop_ffi,
  setActionTimeScale as set_animation_time_scale_ffi,
  setActionWeight as set_animation_weight_ffi,
  playAction as play_animation_action_ffi,
  stopAction as stop_animation_action_ffi,
  getLoopRepeat as loop_repeat,
  getLoopOnce as loop_once,
} from "../../threejs.ffi.mjs";
import {
  playAudio as play_audio_ffi,
  updateAudioConfig as update_audio_config_ffi,
  getAudioSourceForCleanup as get_audio_source_for_cleanup_ffi,
  unregisterAudioSource as unregister_audio_source_ffi,
  getAudioListener as get_audio_listener_ffi,
  createDebugBox as create_debug_box_ffi,
  createDebugSphere as create_debug_sphere_ffi,
  createDebugLine as create_debug_line_ffi,
  createDebugAxes as create_debug_axes_ffi,
  createDebugGrid as create_debug_grid_ffi,
  createDebugPoint as create_debug_point_ffi,
  disposeGeometry as dispose_geometry_ffi,
  disposeMaterial as dispose_material_ffi,
} from "../../tiramisu.ffi.mjs";
import * as $audio from "../../tiramisu/audio.mjs";
import * as $camera from "../../tiramisu/camera.mjs";
import * as $geometry from "../../tiramisu/geometry.mjs";
import * as $object_cache from "../../tiramisu/internal/object_cache.mjs";
import * as $particle_manager from "../../tiramisu/internal/particle_manager.mjs";
import * as $light from "../../tiramisu/light.mjs";
import * as $material from "../../tiramisu/material.mjs";
import * as $object3d from "../../tiramisu/object3d.mjs";
import * as $particle_emitter from "../../tiramisu/particle_emitter.mjs";
import * as $physics from "../../tiramisu/physics.mjs";
import * as $scene from "../../tiramisu/scene.mjs";
import * as $transform from "../../tiramisu/transform.mjs";

export { get_dom_element, render, set_canvas };

export class Dimensions extends $CustomType {
  constructor(width, height) {
    super();
    this.width = width;
    this.height = height;
  }
}

export class RendererOptions extends $CustomType {
  constructor(antialias, alpha, dimensions) {
    super();
    this.antialias = antialias;
    this.alpha = alpha;
    this.dimensions = dimensions;
  }
}

export class RendererState extends $CustomType {
  constructor(renderer, scene, cache, physics_world, audio_manager) {
    super();
    this.renderer = renderer;
    this.scene = scene;
    this.cache = cache;
    this.physics_world = physics_world;
    this.audio_manager = audio_manager;
  }
}

/**
 * Create a new renderer with full state management
 */
export function create(options, audio_manager) {
  let renderer = create_renderer_ffi(options);
  let scene = create_scene_ffi();
  let canvas = get_dom_element(renderer);
  set_canvas(canvas);
  return new RendererState(
    renderer,
    scene,
    $object_cache.init(),
    new None(),
    audio_manager,
  );
}

/**
 * Get the WebGLRenderer from RendererState
 */
export function get_renderer(state) {
  return state.renderer;
}

/**
 * Get the Scene from RendererState
 */
export function get_scene(state) {
  return state.scene;
}

/**
 * Set the physics world for the renderer
 */
export function set_physics_world(state, world) {
  return new RendererState(
    state.renderer,
    state.scene,
    state.cache,
    world,
    state.audio_manager,
  );
}

/**
 * Get the physics world from the renderer
 */
export function get_physics_world(state) {
  return state.physics_world;
}

/**
 * Set the audio manager for the renderer
 */
export function set_audio_manager(state, audio_manager) {
  return new RendererState(
    state.renderer,
    state.scene,
    state.cache,
    state.physics_world,
    audio_manager,
  );
}

function add_to_scene_or_parent(state, object, parent_id) {
  let obj_dynamic = $object_cache.unwrap_object(object);
  if (parent_id instanceof Some) {
    let pid = parent_id[0];
    let $ = $object_cache.get_object(state.cache, pid);
    if ($ instanceof Some) {
      let parent_obj = $[0];
      let parent_dynamic = $object_cache.unwrap_object(parent_obj);
      return add_child_ffi(parent_dynamic, obj_dynamic);
    } else {
      return add_to_scene_ffi(state.scene, obj_dynamic);
    }
  } else {
    return add_to_scene_ffi(state.scene, obj_dynamic);
  }
}

function handle_add_mesh(
  state,
  id,
  geometry,
  material,
  transform,
  physics,
  parent_id
) {
  let geometry_three = $geometry.create_geometry(geometry);
  let material_three = $material.create_material(material);
  let mesh = create_mesh_ffi(geometry_three, material_three);
  apply_transform_ffi(mesh, transform);
  set_shadow_properties_ffi(mesh, true, true);
  let three_obj = $object_cache.wrap_object(mesh);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  let new_state = new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
  let $ = new_state.physics_world;
  if ($ instanceof Some && physics instanceof Some) {
    let world = $[0];
    let physics_config = physics[0];
    let new_world = $physics.create_body(world, id, physics_config, transform);
    return new RendererState(
      new_state.renderer,
      new_state.scene,
      new_state.cache,
      new Some(new_world),
      new_state.audio_manager,
    );
  } else {
    return new_state;
  }
}

function handle_add_instanced_mesh(
  state,
  id,
  geometry,
  material,
  instances,
  parent_id
) {
  let geometry_three = $geometry.create_geometry(geometry);
  let material_three = $material.create_material(material);
  let count = $list.length(instances);
  let mesh = create_instanced_mesh_ffi(geometry_three, material_three, count);
  update_instanced_mesh_transforms_ffi(mesh, instances);
  let three_obj = $object_cache.wrap_object(mesh);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_add_light(state, id, light, transform, parent_id) {
  let light$1 = $light.create_light(light);
  apply_transform_ffi(light$1, transform);
  let three_obj = $object_cache.wrap_object(light$1);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_add_group(state, id, transform, parent_id) {
  let group = create_group_ffi();
  apply_transform_ffi(group, transform);
  let three_obj = $object_cache.wrap_object(group);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function create_lod_level_object(node) {
  if (node instanceof $scene.Mesh) {
    let geometry = node.geometry;
    let material = node.material;
    let transform = node.transform;
    let geometry_three = $geometry.create_geometry(geometry);
    let material_three = $material.create_material(material);
    let mesh = create_mesh_ffi(geometry_three, material_three);
    apply_transform_ffi(mesh, transform);
    return mesh;
  } else if (node instanceof $scene.Group) {
    let transform = node.transform;
    let group = create_group_ffi();
    apply_transform_ffi(group, transform);
    return group;
  } else if (node instanceof $scene.Model3D) {
    let object = node.object;
    let transform = node.transform;
    let cloned = clone_object_ffi(object);
    apply_transform_ffi(cloned, transform);
    return cloned;
  } else {
    return create_group_ffi();
  }
}

function handle_add_lod(state, id, transform, levels, parent_id) {
  let lod = create_lod_ffi();
  apply_transform_ffi(lod, transform);
  $list.each(
    levels,
    (level) => {
      let level_obj = create_lod_level_object(level.node);
      return add_lod_level_ffi(lod, level_obj, level.distance);
    },
  );
  let three_obj = $object_cache.wrap_object(lod);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_add_audio(state, id, audio, parent_id) {
  if (audio instanceof $audio.GlobalAudio) {
    let buffer = audio.buffer;
    let config = audio.config;
    let buffer_dynamic = to_dynamic(buffer);
    let config_dynamic = to_dynamic(config);
    let id_dynamic = to_dynamic(id);
    play_audio_ffi(
      state.audio_manager,
      id_dynamic,
      buffer_dynamic,
      config_dynamic,
      audio,
    )
  } else {
    let buffer = audio.buffer;
    let config = audio.config;
    let buffer_dynamic = to_dynamic(buffer);
    let config_dynamic = to_dynamic(config);
    let id_dynamic = to_dynamic(id);
    play_audio_ffi(
      state.audio_manager,
      id_dynamic,
      buffer_dynamic,
      config_dynamic,
      audio,
    )
  }
  let placeholder = create_group_ffi();
  let three_obj = $object_cache.wrap_object(placeholder);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function calculate_aspect_ratio(viewport, canvas) {
  if (viewport instanceof Some) {
    let width = viewport[0][2];
    let height = viewport[0][3];
    return divideFloat($int.to_float(width), $int.to_float(height));
  } else {
    let canvas_width = get_canvas_client_width_ffi(canvas);
    let canvas_height = get_canvas_client_height_ffi(canvas);
    let $ = (canvas_width > 0.0) && (canvas_height > 0.0);
    if ($) {
      return divideFloat(canvas_width, canvas_height);
    } else {
      let window_width = get_window_width_ffi();
      let window_height = get_window_height_ffi();
      return divideFloat(window_width, window_height);
    }
  }
}

function handle_add_camera(
  state,
  id,
  camera_type,
  transform,
  look_at,
  active,
  viewport,
  parent_id
) {
  let canvas = get_dom_element(state.renderer);
  let aspect = calculate_aspect_ratio(viewport, canvas);
  let projection = $camera.get_projection(camera_type);
  let _block;
  if (projection instanceof $camera.Perspective) {
    let fov = projection.fov;
    let near = projection.near;
    let far = projection.far;
    _block = create_perspective_camera_ffi(fov, aspect, near, far);
  } else {
    let left = projection.left;
    let right = projection.right;
    let top = projection.top;
    let bottom = projection.bottom;
    let near = projection.near;
    let far = projection.far;
    _block = create_orthographic_camera_ffi(left, right, top, bottom, near, far);
  }
  let camera_obj = _block;
  let listener = get_audio_listener_ffi();
  add_child_ffi(camera_obj, listener);
  apply_transform_ffi(camera_obj, transform);
  if (look_at instanceof Some) {
    let target = look_at[0];
    set_camera_user_data_ffi(camera_obj, "lookAtTarget", to_dynamic(target));
    set_camera_user_data_ffi(camera_obj, "needsLookAtUpdate", to_dynamic(true))
  } else {
    undefined
  }
  update_camera_projection_matrix_ffi(camera_obj);
  let three_obj = $object_cache.wrap_object(camera_obj);
  add_to_scene_or_parent(state, three_obj, parent_id);
  if (look_at instanceof Some) {
    let target = look_at[0];
    apply_camera_look_at_ffi(camera_obj, target);
    delete_camera_user_data_ffi(camera_obj, "needsLookAtUpdate")
  } else {
    undefined
  }
  let _block$1;
  if (viewport instanceof Some) {
    let x = viewport[0][0];
    let y = viewport[0][1];
    let width = viewport[0][2];
    let height = viewport[0][3];
    let vp = new $object_cache.Viewport(
      $int.to_float(x),
      $int.to_float(y),
      $int.to_float(width),
      $int.to_float(height),
    );
    _block$1 = $object_cache.set_viewport(state.cache, id, vp);
  } else {
    _block$1 = state.cache;
  }
  let cache_with_viewport = _block$1;
  if (active) {
    set_active_camera_ffi(camera_obj)
  } else {
    undefined
  }
  let new_cache = $object_cache.add_object(cache_with_viewport, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_add_debug_box(state, id, min, max, color, parent_id) {
  let debug_obj = create_debug_box_ffi(min, max, color);
  let three_obj = $object_cache.wrap_object(debug_obj);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_add_debug_sphere(state, id, center, radius, color, parent_id) {
  let debug_obj = create_debug_sphere_ffi(center, radius, color);
  let three_obj = $object_cache.wrap_object(debug_obj);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_add_debug_line(state, id, from, to, color, parent_id) {
  let debug_obj = create_debug_line_ffi(from, to, color);
  let three_obj = $object_cache.wrap_object(debug_obj);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_add_debug_axes(state, id, origin, size, parent_id) {
  let debug_obj = create_debug_axes_ffi(origin, size);
  let three_obj = $object_cache.wrap_object(debug_obj);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_add_debug_grid(state, id, size, divisions, color, parent_id) {
  let debug_obj = create_debug_grid_ffi(size, divisions, color);
  let three_obj = $object_cache.wrap_object(debug_obj);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_add_debug_point(state, id, position, size, color, parent_id) {
  let debug_obj = create_debug_point_ffi(position, size, color);
  let three_obj = $object_cache.wrap_object(debug_obj);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(state.cache, id, three_obj);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_add_particles(state, id, emitter, transform, active, parent_id) {
  let _block;
  let _pipe = $particle_manager.create_particle_system(emitter, transform);
  _block = $particle_manager.set_active(_pipe, active);
  let particle_state = _block;
  let points_object = $particle_manager.get_points_object(particle_state);
  let points_obj3d = points_to_object3d(points_object);
  let three_obj = $object_cache.wrap_object(points_obj3d);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let _block$1;
  let _pipe$1 = $object_cache.add_object(state.cache, id, three_obj);
  _block$1 = $object_cache.add_particle_system(
    _pipe$1,
    id,
    $particle_manager.wrap_as_cache_entry(particle_state),
  );
  let new_cache = _block$1;
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

function handle_remove_node(state, id) {
  let $ = $object_cache.get_object(state.cache, id);
  if ($ instanceof Some) {
    let obj = $[0];
    let obj_dynamic = $object_cache.unwrap_object(obj);
    remove_from_scene_ffi(state.scene, obj_dynamic);
    let geometry = get_object_geometry_ffi(obj_dynamic);
    let material = get_object_material_ffi(obj_dynamic);
    dispose_geometry_ffi(geometry);
    dispose_material_ffi(material);
    let id_dynamic = to_dynamic(id);
    let audio_source = get_audio_source_for_cleanup_ffi(
      state.audio_manager,
      id_dynamic,
    );
    unregister_audio_source_ffi(state.audio_manager, id_dynamic)
    let new_cache = $object_cache.remove_all(state.cache, id);
    let new_state = new RendererState(
      state.renderer,
      state.scene,
      new_cache,
      state.physics_world,
      state.audio_manager,
    );
    let $1 = new_state.physics_world;
    if ($1 instanceof Some) {
      let world = $1[0];
      let new_world = $physics.remove_body(world, id);
      return new RendererState(
        new_state.renderer,
        new_state.scene,
        new_state.cache,
        new Some(new_world),
        new_state.audio_manager,
      );
    } else {
      return new_state;
    }
  } else {
    return state;
  }
}

function handle_update_transform(state, id, transform) {
  let $ = $object_cache.get_object(state.cache, id);
  if ($ instanceof Some) {
    let obj = $[0];
    let object = $object_cache.unwrap_object(obj);
    apply_transform_ffi(object, transform);
    update_matrix_world_ffi(object, true);
    let is_camera = is_perspective_camera_ffi(object) || is_orthographic_camera_ffi(
      object,
    );
    if (is_camera) {
      let $1 = has_camera_user_data_ffi(object, "lookAtTarget");
      if ($1) {
        let target = get_camera_user_data_ffi(object, "lookAtTarget");
        apply_camera_look_at_dynamic_ffi(object, target)
      } else {
        undefined
      }
    } else {
      undefined
    }
    return state;
  } else {
    return state;
  }
}

function handle_update_material(state, id, material) {
  let $ = $object_cache.get_object(state.cache, id);
  if ($ instanceof Some) {
    let obj = $[0];
    let obj_dynamic = $object_cache.unwrap_object(obj);
    let old_material = get_object_material_ffi(obj_dynamic);
    dispose_material_ffi(old_material);
    let new_material = $material.create_material(material);
    set_object_material_ffi(obj_dynamic, new_material);
    return state;
  } else {
    return state;
  }
}

function handle_update_geometry(state, id, geometry) {
  let $ = $object_cache.get_object(state.cache, id);
  if ($ instanceof Some) {
    let obj = $[0];
    let obj_dynamic = $object_cache.unwrap_object(obj);
    let old_geometry = get_object_geometry_ffi(obj_dynamic);
    dispose_geometry_ffi(old_geometry);
    let new_geometry = $geometry.create_geometry(geometry);
    set_object_geometry_ffi(obj_dynamic, new_geometry);
    return state;
  } else {
    return state;
  }
}

function handle_update_light(state, id, light) {
  let $ = $object_cache.get_object(state.cache, id);
  if ($ instanceof Some) {
    let old_obj = $[0];
    let old_object = $object_cache.unwrap_object(old_obj);
    let position = get_object_position_ffi(old_object);
    let rotation = get_object_rotation_ffi(old_object);
    let scale = get_object_scale_ffi(old_object);
    let new_light = $light.create_light(light);
    set_object_position_ffi(new_light, position);
    set_object_rotation_ffi(new_light, rotation);
    set_object_scale_ffi(new_light, scale);
    remove_from_scene_ffi(state.scene, old_object);
    add_to_scene_ffi(state.scene, new_light);
    let new_obj = $object_cache.wrap_object(new_light);
    let new_cache = $object_cache.add_object(state.cache, id, new_obj);
    return new RendererState(
      state.renderer,
      state.scene,
      new_cache,
      state.physics_world,
      state.audio_manager,
    );
  } else {
    return state;
  }
}

function dynamic_to_transform(position, rotation, scale) {
  
  
  
  return $transform.identity;
}

function handle_update_physics(state, id, physics) {
  let $ = state.physics_world;
  if ($ instanceof Some) {
    let world = $[0];
    let world_after_remove = $physics.remove_body(world, id);
    let _block;
    if (physics instanceof Some) {
      let physics_config = physics[0];
      let $1 = $object_cache.get_object(state.cache, id);
      if ($1 instanceof Some) {
        let obj = $1[0];
        let obj_dynamic = $object_cache.unwrap_object(obj);
        let position = get_object_position_ffi(obj_dynamic);
        let rotation = get_object_rotation_ffi(obj_dynamic);
        let scale = get_object_scale_ffi(obj_dynamic);
        let object_transform = dynamic_to_transform(position, rotation, scale);
        _block = $physics.create_body(
          world_after_remove,
          id,
          physics_config,
          object_transform,
        );
      } else {
        _block = world_after_remove;
      }
    } else {
      _block = world_after_remove;
    }
    let new_world = _block;
    return new RendererState(
      state.renderer,
      state.scene,
      state.cache,
      new Some(new_world),
      state.audio_manager,
    );
  } else {
    return state;
  }
}

function handle_update_audio(state, id, audio) {
  let _block;
  if (audio instanceof $audio.GlobalAudio) {
    let config = audio.config;
    _block = new Some(config);
  } else {
    let config = audio.config;
    _block = new Some(config);
  }
  let config = _block;
  if (config instanceof Some) {
    let audio_config = config[0];
    let id_dynamic = to_dynamic(id);
    let config_dynamic = to_dynamic(audio_config);
    update_audio_config_ffi(state.audio_manager, id_dynamic, config_dynamic);
    return state;
  } else {
    return state;
  }
}

function handle_update_instances(state, id, instances) {
  let $ = $object_cache.get_object(state.cache, id);
  if ($ instanceof Some) {
    let obj = $[0];
    let obj_dynamic = $object_cache.unwrap_object(obj);
    let $1 = is_instanced_mesh_ffi(obj_dynamic);
    if ($1) {
      update_instanced_mesh_transforms_ffi(obj_dynamic, instances);
      return state;
    } else {
      return state;
    }
  } else {
    return state;
  }
}

function handle_update_lod_levels(state, id, levels) {
  let $ = $object_cache.get_object(state.cache, id);
  if ($ instanceof Some) {
    let obj = $[0];
    let obj_dynamic = $object_cache.unwrap_object(obj);
    let $1 = is_lod_ffi(obj_dynamic);
    if ($1) {
      clear_lod_levels_ffi(obj_dynamic);
      $list.each(
        levels,
        (level) => {
          let level_obj = create_lod_level_object(level.node);
          return add_lod_level_ffi(obj_dynamic, level_obj, level.distance);
        },
      );
      return state;
    } else {
      return state;
    }
  } else {
    return state;
  }
}

function handle_update_camera(state, id, _, look_at) {
  let $ = $object_cache.get_object(state.cache, id);
  if ($ instanceof Some) {
    let obj = $[0];
    let obj_dynamic = $object_cache.unwrap_object(obj);
    let is_camera = is_perspective_camera_ffi(obj_dynamic) || is_orthographic_camera_ffi(
      obj_dynamic,
    );
    if (is_camera) {
      if (look_at instanceof Some) {
        let target = look_at[0];
        apply_camera_look_at_ffi(obj_dynamic, target)
      } else {
        undefined
      }
      update_camera_projection_matrix_ffi(obj_dynamic);
      return state;
    } else {
      return state;
    }
  } else {
    return state;
  }
}

function handle_set_active_camera(state, id) {
  let $ = $object_cache.get_object(state.cache, id);
  if ($ instanceof Some) {
    let obj = $[0];
    let obj_dynamic = $object_cache.unwrap_object(obj);
    set_active_camera_ffi(obj_dynamic);
    return state;
  } else {
    return state;
  }
}

function handle_update_particle_emitter(state, id, emitter) {
  let $ = $object_cache.get_particle_system(state.cache, id);
  if ($ instanceof Some) {
    let cached_system = $[0];
    let particle_state = $particle_manager.unwrap_from_cache_entry(
      cached_system,
    );
    let updated_state = $particle_manager.update_emitter(
      particle_state,
      emitter,
    );
    let wrapped_system = $particle_manager.wrap_as_cache_entry(updated_state);
    let new_cache = $object_cache.add_particle_system(
      state.cache,
      id,
      wrapped_system,
    );
    return new RendererState(
      state.renderer,
      state.scene,
      new_cache,
      state.physics_world,
      state.audio_manager,
    );
  } else {
    return state;
  }
}

function handle_update_particle_active(state, id, active) {
  let $ = $object_cache.get_particle_system(state.cache, id);
  if ($ instanceof Some) {
    let cached_system = $[0];
    let particle_state = $particle_manager.unwrap_from_cache_entry(
      cached_system,
    );
    let updated_state = $particle_manager.set_active(particle_state, active);
    let wrapped_system = $particle_manager.wrap_as_cache_entry(updated_state);
    let new_cache = $object_cache.add_particle_system(
      state.cache,
      id,
      wrapped_system,
    );
    return new RendererState(
      state.renderer,
      state.scene,
      new_cache,
      state.physics_world,
      state.audio_manager,
    );
  } else {
    return state;
  }
}

function adjust_action_weight(action, weight) {
  let action_dynamic = $object_cache.unwrap_action(action);
  return set_animation_weight_ffi(action_dynamic, weight);
}

function stop_actions(actions) {
  if (actions instanceof $object_cache.SingleAction) {
    let action = actions[0];
    let action_dynamic = $object_cache.unwrap_action(action);
    return stop_animation_action_ffi(action_dynamic);
  } else {
    let from_action = actions.from;
    let to_action = actions.to;
    let from_dynamic = $object_cache.unwrap_action(from_action);
    let to_dynamic$1 = $object_cache.unwrap_action(to_action);
    stop_animation_action_ffi(from_dynamic);
    return stop_animation_action_ffi(to_dynamic$1);
  }
}

function create_animation_action(mixer, animation) {
  let mixer_dynamic = $object_cache.unwrap_mixer(mixer);
  let clip_dynamic = to_dynamic(animation.clip);
  let action_dynamic = create_animation_action_ffi(mixer_dynamic, clip_dynamic);
  let _block;
  let $ = animation.loop;
  if ($ instanceof $object3d.LoopOnce) {
    _block = loop_once();
  } else {
    _block = loop_repeat();
  }
  let loop_mode = _block;
  set_animation_loop_ffi(action_dynamic, loop_mode);
  set_animation_time_scale_ffi(action_dynamic, animation.speed);
  set_animation_weight_ffi(action_dynamic, animation.weight);
  play_animation_action_ffi(action_dynamic);
  return $object_cache.wrap_action(action_dynamic);
}

function setup_animation(cache, id, mixer, playback) {
  let $ = $object_cache.get_actions(cache, id);
  if ($ instanceof Some) {
    let actions = $[0];
    stop_actions(actions)
  } else {
    undefined
  }
  if (playback instanceof $object3d.SingleAnimation) {
    let anim = playback[0];
    let action = create_animation_action(mixer, anim);
    let actions = new $object_cache.SingleAction(action);
    return $object_cache.set_actions(cache, id, actions);
  } else {
    let from_anim = playback.from;
    let to_anim = playback.to;
    let blend_factor = playback.blend_factor;
    let from_action = create_animation_action(mixer, from_anim);
    let to_action = create_animation_action(mixer, to_anim);
    adjust_action_weight(from_action, (1.0 - blend_factor) * from_anim.weight);
    adjust_action_weight(to_action, blend_factor * to_anim.weight);
    let actions = new $object_cache.BlendedActions(from_action, to_action);
    return $object_cache.set_actions(cache, id, actions);
  }
}

function handle_add_model3d(
  state,
  id,
  object,
  transform,
  animation,
  physics,
  parent_id
) {
  apply_transform_ffi(object, transform);
  let mixer_dynamic = create_animation_mixer_ffi(object);
  let mixer = $object_cache.wrap_mixer(mixer_dynamic);
  let cache_with_mixer = $object_cache.add_mixer(state.cache, id, mixer);
  let _block;
  if (animation instanceof Some) {
    let anim_playback = animation[0];
    _block = setup_animation(cache_with_mixer, id, mixer, anim_playback);
  } else {
    _block = cache_with_mixer;
  }
  let cache_with_animation = _block;
  let three_obj = $object_cache.wrap_object(object);
  add_to_scene_or_parent(state, three_obj, parent_id);
  let new_cache = $object_cache.add_object(cache_with_animation, id, three_obj);
  let new_state = new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
  let $ = new_state.physics_world;
  if ($ instanceof Some && physics instanceof Some) {
    let world = $[0];
    let physics_config = physics[0];
    let new_world = $physics.create_body(world, id, physics_config, transform);
    return new RendererState(
      new_state.renderer,
      new_state.scene,
      new_state.cache,
      new Some(new_world),
      new_state.audio_manager,
    );
  } else {
    return new_state;
  }
}

function handle_add_node(state, id, node, parent_id) {
  if (node instanceof $scene.Mesh) {
    let geometry = node.geometry;
    let material = node.material;
    let transform = node.transform;
    let physics = node.physics;
    return handle_add_mesh(
      state,
      id,
      geometry,
      material,
      transform,
      physics,
      parent_id,
    );
  } else if (node instanceof $scene.InstancedMesh) {
    let geometry = node.geometry;
    let material = node.material;
    let instances = node.instances;
    return handle_add_instanced_mesh(
      state,
      id,
      geometry,
      material,
      instances,
      parent_id,
    );
  } else if (node instanceof $scene.Group) {
    let transform = node.transform;
    return handle_add_group(state, id, transform, parent_id);
  } else if (node instanceof $scene.Light) {
    let light = node.light;
    let transform = node.transform;
    return handle_add_light(state, id, light, transform, parent_id);
  } else if (node instanceof $scene.Camera) {
    let camera = node.camera;
    let transform = node.transform;
    let look_at = node.look_at;
    let active = node.active;
    let viewport = node.viewport;
    return handle_add_camera(
      state,
      id,
      camera,
      transform,
      look_at,
      active,
      viewport,
      parent_id,
    );
  } else if (node instanceof $scene.LOD) {
    let levels = node.levels;
    let transform = node.transform;
    return handle_add_lod(state, id, transform, levels, parent_id);
  } else if (node instanceof $scene.Model3D) {
    let object = node.object;
    let transform = node.transform;
    let animation = node.animation;
    let physics = node.physics;
    return handle_add_model3d(
      state,
      id,
      object,
      transform,
      animation,
      physics,
      parent_id,
    );
  } else if (node instanceof $scene.Audio) {
    let audio = node.audio;
    return handle_add_audio(state, id, audio, parent_id);
  } else if (node instanceof $scene.Particles) {
    let emitter = node.emitter;
    let transform = node.transform;
    let active = node.active;
    return handle_add_particles(
      state,
      id,
      emitter,
      transform,
      active,
      parent_id,
    );
  } else if (node instanceof $scene.DebugBox) {
    let min = node.min;
    let max = node.max;
    let color = node.color;
    return handle_add_debug_box(state, id, min, max, color, parent_id);
  } else if (node instanceof $scene.DebugSphere) {
    let center = node.center;
    let radius = node.radius;
    let color = node.color;
    return handle_add_debug_sphere(state, id, center, radius, color, parent_id);
  } else if (node instanceof $scene.DebugLine) {
    let from = node.from;
    let to = node.to;
    let color = node.color;
    return handle_add_debug_line(state, id, from, to, color, parent_id);
  } else if (node instanceof $scene.DebugAxes) {
    let origin = node.origin;
    let size = node.size;
    return handle_add_debug_axes(state, id, origin, size, parent_id);
  } else if (node instanceof $scene.DebugGrid) {
    let size = node.size;
    let divisions = node.divisions;
    let color = node.color;
    return handle_add_debug_grid(state, id, size, divisions, color, parent_id);
  } else {
    let position = node.position;
    let size = node.size;
    let color = node.color;
    return handle_add_debug_point(state, id, position, size, color, parent_id);
  }
}

function handle_update_animation(state, id, animation) {
  let $ = $object_cache.get_mixer(state.cache, id);
  if ($ instanceof Some) {
    let mixer = $[0];
    if (animation instanceof Some) {
      let anim_playback = animation[0];
      let new_cache = setup_animation(state.cache, id, mixer, anim_playback);
      return new RendererState(
        state.renderer,
        state.scene,
        new_cache,
        state.physics_world,
        state.audio_manager,
      );
    } else {
      let $1 = $object_cache.get_actions(state.cache, id);
      if ($1 instanceof Some) {
        let actions = $1[0];
        stop_actions(actions);
        let new_cache = $object_cache.remove_actions(state.cache, id);
        return new RendererState(
          state.renderer,
          state.scene,
          new_cache,
          state.physics_world,
          state.audio_manager,
        );
      } else {
        return state;
      }
    }
  } else {
    return state;
  }
}

/**
 * Apply a single patch to the scene
 */
export function apply_patch(state, patch) {
  if (patch instanceof $scene.AddNode) {
    let id = patch.id;
    let node = patch.node;
    let parent_id = patch.parent_id;
    return handle_add_node(state, id, node, parent_id);
  } else if (patch instanceof $scene.RemoveNode) {
    let id = patch.id;
    return handle_remove_node(state, id);
  } else if (patch instanceof $scene.UpdateTransform) {
    let id = patch.id;
    let transform = patch.transform;
    return handle_update_transform(state, id, transform);
  } else if (patch instanceof $scene.UpdateMaterial) {
    let id = patch.id;
    let material = patch.material;
    return handle_update_material(state, id, material);
  } else if (patch instanceof $scene.UpdateGeometry) {
    let id = patch.id;
    let geometry = patch.geometry;
    return handle_update_geometry(state, id, geometry);
  } else if (patch instanceof $scene.UpdateLight) {
    let id = patch.id;
    let light = patch.light;
    return handle_update_light(state, id, light);
  } else if (patch instanceof $scene.UpdateAnimation) {
    let id = patch.id;
    let animation = patch.animation;
    return handle_update_animation(state, id, animation);
  } else if (patch instanceof $scene.UpdatePhysics) {
    let id = patch.id;
    let physics = patch.physics;
    return handle_update_physics(state, id, physics);
  } else if (patch instanceof $scene.UpdateAudio) {
    let id = patch.id;
    let audio = patch.audio;
    return handle_update_audio(state, id, audio);
  } else if (patch instanceof $scene.UpdateInstances) {
    let id = patch.id;
    let instances = patch.instances;
    return handle_update_instances(state, id, instances);
  } else if (patch instanceof $scene.UpdateLODLevels) {
    let id = patch.id;
    let levels = patch.levels;
    return handle_update_lod_levels(state, id, levels);
  } else if (patch instanceof $scene.UpdateCamera) {
    let id = patch.id;
    let camera_type = patch.camera_type;
    let look_at = patch.look_at;
    return handle_update_camera(state, id, camera_type, look_at);
  } else if (patch instanceof $scene.SetActiveCamera) {
    let id = patch.id;
    return handle_set_active_camera(state, id);
  } else if (patch instanceof $scene.UpdateParticleEmitter) {
    let id = patch.id;
    let emitter = patch.emitter;
    return handle_update_particle_emitter(state, id, emitter);
  } else {
    let id = patch.id;
    let active = patch.active;
    return handle_update_particle_active(state, id, active);
  }
}

/**
 * Apply multiple patches to update the scene
 */
export function apply_patches(state, patches) {
  return $list.fold(
    patches,
    state,
    (st, patch) => { return apply_patch(st, patch); },
  );
}

/**
 * Update all animation mixers with delta time
 */
export function update_mixers(state, delta_time) {
  let mixers = $object_cache.get_all_mixers(state.cache);
  return $list.each(
    mixers,
    (entry) => {
      let mixer;
      mixer = entry[1];
      let mixer_dynamic = $object_cache.unwrap_mixer(mixer);
      return update_animation_mixer_ffi(mixer_dynamic, delta_time);
    },
  );
}

/**
 * Update all particle systems with delta time
 */
export function update_particle_systems(state, delta_time) {
  let particle_systems = $object_cache.get_all_particle_systems(state.cache);
  let updated_cache = $list.fold(
    particle_systems,
    state.cache,
    (cache, entry) => {
      let string_id;
      let cached_system;
      string_id = entry[0];
      cached_system = entry[1];
      let particle_state = $particle_manager.unwrap_from_cache_entry(
        cached_system,
      );
      let updated_state = $particle_manager.update_particles(
        particle_state,
        delta_time,
      );
      let wrapped_system = $particle_manager.wrap_as_cache_entry(updated_state);
      let new_particles = $dict.insert(
        cache.particles,
        string_id,
        wrapped_system,
      );
      return new $object_cache.CacheState(
        cache.objects,
        cache.mixers,
        cache.actions,
        cache.viewports,
        new_particles,
      );
    },
  );
  return new RendererState(
    state.renderer,
    state.scene,
    updated_cache,
    state.physics_world,
    state.audio_manager,
  );
}

/**
 * Sync physics body transforms to Three.js objects
 *
 * Uses quaternions directly from Rapier to avoid rotation errors
 * caused by quaternion-to-Euler-to-quaternion conversion.
 */
export function sync_physics_transforms(state) {
  let $ = state.physics_world;
  if ($ instanceof Some) {
    let world = $[0];
    return $physics.for_each_body_raw(
      world,
      (id, position, quaternion) => {
        let $1 = $object_cache.get_object(state.cache, id);
        if ($1 instanceof Some) {
          let obj = $1[0];
          let obj_dynamic = $object_cache.unwrap_object(obj);
          let scale = new $vec3.Vec3(1.0, 1.0, 1.0);
          apply_transform_with_quaternion_ffi(
            obj_dynamic,
            to_dynamic(position),
            to_dynamic(quaternion),
            to_dynamic(scale),
          );
          return update_matrix_world_ffi(obj_dynamic, true);
        } else {
          return undefined;
        }
      },
    );
  } else {
    return undefined;
  }
}

/**
 * Clear all caches and dispose resources
 */
export function clear_cache(state) {
  let objects = $object_cache.get_all_objects(state.cache);
  $list.each(
    objects,
    (entry) => {
      let obj;
      obj = entry[1];
      let obj_dynamic = $object_cache.unwrap_object(obj);
      let geometry = get_object_geometry_ffi(obj_dynamic);
      let material = get_object_material_ffi(obj_dynamic);
      dispose_geometry_ffi(geometry);
      return dispose_material_ffi(material);
    },
  );
  let new_cache = $object_cache.clear(state.cache);
  return new RendererState(
    state.renderer,
    state.scene,
    new_cache,
    state.physics_world,
    state.audio_manager,
  );
}

/**
 * Get all cameras with viewports for multi-viewport rendering
 * Returns list of (camera, viewport) tuples
 */
export function get_cameras_with_viewports(state) {
  let _pipe = $object_cache.get_cameras_with_viewports(state.cache);
  return $list.map(
    _pipe,
    (entry) => {
      let camera_obj;
      let viewport;
      camera_obj = entry[0];
      viewport = entry[1];
      return [$object_cache.unwrap_object(camera_obj), viewport];
    },
  );
}
