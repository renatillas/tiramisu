import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import * as $vec3 from "../../vec/vec/vec3.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";
import * as $audio from "../tiramisu/audio.mjs";
import * as $camera from "../tiramisu/camera.mjs";
import * as $geometry from "../tiramisu/geometry.mjs";
import * as $light from "../tiramisu/light.mjs";
import * as $material from "../tiramisu/material.mjs";
import * as $object3d from "../tiramisu/object3d.mjs";
import * as $particle_emitter from "../tiramisu/particle_emitter.mjs";
import * as $physics from "../tiramisu/physics.mjs";
import * as $transform from "../tiramisu/transform.mjs";

export class LODLevel extends $CustomType {
  constructor(distance, node) {
    super();
    this.distance = distance;
    this.node = node;
  }
}

export class Mesh extends $CustomType {
  constructor(id, geometry, material, transform, physics) {
    super();
    this.id = id;
    this.geometry = geometry;
    this.material = material;
    this.transform = transform;
    this.physics = physics;
  }
}

/**
 * Instanced mesh - renders many copies of the same geometry/material with 1 draw call
 * Much more efficient than creating individual Mesh nodes for identical objects
 */
export class InstancedMesh extends $CustomType {
  constructor(id, geometry, material, instances) {
    super();
    this.id = id;
    this.geometry = geometry;
    this.material = material;
    this.instances = instances;
  }
}

export class Group extends $CustomType {
  constructor(id, transform, children) {
    super();
    this.id = id;
    this.transform = transform;
    this.children = children;
  }
}

export class Light extends $CustomType {
  constructor(id, light, transform) {
    super();
    this.id = id;
    this.light = light;
    this.transform = transform;
  }
}

/**
 * Camera - defines a viewpoint in the scene
 * Only one camera can be active at a time for rendering (when viewport is None)
 * Use effect.set_active_camera(id) to switch between cameras
 * Set viewport to render in a specific area (for picture-in-picture effects)
 */
export class Camera extends $CustomType {
  constructor(id, camera, transform, look_at, active, viewport) {
    super();
    this.id = id;
    this.camera = camera;
    this.transform = transform;
    this.look_at = look_at;
    this.active = active;
    this.viewport = viewport;
  }
}

/**
 * Level of Detail - automatically switches between different meshes based on camera distance
 * Levels should be ordered from closest (distance: 0.0) to farthest
 */
export class LOD extends $CustomType {
  constructor(id, levels, transform) {
    super();
    this.id = id;
    this.levels = levels;
    this.transform = transform;
  }
}

export class Model3D extends $CustomType {
  constructor(id, object, transform, animation, physics) {
    super();
    this.id = id;
    this.object = object;
    this.transform = transform;
    this.animation = animation;
    this.physics = physics;
  }
}

export class Audio extends $CustomType {
  constructor(id, audio) {
    super();
    this.id = id;
    this.audio = audio;
  }
}

/**
 * Particle system - spawn and animate many small particles for visual effects
 * Particles are simulated in the FFI layer and rendered efficiently using Three.js Points
 */
export class Particles extends $CustomType {
  constructor(id, emitter, transform, active) {
    super();
    this.id = id;
    this.emitter = emitter;
    this.transform = transform;
    this.active = active;
  }
}

export class DebugBox extends $CustomType {
  constructor(id, min, max, color) {
    super();
    this.id = id;
    this.min = min;
    this.max = max;
    this.color = color;
  }
}

export class DebugSphere extends $CustomType {
  constructor(id, center, radius, color) {
    super();
    this.id = id;
    this.center = center;
    this.radius = radius;
    this.color = color;
  }
}

export class DebugLine extends $CustomType {
  constructor(id, from, to, color) {
    super();
    this.id = id;
    this.from = from;
    this.to = to;
    this.color = color;
  }
}

export class DebugAxes extends $CustomType {
  constructor(id, origin, size) {
    super();
    this.id = id;
    this.origin = origin;
    this.size = size;
  }
}

export class DebugGrid extends $CustomType {
  constructor(id, size, divisions, color) {
    super();
    this.id = id;
    this.size = size;
    this.divisions = divisions;
    this.color = color;
  }
}

export class DebugPoint extends $CustomType {
  constructor(id, position, size, color) {
    super();
    this.id = id;
    this.position = position;
    this.size = size;
    this.color = color;
  }
}

export class AddNode extends $CustomType {
  constructor(id, node, parent_id) {
    super();
    this.id = id;
    this.node = node;
    this.parent_id = parent_id;
  }
}

export class RemoveNode extends $CustomType {
  constructor(id) {
    super();
    this.id = id;
  }
}

export class UpdateTransform extends $CustomType {
  constructor(id, transform) {
    super();
    this.id = id;
    this.transform = transform;
  }
}

export class UpdateMaterial extends $CustomType {
  constructor(id, material) {
    super();
    this.id = id;
    this.material = material;
  }
}

export class UpdateGeometry extends $CustomType {
  constructor(id, geometry) {
    super();
    this.id = id;
    this.geometry = geometry;
  }
}

export class UpdateLight extends $CustomType {
  constructor(id, light) {
    super();
    this.id = id;
    this.light = light;
  }
}

export class UpdateAnimation extends $CustomType {
  constructor(id, animation) {
    super();
    this.id = id;
    this.animation = animation;
  }
}

export class UpdatePhysics extends $CustomType {
  constructor(id, physics) {
    super();
    this.id = id;
    this.physics = physics;
  }
}

export class UpdateAudio extends $CustomType {
  constructor(id, audio) {
    super();
    this.id = id;
    this.audio = audio;
  }
}

export class UpdateInstances extends $CustomType {
  constructor(id, instances) {
    super();
    this.id = id;
    this.instances = instances;
  }
}

export class UpdateLODLevels extends $CustomType {
  constructor(id, levels) {
    super();
    this.id = id;
    this.levels = levels;
  }
}

export class UpdateCamera extends $CustomType {
  constructor(id, camera_type, look_at) {
    super();
    this.id = id;
    this.camera_type = camera_type;
    this.look_at = look_at;
  }
}

export class SetActiveCamera extends $CustomType {
  constructor(id) {
    super();
    this.id = id;
  }
}

export class UpdateParticleEmitter extends $CustomType {
  constructor(id, emitter) {
    super();
    this.id = id;
    this.emitter = emitter;
  }
}

export class UpdateParticleActive extends $CustomType {
  constructor(id, active) {
    super();
    this.id = id;
    this.active = active;
  }
}

class NodeWithParent extends $CustomType {
  constructor(node, parent_id) {
    super();
    this.node = node;
    this.parent_id = parent_id;
  }
}

/**
 * Create an LOD level with a distance threshold and scene node.
 *
 * Levels should be ordered from closest (distance: 0.0) to farthest.
 *
 * ## Example
 *
 * ```gleam
 * let high_detail = scene.lod_level(distance: 0.0, node: detailed_mesh)
 * let low_detail = scene.lod_level(distance: 100.0, node: simple_mesh)
 * ```
 */
export function lod_level(distance, node) {
  return new LODLevel(distance, node);
}

function flatten_scene_helper(nodes, parent_id, acc) {
  return $list.fold(
    nodes,
    acc,
    (acc, node) => {
      let acc$1 = $dict.insert(
        acc,
        node.id,
        new NodeWithParent(node, parent_id),
      );
      if (node instanceof Group) {
        let children = node.children;
        return flatten_scene_helper(children, new $option.Some(node.id), acc$1);
      } else {
        return acc$1;
      }
    },
  );
}

function flatten_scene(nodes) {
  return flatten_scene_helper(nodes, new $option.None(), $dict.new$());
}

/**
 * Efficiently concatenate multiple lists using fold + prepend
 * O(n) total instead of list.flatten's O(n * m)
 * 
 * @ignore
 */
function concat_patches(lists) {
  let _pipe = $list.fold(
    lists,
    toList([]),
    (acc, patches) => {
      return $list.fold(
        patches,
        acc,
        (acc2, patch) => { return listPrepend(patch, acc2); },
      );
    },
  );
  return $list.reverse(_pipe);
}

/**
 * Batch patches by type for optimal rendering order
 * Optimized: Single-pass partitioning + manual concatenation (no list.flatten)
 * 
 * @ignore
 */
function batch_patches(removals, parent_change_removals, updates, additions) {
  let $ = $list.fold(
    updates,
    [toList([]), toList([]), toList([]), toList([])],
    (acc, patch) => {
      let transforms;
      let materials;
      let geometries;
      let misc;
      transforms = acc[0];
      materials = acc[1];
      geometries = acc[2];
      misc = acc[3];
      if (patch instanceof UpdateTransform) {
        return [listPrepend(patch, transforms), materials, geometries, misc];
      } else if (patch instanceof UpdateMaterial) {
        return [transforms, listPrepend(patch, materials), geometries, misc];
      } else if (patch instanceof UpdateGeometry) {
        return [transforms, materials, listPrepend(patch, geometries), misc];
      } else {
        return [transforms, materials, geometries, listPrepend(patch, misc)];
      }
    },
  );
  let transform_updates;
  let material_updates;
  let geometry_updates;
  let misc_updates;
  transform_updates = $[0];
  material_updates = $[1];
  geometry_updates = $[2];
  misc_updates = $[3];
  return concat_patches(
    toList([
      removals,
      parent_change_removals,
      $list.reverse(transform_updates),
      $list.reverse(material_updates),
      $list.reverse(geometry_updates),
      $list.reverse(misc_updates),
      additions,
    ]),
  );
}

/**
 * Calculate the depth of a node in the hierarchy (0 = root)
 * 
 * @ignore
 */
function calculate_depth(loop$parent_id, loop$node_dict, loop$current_depth) {
  while (true) {
    let parent_id = loop$parent_id;
    let node_dict = loop$node_dict;
    let current_depth = loop$current_depth;
    if (parent_id instanceof $option.Some) {
      let id = parent_id[0];
      let $ = $dict.get(node_dict, id);
      if ($ instanceof Ok) {
        let parent_parent_id = $[0].parent_id;
        loop$parent_id = parent_parent_id;
        loop$node_dict = node_dict;
        loop$current_depth = current_depth + 1;
      } else {
        return current_depth + 1;
      }
    } else {
      return current_depth;
    }
  }
}

/**
 * Sort AddNode patches so that parents are added before their children
 * Optimized: pre-compute depths as tuples to avoid dict lookups in comparator
 * 
 * @ignore
 */
function sort_patches_by_hierarchy(patches, node_dict) {
  let patches_with_depth = $list.map(
    patches,
    (patch) => {
      if (patch instanceof AddNode) {
        let parent_id = patch.parent_id;
        let depth = calculate_depth(parent_id, node_dict, 0);
        return [depth, patch];
      } else {
        return [0, patch];
      }
    },
  );
  let _pipe = $list.sort(
    patches_with_depth,
    (a, b) => {
      let depth_a;
      depth_a = a[0];
      let depth_b;
      depth_b = b[0];
      let $ = depth_a < depth_b;
      if ($) {
        return new $order.Lt();
      } else {
        let $1 = depth_a > depth_b;
        if ($1) {
          return new $order.Gt();
        } else {
          return new $order.Eq();
        }
      }
    },
  );
  return $list.map(
    _pipe,
    (tuple) => {
      let patch;
      patch = tuple[1];
      return patch;
    },
  );
}

/**
 * Compare Mesh fields using accumulator pattern (no empty list allocations)
 * 
 * @ignore
 */
function compare_mesh_fields(
  id,
  prev_geom,
  prev_mat,
  prev_trans,
  prev_phys,
  curr_geom,
  curr_mat,
  curr_trans,
  curr_phys
) {
  let patches = toList([]);
  let _block;
  let $ = !isEqual(prev_trans, curr_trans);
  if ($) {
    _block = listPrepend(new UpdateTransform(id, curr_trans), patches);
  } else {
    _block = patches;
  }
  let patches$1 = _block;
  let _block$1;
  let $1 = !isEqual(prev_mat, curr_mat);
  if ($1) {
    _block$1 = listPrepend(new UpdateMaterial(id, curr_mat), patches$1);
  } else {
    _block$1 = patches$1;
  }
  let patches$2 = _block$1;
  let _block$2;
  let $2 = !isEqual(prev_geom, curr_geom);
  if ($2) {
    _block$2 = listPrepend(new UpdateGeometry(id, curr_geom), patches$2);
  } else {
    _block$2 = patches$2;
  }
  let patches$3 = _block$2;
  let _block$3;
  let $3 = !isEqual(prev_phys, curr_phys);
  if ($3) {
    _block$3 = listPrepend(new UpdatePhysics(id, curr_phys), patches$3);
  } else {
    _block$3 = patches$3;
  }
  let patches$4 = _block$3;
  return patches$4;
}

/**
 * Compare InstancedMesh fields using accumulator pattern
 * 
 * @ignore
 */
function compare_instanced_mesh_fields(
  id,
  prev_geom,
  prev_mat,
  prev_instances,
  curr_geom,
  curr_mat,
  curr_instances
) {
  let patches = toList([]);
  let _block;
  let $ = !isEqual(prev_mat, curr_mat);
  if ($) {
    _block = listPrepend(new UpdateMaterial(id, curr_mat), patches);
  } else {
    _block = patches;
  }
  let patches$1 = _block;
  let _block$1;
  let $1 = !isEqual(prev_geom, curr_geom);
  if ($1) {
    _block$1 = listPrepend(new UpdateGeometry(id, curr_geom), patches$1);
  } else {
    _block$1 = patches$1;
  }
  let patches$2 = _block$1;
  let _block$2;
  let $2 = !isEqual(prev_instances, curr_instances);
  if ($2) {
    _block$2 = listPrepend(new UpdateInstances(id, curr_instances), patches$2);
  } else {
    _block$2 = patches$2;
  }
  let patches$3 = _block$2;
  return patches$3;
}

/**
 * Compare Light fields using accumulator pattern
 * 
 * @ignore
 */
function compare_light_fields(
  id,
  prev_light,
  prev_trans,
  curr_light,
  curr_trans
) {
  let patches = toList([]);
  let _block;
  let $ = !isEqual(prev_trans, curr_trans);
  if ($) {
    _block = listPrepend(new UpdateTransform(id, curr_trans), patches);
  } else {
    _block = patches;
  }
  let patches$1 = _block;
  let _block$1;
  let $1 = !isEqual(prev_light, curr_light);
  if ($1) {
    _block$1 = listPrepend(new UpdateLight(id, curr_light), patches$1);
  } else {
    _block$1 = patches$1;
  }
  let patches$2 = _block$1;
  return patches$2;
}

/**
 * Compare LOD fields using accumulator pattern
 * 
 * @ignore
 */
function compare_lod_fields(
  id,
  prev_levels,
  prev_trans,
  curr_levels,
  curr_trans
) {
  let patches = toList([]);
  let _block;
  let $ = !isEqual(prev_trans, curr_trans);
  if ($) {
    _block = listPrepend(new UpdateTransform(id, curr_trans), patches);
  } else {
    _block = patches;
  }
  let patches$1 = _block;
  let _block$1;
  let $1 = !isEqual(prev_levels, curr_levels);
  if ($1) {
    _block$1 = listPrepend(new UpdateLODLevels(id, curr_levels), patches$1);
  } else {
    _block$1 = patches$1;
  }
  let patches$2 = _block$1;
  return patches$2;
}

/**
 * Compare Camera fields using accumulator pattern
 * 
 * @ignore
 */
function compare_camera_fields(
  id,
  prev_cam,
  prev_trans,
  prev_look_at,
  prev_active,
  prev_viewport,
  curr_cam,
  curr_trans,
  curr_look_at,
  curr_active,
  curr_viewport
) {
  let patches = toList([]);
  let _block;
  let $ = !isEqual(prev_trans, curr_trans);
  if ($) {
    _block = listPrepend(new UpdateTransform(id, curr_trans), patches);
  } else {
    _block = patches;
  }
  let patches$1 = _block;
  let _block$1;
  let $1 = ((!isEqual(prev_cam, curr_cam)) || (!isEqual(
    prev_look_at,
    curr_look_at
  ))) || (!isEqual(prev_viewport, curr_viewport));
  if ($1) {
    _block$1 = listPrepend(
      new UpdateCamera(id, curr_cam, curr_look_at),
      patches$1,
    );
  } else {
    _block$1 = patches$1;
  }
  let patches$2 = _block$1;
  let _block$2;
  if (curr_active && !prev_active) {
    _block$2 = listPrepend(new SetActiveCamera(id), patches$2);
  } else {
    _block$2 = patches$2;
  }
  let patches$3 = _block$2;
  return patches$3;
}

/**
 * Compare Model3D fields using accumulator pattern
 * 
 * @ignore
 */
function compare_model3d_fields(
  id,
  prev_trans,
  prev_anim,
  prev_phys,
  curr_trans,
  curr_anim,
  curr_phys
) {
  let patches = toList([]);
  let _block;
  let $ = !isEqual(prev_trans, curr_trans);
  if ($) {
    _block = listPrepend(new UpdateTransform(id, curr_trans), patches);
  } else {
    _block = patches;
  }
  let patches$1 = _block;
  let _block$1;
  let $1 = !isEqual(prev_anim, curr_anim);
  if ($1) {
    _block$1 = listPrepend(new UpdateAnimation(id, curr_anim), patches$1);
  } else {
    _block$1 = patches$1;
  }
  let patches$2 = _block$1;
  let _block$2;
  let $2 = !isEqual(prev_phys, curr_phys);
  if ($2) {
    _block$2 = listPrepend(new UpdatePhysics(id, curr_phys), patches$2);
  } else {
    _block$2 = patches$2;
  }
  let patches$3 = _block$2;
  return patches$3;
}

/**
 * Compare Particles fields using accumulator pattern
 * 
 * @ignore
 */
function compare_particle_fields(
  id,
  prev_emitter,
  prev_trans,
  prev_active,
  curr_emitter,
  curr_trans,
  curr_active
) {
  let patches = toList([]);
  let _block;
  let $ = !isEqual(prev_trans, curr_trans);
  if ($) {
    _block = listPrepend(new UpdateTransform(id, curr_trans), patches);
  } else {
    _block = patches;
  }
  let patches$1 = _block;
  let _block$1;
  let $1 = !isEqual(prev_emitter, curr_emitter);
  if ($1) {
    _block$1 = listPrepend(
      new UpdateParticleEmitter(id, curr_emitter),
      patches$1,
    );
  } else {
    _block$1 = patches$1;
  }
  let patches$2 = _block$1;
  let _block$2;
  let $2 = prev_active !== curr_active;
  if ($2) {
    _block$2 = listPrepend(new UpdateParticleActive(id, curr_active), patches$2);
  } else {
    _block$2 = patches$2;
  }
  let patches$3 = _block$2;
  return patches$3;
}

/**
 * Detailed comparison of node properties (called only when nodes differ)
 * Uses accumulator pattern to avoid empty list allocations
 * 
 * @ignore
 */
function compare_nodes_detailed(id, prev, curr) {
  if (curr instanceof Mesh && prev instanceof Mesh) {
    let curr_geom = curr.geometry;
    let curr_mat = curr.material;
    let curr_trans = curr.transform;
    let curr_phys = curr.physics;
    let prev_geom = prev.geometry;
    let prev_mat = prev.material;
    let prev_trans = prev.transform;
    let prev_phys = prev.physics;
    return compare_mesh_fields(
      id,
      prev_geom,
      prev_mat,
      prev_trans,
      prev_phys,
      curr_geom,
      curr_mat,
      curr_trans,
      curr_phys,
    );
  } else if (curr instanceof InstancedMesh && prev instanceof InstancedMesh) {
    let curr_geom = curr.geometry;
    let curr_mat = curr.material;
    let curr_instances = curr.instances;
    let prev_geom = prev.geometry;
    let prev_mat = prev.material;
    let prev_instances = prev.instances;
    return compare_instanced_mesh_fields(
      id,
      prev_geom,
      prev_mat,
      prev_instances,
      curr_geom,
      curr_mat,
      curr_instances,
    );
  } else if (curr instanceof Group && prev instanceof Group) {
    let curr_trans = curr.transform;
    let prev_trans = prev.transform;
    let $ = !isEqual(prev_trans, curr_trans);
    if ($) {
      return toList([new UpdateTransform(id, curr_trans)]);
    } else {
      return toList([]);
    }
  } else if (curr instanceof Light && prev instanceof Light) {
    let curr_light = curr.light;
    let curr_trans = curr.transform;
    let prev_light = prev.light;
    let prev_trans = prev.transform;
    return compare_light_fields(
      id,
      prev_light,
      prev_trans,
      curr_light,
      curr_trans,
    );
  } else if (curr instanceof Camera && prev instanceof Camera) {
    let curr_cam = curr.camera;
    let curr_trans = curr.transform;
    let curr_look_at = curr.look_at;
    let curr_active = curr.active;
    let curr_viewport = curr.viewport;
    let prev_cam = prev.camera;
    let prev_trans = prev.transform;
    let prev_look_at = prev.look_at;
    let prev_active = prev.active;
    let prev_viewport = prev.viewport;
    return compare_camera_fields(
      id,
      prev_cam,
      prev_trans,
      prev_look_at,
      prev_active,
      prev_viewport,
      curr_cam,
      curr_trans,
      curr_look_at,
      curr_active,
      curr_viewport,
    );
  } else if (curr instanceof LOD && prev instanceof LOD) {
    let curr_levels = curr.levels;
    let curr_trans = curr.transform;
    let prev_levels = prev.levels;
    let prev_trans = prev.transform;
    return compare_lod_fields(
      id,
      prev_levels,
      prev_trans,
      curr_levels,
      curr_trans,
    );
  } else if (curr instanceof Model3D && prev instanceof Model3D) {
    let curr_trans = curr.transform;
    let curr_anim = curr.animation;
    let curr_phys = curr.physics;
    let prev_trans = prev.transform;
    let prev_anim = prev.animation;
    let prev_phys = prev.physics;
    return compare_model3d_fields(
      id,
      prev_trans,
      prev_anim,
      prev_phys,
      curr_trans,
      curr_anim,
      curr_phys,
    );
  } else if (curr instanceof Audio && prev instanceof Audio) {
    let curr_audio = curr.audio;
    let prev_audio = prev.audio;
    let $ = !isEqual(prev_audio, curr_audio);
    if ($) {
      return toList([new UpdateAudio(id, curr_audio)]);
    } else {
      return toList([]);
    }
  } else if (curr instanceof Particles && prev instanceof Particles) {
    let curr_emitter = curr.emitter;
    let curr_trans = curr.transform;
    let curr_active = curr.active;
    let prev_emitter = prev.emitter;
    let prev_trans = prev.transform;
    let prev_active = prev.active;
    return compare_particle_fields(
      id,
      prev_emitter,
      prev_trans,
      prev_active,
      curr_emitter,
      curr_trans,
      curr_active,
    );
  } else {
    return toList([]);
  }
}

function compare_nodes(id, prev, curr) {
  let $ = isEqual(prev, curr);
  if ($) {
    return toList([]);
  } else {
    return compare_nodes_detailed(id, prev, curr);
  }
}

export function diff(previous, current) {
  let prev_dict = flatten_scene(previous);
  let curr_dict = flatten_scene(current);
  let prev_size = $dict.size(prev_dict);
  let curr_size = $dict.size(curr_dict);
  let $ = (prev_size === 0) && (curr_size === 0);
  if ($) {
    return toList([]);
  } else {
    let prev_ids = $dict.keys(prev_dict);
    let curr_ids = $dict.keys(curr_dict);
    let prev_id_set = $set.from_list(prev_ids);
    let curr_id_set = $set.from_list(curr_ids);
    let _block;
    let _pipe = $list.filter(
      prev_ids,
      (id) => { return !$set.contains(curr_id_set, id); },
    );
    _block = $list.map(_pipe, (id) => { return new RemoveNode(id); });
    let removals = _block;
    let _block$1;
    let _pipe$1 = $list.filter(
      curr_ids,
      (id) => { return $set.contains(prev_id_set, id); },
    );
    _block$1 = $list.partition(
      _pipe$1,
      (id) => {
        let $2 = $dict.get(prev_dict, id);
        let $3 = $dict.get(curr_dict, id);
        if ($3 instanceof Ok && $2 instanceof Ok) {
          let curr_parent = $3[0].parent_id;
          let prev_parent = $2[0].parent_id;
          return !isEqual(prev_parent, curr_parent);
        } else {
          return false;
        }
      },
    );
    let $1 = _block$1;
    let parent_changed_ids;
    let same_parent_ids;
    parent_changed_ids = $1[0];
    same_parent_ids = $1[1];
    let parent_change_removals = $list.map(
      parent_changed_ids,
      (id) => { return new RemoveNode(id); },
    );
    let parent_change_additions = $list.filter_map(
      parent_changed_ids,
      (id) => {
        let $2 = $dict.get(curr_dict, id);
        if ($2 instanceof Ok) {
          let node = $2[0].node;
          let parent_id = $2[0].parent_id;
          return new Ok(new AddNode(id, node, parent_id));
        } else {
          return new Error(undefined);
        }
      },
    );
    let _block$2;
    let _pipe$2 = $list.filter(
      curr_ids,
      (id) => { return !$set.contains(prev_id_set, id); },
    );
    let _pipe$3 = $list.filter_map(
      _pipe$2,
      (id) => {
        let $2 = $dict.get(curr_dict, id);
        if ($2 instanceof Ok) {
          let node = $2[0].node;
          let parent_id = $2[0].parent_id;
          return new Ok(new AddNode(id, node, parent_id));
        } else {
          return new Error(undefined);
        }
      },
    );
    let _pipe$4 = $list.append(_pipe$3, parent_change_additions);
    _block$2 = sort_patches_by_hierarchy(_pipe$4, curr_dict);
    let additions = _block$2;
    let updates = $list.flat_map(
      same_parent_ids,
      (id) => {
        let $2 = $dict.get(prev_dict, id);
        let $3 = $dict.get(curr_dict, id);
        if ($3 instanceof Ok && $2 instanceof Ok) {
          let curr_node = $3[0].node;
          let prev_node = $2[0].node;
          return compare_nodes(id, prev_node, curr_node);
        } else {
          return toList([]);
        }
      },
    );
    return batch_patches(removals, parent_change_removals, updates, additions);
  }
}
