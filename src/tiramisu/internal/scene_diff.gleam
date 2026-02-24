//// Pure Gleam scene diff algorithm.
////
//// Compares two scene trees and produces a minimal list of patches.
//// Uses Dict for O(1) lookups by node ID. The algorithm:
////
//// 1. Flatten the old tree into a Dict(id -> #(SceneNode, parent_id))
//// 2. Walk the new tree recursively, tracking parent_id:
////    - If the node exists in the old map: compare properties, emit updates,
////      check for reparent, delete from old map (marks as visited)
////    - If the node is new: emit a create patch
//// 3. Remaining entries in the old map (not visited) -> emit remove patches
////
//// This is O(n) where n = total nodes.

import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option

import tiramisu/internal/scene.{type SceneNode}
import tiramisu/internal/scene_patch.{type ScenePatch}

// DIFF ------------------------------------------------------------------------

/// Diff two scene trees and produce a list of patches.
/// `old_scene` is the previous frame's scene, `new_scene` is the current.
pub fn diff(
  old_scene: List(SceneNode),
  new_scene: List(SceneNode),
  scene_id: String,
) -> List(ScenePatch) {
  // Step 1: Flatten old tree into a lookup map
  let old_map = flatten(dict.new(), old_scene, scene_id)

  // Step 2: Walk new tree, emitting patches
  let #(patches, old_map) = walk(new_scene, scene_id, old_map, [])

  // Step 3: Remaining entries in old_map are removals
  let patches =
    dict.fold(old_map, patches, fn(acc, id, _entry) {
      [scene_patch.Remove(id:), ..acc]
    })

  // Reverse to get patches in forward order (creates before updates)
  list.reverse(patches)
}

// FLATTEN ---------------------------------------------------------------------

/// Recursively flatten a scene tree into a Dict(id -> #(node, parent_id)).
fn flatten(
  map: Dict(String, #(SceneNode, String)),
  nodes: List(SceneNode),
  parent_id: String,
) -> Dict(String, #(SceneNode, String)) {
  list.fold(nodes, map, fn(acc, node) {
    let id = node.key
    // Skip nodes without IDs — they can't be diffed
    case id {
      "" -> flatten(acc, node.children, parent_id)
      _ -> {
        let acc = dict.insert(acc, id, #(node, parent_id))
        flatten(acc, node.children, id)
      }
    }
  })
}

// WALK ------------------------------------------------------------------------

/// Walk the new scene tree, comparing against the old map.
/// Returns the accumulated patches and the remaining old map entries
/// (unvisited nodes = removals).
fn walk(
  nodes: List(SceneNode),
  parent_id: String,
  old_map: Dict(String, #(SceneNode, String)),
  patches: List(ScenePatch),
) -> #(List(ScenePatch), Dict(String, #(SceneNode, String))) {
  list.fold(nodes, #(patches, old_map), fn(acc, node) {
    let #(patches, old_map) = acc
    let id = node.key
    case id {
      // Skip nodes without IDs
      "" -> walk(node.children, parent_id, old_map, patches)
      _ -> {
        case dict.get(old_map, id) {
          Ok(#(old_node, old_parent_id)) -> {
            // Node exists in old tree — mark as visited by removing from map
            let old_map = dict.delete(old_map, id)

            // Check for reparent
            let patches = case old_parent_id == parent_id {
              True -> patches
              False -> [
                scene_patch.Reparent(id:, new_parent_id: parent_id),
                ..patches
              ]
            }

            // Compare properties and emit update patches
            let patches = diff_node(old_node, node, patches)

            // Recurse into children
            walk(node.children, id, old_map, patches)
          }
          Error(Nil) -> {
            // New node — emit create patch
            let patches = create_patches(node, parent_id, patches)

            // Recurse into children
            walk(node.children, id, old_map, patches)
          }
        }
      }
    }
  })
}

// DIFF NODE -------------------------------------------------------------------

/// Compare two nodes of the same ID and emit update patches for changed props.
fn diff_node(
  old: SceneNode,
  new: SceneNode,
  patches: List(ScenePatch),
) -> List(ScenePatch) {
  case old, new {
    scene.MeshNode(
      key: id,
      geometry: old_geometry,
      src: old_src,
      material_type: old_material_type,
      color: old_color,
      metalness: old_metalness,
      roughness: old_roughness,
      opacity: old_opacity,
      wireframe: old_wireframe,
      emissive: old_emissive,
      emissive_intensity: old_emissive_intensity,
      side: old_side,
      color_map: old_color_map,
      normal_map: old_normal_map,
      ao_map: old_ao_map,
      roughness_map: old_roughness_map,
      metalness_map: old_metalness_map,
      displacement_map: old_displacement_map,
      displacement_scale: old_displacement_scale,
      displacement_bias: old_displacement_bias,
      shininess: old_shininess,
      alpha_test: old_alpha_test,
      transparent: old_transparent,
      transform: old_transform,
      visible: old_visible,
      cast_shadow: old_cast_shadow,
      receive_shadow: old_receive_shadow,
      ..,
    ),
      scene.MeshNode(
        geometry: new_geometry,
        src: new_src,
        material_type: new_material_type,
        color: new_color,
        metalness: new_metalness,
        roughness: new_roughness,
        opacity: new_opacity,
        wireframe: new_wireframe,
        emissive: new_emissive,
        emissive_intensity: new_emissive_intensity,
        side: new_side,
        color_map: new_color_map,
        normal_map: new_normal_map,
        ao_map: new_ao_map,
        roughness_map: new_roughness_map,
        metalness_map: new_metalness_map,
        displacement_map: new_displacement_map,
        displacement_scale: new_displacement_scale,
        displacement_bias: new_displacement_bias,
        shininess: new_shininess,
        alpha_test: new_alpha_test,
        transparent: new_transparent,
        transform: new_transform,
        visible: new_visible,
        cast_shadow: new_cast_shadow,
        receive_shadow: new_receive_shadow,
        physics_controlled: new_physics_controlled,
        ..,
      )
    -> {
      let patches = {
        use <- bool.guard(old_geometry == new_geometry, patches)
        [scene_patch.UpdateMeshGeometry(id:, geometry: new_geometry), ..patches]
      }
      let patches = {
        use <- bool.guard(old_src == new_src, patches)
        [scene_patch.UpdateMeshSrc(id:, src: new_src), ..patches]
      }
      // Material type change requires full material rebuild; otherwise check
      // individual material properties
      let material_unchanged =
        old_material_type == new_material_type
        && old_color == new_color
        && old_metalness == new_metalness
        && old_roughness == new_roughness
        && old_opacity == new_opacity
        && old_wireframe == new_wireframe
        && old_emissive == new_emissive
        && old_emissive_intensity == new_emissive_intensity
        && old_side == new_side
        && old_color_map == new_color_map
        && old_normal_map == new_normal_map
        && old_ao_map == new_ao_map
        && old_roughness_map == new_roughness_map
        && old_metalness_map == new_metalness_map
        && old_displacement_map == new_displacement_map
        && old_displacement_scale == new_displacement_scale
        && old_displacement_bias == new_displacement_bias
        && old_shininess == new_shininess
        && old_alpha_test == new_alpha_test
        && old_transparent == new_transparent
      let patches = {
        use <- bool.guard(material_unchanged, patches)
        [
          scene_patch.UpdateMeshMaterial(
            id:,
            material_type: new_material_type,
            color: new_color,
            metalness: new_metalness,
            roughness: new_roughness,
            opacity: new_opacity,
            wireframe: new_wireframe,
            emissive: new_emissive,
            emissive_intensity: new_emissive_intensity,
            side: new_side,
            color_map: new_color_map,
            normal_map: new_normal_map,
            ao_map: new_ao_map,
            roughness_map: new_roughness_map,
            metalness_map: new_metalness_map,
            displacement_map: new_displacement_map,
            displacement_scale: new_displacement_scale,
            displacement_bias: new_displacement_bias,
            shininess: new_shininess,
            alpha_test: new_alpha_test,
            transparent: new_transparent,
          ),
          ..patches
        ]
      }
      let patches = {
        use <- bool.guard(old_visible == new_visible, patches)
        [scene_patch.UpdateMeshVisibility(id:, visible: new_visible), ..patches]
      }
      let patches = {
        use <- bool.guard(
          old_cast_shadow == new_cast_shadow
            && old_receive_shadow == new_receive_shadow,
          patches,
        )
        [
          scene_patch.UpdateMeshShadow(
            id:,
            cast_shadow: new_cast_shadow,
            receive_shadow: new_receive_shadow,
          ),
          ..patches
        ]
      }
      // Skip transform updates for physics-controlled meshes
      let is_physics_controlled = option.unwrap(new_physics_controlled, False)
      let patches = {
        use <- bool.guard(
          is_physics_controlled || old_transform == new_transform,
          patches,
        )
        [scene_patch.UpdateTransform(id:, transform: new_transform), ..patches]
      }
      patches
    }

    scene.CameraNode(
      key: id,
      camera_type: old_camera_type,
      fov: old_fov,
      near: old_near,
      far: old_far,
      transform: old_transform,
      active: old_active,
      ..,
    ),
      scene.CameraNode(
        camera_type: new_camera_type,
        fov: new_fov,
        near: new_near,
        far: new_far,
        transform: new_transform,
        active: new_active,
        ..,
      )
    -> {
      let patches = {
        use <- bool.guard(
          old_camera_type == new_camera_type
            && old_fov == new_fov
            && old_near == new_near
            && old_far == new_far,
          patches,
        )
        [
          scene_patch.UpdateCameraProps(
            id:,
            camera_type: new_camera_type,
            fov: new_fov,
            near: new_near,
            far: new_far,
          ),
          ..patches
        ]
      }
      let patches = {
        use <- bool.guard(old_active == new_active, patches)
        [scene_patch.UpdateCameraActive(id:, active: new_active), ..patches]
      }
      let patches = {
        use <- bool.guard(old_transform == new_transform, patches)
        [scene_patch.UpdateTransform(id:, transform: new_transform), ..patches]
      }
      patches
    }

    scene.LightNode(
      key: id,
      color: old_color,
      intensity: old_intensity,
      transform: old_transform,
      cast_shadow: old_cast_shadow,
      ..,
    ),
      scene.LightNode(
        color: new_color,
        intensity: new_intensity,
        transform: new_transform,
        cast_shadow: new_cast_shadow,
        ..,
      )
    -> {
      let patches = {
        use <- bool.guard(
          old_color == new_color
            && old_intensity == new_intensity
            && old_cast_shadow == new_cast_shadow,
          patches,
        )
        [
          scene_patch.UpdateLightProps(
            id:,
            color: new_color,
            intensity: new_intensity,
            cast_shadow: new_cast_shadow,
          ),
          ..patches
        ]
      }
      let patches = {
        use <- bool.guard(old_transform == new_transform, patches)
        [scene_patch.UpdateTransform(id:, transform: new_transform), ..patches]
      }
      patches
    }

    scene.EmptyNode(key: id, transform: old_transform, visible: old_visible, ..),
      scene.EmptyNode(transform: new_transform, visible: new_visible, ..)
    -> {
      let patches = {
        use <- bool.guard(old_visible == new_visible, patches)
        [
          scene_patch.UpdateGroupVisibility(id:, visible: new_visible),
          ..patches
        ]
      }
      let patches = {
        use <- bool.guard(old_transform == new_transform, patches)
        [scene_patch.UpdateTransform(id:, transform: new_transform), ..patches]
      }
      patches
    }

    scene.AudioNode(
      key: id,
      src: old_src,
      volume: old_volume,
      loop: old_loop,
      playing: old_playing,
      playback_rate: old_playback_rate,
      detune: old_detune,
      ..,
    ),
      scene.AudioNode(
        src: new_src,
        volume: new_volume,
        loop: new_loop,
        playing: new_playing,
        playback_rate: new_playback_rate,
        detune: new_detune,
        ..,
      )
    -> {
      use <- bool.guard(
        old_src == new_src
          && old_volume == new_volume
          && old_loop == new_loop
          && old_playing == new_playing
          && old_playback_rate == new_playback_rate
          && old_detune == new_detune,
        patches,
      )
      [
        scene_patch.UpdateAudioProps(
          id:,
          src: new_src,
          volume: new_volume,
          loop: new_loop,
          playing: new_playing,
          playback_rate: new_playback_rate,
          detune: new_detune,
        ),
        ..patches
      ]
    }

    scene.PositionalAudioNode(
      key: id,
      src: old_src,
      volume: old_volume,
      loop: old_loop,
      playing: old_playing,
      playback_rate: old_playback_rate,
      detune: old_detune,
      transform: old_transform,
      ref_distance: old_ref_distance,
      max_distance: old_max_distance,
      rolloff_factor: old_rolloff_factor,
      ..,
    ),
      scene.PositionalAudioNode(
        src: new_src,
        volume: new_volume,
        loop: new_loop,
        playing: new_playing,
        playback_rate: new_playback_rate,
        detune: new_detune,
        transform: new_transform,
        ref_distance: new_ref_distance,
        max_distance: new_max_distance,
        rolloff_factor: new_rolloff_factor,
        ..,
      )
    -> {
      use <- bool.guard(
        old_src == new_src
          && old_volume == new_volume
          && old_loop == new_loop
          && old_playing == new_playing
          && old_playback_rate == new_playback_rate
          && old_detune == new_detune
          && old_transform == new_transform
          && old_ref_distance == new_ref_distance
          && old_max_distance == new_max_distance
          && old_rolloff_factor == new_rolloff_factor,
        patches,
      )
      [
        scene_patch.UpdatePositionalAudio(
          id:,
          src: new_src,
          volume: new_volume,
          loop: new_loop,
          playing: new_playing,
          playback_rate: new_playback_rate,
          detune: new_detune,
          transform: new_transform,
          ref_distance: new_ref_distance,
          max_distance: new_max_distance,
          rolloff_factor: new_rolloff_factor,
        ),
        ..patches
      ]
    }

    // Debug nodes are immutable — any change means remove + recreate
    scene.DebugNode(
      key: id,
      debug_type: old_debug_type,
      size: old_size,
      divisions: old_divisions,
      color: old_color,
      transform: old_transform,
      ..,
    ),
      scene.DebugNode(
        debug_type: new_debug_type,
        size: new_size,
        divisions: new_divisions,
        color: new_color,
        transform: new_transform,
        ..,
      )
    -> {
      use <- bool.guard(
        old_debug_type == new_debug_type
          && old_size == new_size
          && old_divisions == new_divisions
          && old_color == new_color
          && old_transform == new_transform,
        patches,
      )
      [scene_patch.Remove(id:), ..patches]
    }

    scene.InstancedMeshNode(
      key: id,
      geometry: old_geometry,
      material_type: old_material_type,
      color: old_color,
      metalness: old_metalness,
      roughness: old_roughness,
      opacity: old_opacity,
      wireframe: old_wireframe,
      transparent: old_transparent,
      instances: old_instances,
      transform: old_transform,
      visible: old_visible,
      cast_shadow: old_cast_shadow,
      receive_shadow: old_receive_shadow,
      ..,
    ),
      scene.InstancedMeshNode(
        geometry: new_geometry,
        material_type: new_material_type,
        color: new_color,
        metalness: new_metalness,
        roughness: new_roughness,
        opacity: new_opacity,
        wireframe: new_wireframe,
        transparent: new_transparent,
        instances: new_instances,
        transform: new_transform,
        visible: new_visible,
        cast_shadow: new_cast_shadow,
        receive_shadow: new_receive_shadow,
        ..,
      )
    -> {
      // Geometry change requires full remove + recreate
      let patches = {
        use <- bool.guard(old_geometry == new_geometry, patches)
        [scene_patch.Remove(id:), ..patches]
      }
      // Only emit granular updates if geometry didn't change
      case old_geometry == new_geometry {
        False -> patches
        True -> {
          let patches = {
            use <- bool.guard(
              old_material_type == new_material_type
                && old_color == new_color
                && old_metalness == new_metalness
                && old_roughness == new_roughness
                && old_opacity == new_opacity
                && old_wireframe == new_wireframe
                && old_transparent == new_transparent,
              patches,
            )
            [
              scene_patch.UpdateInstancedMeshMaterial(
                id:,
                material_type: new_material_type,
                color: new_color,
                metalness: new_metalness,
                roughness: new_roughness,
                opacity: new_opacity,
                wireframe: new_wireframe,
                transparent: new_transparent,
              ),
              ..patches
            ]
          }
          let patches = {
            use <- bool.guard(old_instances == new_instances, patches)
            [
              scene_patch.UpdateInstancedMeshInstances(
                id:,
                instances: new_instances,
              ),
              ..patches
            ]
          }
          let patches = {
            use <- bool.guard(old_visible == new_visible, patches)
            [
              scene_patch.UpdateInstancedMeshVisibility(
                id:,
                visible: new_visible,
              ),
              ..patches
            ]
          }
          let patches = {
            use <- bool.guard(
              old_cast_shadow == new_cast_shadow
                && old_receive_shadow == new_receive_shadow,
              patches,
            )
            [
              scene_patch.UpdateInstancedMeshShadow(
                id:,
                cast_shadow: new_cast_shadow,
                receive_shadow: new_receive_shadow,
              ),
              ..patches
            ]
          }
          let patches = {
            use <- bool.guard(old_transform == new_transform, patches)
            [
              scene_patch.UpdateTransform(id:, transform: new_transform),
              ..patches
            ]
          }
          patches
        }
      }
    }

    // Type changed — remove old, create new (handled by remove + create)
    _, _ -> [scene_patch.Remove(id: old.key), ..patches]
  }
}

// CREATE PATCHES --------------------------------------------------------------

/// Emit creation patch(es) for a new node.
fn create_patches(
  node: SceneNode,
  parent_id: String,
  patches: List(ScenePatch),
) -> List(ScenePatch) {
  case node {
    scene.MeshNode(
      key: id,
      geometry:,
      src:,
      material_type:,
      color:,
      metalness:,
      roughness:,
      opacity:,
      wireframe:,
      emissive:,
      emissive_intensity:,
      side:,
      color_map:,
      normal_map:,
      ao_map:,
      roughness_map:,
      metalness_map:,
      displacement_map:,
      displacement_scale:,
      displacement_bias:,
      shininess:,
      alpha_test:,
      transparent:,
      transform:,
      visible:,
      cast_shadow:,
      receive_shadow:,
      ..,
    ) -> [
      scene_patch.CreateMesh(
        id:,
        parent_id:,
        geometry:,
        src:,
        material_type:,
        color:,
        metalness:,
        roughness:,
        opacity:,
        wireframe:,
        emissive:,
        emissive_intensity:,
        side:,
        color_map:,
        normal_map:,
        ao_map:,
        roughness_map:,
        metalness_map:,
        displacement_map:,
        displacement_scale:,
        displacement_bias:,
        shininess:,
        alpha_test:,
        transparent:,
        transform:,
        visible:,
        cast_shadow:,
        receive_shadow:,
      ),
      ..patches
    ]

    scene.CameraNode(
      key: id,
      camera_type:,
      fov:,
      near:,
      far:,
      transform:,
      active:,
      ..,
    ) -> [
      scene_patch.CreateCamera(
        id:,
        parent_id:,
        camera_type:,
        fov:,
        near:,
        far:,
        transform:,
        active:,
      ),
      ..patches
    ]

    scene.LightNode(
      key: id,
      light_type:,
      color:,
      intensity:,
      transform:,
      cast_shadow:,
      ..,
    ) -> [
      scene_patch.CreateLight(
        id:,
        parent_id:,
        light_type:,
        color:,
        intensity:,
        transform:,
        cast_shadow:,
      ),
      ..patches
    ]

    scene.EmptyNode(key: id, transform:, visible:, ..) -> [
      scene_patch.CreateGroup(id:, parent_id:, transform:, visible:),
      ..patches
    ]

    scene.AudioNode(
      key: id,
      src:,
      volume:,
      loop:,
      playing:,
      playback_rate:,
      detune:,
      ..,
    ) -> [
      scene_patch.CreateAudio(
        id:,
        parent_id:,
        src:,
        volume:,
        loop:,
        playing:,
        playback_rate:,
        detune:,
      ),
      ..patches
    ]

    scene.PositionalAudioNode(
      key: id,
      src:,
      volume:,
      loop:,
      playing:,
      playback_rate:,
      detune:,
      transform:,
      ref_distance:,
      max_distance:,
      rolloff_factor:,
      ..,
    ) -> [
      scene_patch.CreatePositionalAudio(
        id:,
        parent_id:,
        src:,
        volume:,
        loop:,
        playing:,
        playback_rate:,
        detune:,
        transform:,
        ref_distance:,
        max_distance:,
        rolloff_factor:,
      ),
      ..patches
    ]

    scene.DebugNode(
      key: id,
      debug_type:,
      size:,
      divisions:,
      color:,
      transform:,
      ..,
    ) -> [
      scene_patch.CreateDebug(
        id:,
        parent_id:,
        debug_type:,
        size:,
        divisions:,
        color:,
        transform:,
      ),
      ..patches
    ]

    scene.InstancedMeshNode(
      key: id,
      geometry:,
      material_type:,
      color:,
      metalness:,
      roughness:,
      opacity:,
      wireframe:,
      transparent:,
      instances:,
      transform:,
      visible:,
      cast_shadow:,
      receive_shadow:,
      ..,
    ) -> [
      scene_patch.CreateInstancedMesh(
        id:,
        parent_id:,
        geometry:,
        material_type:,
        color:,
        metalness:,
        roughness:,
        opacity:,
        wireframe:,
        transparent:,
        instances:,
        transform:,
        visible:,
        cast_shadow:,
        receive_shadow:,
      ),
      ..patches
    ]

    scene.UnknownNode(..) -> {
      // Unknown nodes are pass-through — no Three.js object to create
      patches
    }
  }
}
