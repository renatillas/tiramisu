//// Pure Gleam scene diff algorithm.
////
//// Compares two scene trees and produces a minimal list of patches.
//// Uses MutableMap for O(1) lookups by node ID. The algorithm:
////
//// 1. Flatten the old tree into a MutableMap(id -> #(SceneNode, parent_id))
//// 2. Walk the new tree recursively, tracking parent_id:
////    - If the node exists in the old map: compare properties, emit updates,
////      check for reparent, delete from old map (marks as visited)
////    - If the node is new: emit a create patch
//// 3. Remaining entries in the old map (not visited) -> emit remove patches
////
//// This is O(n) where n = total nodes.

import gleam/list

import tiramisu/internal/mutable_map.{type MutableMap}
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

  let old_map = mutable_map.new() |> flatten(old_scene, scene_id)

  // Step 2: Walk new tree, emitting patches
  let patches = walk(new_scene, scene_id, old_map, [])

  // Step 3: Remaining entries in old_map are removals
  let patches =
    mutable_map.fold(old_map, patches, fn(acc, id, _entry) {
      [scene_patch.Remove(id:), ..acc]
    })

  // Reverse to get patches in forward order (creates before updates)
  list.reverse(patches)
}

// FLATTEN ---------------------------------------------------------------------

/// Recursively flatten a scene tree into a MutableMap(id -> #(node, parent_id)).
fn flatten(
  map: MutableMap(String, #(SceneNode, String)),
  nodes: List(SceneNode),
  parent_id: String,
) -> MutableMap(String, #(SceneNode, String)) {
  list.fold(nodes, map, fn(acc, node) {
    let id = scene.key(node)
    // Skip nodes without IDs — they can't be diffed
    case id {
      "" -> flatten(acc, scene.children(node), parent_id)
      _ -> {
        let acc = mutable_map.insert(acc, id, #(node, parent_id))
        flatten(acc, scene.children(node), id)
      }
    }
  })
}

// WALK ------------------------------------------------------------------------

/// Walk the new scene tree, comparing against the old map.
fn walk(
  nodes: List(SceneNode),
  parent_id: String,
  old_map: MutableMap(String, #(SceneNode, String)),
  patches: List(ScenePatch),
) -> List(ScenePatch) {
  list.fold(nodes, patches, fn(acc, node) {
    let id = scene.key(node)
    case id {
      // Skip nodes without IDs
      "" -> walk(scene.children(node), parent_id, old_map, acc)
      _ -> {
        case mutable_map.has_key(old_map, id) {
          True -> {
            // Node exists in old tree — compare and update
            let #(old_node, old_parent_id) = mutable_map.unsafe_get(old_map, id)
            let _ = mutable_map.delete(old_map, id)

            // Check for reparent
            let acc = case old_parent_id == parent_id {
              True -> acc
              False -> [
                scene_patch.Reparent(id:, new_parent_id: parent_id),
                ..acc
              ]
            }

            // Compare properties and emit update patches
            let acc = diff_node(old_node, node, acc)

            // Recurse into children
            walk(scene.children(node), id, old_map, acc)
          }
          False -> {
            // New node — emit create patch
            let acc = create_patches(node, parent_id, acc)

            // Recurse into children
            walk(scene.children(node), id, old_map, acc)
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
      geometry: og,
      src: os,
      material_type: omt,
      color: oc,
      metalness: om,
      roughness: or_,
      opacity: oo,
      wireframe: ow,
      emissive: oe,
      emissive_intensity: oei,
      side: osd,
      color_map: ocm,
      normal_map: onm,
      ao_map: oam,
      roughness_map: orm,
      metalness_map: omm,
      displacement_map: odm,
      displacement_scale: ods,
      displacement_bias: odb,
      shininess: osh,
      alpha_test: oat,
      transparent: otr,
      transform: ot,
      visible: ov,
      cast_shadow: ocs,
      receive_shadow: ors,
      physics_controlled: opc,
      ..,
    ),
      scene.MeshNode(
        geometry: ng,
        src: ns,
        material_type: nmt,
        color: nc,
        metalness: nm,
        roughness: nr,
        opacity: no,
        wireframe: nw,
        emissive: ne,
        emissive_intensity: nei,
        side: nsd,
        color_map: ncm,
        normal_map: nnm,
        ao_map: nam,
        roughness_map: nrm,
        metalness_map: nmm,
        displacement_map: ndm,
        displacement_scale: nds,
        displacement_bias: ndb,
        shininess: nsh,
        alpha_test: nat,
        transparent: ntr,
        transform: nt,
        visible: nv,
        cast_shadow: ncs,
        receive_shadow: nrs,
        physics_controlled: npc,
        ..,
      )
    -> {
      let patches = case og == ng {
        True -> patches
        False -> [scene_patch.UpdateMeshGeometry(id:, geometry: ng), ..patches]
      }
      let patches = case os == ns {
        True -> patches
        False -> [scene_patch.UpdateMeshSrc(id:, src: ns), ..patches]
      }
      // Material type change requires full material rebuild; otherwise check
      // individual material properties
      let patches = case
        omt == nmt
        && oc == nc
        && om == nm
        && or_ == nr
        && oo == no
        && ow == nw
        && oe == ne
        && oei == nei
        && osd == nsd
        && ocm == ncm
        && onm == nnm
        && oam == nam
        && orm == nrm
        && omm == nmm
        && odm == ndm
        && ods == nds
        && odb == ndb
        && osh == nsh
        && oat == nat
        && otr == ntr
      {
        True -> patches
        False -> [
          scene_patch.UpdateMeshMaterial(
            id:,
            material_type: nmt,
            color: nc,
            metalness: nm,
            roughness: nr,
            opacity: no,
            wireframe: nw,
            emissive: ne,
            emissive_intensity: nei,
            side: nsd,
            color_map: ncm,
            normal_map: nnm,
            ao_map: nam,
            roughness_map: nrm,
            metalness_map: nmm,
            displacement_map: ndm,
            displacement_scale: nds,
            displacement_bias: ndb,
            shininess: nsh,
            alpha_test: nat,
            transparent: ntr,
          ),
          ..patches
        ]
      }
      let patches = case ov == nv {
        True -> patches
        False -> [scene_patch.UpdateMeshVisibility(id:, visible: nv), ..patches]
      }
      // Shadow property updates
      let patches = case ocs == ncs && ors == nrs {
        True -> patches
        False -> [
          scene_patch.UpdateMeshShadow(
            id:,
            cast_shadow: ncs,
            receive_shadow: nrs,
          ),
          ..patches
        ]
      }
      // Skip transform updates for physics-controlled meshes
      let patches = case npc {
        True -> patches
        False ->
          case ot == nt {
            True -> patches
            False -> [
              scene_patch.UpdateTransform(id:, transform: nt),
              ..patches
            ]
          }
      }
      // Suppress unused variable warnings
      let _ = opc
      patches
    }

    scene.CameraNode(
      key: id,
      camera_type: oct,
      fov: of,
      near: on,
      far: ofa,
      transform: ot,
      active: oa,
    ),
      scene.CameraNode(
        key: _,
        camera_type: nct,
        fov: nf,
        near: nn,
        far: nfa,
        transform: nt,
        active: na,
      )
    -> {
      let patches = case oct == nct && of == nf && on == nn && ofa == nfa {
        True -> patches
        False -> [
          scene_patch.UpdateCameraProps(
            id:,
            camera_type: nct,
            fov: nf,
            near: nn,
            far: nfa,
          ),
          ..patches
        ]
      }
      let patches = case oa == na {
        True -> patches
        False -> [scene_patch.UpdateCameraActive(id:, active: na), ..patches]
      }
      let patches = case ot == nt {
        True -> patches
        False -> [scene_patch.UpdateTransform(id:, transform: nt), ..patches]
      }
      patches
    }

    scene.LightNode(
      key: id,
      color: oc,
      intensity: oi,
      transform: ot,
      cast_shadow: ocs,
      ..,
    ),
      scene.LightNode(
        color: nc,
        intensity: ni,
        transform: nt,
        cast_shadow: ncs,
        ..,
      )
    -> {
      let patches = case oc == nc && oi == ni && ocs == ncs {
        True -> patches
        False -> [
          scene_patch.UpdateLightProps(
            id:,
            color: nc,
            intensity: ni,
            cast_shadow: ncs,
          ),
          ..patches
        ]
      }
      let patches = case ot == nt {
        True -> patches
        False -> [scene_patch.UpdateTransform(id:, transform: nt), ..patches]
      }
      patches
    }

    scene.EmptyNode(key: id, transform: ot, visible: ov, ..),
      scene.EmptyNode(transform: nt, visible: nv, ..)
    -> {
      let patches = case ov == nv {
        True -> patches
        False -> [
          scene_patch.UpdateGroupVisibility(id:, visible: nv),
          ..patches
        ]
      }
      let patches = case ot == nt {
        True -> patches
        False -> [scene_patch.UpdateTransform(id:, transform: nt), ..patches]
      }
      patches
    }

    scene.AudioNode(
      key: id,
      src: os,
      volume: ov,
      loop: ol,
      playing: op,
      playback_rate: opr,
    ),
      scene.AudioNode(
        src: ns,
        volume: nv,
        loop: nl,
        playing: np,
        playback_rate: npr,
        ..,
      )
    -> {
      case os == ns && ov == nv && ol == nl && op == np && opr == npr {
        True -> patches
        False -> [
          scene_patch.UpdateAudioProps(
            id:,
            src: ns,
            volume: nv,
            loop: nl,
            playing: np,
            playback_rate: npr,
          ),
          ..patches
        ]
      }
    }

    scene.PositionalAudioNode(
      key: id,
      src: os,
      volume: ov,
      loop: ol,
      playing: op,
      playback_rate: opr,
      transform: ot,
      ref_distance: ord,
      max_distance: omd,
      rolloff_factor: orf,
      ..,
    ),
      scene.PositionalAudioNode(
        src: ns,
        volume: nv,
        loop: nl,
        playing: np,
        playback_rate: npr,
        transform: nt,
        ref_distance: nrd,
        max_distance: nmd,
        rolloff_factor: nrf,
        ..,
      )
    -> {
      case
        os == ns
        && ov == nv
        && ol == nl
        && op == np
        && opr == npr
        && ot == nt
        && ord == nrd
        && omd == nmd
        && orf == nrf
      {
        True -> patches
        False -> [
          scene_patch.UpdatePositionalAudio(
            id:,
            src: ns,
            volume: nv,
            loop: nl,
            playing: np,
            playback_rate: npr,
            transform: nt,
            ref_distance: nrd,
            max_distance: nmd,
            rolloff_factor: nrf,
          ),
          ..patches
        ]
      }
    }

    // Type changed — remove old, create new (handled by remove + create)
    _, _ -> {
      let id = scene.key(old)
      [scene_patch.Remove(id:), ..patches]
    }
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

    scene.AudioNode(key: id, src:, volume:, loop:, playing:, playback_rate:) -> [
      scene_patch.CreateAudio(
        id:,
        src:,
        volume:,
        loop:,
        playing:,
        playback_rate:,
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
        transform:,
        ref_distance:,
        max_distance:,
        rolloff_factor:,
      ),
      ..patches
    ]

    scene.UnknownNode(..) -> {
      // Unknown nodes are pass-through — no Three.js object to create
      patches
    }
  }
}
