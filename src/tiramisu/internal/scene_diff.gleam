//// Pure Gleam scene diff algorithm.
////
//// Compares two scene trees and produces a minimal list of patches.
//// Uses Dict for O(1) lookups by node ID. The algorithm:
////
//// 1. Flatten the old tree into a Dict(id -> #(SceneNode, parent_id))
//// 2. Walk the new tree recursively, tracking parent_id:
////    - If the node exists in the old map: compare tag/attrs/transform,
////      emit UpdateNode if anything changed, delete from old map
////    - If the node is new: emit a CreateNode patch
//// 3. Remaining entries in the old map (not visited) -> emit Remove patches
////
//// This is O(n) where n = total nodes.

import gleam/bool
import gleam/dict.{type Dict}
import gleam/list

import tiramisu/internal/scene.{type Node}
import tiramisu/internal/scene_patch.{type ScenePatch}

// DIFF ------------------------------------------------------------------------

/// Diff two scene trees and produce a list of patches.
/// `old_scene` is the previous frame's scene, `new_scene` is the current.
pub fn diff(
  old_scene: List(Node),
  new_scene: List(Node),
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
  map: Dict(String, #(Node, String)),
  nodes: List(Node),
  parent_id: String,
) -> Dict(String, #(Node, String)) {
  list.fold(nodes, map, fn(acc, node) {
    case node.key {
      // Skip nodes without IDs — they can't be diffed
      "" -> flatten(acc, node.children, parent_id)
      id -> {
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
  nodes: List(Node),
  parent_id: String,
  old_map: Dict(String, #(Node, String)),
  patches: List(ScenePatch),
) -> #(List(ScenePatch), Dict(String, #(Node, String))) {
  list.fold(nodes, #(patches, old_map), fn(acc, node) {
    let #(patches, old_map) = acc
    case node.key {
      // Skip nodes without IDs
      "" -> walk(node.children, parent_id, old_map, patches)
      id -> {
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

            // Compare and emit update if anything changed
            let patches = diff_node(old_node, node, id, patches)

            // Recurse into children
            walk(node.children, id, old_map, patches)
          }
          Error(Nil) -> {
            // New node — emit create patch
            let patches = [
              scene_patch.CreateNode(
                id:,
                parent_id:,
                tag: node.tag,
                attrs: node.attrs,
                transform: node.transform,
              ),
              ..patches
            ]

            // Recurse into children
            walk(node.children, id, old_map, patches)
          }
        }
      }
    }
  })
}

// DIFF NODE -------------------------------------------------------------------

/// Compare two nodes of the same ID and emit an update patch if anything changed.
fn diff_node(
  old: Node,
  new: Node,
  id: String,
  patches: List(ScenePatch),
) -> List(ScenePatch) {
  // Tag changed — remove old. The new node will be recreated on the next diff
  // cycle (when previous_scene no longer contains the old entry).
  use <- bool.guard(old.tag != new.tag, [scene_patch.Remove(id:), ..patches])
  // Nothing changed
  use <- bool.guard(
    old.transform == new.transform && old.attrs == new.attrs,
    patches,
  )
  [
    scene_patch.UpdateNode(
      id:,
      tag: new.tag,
      old_attrs: old.attrs,
      new_attrs: new.attrs,
      transform: new.transform,
    ),
    ..patches
  ]
}
