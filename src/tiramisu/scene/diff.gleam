/// Scene diffing algorithm - compares two scene trees and generates patches
import gleam/dict
import gleam/list
import gleam/option
import tiramisu/scene

/// A patch represents a change to apply to the Three.js scene
pub type Patch {
  AddNode(id: String, node: scene.SceneNode, parent_id: option.Option(String))
  RemoveNode(id: String)
  UpdateTransform(id: String, transform: scene.Transform)
  UpdateMaterial(id: String, material: scene.MaterialType)
  UpdateGeometry(id: String, geometry: scene.GeometryType)
  UpdateLight(id: String, light_type: scene.LightType)
}

/// Node with parent info for tracking hierarchy
type NodeWithParent {
  NodeWithParent(node: scene.SceneNode, parent_id: option.Option(String))
}

/// Flatten a scene tree into a dictionary with parent information
fn flatten_scene(
  nodes: List(scene.SceneNode),
) -> dict.Dict(String, NodeWithParent) {
  flatten_scene_helper(nodes, option.None, dict.new())
}

fn flatten_scene_helper(
  nodes: List(scene.SceneNode),
  parent_id: option.Option(String),
  acc: dict.Dict(String, NodeWithParent),
) -> dict.Dict(String, NodeWithParent) {
  list.fold(nodes, acc, fn(acc, node) {
    let node_id = get_node_id(node)
    let acc = dict.insert(acc, node_id, NodeWithParent(node, parent_id))
    case node {
      scene.Group(_, _, children) ->
        flatten_scene_helper(children, option.Some(node_id), acc)
      _ -> acc
    }
  })
}

/// Get the ID from a scene node
fn get_node_id(node: scene.SceneNode) -> String {
  case node {
    scene.Mesh(id, ..) -> id
    scene.Group(id, ..) -> id
    scene.Light(id, ..) -> id
  }
}

/// Compare two scene trees and generate patches
pub fn diff(
  previous: List(scene.SceneNode),
  current: List(scene.SceneNode),
) -> List(Patch) {
  let prev_dict = flatten_scene(previous)
  let curr_dict = flatten_scene(current)

  let prev_ids = dict.keys(prev_dict)
  let curr_ids = dict.keys(curr_dict)

  // Find removals: IDs in previous but not in current
  let removals =
    list.filter(prev_ids, fn(id) { !list.contains(curr_ids, id) })
    |> list.map(fn(id) { RemoveNode(id) })

  // Find additions: IDs in current but not in previous
  let additions =
    list.filter(curr_ids, fn(id) { !list.contains(prev_ids, id) })
    |> list.filter_map(fn(id) {
      case dict.get(curr_dict, id) {
        Ok(NodeWithParent(node, parent_id)) ->
          Ok(AddNode(id, node, parent_id))
        Error(_) -> Error(Nil)
      }
    })

  // Find updates: IDs in both, compare node properties
  let updates =
    list.filter(curr_ids, fn(id) { list.contains(prev_ids, id) })
    |> list.flat_map(fn(id) {
      case dict.get(prev_dict, id), dict.get(curr_dict, id) {
        Ok(NodeWithParent(prev_node, _)), Ok(NodeWithParent(curr_node, _)) ->
          compare_nodes(id, prev_node, curr_node)
        _, _ -> []
      }
    })

  list.flatten([removals, additions, updates])
}

/// Compare two nodes and generate update patches
fn compare_nodes(
  id: String,
  prev: scene.SceneNode,
  curr: scene.SceneNode,
) -> List(Patch) {
  case prev, curr {
    scene.Mesh(_, prev_geom, prev_mat, prev_trans),
      scene.Mesh(_, curr_geom, curr_mat, curr_trans)
    -> {
      []
      |> list.append(case prev_trans != curr_trans {
        True -> [UpdateTransform(id, curr_trans)]
        False -> []
      })
      |> list.append(case prev_mat != curr_mat {
        True -> [UpdateMaterial(id, curr_mat)]
        False -> []
      })
      |> list.append(case prev_geom != curr_geom {
        True -> [UpdateGeometry(id, curr_geom)]
        False -> []
      })
    }

    scene.Light(_, prev_light, prev_trans),
      scene.Light(_, curr_light, curr_trans)
    -> {
      []
      |> list.append(case prev_trans != curr_trans {
        True -> [UpdateTransform(id, curr_trans)]
        False -> []
      })
      |> list.append(case prev_light != curr_light {
        True -> [UpdateLight(id, curr_light)]
        False -> []
      })
    }

    scene.Group(_, prev_trans, _), scene.Group(_, curr_trans, _) -> {
      case prev_trans != curr_trans {
        True -> [UpdateTransform(id, curr_trans)]
        False -> []
      }
    }

    _, _ -> []
  }
}
