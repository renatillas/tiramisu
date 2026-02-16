//// The tiramisu-lod element.
////
//// LOD (Level of Detail) containers automatically swap child meshes based
//// on camera distance. Children are `<tiramisu-mesh>` elements with a
//// `distance` attribute that sets the threshold for each detail level.
////
//// ## Usage
////
//// ```html
//// <tiramisu-lod id="tree-lod" transform="pos:10,0,0">
////   <tiramisu-mesh id="tree-hi" distance="0" geometry="sphere:1,32,16" color="#00ff00"></tiramisu-mesh>
////   <tiramisu-mesh id="tree-lo" distance="50" geometry="sphere:1,8,4" color="#00ff00"></tiramisu-mesh>
//// </tiramisu-lod>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier (required)
//// - `transform`: Transform as "pos:x,y,z quat:x,y,z,w scale:x,y,z"

import gleam/float

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

import tiramisu/transform.{type Transform}

import vec/vec3

// CONSTANTS -------------------------------------------------------------------

/// The tag name for LOD elements.
pub const tag_name = "tiramisu-lod"

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-lod element.
///
/// LOD containers swap child meshes based on camera distance. Each child mesh
/// should have a `distance` attribute indicating its visibility threshold.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/lod
/// import tiramisu/mesh
///
/// lod.lod("tree-lod", [
///   lod.transform(transform.translate(vec3.Vec3(10.0, 0.0, 0.0))),
/// ], [
///   mesh.mesh("tree-hi", [mesh.geometry_sphere_simple(1.0), mesh.distance(0.0)], []),
///   mesh.mesh("tree-lo", [mesh.geometry_sphere_simple(1.0), mesh.distance(50.0)], []),
/// ])
/// ```
///
pub fn lod(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the full transform of the LOD container.
///
pub fn transform(t: Transform) -> Attribute(msg) {
  let vec3.Vec3(px, py, pz) = transform.position(t)
  let #(qx, qy, qz, qw) = transform.to_quaternion_xyzw(t)
  let vec3.Vec3(sx, sy, sz) = transform.scale(t)

  attribute.attribute(
    "transform",
    "pos:"
      <> float.to_string(px)
      <> ","
      <> float.to_string(py)
      <> ","
      <> float.to_string(pz)
      <> " quat:"
      <> float.to_string(qx)
      <> ","
      <> float.to_string(qy)
      <> ","
      <> float.to_string(qz)
      <> ","
      <> float.to_string(qw)
      <> " scale:"
      <> float.to_string(sx)
      <> ","
      <> float.to_string(sy)
      <> ","
      <> float.to_string(sz),
  )
}
