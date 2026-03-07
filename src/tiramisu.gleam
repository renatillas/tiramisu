import lustre
import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

import tiramisu/camera
import tiramisu/dev/extension
import tiramisu/empty
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
import tiramisu/primitive
import tiramisu/scene

pub fn register(
  extensions: List(extension.Extension),
) -> Result(Nil, lustre.Error) {
  scene.register(extensions)
}

pub fn scene(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(
    scene.tag,
    [attribute.attribute("scene-id", id), ..attributes],
    children,
  )
}

pub fn camera(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(camera.tag, [attribute.id(id), ..attributes], children)
}

pub fn light(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(light.tag, [attribute.id(id), ..attributes], children)
}

pub fn primitive(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(primitive.tag, [attribute.id(id), ..attributes], children)
}

pub fn mesh(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(mesh.tag, [attribute.id(id), ..attributes], children)
}

pub fn empty(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(empty.tag, [attribute.id(id), ..attributes], children)
}

pub fn builtin_extensions() -> List(extension.Extension) {
  [
    mesh.extension(),
    primitive.extension(),
    camera.extension(),
    light.extension(),
    empty.extension(),
    material.extension(),
  ]
}
