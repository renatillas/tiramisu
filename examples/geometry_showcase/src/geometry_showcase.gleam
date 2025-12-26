/// Geometry Showcase Example
///
/// Demonstrates all available geometry types in Tiramisu:
/// - Box, Sphere, Cone, Plane, Circle
/// - Cylinder, Torus, Tetrahedron, Icosahedron
///
/// Each geometry rotates slowly to show its shape
import gleam/option
import gleam/time/duration
import tiramisu
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Model {
  Model(rotation: Float)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  let assert Ok(Nil) =
    tiramisu.run(
      dimensions: option.None,
      init: init,
      update: update,
      view: view,
      selector: "body",
    )
  Nil
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg), option.Option(_)) {
  #(Model(rotation: 0.0), effect.tick(Tick), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. duration.to_seconds(ctx.delta_time)
      #(Model(rotation: new_rotation), effect.tick(Tick), option.None)
    }
  }
}

fn view(model: Model, _) -> scene.Node {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let camera =
    camera
    |> scene.camera(
      id: "main_camera",
      camera: _,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 20.0)),
      look_at: option.None,
      active: True,
      viewport: option.None,
      postprocessing: option.None,
    )
  let lights =
    scene.empty(id: "lights", transform: transform.identity, children: [
      scene.light(
        id: "ambient",
        light: {
          let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.4)
          light
        },
        transform: transform.identity,
      ),
      scene.light(
        id: "directional",
        light: {
          let assert Ok(light) =
            light.directional(color: 0xffffff, intensity: 0.8)
          light
        },
        transform: transform.at(position: vec3.Vec3(10.0, 10.0, 10.0)),
      ),
    ])

  // Grid layout: 3 rows x 3 columns
  let assert Ok(box_geom) = geometry.box(width: 2.0, height: 2.0, depth: 2.0)
  let assert Ok(sphere_geom) =
    geometry.sphere(radius: 1.2, width_segments: 32, height_segments: 32)
  let assert Ok(cone_geom) =
    geometry.cone(radius: 1.0, height: 2.0, segments: 32)
  let assert Ok(plane_geom) = geometry.plane(width: 2.5, height: 2.5)
  let assert Ok(circle_geom) = geometry.circle(radius: 1.3, segments: 32)
  let assert Ok(cylinder_geom) =
    geometry.cylinder(
      radius_top: 1.0,
      radius_bottom: 1.0,
      height: 2.0,
      radial_segments: 32,
    )
  let assert Ok(torus_geom) =
    geometry.torus(
      radius: 1.0,
      tube: 0.4,
      radial_segments: 16,
      tubular_segments: 32,
    )
  let assert Ok(tetrahedron_geom) = geometry.tetrahedron(radius: 1.5, detail: 0)
  let assert Ok(icosahedron_geom) = geometry.icosahedron(radius: 1.3, detail: 0)

  let geometries =
    scene.empty(id: "geometries", transform: transform.identity, children: [
      // Row 1
      create_mesh("box", box_geom, -8.0, 4.0, model.rotation, 0xff6b6b),
      create_mesh("sphere", sphere_geom, 0.0, 4.0, model.rotation, 0x4ecdc4),
      create_mesh("cone", cone_geom, 8.0, 4.0, model.rotation, 0xffe66d),
      // Row 2
      create_mesh("plane", plane_geom, -8.0, 0.0, model.rotation, 0x95e1d3),
      create_mesh("circle", circle_geom, 0.0, 0.0, model.rotation, 0xf38181),
      create_mesh("cylinder", cylinder_geom, 8.0, 0.0, model.rotation, 0xa8e6cf),
      // Row 3
      create_mesh("torus", torus_geom, -8.0, -4.0, model.rotation, 0xdcedc1),
      create_mesh(
        "tetrahedron",
        tetrahedron_geom,
        0.0,
        -4.0,
        model.rotation,
        0xffd3b6,
      ),
      create_mesh(
        "icosahedron",
        icosahedron_geom,
        8.0,
        -4.0,
        model.rotation,
        0xffaaa5,
      ),
    ])

  scene.empty(id: "scene", transform: transform.identity, children: [
    lights,
    camera,
    geometries,
  ])
}

fn create_mesh(
  id: String,
  geometry: geometry.Geometry,
  x: Float,
  y: Float,
  rotation: Float,
  color: Int,
) -> scene.Node {
  let assert Ok(material) =
    material.new() |> material.with_color(color) |> material.build

  scene.mesh(
    id: id,
    geometry: geometry,
    material: material,
    transform: transform.at(position: vec3.Vec3(x, y, 0.0))
      |> transform.with_euler_rotation(vec3.Vec3(
        rotation *. 0.5,
        rotation,
        rotation *. 0.3,
      )),
    physics: option.None,
  )
}
