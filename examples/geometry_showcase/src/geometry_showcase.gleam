/// Geometry Showcase Example
///
/// Demonstrates all available geometry types in Tiramisu:
/// - Box, Sphere, Cone, Plane, Circle
/// - Cylinder, Torus, Tetrahedron, Icosahedron
///
/// Each geometry rotates slowly to show its shape
import gleam/list
import gleam/option
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/math/vec3
import tiramisu/scene
import tiramisu/transform

pub type Model {
  Model(rotation: Float)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  let assert Ok(cam) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 1200.0 /. 800.0,
      near: 0.1,
      far: 1000.0,
    )

  let cam =
    cam
    |> camera.set_position(vec3.Vec3(0.0, 5.0, 20.0))
    |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

  game.run(
    width: 1200,
    height: 800,
    background: 0x1a1a2e,
    camera: cam,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  #(Model(rotation: 0.0), effect.tick(Tick))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time
      #(Model(rotation: new_rotation), effect.tick(Tick))
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.4),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.8),
      transform: transform.Transform(
        position: vec3.Vec3(10.0, 10.0, 10.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  // Grid layout: 3 rows x 3 columns
  let geometries = [
    // Row 1
    create_mesh(
      "box",
      scene.BoxGeometry(2.0, 2.0, 2.0),
      -8.0,
      4.0,
      model.rotation,
      0xff6b6b,
    ),
    create_mesh(
      "sphere",
      scene.SphereGeometry(1.2, 32, 32),
      0.0,
      4.0,
      model.rotation,
      0x4ecdc4,
    ),
    create_mesh(
      "cone",
      scene.ConeGeometry(1.0, 2.0, 32),
      8.0,
      4.0,
      model.rotation,
      0xffe66d,
    ),
    // Row 2
    create_mesh(
      "plane",
      scene.PlaneGeometry(2.5, 2.5),
      -8.0,
      0.0,
      model.rotation,
      0x95e1d3,
    ),
    create_mesh(
      "circle",
      scene.CircleGeometry(1.3, 32),
      0.0,
      0.0,
      model.rotation,
      0xf38181,
    ),
    create_mesh(
      "cylinder",
      scene.CylinderGeometry(1.0, 1.0, 2.0, 32),
      8.0,
      0.0,
      model.rotation,
      0xa8e6cf,
    ),
    // Row 3
    create_mesh(
      "torus",
      scene.TorusGeometry(1.0, 0.4, 16, 32),
      -8.0,
      -4.0,
      model.rotation,
      0xdcedc1,
    ),
    create_mesh(
      "tetrahedron",
      scene.TetrahedronGeometry(1.5, 0),
      0.0,
      -4.0,
      model.rotation,
      0xffd3b6,
    ),
    create_mesh(
      "icosahedron",
      scene.IcosahedronGeometry(1.3, 0),
      8.0,
      -4.0,
      model.rotation,
      0xffaaa5,
    ),
  ]

  list.flatten([lights, geometries])
}

fn create_mesh(
  id: String,
  geometry: scene.GeometryType,
  x: Float,
  y: Float,
  rotation: Float,
  color: Int,
) -> scene.SceneNode {
  scene.Mesh(
    id: id,
    geometry: geometry,
    material: scene.StandardMaterial(
      color: color,
      metalness: 0.2,
      roughness: 0.6,
      map: option.None,
    ),
    transform: transform.Transform(
      position: vec3.Vec3(x, y, 0.0),
      rotation: vec3.Vec3(rotation *. 0.5, rotation, rotation *. 0.3),
      scale: vec3.Vec3(1.0, 1.0, 1.0),
    ),
    physics: option.None,
  )
}
