/// Camera 2D Example
///
/// Demonstrates 2D camera modes and orthographic projection
import gleam/float
import gleam/option
import tiramisu
import tiramisu/background
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Model {
  Model(time: Float)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x1a1a2e),
    init: init,
    update: update,
    view: view,
  )
}

fn init(
  _ctx: tiramisu.Context(String),
) -> #(Model, Effect(Msg), option.Option(_)) {
  #(Model(time: 0.0), effect.tick(Tick), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(String),
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let new_time = model.time +. ctx.delta_time
      #(Model(time: new_time), effect.tick(Tick), option.None)
    }
  }
}

fn view(model: Model, ctx: tiramisu.Context(String)) -> scene.Node(String) {
  // Use actual canvas dimensions for proper aspect ratio on all devices
  let cam =
    camera.camera_2d(
      width: float.round(ctx.canvas_width),
      height: float.round(ctx.canvas_height),
    )

  scene.empty(id: "scene", transform: transform.identity, children: [
    scene.camera(
      id: "camera",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 20.0)),
      active: True,
      look_at: option.None,
      viewport: option.None,
      postprocessing: option.None,
    ),
    scene.light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 1.0)
        light
      },
      transform: transform.identity,
    ),
    // Center square
    scene.mesh(
      id: "center",
      geometry: {
        let assert Ok(geom) = geometry.plane(width: 50.0, height: 50.0)
        geom
      },
      material: {
        let assert Ok(mat) =
          material.basic(
            color: 0x4ecdc4,
            transparent: False,
            opacity: 1.0,
            map: option.None,
          )
        mat
      },
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
      physics: option.None,
    ),
    // Grid of squares in 2D space
    scene.mesh(
      id: "top-left",
      geometry: {
        let assert Ok(geom) = geometry.plane(width: 30.0, height: 30.0)
        geom
      },
      material: {
        let assert Ok(mat) =
          material.basic(
            color: 0xff6b6b,
            transparent: False,
            opacity: 1.0,
            map: option.None,
          )
        mat
      },
      transform: transform.at(position: vec3.Vec3(-150.0, 150.0, -1.0))
        |> transform.with_euler_rotation(vec3.Vec3(
          0.0,
          0.0,
          model.time /. 1000.0,
        )),
      physics: option.None,
    ),
    scene.mesh(
      id: "top-right",
      geometry: {
        let assert Ok(geom) = geometry.plane(width: 30.0, height: 30.0)
        geom
      },
      material: {
        let assert Ok(mat) =
          material.basic(
            color: 0xffe66d,
            transparent: False,
            opacity: 1.0,
            map: option.None,
          )
        mat
      },
      transform: transform.at(position: vec3.Vec3(150.0, 150.0, -1.0))
        |> transform.with_euler_rotation(vec3.Vec3(
          0.0,
          0.0,
          model.time /. 1000.0,
        )),
      physics: option.None,
    ),
    scene.mesh(
      id: "bottom-left",
      geometry: {
        let assert Ok(geom) = geometry.plane(width: 30.0, height: 30.0)
        geom
      },
      material: {
        let assert Ok(mat) =
          material.basic(
            color: 0x95e1d3,
            transparent: False,
            opacity: 1.0,
            map: option.None,
          )
        mat
      },
      transform: transform.at(position: vec3.Vec3(-150.0, -150.0, -1.0))
        |> transform.with_euler_rotation(vec3.Vec3(
          0.0,
          0.0,
          model.time /. 1000.0,
        )),
      physics: option.None,
    ),
    scene.mesh(
      id: "bottom-right",
      geometry: {
        let assert Ok(geom) = geometry.plane(width: 30.0, height: 30.0)
        geom
      },
      material: {
        let assert Ok(mat) =
          material.basic(
            color: 0xf38181,
            transparent: False,
            opacity: 1.0,
            map: option.None,
          )
        mat
      },
      transform: transform.at(position: vec3.Vec3(150.0, -150.0, -1.0))
        |> transform.with_euler_rotation(vec3.Vec3(
          0.0,
          0.0,
          model.time /. 1000.0,
        )),
      physics: option.None,
    ),
  ])
}
