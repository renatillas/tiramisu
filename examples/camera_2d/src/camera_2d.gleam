/// Camera 2D Example
///
/// Demonstrates 2D camera modes and orthographic projection
import gleam/float
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
import vec/vec2
import vec/vec3

pub type Model {
  Model(time: duration.Duration)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  let assert Ok(Nil) =
    tiramisu.run(
      dimensions: option.None,
      bridge: option.None,
      init: init,
      update: update,
      view: view,
      selector: "body",
    )
  Nil
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg), option.Option(_)) {
  #(Model(time: duration.milliseconds(0)), effect.tick(Tick), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> #(
      Model(time: duration.add(model.time, ctx.delta_time)),
      effect.tick(Tick),
      option.None,
    )
  }
}

fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  scene.empty(id: "scene", transform: transform.identity, children: [
    scene.camera(
      id: "camera",
      camera: camera.camera_2d(ctx.canvas_size |> vec2.map(float.round)),
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
    scene.mesh(
      id: "center",
      geometry: {
        let assert Ok(geom) = geometry.plane(vec2.Vec2(50.0, 50.0))
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
        let assert Ok(geom) = geometry.plane(vec2.Vec2(30.0, 30.0))
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
          duration.to_seconds(model.time),
        )),
      physics: option.None,
    ),
    scene.mesh(
      id: "top-right",
      geometry: {
        let assert Ok(geom) = geometry.plane(vec2.Vec2(30.0, 30.0))
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
          duration.to_seconds(model.time),
        )),
      physics: option.None,
    ),
    scene.mesh(
      id: "bottom-left",
      geometry: {
        let assert Ok(geom) = geometry.plane(vec2.Vec2(30.0, 30.0))
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
          duration.to_seconds(model.time),
        )),
      physics: option.None,
    ),
    scene.mesh(
      id: "bottom-right",
      geometry: {
        let assert Ok(geom) = geometry.plane(vec2.Vec2(30.0, 30.0))
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
          duration.to_seconds(model.time),
        )),
      physics: option.None,
    ),
  ])
}
