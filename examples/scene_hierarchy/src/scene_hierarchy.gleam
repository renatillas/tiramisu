import gleam/option
import gleam/result
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
  Model(rotation: Float, show_planets: Bool)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  let assert Ok(Nil) =
    tiramisu.run(
      dimensions: option.None,
      selector: "body",
      init: init,
      update: update,
      view: view,
    )
  Nil
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg), option.Option(_)) {
  #(Model(rotation: 0.0, show_planets: True), effect.tick(Tick), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. duration.to_seconds(ctx.delta_time)
      #(
        Model(rotation: new_rotation, show_planets: True),
        effect.tick(Tick),
        option.None,
      )
    }
  }
}

fn view(model: Model, _) -> scene.Node {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    |> result.map(scene.camera(
      id: "main-camera",
      camera: _,
      active: True,
      transform: transform.at(position: vec3.Vec3(0.0, 5.0, 15.0)),
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      viewport: option.None,
      postprocessing: option.None,
    ))

  let lights =
    scene.light(
      id: "point",
      light: {
        let assert Ok(light) =
          light.point(color: 0xfffb00, intensity: 100.5, distance: 100.0)
        light
      },
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
    )

  // Solar system: sun with orbiting planets in groups
  let sun =
    scene.mesh(
      id: "sun",
      geometry: {
        let assert Ok(geometry) =
          geometry.box(width: 1.0, height: 1.0, depth: 1.0)
        geometry
      },
      material: {
        let assert Ok(material) =
          material.basic(
            color: 0xffff00,
            transparent: False,
            opacity: 1.0,
            map: option.None,
          )
        material
      },
      transform: transform.identity
        |> transform.with_euler_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
      physics: option.None,
    )
    |> scene.with_children([
      lights,
      scene.mesh(
        id: "planet-1",
        geometry: {
          let assert Ok(geometry) =
            geometry.box(width: 1.0, height: 1.0, depth: 1.0)
          geometry
        },
        material: {
          let assert Ok(material) =
            material.new()
            |> material.with_color(0x4ecdc4)
            |> material.build
          material
        },
        transform: transform.at(position: vec3.Vec3(4.0, 0.0, 10.0))
          |> transform.with_euler_rotation(vec3.Vec3(
            0.0,
            model.rotation *. 2.0,
            0.0,
          )),
        physics: option.None,
      )
        |> scene.with_children([
          scene.mesh(
            id: "planet-1-moon",
            geometry: {
              let assert Ok(geometry) =
                geometry.box(width: 0.5, height: 0.5, depth: 0.5)
              geometry
            },
            material: {
              let assert Ok(material) =
                material.new()
                |> material.with_color(0xcccccc)
                |> material.build()
              material
            },
            transform: transform.at(position: vec3.Vec3(1.0, 0.0, 0.0))
              |> transform.with_euler_rotation(vec3.Vec3(
                0.0,
                model.rotation,
                0.0,
              )),
            physics: option.None,
          ),
        ]),
      scene.mesh(
        id: "planet-2",
        geometry: {
          let assert Ok(geometry) =
            geometry.box(width: 1.0, height: 1.0, depth: 1.0)
          geometry
        },
        material: {
          let assert Ok(material) =
            material.new()
            |> material.with_color(0xff6b6b)
            |> material.build()
          material
        },
        transform: transform.at(position: vec3.Vec3(7.0, 0.0, 0.0))
          |> transform.with_euler_rotation(vec3.Vec3(
            0.0,
            model.rotation *. 0.7,
            0.0,
          )),
        physics: option.None,
      ),
    ])

  scene.empty(id: "scene", transform: transform.identity, children: [
    camera,
    sun,
  ])
}
