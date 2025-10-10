/// Scene Hierarchy Example
///
/// Demonstrates Group nodes, parent-child relationships, and nested transforms
import gleam/list
import gleam/option
import gleam/result
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
  tiramisu.run(
    dimensions: option.None,
    background: 0x0f0f0f,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg)) {
  #(Model(rotation: 0.0, show_planets: True), effect.tick(Tick))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time
      #(Model(rotation: new_rotation, show_planets: True), effect.tick(Tick))
    }
  }
}

fn view(model: Model) -> List(scene.Node) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    |> result.map(fn(camera) {
      camera
      |> scene.Camera(
        id: "main-camera",
        camera: _,
        active: True,
        transform: transform.at(position: vec3.Vec3(0.0, 5.0, 15.0)),
        look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
        viewport: option.None,
      )
      |> list.wrap
    })

  let lights = [
    scene.Light(
      id: "point",
      light: {
        let assert Ok(light) =
          light.point(color: 0xfffb00, intensity: 100.5, distance: 100.0)
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 0.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  // Solar system: sun with orbiting planets in groups
  let sun = [
    scene.Mesh(
      id: "sun",
      geometry: {
        let assert Ok(geometry) =
          geometry.sphere(radius: 1.5, width_segments: 32, height_segments: 32)
        geometry
      },
      material: {
        let assert Ok(material) =
          material.basic(
            color: 0xffff00,
            transparent: False,
            opacity: 1.0,
            map: option.None,
            normal_map: option.None,
          )
        material
      },
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 0.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]

  // Planets as children of rotating groups
  let planet_system = case model.show_planets {
    True -> [
      // Planet 1 group (rotates around sun)
      scene.Group(
        id: "planet1-orbit",
        transform: transform.Transform(
          position: vec3.Vec3(0.0, 0.0, 0.0),
          rotation: vec3.Vec3(0.0, model.rotation *. 0.5, 0.0),
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
        children: [
          scene.Mesh(
            id: "planet1",
            geometry: {
              let assert Ok(geometry) =
                geometry.sphere(
                  radius: 0.5,
                  width_segments: 32,
                  height_segments: 32,
                )
              geometry
            },
            material: {
              let assert Ok(material) =
                material.new()
                |> material.with_color(0x4ecdc4)
                |> material.build
              material
            },
            transform: transform.Transform(
              position: vec3.Vec3(4.0, 0.0, 0.0),
              rotation: vec3.Vec3(0.0, model.rotation *. 2.0, 0.0),
              scale: vec3.Vec3(1.0, 1.0, 1.0),
            ),
            physics: option.None,
          ),
          // Moon orbiting planet1
          scene.Group(
            id: "moon1-orbit",
            transform: transform.Transform(
              position: vec3.Vec3(4.0, 0.0, 0.0),
              rotation: vec3.Vec3(0.0, model.rotation *. 2.0, 0.0),
              scale: vec3.Vec3(1.0, 1.0, 1.0),
            ),
            children: [
              scene.Mesh(
                id: "moon1",
                geometry: {
                  let assert Ok(geometry) =
                    geometry.sphere(
                      radius: 0.2,
                      width_segments: 16,
                      height_segments: 16,
                    )
                  geometry
                },
                material: {
                  let assert Ok(material) =
                    material.new()
                    |> material.with_color(0xcccccc)
                    |> material.build()
                  material
                },
                transform: transform.Transform(
                  position: vec3.Vec3(1.0, 0.0, 0.0),
                  rotation: vec3.Vec3(0.0, 0.0, 0.0),
                  scale: vec3.Vec3(1.0, 1.0, 1.0),
                ),
                physics: option.None,
              ),
            ],
          ),
        ],
      ),
      // Planet 2 group (rotates around sun)
      scene.Group(
        id: "planet2-orbit",
        transform: transform.Transform(
          position: vec3.Vec3(0.0, 0.0, 0.0),
          rotation: vec3.Vec3(0.0, model.rotation *. 0.3, 0.0),
          scale: vec3.Vec3(1.0, 1.0, 1.0),
        ),
        children: [
          scene.Mesh(
            id: "planet2",
            geometry: {
              let assert Ok(geometry) =
                geometry.sphere(
                  radius: 0.7,
                  width_segments: 32,
                  height_segments: 32,
                )
              geometry
            },
            material: {
              let assert Ok(material) =
                material.new()
                |> material.with_color(0xff6b6b)
                |> material.build()
              material
            },
            transform: transform.Transform(
              position: vec3.Vec3(7.0, 0.0, 0.0),
              rotation: vec3.Vec3(0.0, model.rotation, 0.0),
              scale: vec3.Vec3(1.0, 1.0, 1.0),
            ),
            physics: option.None,
          ),
        ],
      ),
    ]
    False -> []
  }

  list.flatten([camera, lights, sun, planet_system])
}
