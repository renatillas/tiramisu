import gleam/option
import tiramisu
import tiramisu/animation
import tiramisu/background
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Model {
  Model(tween: animation.Tween(vec3.Vec3(Float)), current_easing: Int)
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
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  let tween =
    animation.tween_vec3(
      vec3.Vec3(-5.0, 0.0, 0.0),
      vec3.Vec3(5.0, 0.0, 0.0),
      2000.0,
      animation.Linear,
    )

  #(Model(tween: tween, current_easing: 0), effect.tick(Tick), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(String),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let updated_tween = animation.update_tween(model.tween, ctx.delta_time)

      // Reset tween with next easing function when complete
      let #(final_tween, easing_index) = case
        animation.is_tween_complete(updated_tween)
      {
        True -> {
          let next_easing = { model.current_easing + 1 } % 9
          let easing = get_easing(next_easing)
          #(
            animation.tween_vec3(
              vec3.Vec3(-5.0, 0.0, 0.0),
              vec3.Vec3(5.0, 0.0, 0.0),
              2000.0,
              easing,
            ),
            next_easing,
          )
        }
        False -> #(updated_tween, model.current_easing)
      }

      #(
        Model(tween: final_tween, current_easing: easing_index),
        effect.tick(Tick),
        option.None,
      )
    }
  }
}

fn get_easing(index: Int) -> animation.Easing {
  case index {
    0 -> animation.Linear
    1 -> animation.EaseInQuad
    2 -> animation.EaseOutQuad
    3 -> animation.EaseInOutQuad
    4 -> animation.EaseInCubic
    5 -> animation.EaseOutCubic
    6 -> animation.EaseInOutCubic
    7 -> animation.EaseInSine
    8 -> animation.EaseOutSine
    _ -> animation.Linear
  }
}

fn easing_name(index: Int) -> String {
  case index {
    0 -> "Linear"
    1 -> "EaseInQuad"
    2 -> "EaseOutQuad"
    3 -> "EaseInOutQuad"
    4 -> "EaseInCubic"
    5 -> "EaseOutCubic"
    6 -> "EaseInOutCubic"
    7 -> "EaseInSine"
    8 -> "EaseOutSine"
    _ -> "Linear"
  }
}

fn view(model: Model, _) -> scene.Node(String) {
  let position = animation.get_tween_value(model.tween)
  let _ = easing_name(model.current_easing)
  let assert Ok(camera) =
    camera.perspective(field_of_view: 45.0, near: 0.1, far: 100.0)

  scene.empty(id: "scene", transform: transform.identity, children: [
    scene.camera(
      id: "main_camera",
      camera: camera,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 15.0)),
      look_at: option.None,
      active: True,
      viewport: option.None,
      postprocessing: option.None,
    ),
    scene.light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.6)
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
    scene.mesh(
      id: "sphere",
      geometry: {
        let assert Ok(geom) =
          geometry.sphere(radius: 1.0, width_segments: 32, height_segments: 32)
        geom
      },
      material: {
        let assert Ok(mat) =
          material.new()
          |> material.with_color(0x4ecdc4)
          |> material.build()
        mat
      },
      transform: transform.at(position: position),
      physics: option.None,
    ),
    scene.mesh(
      id: "start",
      geometry: {
        let assert Ok(geom) = geometry.box(width: 0.5, height: 0.5, depth: 0.5)
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
      transform: transform.at(position: vec3.Vec3(-5.0, 0.0, 0.0)),
      physics: option.None,
    ),
    scene.mesh(
      id: "end",
      geometry: {
        let assert Ok(geom) = geometry.box(width: 0.5, height: 0.5, depth: 0.5)
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
      transform: transform.at(position: vec3.Vec3(5.0, 0.0, 0.0)),
      physics: option.None,
    ),
  ])
}
