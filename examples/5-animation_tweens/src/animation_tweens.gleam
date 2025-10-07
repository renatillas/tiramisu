import gleam/option
import tiramisu
import tiramisu/animation
import tiramisu/camera
import tiramisu/effect
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
    width: 1200,
    height: 800,
    background: 0x1a1a2e,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg)) {
  let tween =
    animation.tween_vec3(
      vec3.Vec3(-5.0, 0.0, 0.0),
      vec3.Vec3(5.0, 0.0, 0.0),
      2.0,
      animation.Linear,
    )

  #(Model(tween: tween, current_easing: 0), effect.tick(Tick))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg)) {
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
              2.0,
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

fn view(model: Model) -> List(scene.SceneNode) {
  let position = animation.get_tween_value(model.tween)
  let _ = easing_name(model.current_easing)
  let assert Ok(camera) =
    camera.perspective(
      field_of_view: 45.0,
      aspect: 1200.0 /. 800.0,
      near: 0.1,
      far: 100.0,
    )

  [
    scene.Camera(
      id: "main_camera",
      camera: camera,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 15.0)),
      look_at: option.None,
      active: True,
      viewport: option.None,
    ),
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) = scene.ambient_light(color: 0xffffff, intensity: 0.6)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light: {
        let assert Ok(light) = scene.directional_light(color: 0xffffff, intensity: 0.8)
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(10.0, 10.0, 10.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
    scene.Mesh(
      id: "sphere",
      geometry: {
        let assert Ok(geom) = scene.sphere(radius: 1.0, width_segments: 32, height_segments: 32)
        geom
      },
      material: {
        let assert Ok(mat) = scene.standard_material(
          color: 0x4ecdc4,
          metalness: 0.5,
          roughness: 0.3,
          map: option.None,
          normal_map: option.None,
        )
        mat
      },
      transform: transform.Transform(
        position: position,
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "start",
      geometry: {
        let assert Ok(geom) = scene.box(width: 0.5, height: 0.5, depth: 0.5)
        geom
      },
      material: {
        let assert Ok(mat) = scene.basic_material(
          color: 0xff6b6b,
          transparent: False,
          opacity: 1.0,
          map: option.None,
          normal_map: option.None,
        )
        mat
      },
      transform: transform.Transform(
        position: vec3.Vec3(-5.0, 0.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "end",
      geometry: {
        let assert Ok(geom) = scene.box(width: 0.5, height: 0.5, depth: 0.5)
        geom
      },
      material: {
        let assert Ok(mat) = scene.basic_material(
          color: 0x95e1d3,
          transparent: False,
          opacity: 1.0,
          map: option.None,
          normal_map: option.None,
        )
        mat
      },
      transform: transform.Transform(
        position: vec3.Vec3(5.0, 0.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]
}
