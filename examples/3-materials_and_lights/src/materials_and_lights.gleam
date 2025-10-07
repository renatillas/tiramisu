import gleam/list
import gleam/option
import tiramisu
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Model {
  Model(rotation: Float, light_intensity: Float)
}

pub type Msg {
  Tick
}

pub fn main() -> Nil {
  tiramisu.run(
    width: 1200,
    height: 800,
    background: 0x0a0a0a,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg)) {
  #(Model(rotation: 0.0, light_intensity: 1.0), effect.tick(Tick))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time
      #(Model(..model, rotation: new_rotation), effect.tick(Tick))
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let assert Ok(camera) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 1200.0 /. 800.0,
      near: 0.1,
      far: 1000.0,
    )

  let camera = camera
  let camera =
    scene.Camera(
      id: "main_camera",
      camera:,
      transform: transform.at(position: vec3.Vec3(0.0, 3.0, 15.0)),
      active: True,
      look_at: option.None,
      viewport: option.None,
    )
  let lights = [
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) = scene.ambient_light(color: 0x404040, intensity: 0.3)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light: {
        let assert Ok(light) = scene.directional_light(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(5.0, 10.0, 5.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
    scene.Light(
      id: "point",
      light: {
        let assert Ok(light) = scene.point_light(
          color: 0xff6b6b,
          intensity: 1.0,
          distance: 50.0,
        )
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(-5.0, 3.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
    scene.Light(
      id: "hemisphere",
      light: {
        let assert Ok(light) = scene.hemisphere_light(
          sky_color: 0xffffff,
          ground_color: 0xff0000,
          intensity: 1.0,
        )
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 10.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  let assert Ok(box_geom) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(basic_mat) = scene.basic_material(
    color: 0xff6b6b,
    transparent: False,
    opacity: 1.0,
    map: option.None,
    normal_map: option.None,
  )
  let assert Ok(standard_mat) = scene.standard_material(
    color: 0x4ecdc4,
    metalness: 0.8,
    roughness: 0.2,
    map: option.None,
    normal_map: option.None,
  )
  let assert Ok(phong_mat) = scene.phong_material(
    color: 0xffe66d,
    shininess: 100.0,
    map: option.None,
    normal_map: option.None,
  )
  let assert Ok(lambert_mat) = scene.lambert_material(
    color: 0x95e1d3,
    map: option.None,
    normal_map: option.None,
  )
  let assert Ok(toon_mat) = scene.toon_material(
    color: 0xf38181,
    map: option.None,
    normal_map: option.None,
  )

  let materials = [
    scene.Mesh(
      id: "basic",
      geometry: box_geom,
      material: basic_mat,
      transform: transform.Transform(
        position: vec3.Vec3(-6.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "standard",
      geometry: box_geom,
      material: standard_mat,
      transform: transform.Transform(
        position: vec3.Vec3(-3.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "phong",
      geometry: box_geom,
      material: phong_mat,
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "lambert",
      geometry: box_geom,
      material: lambert_mat,
      transform: transform.Transform(
        position: vec3.Vec3(3.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "toon",
      geometry: box_geom,
      material: toon_mat,
      transform: transform.Transform(
        position: vec3.Vec3(6.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]

  let assert Ok(plane_geom) = scene.plane(width: 20.0, height: 20.0)
  let assert Ok(ground_mat) = scene.standard_material(
    color: 0x6a6a6a,
    metalness: 0.0,
    roughness: 0.8,
    map: option.None,
    normal_map: option.None,
  )

  let ground = [
    scene.Mesh(
      id: "ground",
      geometry: plane_geom,
      material: ground_mat,
      transform: transform.Transform(
        position: vec3.Vec3(0.0, -2.0, 0.0),
        rotation: vec3.Vec3(-1.5708, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]

  list.flatten([[camera], lights, materials, ground])
}
