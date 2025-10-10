/// Bread Model Example
///
/// Demonstrates loading OBJ 3D models with MTL materials and textures
import gleam/io
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/result
import tiramisu
import tiramisu/asset
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/object3d.{type Object3D}
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type LoadState {
  Loading
  Loaded(Object3D)
  Failed(String)
}

pub type Model {
  Model(rotation: Float, load_state: LoadState)
}

pub type Msg {
  Tick
  ModelLoaded(Object3D)
  LoadingFailed(asset.LoadError)
}

pub fn main() -> Nil {
  tiramisu.run(
    dimensions: option.None,
    background: 0x1a1a2e,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg)) {
  let model = Model(rotation: 0.0, load_state: Loading)

  // Load the OBJ file with MTL materials
  let load_effect =
    effect.from_promise(
      promise.map(
        asset.load_obj(obj_url: "model.obj", mtl_url: "materials.mtl"),
        fn(result) {
          case result {
            Ok(object) -> ModelLoaded(object)
            Error(error) -> LoadingFailed(error)
          }
        },
      ),
    )

  #(model, effect.batch([effect.tick(Tick), load_effect]))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.5
      #(Model(..model, rotation: new_rotation), effect.tick(Tick))
    }

    ModelLoaded(object) -> {
      io.println("✅ Loaded bread model successfully!")
      #(Model(..model, load_state: Loaded(object)), effect.none())
    }

    LoadingFailed(error) -> {
      let error_msg = case error {
        asset.LoadError(msg) -> "Load error: " <> msg
        asset.InvalidUrl(url) -> "Invalid URL: " <> url
        asset.ParseError(msg) -> "Parse error: " <> msg
      }
      io.println("❌ Failed to load model: " <> error_msg)
      #(Model(..model, load_state: Failed(error_msg)), effect.none())
    }
  }
}

fn view(model: Model) -> List(scene.Node) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    |> result.map(fn(camera) {
      scene.Camera(
        id: "main-camera",
        camera: camera,
        transform: transform.at(position: vec3.Vec3(0.0, 1.0, 3.0)),
        look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
        active: True,
        viewport: option.None,
      )
      |> list.wrap
    })

  let lights = [
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 1.5)
        light
      },
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 7.5)),
    ),
  ]

  case model.load_state {
    Loading -> {
      // Show a spinning cube while loading
      let loading_cube =
        scene.Mesh(
          id: "loading",
          geometry: {
            let assert Ok(geometry) =
              geometry.box(width: 0.5, height: 0.5, depth: 0.5)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.standard(
                color: 0xffa500,
                metalness: 0.3,
                roughness: 0.5,
                map: option.None,
                normal_map: option.None,
                ambient_oclusion_map: option.None,
                roughness_map: option.None,
                metalness_map: option.None,
              )
            material
          },
          transform: transform.Transform(
            position: vec3.Vec3(0.0, 0.0, 0.0),
            rotation: vec3.Vec3(model.rotation, model.rotation, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          physics: option.None,
        )
        |> list.wrap
      list.flatten([camera, loading_cube, lights])
    }

    Failed(_error_msg) -> {
      // Show a red cube to indicate error
      let error_cube =
        scene.Mesh(
          id: "error",
          geometry: {
            let assert Ok(geometry) =
              geometry.box(width: 0.5, height: 0.5, depth: 0.5)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.standard(
                color: 0xff0000,
                metalness: 0.5,
                roughness: 0.5,
                map: option.None,
                normal_map: option.None,
                ambient_oclusion_map: option.None,
                roughness_map: option.None,
                metalness_map: option.None,
              )
            material
          },
          transform: transform.Transform(
            position: vec3.Vec3(0.0, 0.0, 0.0),
            rotation: vec3.Vec3(0.0, model.rotation, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          physics: option.None,
        )
        |> list.wrap
      list.flatten([camera, error_cube, lights])
    }

    Loaded(bread_model) -> {
      // Show the loaded bread model
      let model_node =
        scene.Model3D(
          id: "bread",
          object: bread_model,
          transform: transform.Transform(
            position: vec3.Vec3(0.0, 0.0, 0.0),
            rotation: vec3.Vec3(0.0, model.rotation, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          animation: option.None,
          physics: option.None,
        )

      list.flatten([camera, [model_node], lights])
    }
  }
}
