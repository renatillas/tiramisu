/// STL Loader Example
///
/// Demonstrates loading 3D models from STL files
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/result
import tiramisu
import tiramisu/asset
import tiramisu/background
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3
import vec/vec3f

pub type Id {
  MainCamera
  Ambient
  Directional
  LoadingCube
  ErrorCube
  StlGroup
  Standard
  StlModel
}

pub type LoadState {
  Loading
  Loaded(asset.BufferGeometry)
  Failed
}

pub type Model {
  Model(rotation: Float, load_state: LoadState)
}

pub type Msg {
  Tick
  ModelLoaded(asset.BufferGeometry)
  LoadingFailed
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

fn init(_ctx: tiramisu.Context(Id)) -> #(Model, Effect(Msg), option.Option(_)) {
  let model = Model(rotation: 0.0, load_state: Loading)

  // Load an STL file from the asset directory
  // Place your STL file in the Lustre asset folder
  let load_effect =
    effect.from_promise(
      promise.map(asset.load_stl("model.stl"), fn(result) {
        case result {
          Ok(geom) -> ModelLoaded(geom)
          Error(error) -> {
            echo error
            LoadingFailed
          }
        }
      }),
    )

  #(model, effect.batch([effect.tick(Tick), load_effect]), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(Id),
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.0005
      #(Model(..model, rotation: new_rotation), effect.tick(Tick), option.None)
    }

    ModelLoaded(geom) -> {
      #(Model(..model, load_state: Loaded(geom)), effect.none(), option.None)
    }

    LoadingFailed -> {
      #(Model(..model, load_state: Failed), effect.none(), option.None)
    }
  }
}

fn view(model: Model, _) -> List(scene.Node(Id)) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    |> result.map(fn(camera) {
      camera
      |> scene.camera(
        id: MainCamera,
        camera: _,
        transform: transform.at(position: vec3.Vec3(0.0, 0.0, 15.0)),
        look_at: option.None,
        active: True,
        viewport: option.None,
      )
      |> list.wrap
    })

  let lights = [
    scene.light(
      id: Ambient,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 1.0)
        light
      },
      transform: transform.identity,
    ),
    scene.light(
      id: Directional,
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 10.5)
        light
      },
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 70.5)),
    ),
  ]

  case model.load_state {
    Loading -> {
      // Show a spinning cube while loading
      let loading_cube =
        scene.mesh(
          id: LoadingCube,
          geometry: {
            let assert Ok(geometry) =
              geometry.box(width: 1.0, height: 1.0, depth: 1.0)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.phong(
                0xffffff,
                30.0,
                option.None,
                option.None,
                option.None,
              )
            material
          },
          transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
            |> transform.with_euler_rotation(vec3.Vec3(
              model.rotation,
              model.rotation,
              0.0,
            )),
          physics: option.None,
        )
        |> list.wrap
      list.flatten([camera, loading_cube, lights])
    }

    Failed -> {
      // Show a red cube to indicate error
      let error_cube =
        scene.mesh(
          id: ErrorCube,
          geometry: {
            let assert Ok(geometry) =
              geometry.box(width: 1.0, height: 1.0, depth: 1.0)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.new()
              |> material.with_color(0xff0000)
              |> material.with_metalness(0.5)
              |> material.with_roughness(0.5)
              |> material.build()
            material
          },
          transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
            |> transform.with_euler_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
          physics: option.None,
        )
        |> list.wrap
      list.flatten([camera, error_cube, lights])
    }

    Loaded(geom) -> {
      // Show the loaded STL model
      let model_node =
        scene.group(id: StlGroup, transform: transform.identity, children: [
          scene.mesh(
            id: Standard,
            geometry: {
              let assert Ok(geometry) =
                geometry.box(width: 1.0, height: 1.0, depth: 1.0)
              geometry
            },
            material: {
              let assert Ok(material) =
                material.new()
                |> material.with_color(0x4ecdc4)
                |> material.with_metalness(0.8)
                |> material.with_roughness(0.2)
                |> material.build()
              material
            },
            transform: transform.at(position: vec3.Vec3(-3.0, 2.0, 0.0))
              |> transform.with_euler_rotation(vec3.Vec3(
                0.0,
                model.rotation,
                0.0,
              )),
            physics: option.None,
          ),
          scene.mesh(
            id: StlModel,
            geometry: geometry.custom_geometry(geom),
            material: {
              let assert Ok(material) =
                material.new()
                |> material.with_color(0x4ecdc4)
                |> material.with_metalness(0.8)
                |> material.with_roughness(0.2)
                |> material.build()
              material
            },
            transform: transform.at(position: vec3f.zero)
              |> transform.with_euler_rotation(vec3.Vec3(
                0.0,
                model.rotation,
                0.0,
              ))
              |> transform.with_scale(vec3.Vec3(0.1, 0.1, 0.1)),
            physics: option.None,
          ),
        ])
        |> list.wrap

      list.flatten([camera, model_node, lights])
    }
  }
}
