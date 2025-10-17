/// GLTF Animated Model Example
///
/// Demonstrates loading 3D models with animations from GLTF/GLB files
import gleam/float
import gleam/int
import gleam/io
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
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/object3d
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Id {
  MainCamera
  Ambient
  Directional
  LoadingCube
  ErrorCube
  GltfModel
}

pub type LoadState {
  Loading
  Loaded(asset.GLTFData)
  Failed(String)
}

pub type Model {
  Model(
    rotation: Float,
    load_state: LoadState,
    current_animation: option.Option(object3d.AnimationClip),
    animation_speed: Float,
  )
}

pub type Msg {
  Tick
  ModelLoaded(asset.GLTFData)
  LoadingFailed(asset.LoadError)
  NextAnimation
  ToggleSpeed
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
  let model =
    Model(
      rotation: 0.0,
      load_state: Loading,
      current_animation: option.None,
      animation_speed: 1.0,
    )

  // Load a GLTF file from the asset directory
  // Place your GLTF/GLB file in the asset directory
  let load_effect =
    effect.from_promise(
      promise.map(asset.load_gltf("model.glb"), fn(result) {
        case result {
          Ok(data) -> ModelLoaded(data)
          Error(error) -> LoadingFailed(error)
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
      // When I press space I send a next animation message
      let effects = case input.is_key_just_pressed(ctx.input, input.Space) {
        True ->
          effect.batch([
            effect.tick(Tick),
            effect.from(fn(dispatch) { dispatch(NextAnimation) }),
          ])
        False -> effect.tick(Tick)
      }
      let new_rotation = model.rotation +. ctx.delta_time *. 0.3
      #(Model(..model, rotation: new_rotation), effects, option.None)
    }

    ModelLoaded(data) -> {
      let animation_count = data.animations |> list.length()
      io.println(
        "Loaded GLTF model with "
        <> int.to_string(animation_count)
        <> " animations",
      )

      // Print animation names
      case data.animations {
        [] -> io.println("No animations found")
        clips -> {
          io.println("Available animations:")
          list.index_map(clips, fn(clip, idx) {
            io.println(
              "  [" <> int.to_string(idx) <> "] " <> object3d.clip_name(clip),
            )
          })
          Nil
        }
      }

      #(Model(..model, load_state: Loaded(data)), effect.none(), option.None)
    }

    LoadingFailed(error) -> {
      let error_msg = case error {
        asset.LoadError(msg) -> "Load error: " <> msg
        asset.InvalidUrl(url) -> "Invalid URL: " <> url
        asset.ParseError(msg) -> "Parse error: " <> msg
      }
      io.println("Failed to load model: " <> error_msg)
      #(
        Model(..model, load_state: Failed(error_msg)),
        effect.none(),
        option.None,
      )
    }

    NextAnimation -> {
      case model.load_state {
        Loaded(data) -> {
          let new_animation =
            list.sample(data.animations, 1)
            |> list.first
            |> option.from_result()
          #(
            Model(..model, current_animation: new_animation),
            effect.none(),
            option.None,
          )
        }
        _ -> #(model, effect.none(), option.None)
      }
    }

    ToggleSpeed -> {
      let new_speed = case model.animation_speed {
        1.0 -> 2.0
        2.0 -> 0.5
        _ -> 1.0
      }
      io.println("Animation speed: " <> float.to_string(new_speed) <> "x")
      #(Model(..model, animation_speed: new_speed), effect.none(), option.None)
    }
  }
}

fn view(model: Model, _ctx: tiramisu.Context(Id)) -> List(scene.Node(Id)) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    |> result.map(fn(camera) {
      camera
      |> scene.Camera(
        id: MainCamera,
        transform: transform.at(position: vec3.Vec3(0.0, 0.0, 20.0)),
        look_at: option.None,
        active: True,
        viewport: option.None,
        camera: _,
      )
      |> list.wrap
    })

  let lights = [
    scene.Light(
      id: Ambient,
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: Directional,
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 2.0)
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
          id: LoadingCube,
          geometry: {
            let assert Ok(geometry) =
              geometry.box(width: 1.0, height: 1.0, depth: 1.0)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.phong(
                0x4ecdc4,
                30.0,
                option.None,
                option.None,
                option.None,
              )
            material
          },
          transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
        |> transform.with_rotation(vec3.Vec3(model.rotation, model.rotation, 0.0)),
          physics: option.None,
        )
        |> list.wrap
      list.flatten([camera, loading_cube, lights])
    }

    Failed(_error_msg) -> {
      // Show a red cube to indicate error
      let error_cube =
        scene.Mesh(
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
        |> transform.with_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
          physics: option.None,
        )
        |> list.wrap
      list.flatten([camera, error_cube, lights])
    }

    Loaded(gltf_model) -> {
      // Get the animation to play
      let animation = case
        list.filter(gltf_model.animations, keeping: fn(clip) {
          case model.current_animation {
            option.Some(current) ->
              object3d.clip_name(clip) == object3d.clip_name(current)
            option.None -> True
          }
        })
        |> list.first
      {
        Ok(clip) ->
          option.Some(object3d.SingleAnimation(
            object3d.new_animation(clip)
            |> object3d.set_loop(object3d.LoopRepeat)
            |> object3d.set_speed(model.animation_speed),
          ))
        Error(_) -> option.None
      }

      // Show the loaded GLTF model with animation
      let model_node =
        scene.Model3D(
          id: GltfModel,
          object: gltf_model.scene,
          transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
        |> transform.with_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
          animation: animation,
          physics: option.None,
        )

      [model_node, ..lights]
    }
  }
}
