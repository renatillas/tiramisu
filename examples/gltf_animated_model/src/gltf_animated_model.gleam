/// GLTF Animated Model Example
///
/// Demonstrates loading 3D models with animations from GLTF/GLB files
import gleam/float
import gleam/int
import gleam/io
import gleam/javascript/promise
import gleam/list
import gleam/option
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/gltf
import tiramisu/object3d
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub type LoadState {
  Loading
  Loaded(gltf.GLTFData)
  Failed(String)
}

pub type Model {
  Model(
    rotation: Float,
    load_state: LoadState,
    current_animation: Int,
    animation_speed: Float,
  )
}

pub type Msg {
  Tick
  ModelLoaded(gltf.GLTFData)
  LoadingFailed(gltf.GLTFError)
  NextAnimation
  PreviousAnimation
  ToggleSpeed
}

pub fn main() -> Nil {
  let assert Ok(cam) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 1200.0 /. 800.0,
      near: 0.1,
      far: 1000.0,
    )

  let cam =
    cam
    |> camera.set_position(vec3.Vec3(0.0, 2.0, 10.0))
    |> camera.look(at: vec3.Vec3(0.0, 1.0, 0.0))

  game.run(
    width: 1200,
    height: 800,
    background: 0x1a1a2e,
    camera: cam,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: GameContext) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      rotation: 0.0,
      load_state: Loading,
      current_animation: 1,
      animation_speed: 1.0,
    )

  // Load a GLTF file from the assets directory
  // Place your GLTF/GLB file in the assets directory
  let load_effect =
    effect.from_promise(
      promise.map(gltf.load("model.glb"), fn(result) {
        case result {
          Ok(data) -> ModelLoaded(data)
          Error(error) -> LoadingFailed(error)
        }
      }),
    )

  #(model, effect.batch([effect.tick(Tick), load_effect]))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.3
      #(Model(..model, rotation: new_rotation), effect.tick(Tick))
    }

    ModelLoaded(data) -> {
      let animation_count = gltf.animation_count(data)
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

      #(Model(..model, load_state: Loaded(data)), effect.none())
    }

    LoadingFailed(error) -> {
      let error_msg = case error {
        gltf.LoadError(msg) -> "Load error: " <> msg
        gltf.InvalidUrl(url) -> "Invalid URL: " <> url
        gltf.ParseError(msg) -> "Parse error: " <> msg
      }
      io.println("Failed to load model: " <> error_msg)
      #(Model(..model, load_state: Failed(error_msg)), effect.none())
    }

    NextAnimation -> {
      case model.load_state {
        Loaded(data) -> {
          let animation_count = gltf.animation_count(data)
          let new_anim = { model.current_animation + 1 } % animation_count
          io.println("Switching to animation: " <> int.to_string(new_anim))
          #(Model(..model, current_animation: new_anim), effect.none())
        }
        _ -> #(model, effect.none())
      }
    }

    PreviousAnimation -> {
      case model.load_state {
        Loaded(data) -> {
          let animation_count = gltf.animation_count(data)
          let new_anim = case model.current_animation - 1 {
            n if n < 0 -> animation_count - 1
            n -> n
          }
          io.println("Switching to animation: " <> int.to_string(new_anim))
          #(Model(..model, current_animation: new_anim), effect.none())
        }
        _ -> #(model, effect.none())
      }
    }

    ToggleSpeed -> {
      let new_speed = case model.animation_speed {
        1.0 -> 2.0
        2.0 -> 0.5
        _ -> 1.0
      }
      io.println("Animation speed: " <> float.to_string(new_speed) <> "x")
      #(Model(..model, animation_speed: new_speed), effect.none())
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.5),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 2.0),
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 7.5)),
    ),
  ]

  case model.load_state {
    Loading -> {
      // Show a spinning cube while loading
      let loading_cube =
        scene.Mesh(
          id: "loading",
          geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
          material: scene.PhongMaterial(
            color: 0x4ecdc4,
            shininess: 30.0,
            map: option.None,
          ),
          transform: transform.Transform(
            position: vec3.Vec3(0.0, 0.0, 0.0),
            rotation: vec3.Vec3(model.rotation, model.rotation, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          physics: option.None,
        )
      [loading_cube, ..lights]
    }

    Failed(_error_msg) -> {
      // Show a red cube to indicate error
      let error_cube =
        scene.Mesh(
          id: "error",
          geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
          material: scene.StandardMaterial(
            color: 0xff0000,
            metalness: 0.5,
            roughness: 0.5,
            map: option.None,
            normal_map: option.None,
          ),
          transform: transform.Transform(
            position: vec3.Vec3(0.0, 0.0, 0.0),
            rotation: vec3.Vec3(0.0, model.rotation, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          physics: option.None,
        )
      [error_cube, ..lights]
    }

    Loaded(gltf_model) -> {
      // Get the animation to play
      let animation = case
        gltf.get_animation(gltf_model, model.current_animation)
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
          id: "gltf_model",
          object: gltf_model.scene,
          transform: transform.Transform(
            position: vec3.Vec3(0.0, 0.0, 0.0),
            rotation: vec3.Vec3(0.0, model.rotation, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          animation: animation,
          physics: option.None,
        )

      [model_node, ..lights]
    }
  }
}
