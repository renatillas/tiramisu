/// Character Controller Example
///
/// Demonstrates animation state machines with smooth transitions
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
import tiramisu/state_machine
import tiramisu/transform
import vec/vec3

pub type Id {
  Main
  Ambient
  Directional
  LoadingCube
  ErrorCube
  Character
}

pub type State {
  Idle
  Walking
  Crouching
}

pub type LoadState {
  Loading
  Loaded(
    asset.GLTFData,
    state_machine.StateMachine(State, tiramisu.Context(Id)),
  )
  Failed(String)
}

pub type Model {
  Model(rotation: Float, load_state: LoadState)
}

pub type Msg {
  Tick
  ModelLoaded(asset.GLTFData)
  LoadingFailed(asset.LoadError)
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

  // Load a GLTF file with animations
  // Use a model with Idle, Walk, Run animations
  // Good models: RobotExpressive.glb, Fox.glb from Khronos samples
  let load_effect =
    effect.from_promise(
      promise.map(asset.load_gltf("character.glb"), fn(result) {
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
      let new_rotation = model.rotation +. ctx.delta_time *. 0.3

      // Update state machine if model is loaded
      let new_load_state = case model.load_state {
        Loaded(data, machine) -> {
          let #(updated_machine, _transitioned) =
            state_machine.update(machine, ctx, ctx.delta_time)
          Loaded(data, updated_machine)
        }
        other -> other
      }

      #(
        Model(rotation: new_rotation, load_state: new_load_state),
        effect.tick(Tick),
        option.None,
      )
    }

    ModelLoaded(data) -> {
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

      // Create state machine with animations
      let machine = create_state_machine(data)

      #(
        Model(..model, load_state: Loaded(data, machine)),
        effect.none(),
        option.None,
      )
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
  }
}

/// Create an animation state machine for the character
fn create_state_machine(
  data: asset.GLTFData,
) -> state_machine.StateMachine(State, tiramisu.Context(Id)) {
  // Get animation clips (assume model has at least 2 animations)
  case data.animations {
    [idle_clip, walk_clip, crouching_clip, ..] -> {
      // Create state machine
      let idle_anim =
        object3d.new_animation(idle_clip)
        |> object3d.set_loop(object3d.LoopRepeat)

      let walk_anim =
        object3d.new_animation(walk_clip)
        |> object3d.set_loop(object3d.LoopRepeat)

      let crouching_anim =
        object3d.new_animation(crouching_clip)
        |> object3d.set_loop(object3d.LoopRepeat)

      state_machine.new(Idle)
      |> state_machine.add_state(Idle, idle_anim, looping: True)
      |> state_machine.add_state(Walking, walk_anim, looping: True)
      |> state_machine.add_state(Crouching, crouching_anim, looping: True)
      |> state_machine.add_transition(
        from: Idle,
        to: Walking,
        condition: state_machine.Custom(fn(ctx: tiramisu.Context(Id)) {
          input.is_key_pressed(ctx.input, input.KeyW)
        }),
        blend_duration: 0.3,
      )
      |> state_machine.add_transition(
        from: Walking,
        to: Idle,
        condition: state_machine.Custom(fn(ctx: tiramisu.Context(Id)) {
          !input.is_key_pressed(ctx.input, input.KeyW)
        }),
        blend_duration: 0.3,
      )
      |> state_machine.add_transition(
        from: Walking,
        to: Crouching,
        condition: state_machine.Custom(fn(ctx: tiramisu.Context(Id)) {
          input.is_key_pressed(ctx.input, input.ShiftLeft)
        }),
        blend_duration: 0.2,
      )
      |> state_machine.add_transition(
        from: Crouching,
        to: Walking,
        condition: state_machine.Custom(fn(ctx: tiramisu.Context(Id)) {
          !input.is_key_pressed(ctx.input, input.ShiftLeft)
        }),
        blend_duration: 0.2,
      )
    }
    _ -> {
      state_machine.new(Idle)
    }
  }
}

fn view(model: Model, _ctx: tiramisu.Context(Id)) -> List(scene.Node(Id)) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    |> result.map(fn(camera) {
      camera
      |> scene.Camera(
        id: Main,
        camera: _,
        look_at: option.None,
        active: True,
        transform: transform.at(position: vec3.Vec3(0.0, 1.5, 20.0)),
        viewport: option.None,
      )
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
            let assert Ok(scene) =
              material.phong(
                0x4ecdc4,
                30.0,
                option.None,
                option.None,
                option.None,
              )
            scene
          },
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
          id: ErrorCube,
          geometry: {
            let assert Ok(geometry) =
              geometry.box(width: 1.0, height: 1.0, depth: 1.0)
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
      [error_cube, ..lights]
    }

    Loaded(data, machine) -> {
      // Get animation from state machine
      let animation = case state_machine.get_current_animation(machine) {
        state_machine.Single(anim) ->
          option.Some(object3d.SingleAnimation(anim))
        state_machine.Blend(from, to, factor) ->
          option.Some(object3d.BlendedAnimations(from, to, factor))
        state_machine.None -> option.None
      }

      // Show the character model with animation
      let character =
        scene.Model3D(
          id: Character,
          object: data.scene,
          transform: transform.Transform(
            position: vec3.Vec3(0.0, 0.0, 0.0),
            rotation: vec3.Vec3(0.0, model.rotation, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          animation: animation,
          physics: option.None,
        )

      [camera, character, ..lights]
    }
  }
}
