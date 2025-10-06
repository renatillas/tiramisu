/// Character Controller Example
///
/// Demonstrates animation state machines with smooth transitions
import gleam/int
import gleam/io
import gleam/javascript/promise
import gleam/list
import gleam/option
import tiramisu/animation/state_machine
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/gltf
import tiramisu/input
import tiramisu/object3d
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

pub type LoadState {
  Loading
  Loaded(gltf.GLTFData, state_machine.StateMachine(GameContext))
  Failed(String)
}

pub type Model {
  Model(rotation: Float, load_state: LoadState)
}

pub type Msg {
  Tick
  ModelLoaded(gltf.GLTFData)
  LoadingFailed(gltf.GLTFError)
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
    |> camera.set_position(vec3.Vec3(0.0, 2.0, 12.0))
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
  let model = Model(rotation: 0.0, load_state: Loading)

  // Load a GLTF file with animations
  // Use a model with Idle, Walk, Run animations
  // Good models: RobotExpressive.glb, Fox.glb from Khronos samples
  let load_effect =
    effect.from_promise(
      promise.map(gltf.load("character.glb"), fn(result) {
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
      )
    }

    ModelLoaded(data) -> {
      let animation_count = gltf.animation_count(data)
      io.println(
        "Loaded character with "
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

      // Create state machine with animations
      let machine = create_state_machine(data, ctx)

      #(Model(..model, load_state: Loaded(data, machine)), effect.none())
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
  }
}

/// Create an animation state machine for the character
fn create_state_machine(
  data: gltf.GLTFData,
  _ctx: GameContext,
) -> state_machine.StateMachine(GameContext) {
  // Get animation clips (assume model has at least 2 animations)
  case data.animations {
    [idle_clip, walk_clip, ..rest] -> {
      // Create state machine
      let idle_anim =
        object3d.new_animation(idle_clip)
        |> object3d.set_loop(object3d.LoopRepeat)

      let walk_anim =
        object3d.new_animation(walk_clip)
        |> object3d.set_loop(object3d.LoopRepeat)

      let machine =
        state_machine.new("idle")
        |> state_machine.add_state("idle", idle_anim, looping: True)
        |> state_machine.add_state("walk", walk_anim, looping: True)
        |> state_machine.add_transition(
          from: "idle",
          to: "walk",
          condition: state_machine.Custom(fn(ctx: GameContext) {
            input.is_key_pressed(ctx.input, input.KeyW)
          }),
          blend_duration: 0.3,
        )
        |> state_machine.add_transition(
          from: "walk",
          to: "idle",
          condition: state_machine.Custom(fn(ctx: GameContext) {
            !input.is_key_pressed(ctx.input, input.KeyW)
          }),
          blend_duration: 0.3,
        )

      // Add run animation if available
      case rest {
        [run_clip, ..] -> {
          let run_anim =
            object3d.new_animation(run_clip)
            |> object3d.set_loop(object3d.LoopRepeat)

          machine
          |> state_machine.add_state("run", run_anim, looping: True)
          |> state_machine.add_transition(
            from: "walk",
            to: "run",
            condition: state_machine.Custom(fn(ctx: GameContext) {
              input.is_key_pressed(ctx.input, input.Shift)
            }),
            blend_duration: 0.2,
          )
          |> state_machine.add_transition(
            from: "run",
            to: "walk",
            condition: state_machine.Custom(fn(ctx: GameContext) {
              !input.is_key_pressed(ctx.input, input.Shift)
            }),
            blend_duration: 0.2,
          )
        }
        [] -> machine
      }
    }
    _ -> {
      io.println("Not enough animations for state machine")
      state_machine.new("default")
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
          id: "character",
          object: data.scene,
          transform: transform.Transform(
            position: vec3.Vec3(0.0, 0.0, 0.0),
            rotation: vec3.Vec3(0.0, model.rotation, 0.0),
            scale: vec3.Vec3(1.0, 1.0, 1.0),
          ),
          animation: animation,
          physics: option.None,
        )

      [character, ..lights]
    }
  }
}
