//
/// Character Controller Example
/// Demonstrates animation state machines with smooth transitions
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/time/duration
import statemachine
import tiramisu
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/model
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type LoadState {
  Loading
  Loaded(
    model.GLTFData,
    statemachine.StateMachine(model.Animation, tiramisu.Context),
  )
  Failed(String)
}

pub type Model {
  Model(rotation: Float, load_state: LoadState)
}

pub type Msg {
  Tick
  ModelLoaded(model.GLTFData)
  LoadingFailed
}

pub fn main() -> Nil {
  let assert Ok(Nil) =
    tiramisu.application(init, update, view)
    |> tiramisu.start("body", tiramisu.FullScreen, option.None)
  Nil
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg), option.Option(_)) {
  let model = Model(rotation: 0.0, load_state: Loading)

  let load_effect =
    model.load_gltf(
      from: "character.glb",
      on_success: ModelLoaded,
      on_error: LoadingFailed,
    )
  #(model, effect.batch([effect.dispatch(Tick), load_effect]), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. duration.to_seconds(ctx.delta_time)

      // Update state machine if model is loaded
      let new_load_state = case model.load_state {
        Loaded(data, machine) -> {
          let #(updated_machine, _transitioned) =
            statemachine.update(machine, ctx, ctx.delta_time)
          Loaded(data, updated_machine)
        }
        other -> other
      }

      #(
        Model(rotation: new_rotation, load_state: new_load_state),
        effect.dispatch(Tick),
        option.None,
      )
    }

    ModelLoaded(data) -> {
      // Print animation names
      case model.get_animations(data) {
        [] -> io.println("No animations found")
        clips -> {
          io.println("Available animations:")
          list.index_map(clips, fn(clip, idx) {
            io.println(
              "  [" <> int.to_string(idx) <> "] " <> model.clip_name(clip),
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

    LoadingFailed -> {
      io.println("Failed to load model")
      #(
        Model(..model, load_state: Failed("Failed to load model")),
        effect.none(),
        option.None,
      )
    }
  }
}

/// Create an animation state machine for the character
fn create_state_machine(
  data: model.GLTFData,
) -> statemachine.StateMachine(model.Animation, tiramisu.Context) {
  // Get animation clips (assume model has at least 2 animations)
  case model.get_animations(data) {
    [idle_clip, walk_clip, crouching_clip, ..] -> {
      // Create state machine
      let idle =
        model.new_animation(idle_clip)
        |> model.set_loop(model.LoopRepeat)

      let walking =
        model.new_animation(walk_clip)
        |> model.set_loop(model.LoopRepeat)

      let crouching =
        model.new_animation(crouching_clip)
        |> model.set_loop(model.LoopRepeat)

      statemachine.new(idle)
      |> statemachine.with_state(walking)
      |> statemachine.with_state(crouching)
      |> statemachine.with_transition(
        from: idle,
        to: walking,
        condition: statemachine.Custom(fn(ctx: tiramisu.Context) {
          input.is_key_pressed(ctx.input, input.KeyW)
        }),
        blend_duration: duration.milliseconds(300),
        easing: option.None,
        weight: 1,
      )
      |> statemachine.with_transition(
        from: walking,
        to: idle,
        condition: statemachine.Custom(fn(ctx: tiramisu.Context) {
          !input.is_key_pressed(ctx.input, input.KeyW)
        }),
        blend_duration: duration.milliseconds(300),
        easing: option.None,
        weight: 1,
      )
      |> statemachine.with_transition(
        from: walking,
        to: crouching,
        condition: statemachine.Custom(fn(ctx: tiramisu.Context) {
          input.is_key_pressed(ctx.input, input.ShiftLeft)
        }),
        blend_duration: duration.milliseconds(200),
        easing: option.None,
        weight: 1,
      )
      |> statemachine.with_transition(
        from: crouching,
        to: walking,
        condition: statemachine.Custom(fn(ctx: tiramisu.Context) {
          !input.is_key_pressed(ctx.input, input.ShiftLeft)
        }),
        blend_duration: duration.milliseconds(200),
        easing: option.None,
        weight: 1,
      )
    }
    _ -> {
      panic
    }
  }
}

fn view(model: Model, _ctx: tiramisu.Context) -> scene.Node {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    |> result.map(fn(camera) {
      camera
      |> scene.camera(
        id: "main",
        camera: _,
        active: True,
        transform: transform.at(position: vec3.Vec3(0.0, 1.5, 20.0)),
        viewport: option.None,
        postprocessing: option.None,
      )
    })

  let ambient =
    scene.light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    )

  let directional =
    scene.light(
      id: "directional",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 2.0)
        light
      },
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 7.5)),
    )

  case model.load_state {
    Loading -> {
      // Show a spinning cube while loading
      let loading_cube =
        scene.mesh(
          id: "loading-cube",
          geometry: {
            let assert Ok(geometry) =
              geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
            geometry
          },
          material: {
            let assert Ok(mat) =
              material.phong(
                0x4ecdc4,
                30.0,
                option.None,
                option.None,
                option.None,
                False,
                0.0,
                0.1,
              )
            mat
          },
          transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
            |> transform.with_euler_rotation(vec3.Vec3(
              model.rotation,
              model.rotation,
              0.0,
            )),
          physics: option.None,
        )
      scene.empty(id: "scene", transform: transform.identity, children: [
        camera,
        ambient,
        directional,
        loading_cube,
      ])
    }

    Failed(_error_msg) -> {
      // Show a red cube to indicate error
      let error_cube =
        scene.mesh(
          id: "error-cube",
          geometry: {
            let assert Ok(geometry) =
              geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
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
      scene.empty(id: "scene", transform: transform.identity, children: [
        camera,
        ambient,
        directional,
        error_cube,
      ])
    }

    Loaded(data, machine) -> {
      // Get animation from state machine
      let anim_playback = case statemachine.state_data(machine) {
        statemachine.Single(state) -> option.Some(model.SingleAnimation(state))
        statemachine.BlendingData(from:, to:, factor:) ->
          option.Some(model.BlendedAnimations(from, to, factor))
      }

      // Show the character model with animation
      let character =
        scene.object_3d(
          id: "character",
          object: model.get_scene(data),
          transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
            |> transform.with_euler_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
          animation: anim_playback,
          physics: option.None,
          material: option.None,
          transparent: False,
        )

      scene.empty(id: "scene", transform: transform.identity, children: [
        camera,
        ambient,
        directional,
        character,
      ])
    }
  }
}
