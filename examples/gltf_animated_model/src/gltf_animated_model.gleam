import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/time/duration
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
import vec/vec3f

pub type LoadState {
  Loading
  Loaded(model.GLTFData)
  Failed(String)
}

pub type Model {
  Model(
    rotation: Float,
    load_state: LoadState,
    current_animation: option.Option(model.Animation),
    animation_speed: Float,
  )
}

pub type Msg {
  Tick
  ModelLoaded(model.GLTFData)
  LoadingFailed
  NextAnimation
  ToggleSpeed
}

pub fn main() -> Nil {
  let assert Ok(Nil) =
    tiramisu.application(init, update, view)
    |> tiramisu.start("body", tiramisu.FullScreen, option.None)
  Nil
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg), option.Option(_)) {
  let model =
    Model(
      rotation: 0.0,
      load_state: Loading,
      current_animation: option.None,
      animation_speed: 1.0,
    )

  let load_effect = model.load_gltf("model.glb", ModelLoaded, LoadingFailed)

  #(model, effect.batch([effect.dispatch(Tick), load_effect]), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let effects = case input.is_key_just_pressed(ctx.input, input.Space) {
        True ->
          effect.batch([
            effect.dispatch(Tick),
            effect.dispatch(NextAnimation),
          ])
        False -> effect.dispatch(Tick)
      }
      let new_rotation =
        model.rotation +. 0.3 *. duration.to_seconds(ctx.delta_time)
      #(Model(..model, rotation: new_rotation), effects, option.None)
    }

    ModelLoaded(data) -> {
      let clips = model.get_animations(data)
      let animation_count = list.length(clips)
      io.println(
        "Loaded GLTF model with "
        <> int.to_string(animation_count)
        <> " animations",
      )

      // Print animation names
      case clips {
        [] -> io.println("No animations found")
        _ -> {
          io.println("Available animations:")
          list.index_map(clips, fn(clip, idx) {
            io.println(
              "  [" <> int.to_string(idx) <> "] " <> model.clip_name(clip),
            )
          })
          Nil
        }
      }

      // Create initial animation from first clip
      let initial_animation =
        list.first(clips)
        |> result.map(fn(clip) {
          model.new_animation(clip)
          |> model.set_loop(model.LoopRepeat)
          |> model.set_speed(model.animation_speed)
        })
        |> option.from_result()

      #(
        Model(
          ..model,
          load_state: Loaded(data),
          current_animation: initial_animation,
        ),
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

    NextAnimation -> {
      case model.load_state {
        Loaded(data) -> {
          let new_animation =
            list.sample(model.get_animations(data), 1)
            |> list.first
            |> result.map(fn(clip) {
              model.new_animation(clip)
              |> model.set_loop(model.LoopRepeat)
              |> model.set_speed(model.animation_speed)
            })
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
      // Update the stored animation's speed
      let updated_animation =
        option.map(model.current_animation, fn(anim) {
          model.set_speed(anim, new_speed)
        })
      #(
        Model(
          ..model,
          animation_speed: new_speed,
          current_animation: updated_animation,
        ),
        effect.none(),
        option.None,
      )
    }
  }
}

fn view(model: Model, _ctx: tiramisu.Context) -> scene.Node {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
    |> result.map(fn(camera) {
      camera
      |> scene.camera(
        id: "main-camera",
        transform: transform.at(position: vec3.Vec3(0.0, 0.0, 20.0)),
        active: True,
        viewport: option.None,
        camera: _,
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
            let assert Ok(geometry) = geometry.box(size: vec3f.one)
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
                transparent: False,
                opacity: 1.0,
                alpha_test: 0.0,
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
            let assert Ok(geometry) = geometry.box(size: vec3f.one)
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

    Loaded(gltf_model) -> {
      // Use the stored animation directly (same object reference each frame)
      let anim_playback =
        option.map(model.current_animation, model.SingleAnimation)

      // Show the loaded GLTF model with animation
      let model_node =
        scene.object_3d(
          id: "gltf-model",
          object: model.get_scene(gltf_model),
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
        model_node,
      ])
    }
  }
}
