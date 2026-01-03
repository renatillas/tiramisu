import gleam/javascript/promise
import gleam/option
import gleam/time/duration
import tiramisu
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
  Scene
  Lights
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
  Loaded(geometry.CustomGeometry)
  Failed
}

pub type Model {
  Model(rotation: Float, load_state: LoadState)
}

pub type Msg {
  Tick
  ModelLoaded(geometry.CustomGeometry)
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

  // Load an STL file from the asset directory
  // Place your STL file in the Lustre asset folder
  let load_effect = geometry.load_stl("model.stl", ModelLoaded, LoadingFailed)

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
      #(Model(..model, rotation: new_rotation), effect.dispatch(Tick), option.None)
    }

    ModelLoaded(geom) -> {
      // Center the geometry so it rotates around its geometric center
      let centered_geom = geometry.center(geom)
      #(
        Model(..model, load_state: Loaded(centered_geom)),
        effect.none(),
        option.None,
      )
    }

    LoadingFailed -> {
      #(Model(..model, load_state: Failed), effect.none(), option.None)
    }
  }
}

fn view(model: Model, _) -> scene.Node {
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let camera_node =
    scene.camera(
      id: "main-camera",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 15.0)),
      active: True,
      viewport: option.None,
      postprocessing: option.None,
    )

  let lights =
    scene.empty(id: "lights", transform: transform.identity, children: [
      scene.light(
        id: "ambient",
        light: {
          let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 1.0)
          light
        },
        transform: transform.identity,
      ),
      scene.light(
        id: "directional",
        light: {
          let assert Ok(light) =
            light.directional(color: 0xffffff, intensity: 10.5)
          light
        },
        transform: transform.at(position: vec3.Vec3(5.0, 10.0, 70.5)),
      ),
    ])

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
                0xffffff,
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
        camera_node,
        loading_cube,
        lights,
      ])
    }

    Failed -> {
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
        camera_node,
        error_cube,
        lights,
      ])
    }

    Loaded(geom) -> {
      // Show the loaded STL model
      let model_node =
        scene.mesh(
          id: "stl-model",
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
            |> transform.with_euler_rotation(vec3.Vec3(0.0, model.rotation, 0.0))
            |> transform.with_scale(vec3.Vec3(0.1, 0.1, 0.1)),
          physics: option.None,
        )
      scene.empty(id: "scene", transform: transform.identity, children: [
        camera_node,
        model_node,
        lights,
      ])
    }
  }
}
