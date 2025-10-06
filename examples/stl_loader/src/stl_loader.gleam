/// STL Loader Example
///
/// Demonstrates loading 3D models from STL files
import gleam/javascript/promise
import gleam/option
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/game.{type GameContext}
import tiramisu/math/vec3
import tiramisu/scene.{type BufferGeometry}
import tiramisu/stl
import tiramisu/transform

pub type LoadState {
  Loading
  Loaded(BufferGeometry)
  Failed
}

pub type Model {
  Model(rotation: Float, load_state: LoadState)
}

pub type Msg {
  Tick
  ModelLoaded(BufferGeometry)
  LoadingFailed
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
    |> camera.set_position(vec3.Vec3(0.0, 5.0, 10.0))
    |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

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

  // Load an STL file from the assets directory
  // Place your STL file in the Lustre assets folder
  let load_effect =
    effect.from_promise(
      promise.map(stl.load("model.stl"), fn(result) {
        case result {
          Ok(geom) -> ModelLoaded(geom)
          Error(error) -> {
            echo error
            LoadingFailed
          }
        }
      }),
    )

  #(model, effect.batch([effect.tick(Tick), load_effect]))
}

fn update(model: Model, msg: Msg, ctx: GameContext) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.5
      #(Model(..model, rotation: new_rotation), effect.tick(Tick))
    }

    ModelLoaded(geom) -> {
      #(Model(..model, load_state: Loaded(geom)), effect.none())
    }

    LoadingFailed -> {
      #(Model(..model, load_state: Failed), effect.none())
    }
  }
}

fn view(model: Model) -> List(scene.SceneNode) {
  let lights = [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 1.0),
      transform: transform.identity(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 10.5),
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 70.5)),
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
            color: 0xffffff,
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

    Failed -> {
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

    Loaded(geom) -> {
      // Show the loaded STL model
      let model_node =
        scene.Group(id: "stl_group", transform: transform.identity(), children: [
          scene.Mesh(
            id: "standard",
            geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
            material: scene.StandardMaterial(
              color: 0x4ecdc4,
              metalness: 0.8,
              roughness: 0.2,
              map: option.None,
            ),
            transform: transform.Transform(
              position: vec3.Vec3(-3.0, 2.0, 0.0),
              rotation: vec3.Vec3(0.0, model.rotation, 0.0),
              scale: vec3.Vec3(1.0, 1.0, 1.0),
            ),
            physics: option.None,
          ),
          scene.Mesh(
            id: "stl_model",
            geometry: scene.CustomGeometry(geom),
            material: scene.StandardMaterial(
              color: 0x4ecdc4,
              metalness: 0.8,
              roughness: 0.2,
              map: option.None,
            ),
            transform: transform.Transform(
              position: vec3.zero(),
              rotation: vec3.Vec3(0.0, model.rotation, 0.0),
              scale: vec3.Vec3(0.1, 0.1, 0.1),
            ),
            physics: option.None,
          ),
        ])

      [model_node, ..lights]
    }
  }
}
