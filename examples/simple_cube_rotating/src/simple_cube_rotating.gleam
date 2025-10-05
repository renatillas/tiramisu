import gleam/option
import tiramisu/core/game
import tiramisu/math/vec3
import tiramisu/scene
import tiramisu/three/camera

pub type GameState {
  GameState(rotation: Float, time: Float)
}

pub fn main() -> Nil {
  let assert Ok(cam) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 800.0 /. 600.0,
      near: 0.1,
      far: 1000.0,
    )

  game.run(
    width: 800,
    height: 600,
    background: 0x1a1a2e,
    camera: cam,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_context: game.GameContext) -> GameState {
  GameState(rotation: 0.0, time: 0.0)
}

fn update(state: GameState, context: game.GameContext) -> GameState {
  GameState(
    rotation: state.rotation +. context.delta_time,
    time: state.time +. context.delta_time,
  )
}

fn view(state: GameState) -> List(scene.SceneNode) {
  [
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.5),
      transform: scene.identity_transform(),
    ),
    scene.Light(
      id: "directional",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.8),
      transform: scene.Transform(
        position: vec3.Vec3(5.0, 5.0, 5.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
    scene.Mesh(
      id: "cube",
      geometry: scene.BoxGeometry(width: 2.0, height: 2.0, depth: 2.0),
      material: scene.StandardMaterial(
        color: 0x4ecdc4,
        metalness: 0.3,
        roughness: 0.4,
        map: option.None,
      ),
      transform: scene.Transform(
        position: vec3.Vec3(0.0, 0.0, -5.0),
        rotation: vec3.Vec3(state.rotation, state.rotation *. 0.7, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]
}
