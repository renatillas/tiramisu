import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam_community/maths
import tiramisu
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec2
import vec/vec3

// Camera field of view in degrees
const camera_fov = 75.0

// Z-depth for main cube
const cube_depth = -5.0

// Z-depth for touch indicator spheres
const touch_indicator_depth = -3.0

pub type Model {
  Model(
    cube_position: vec3.Vec3(Float),
    cube_scale: Float,
    touches: List(input.Touch),
    canvas_width: Float,
    canvas_height: Float,
  )
}

pub type Msg {
  Tick
}

/// Convert screen coordinates to world coordinates at a given depth.
/// Uses perspective projection with the camera's FOV.
fn screen_to_world(
  screen: vec2.Vec2(Float),
  canvas_size: vec2.Vec2(Float),
  z_depth: Float,
) -> #(Float, Float) {
  // Calculate visible world dimensions at the given depth
  let fov_radians = camera_fov *. maths.pi() /. 180.0
  let tan_half_fov = maths.tan(fov_radians /. 2.0)
  let object_distance = float.absolute_value(z_depth)
  let visible_height = 2.0 *. object_distance *. tan_half_fov
  let aspect = canvas_size.x /. canvas_size.y
  let visible_width = visible_height *. aspect

  // Convert screen pixels to world units
  let world_x =
    { screen.x -. canvas_size.x /. 2.0 } *. visible_width /. canvas_size.x
  let world_y =
    { canvas_size.y /. 2.0 -. screen.y } *. visible_height /. canvas_size.y

  #(world_x, world_y)
}

pub fn main() -> Nil {
  let assert Ok(Nil) =
    tiramisu.application(init, update, view)
    |> tiramisu.start("body", tiramisu.FullScreen, option.None)
  Nil
}

fn init(ctx: tiramisu.Context) -> #(Model, Effect(Msg), option.Option(_)) {
  let model =
    Model(
      cube_position: vec3.Vec3(0.0, 0.0, cube_depth),
      cube_scale: 1.0,
      touches: [],
      canvas_width: ctx.canvas_size.x,
      canvas_height: ctx.canvas_size.y,
    )
  #(model, effect.dispatch(Tick), option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let touches = get_touches_or_mouse(ctx.input)
      let new_position = calculate_cube_position(touches, ctx, model)
      let new_scale = calculate_pinch_scale(touches, model)

      let new_model =
        Model(
          cube_position: new_position,
          cube_scale: new_scale,
          touches:,
          canvas_width: ctx.canvas_size.x,
          canvas_height: ctx.canvas_size.y,
        )

      #(new_model, effect.dispatch(Tick), option.None)
    }
  }
}

/// Get touch inputs, or simulate with mouse for desktop testing
fn get_touches_or_mouse(input: input.InputState) -> List(input.Touch) {
  case input.touches(input) {
    [] ->
      case input.is_left_button_just_pressed(input) {
        True -> {
          let mouse = input.mouse_position(input)
          [input.Touch(id: 0, position: mouse)]
        }
        False -> []
      }
    touches -> touches
  }
}

/// Calculate cube position from single touch
fn calculate_cube_position(
  touches: List(input.Touch),
  ctx: tiramisu.Context,
  model: Model,
) -> vec3.Vec3(Float) {
  case touches {
    [first, ..] -> {
      let #(x, y) = screen_to_world(first.position, ctx.canvas_size, cube_depth)
      vec3.Vec3(x, y, cube_depth)
    }
    [] -> model.cube_position
  }
}

/// Calculate scale from pinch gesture (two touches)
fn calculate_pinch_scale(touches: List(input.Touch), model: Model) -> Float {
  case touches {
    [first, second, ..] -> {
      let dx = first.position.x -. second.position.x
      let dy = first.position.y -. second.position.y
      let distance =
        float.square_root({ dx *. dx } +. { dy *. dy })
        |> result.unwrap(200.0)

      // Map distance to scale (200px = 1.0 scale)
      let scale = distance /. 200.0
      case scale {
        s if s >. 0.2 && s <. 3.0 -> s
        _ -> model.cube_scale
      }
    }
    _ -> model.cube_scale
  }
}

fn view(model: Model, ctx) -> scene.Node {
  let camera_node = create_camera()
  let lights = create_lights()
  let main_cube = create_main_cube(model)
  let touch_indicators = create_touch_indicators(model, ctx)

  scene.empty(
    id: "scene",
    transform: transform.identity,
    children: list.flatten([[camera_node], lights, main_cube, touch_indicators]),
  )
}

/// Create the main camera
fn create_camera() -> scene.Node {
  let assert Ok(cam) =
    camera.perspective(field_of_view: camera_fov, near: 0.1, far: 1000.0)

  scene.camera(
    id: "main",
    camera: cam,
    transform: transform.identity,
    active: True,
    viewport: option.None,
    postprocessing: option.None,
  )
}

/// Create scene lights
fn create_lights() -> List(scene.Node) {
  let assert Ok(ambient) = light.ambient(color: 0xffffff, intensity: 0.6)
  let assert Ok(directional) =
    light.directional(color: 0xffffff, intensity: 0.8)

  [
    scene.light(id: "ambient", light: ambient, transform: transform.identity),
    scene.light(
      id: "directional",
      light: directional,
      transform: transform.at(position: vec3.Vec3(5.0, 5.0, 5.0)),
    ),
  ]
}

/// Create the main controllable cube
fn create_main_cube(model: Model) -> List(scene.Node) {
  let assert Ok(box) = geometry.box(size: vec3.Vec3(2.0, 2.0, 2.0))
  let assert Ok(cube_material) =
    material.new()
    |> material.with_color(0x4ecdc4)
    |> material.build

  [
    scene.mesh(
      id: "cube",
      geometry: box,
      material: cube_material,
      transform: transform.at(position: model.cube_position)
        |> transform.with_euler_rotation(vec3.Vec3(0.0, 0.0, 0.0))
        |> transform.scale_uniform(model.cube_scale),
      physics: option.None,
    ),
  ]
}

/// Create visual indicators for touch points
fn create_touch_indicators(
  model: Model,
  ctx: tiramisu.Context,
) -> List(scene.Node) {
  list.map(model.touches, fn(touch) {
    create_touch_indicator(touch, ctx.canvas_size)
  })
}

/// Create a single touch indicator sphere
fn create_touch_indicator(
  touch: input.Touch,
  canvas_size: vec2.Vec2(Float),
) -> scene.Node {
  let assert Ok(sphere) =
    geometry.sphere(radius: 0.3, segments: vec2.Vec2(16, 16))
  let assert Ok(indicator_material) =
    material.basic(
      color: 0xff00ff,
      transparent: True,
      opacity: 0.7,
      map: option.None,
      side: material.FrontSide,
      alpha_test: 0.0,
      depth_write: True,
    )

  let #(x, y) =
    screen_to_world(touch.position, canvas_size, touch_indicator_depth)

  scene.mesh(
    id: "touch-" <> int.to_string(touch.id),
    geometry: sphere,
    material: indicator_material,
    transform: transform.at(position: vec3.Vec3(x, y, touch_indicator_depth)),
    physics: option.None,
  )
}
