//// Snake Game - A classic snake game built with Tiramisu

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleam/time/duration
import tiramisu
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec2
import vec/vec3

// =============================================================================
// Constants
// =============================================================================

const box_width = 50.0

const highscore_key = "Highscore"

// =============================================================================
// Types
// =============================================================================

pub type Model {
  Model(
    time: Float,
    head: BoxData,
    tail: List(BoxData),
    beute_pos: #(Float, Float),
    game_state: GameState,
    maybe_font: option.Option(geometry.Font),
    score_info: ScoreInfo,
  )
}

pub type GameState {
  Running
  NotStarted
  GameOver
}

pub type ScoreInfo {
  ScoreInfo(
    current_score: Int,
    highscore: option.Option(Int),
    new_high_score: Bool,
  )
}

pub type BoxData {
  BoxData(x: Float, y: Float, direction: Direction)
}

pub type Direction {
  Right
  Left
  Up
  Down
}

pub type Msg {
  FontLoaded(geometry.Font)
  FontLoadFailed
  Tick
}

type Color {
  BeuteColor
  SnakeHeadColor
  SnakeTailColor
  PrimeColor
  SecColor
  BorderColor
}

// =============================================================================
// Main Entry Point
// =============================================================================

pub fn main() -> Nil {
  let assert Ok(_) =
    tiramisu.run(
      selector: "#app",
      dimensions: option.None,
      bridge: option.None,
      init: init,
      update: update,
      view: view,
    )
  Nil
}

// =============================================================================
// Init
// =============================================================================

pub fn init(ctx: tiramisu.Context) {
  let pot_highscore =
    get_localstorage(highscore_key)
    |> result.try(fn(x) {
      let assert Ok(val) = decode.run(x, decode.int)
      Ok(val)
    })
  let highscore = option.from_result(pot_highscore)
  #(
    init_model(highscore, option.None, ctx),
    effect.batch([
      effect.tick(Tick),
      geometry.load_font(
        from: "fonts/helvetiker_regular.typeface.json",
        on_success: FontLoaded,
        on_error: FontLoadFailed,
      ),
    ]),
    option.None,
  )
}

fn init_model(
  highscore: option.Option(Int),
  maybe_font: option.Option(geometry.Font),
  ctx: tiramisu.Context,
) -> Model {
  let init_beute_pos = random_pos(ctx)
  Model(
    time: 0.0,
    head: BoxData(x: 0.0, y: 0.0, direction: Right),
    tail: [],
    beute_pos: init_beute_pos,
    game_state: Running,
    maybe_font: maybe_font,
    score_info: ScoreInfo(
      current_score: 0,
      highscore: highscore,
      new_high_score: False,
    ),
  )
}

// =============================================================================
// Update
// =============================================================================

pub fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  let updatet_model = case msg {
    Tick -> {
      case model.game_state {
        Running -> update_running_model(model, ctx)
        GameOver -> {
          case
            input.is_left_button_just_pressed(ctx.input)
            || input.is_key_just_pressed(ctx.input, input.Enter)
          {
            True -> {
              init_model(model.score_info.highscore, model.maybe_font, ctx)
            }
            False -> model
          }
        }
        _ -> model
      }
    }
    FontLoaded(font) -> Model(..model, maybe_font: option.Some(font))
    FontLoadFailed -> model
  }
  #(updatet_model, effect.tick(Tick), option.None)
}

fn update_running_model(model: Model, ctx: tiramisu.Context) -> Model {
  case check_game_over(model, ctx) {
    True -> {
      let score = model.score_info.current_score
      let pot_new_highscore = case model.score_info.highscore {
        option.Some(highscore_val) ->
          case score > highscore_val {
            True -> {
              set_localstorage(highscore_key, int.to_string(score))

              ScoreInfo(
                ..model.score_info,
                highscore: option.Some(score),
                new_high_score: True,
              )
            }
            _ -> model.score_info
          }
        _ ->
          ScoreInfo(
            ..model.score_info,
            highscore: option.Some(score),
            new_high_score: True,
          )
      }
      Model(..model, game_state: GameOver, score_info: pot_new_highscore)
    }
    False -> update_snake_beute(model, ctx)
  }
}

fn check_game_over(model: Model, ctx: tiramisu.Context) -> Bool {
  let hx = model.head.x
  let hy = model.head.y
  let border_check =
    hx >. right_border(ctx)
    || hx <. left_border(ctx)
    || hy >. upper_border(ctx)
    || hy <. down_border(ctx)

  let own_tail_check = list.any(model.tail, fn(a) { a.x == hx && a.y == hy })
  border_check || own_tail_check
}

fn calculate_new_beute_pos(
  model: Model,
  safety: Int,
  ctx: tiramisu.Context,
) -> #(Float, Float) {
  let new_pos_candidate = random_pos(ctx)
  let is_too_close =
    list.append([model.head], model.tail)
    |> list.any(spawn_too_close(_, new_pos_candidate))
  case is_too_close && safety < 5 {
    True -> {
      calculate_new_beute_pos(model, safety + 1, ctx)
    }
    False -> new_pos_candidate
  }
}

fn random_pos(ctx: tiramisu.Context) -> #(Float, Float) {
  let abs_x = ctx.canvas_size.y -. 2.0 *. box_width
  let rand_x = float.round(abs_x /. box_width) - 1

  let abs_y = ctx.canvas_size.y -. 2.0 *. horz_border_dist() -. 2.0 *. box_width
  let rand_y = float.round(abs_y /. box_width) - 1
  #(
    int.to_float(int.random(rand_x) - rand_x / 2) *. box_width,
    int.to_float(int.random(rand_y) - rand_y / 2) *. box_width,
  )
}

fn spawn_too_close(snake_element: BoxData, cand: #(Float, Float)) -> Bool {
  let dist_x = float.absolute_value(snake_element.x -. cand.0)
  let dist_y = float.absolute_value(snake_element.y -. cand.1)
  dist_x <. 2.0 *. box_width && dist_y <. 2.0 *. box_width
}

fn update_snake_beute(model: Model, ctx: tiramisu.Context) -> Model {
  let delta_seconds = duration.to_seconds(ctx.delta_time)
  let new_time = model.time +. delta_seconds /. 10.0
  let new_direction = parse_direction_from_key(ctx, model)

  let threshold = 0.02
  case new_time >. threshold {
    True -> {
      let is_grefressen = is_gefressen_cal(model)
      let new_score = case is_grefressen {
        True -> model.score_info.current_score + 1
        False -> model.score_info.current_score
      }

      let new_beute_pos = case is_grefressen {
        False -> model.beute_pos
        _ -> calculate_new_beute_pos(model, 0, ctx)
      }

      let enhanced_tail = case is_grefressen {
        False -> model.tail
        _ -> {
          let last_element = case model.tail {
            [] -> model.head
            [_, ..] -> {
              let assert Ok(last_element) = list.last(model.tail)
              last_element
            }
          }
          let new_tail_element = case last_element.direction {
            Right -> BoxData(..last_element, x: last_element.x -. box_width)
            Left -> BoxData(..last_element, x: last_element.x +. box_width)
            Up -> BoxData(..last_element, y: last_element.y -. box_width)
            Down -> BoxData(..last_element, y: last_element.y +. box_width)
          }
          list.append(model.tail, [new_tail_element])
        }
      }
      let new_tail = update_tail_pos(model.head, enhanced_tail)

      let #(new_x, new_y) = update_head_pos(model.head, new_direction)

      Model(
        ..model,
        time: 0.0,
        head: BoxData(x: new_x, y: new_y, direction: new_direction),
        tail: new_tail,
        beute_pos: new_beute_pos,
        game_state: Running,
        score_info: ScoreInfo(..model.score_info, current_score: new_score),
      )
    }
    False -> {
      Model(
        ..model,
        time: new_time,
        head: BoxData(..model.head, direction: new_direction),
      )
    }
  }
}

fn update_head_pos(box_data: BoxData, direction: Direction) -> #(Float, Float) {
  let horizontal_mov = case direction {
    Right -> box_width
    Left -> float.negate(box_width)
    _ -> 0.0
  }

  let vertical_mov = case direction {
    Up -> box_width
    Down -> float.negate(box_width)
    _ -> 0.0
  }
  #(box_data.x +. horizontal_mov, box_data.y +. vertical_mov)
}

fn parse_direction_from_key(ctx: tiramisu.Context, model: Model) -> Direction {
  let is_left =
    input.is_key_just_pressed(ctx.input, input.ArrowLeft)
    || input.is_key_just_pressed(ctx.input, input.KeyA)
  let is_right =
    input.is_key_just_pressed(ctx.input, input.ArrowRight)
    || input.is_key_just_pressed(ctx.input, input.KeyD)
  let is_up =
    input.is_key_just_pressed(ctx.input, input.ArrowUp)
    || input.is_key_just_pressed(ctx.input, input.KeyW)
  let is_down =
    input.is_key_just_pressed(ctx.input, input.ArrowDown)
    || input.is_key_just_pressed(ctx.input, input.KeyS)
  case is_left, is_right, is_up, is_down {
    True, _, _, _ -> check_new_direction_is_possible(Left, model)
    _, True, _, _ -> check_new_direction_is_possible(Right, model)
    _, _, True, _ -> check_new_direction_is_possible(Up, model)
    _, _, _, True -> check_new_direction_is_possible(Down, model)
    _, _, _, _ -> model.head.direction
  }
}

fn check_new_direction_is_possible(
  new_direction: Direction,
  model: Model,
) -> Direction {
  case model.tail {
    [] -> new_direction
    [first_tail, ..] -> {
      let head = model.head
      let old_direction = head.direction
      case new_direction {
        Right ->
          if_true_old_else_new(
            first_tail.x >. head.x && first_tail.y == head.y,
            old_direction,
            new_direction,
          )
        Left ->
          if_true_old_else_new(
            first_tail.x <. head.x && first_tail.y == head.y,
            old_direction,
            new_direction,
          )
        Up ->
          if_true_old_else_new(
            first_tail.y >. head.y && first_tail.x == head.x,
            old_direction,
            new_direction,
          )
        Down ->
          if_true_old_else_new(
            first_tail.y <. head.y && first_tail.x == head.x,
            old_direction,
            new_direction,
          )
      }
    }
  }
}

fn if_true_old_else_new(
  check: Bool,
  old_direction: Direction,
  new_direction: Direction,
) -> Direction {
  case check {
    True -> old_direction
    False -> new_direction
  }
}

fn is_gefressen_cal(model: Model) -> Bool {
  let threshold = box_width /. 10.0
  let BoxData(hx, hy, _) = model.head
  let #(bx, by) = model.beute_pos
  float.absolute_value(hx -. bx) <. threshold
  && float.absolute_value(hy -. by) <. threshold
}

fn update_tail_pos(head_pos: BoxData, tail_pos: List(BoxData)) -> List(BoxData) {
  case tail_pos {
    [] -> tail_pos
    _ ->
      [head_pos]
      |> list.append(list.reverse(tail_pos) |> list.drop(1) |> list.reverse)
  }
}

// =============================================================================
// View
// =============================================================================

pub fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  let cam =
    camera.camera_2d(size: vec2.Vec2(
      float.round(ctx.canvas_size.x),
      float.round(ctx.canvas_size.y),
    ))

  let init_elements = [
    scene.camera(
      id: "camera",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 20.0)),
      look_at: option.None,
      active: True,
      viewport: option.None,
      postprocessing: option.None,
    ),
    scene.light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 1.0)
        light
      },
      transform: transform.identity,
    ),
  ]
  let base_elements =
    list.append(init_elements, create_static_view(ctx))
    |> list.append(create_score_display(model, ctx))

  let all_elements =
    base_elements
    |> list.append(case model.game_state {
      Running -> create_running_game_view(model)
      GameOver -> create_game_over(model)
      _ -> []
    })
  scene.empty(
    id: "rootNode",
    children: all_elements,
    transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
  )
}

fn color_hex(color: Color) -> Int {
  case color {
    BeuteColor -> 0xfcba03
    SnakeHeadColor -> 0x34eb4c
    SnakeTailColor -> 0x42f5b6
    PrimeColor -> 0xeb0933
    BorderColor -> 0xeb4034
    SecColor -> 0x34d0eb
  }
}

fn create_static_view(ctx: tiramisu.Context) -> List(scene.Node) {
  let assert Ok(border_material) =
    material.new()
    |> material.with_color(color_hex(BorderColor))
    |> material.with_metalness(0.2)
    |> material.with_roughness(0.9)
    |> material.build()

  let assert Ok(horizontal_border_geometry) =
    geometry.box(size: vec3.Vec3(ctx.canvas_size.x, box_width /. 5.0, 1.0))

  let assert Ok(vertical_border_geometry) =
    geometry.box(size: vec3.Vec3(
      box_width /. 5.0,
      ctx.canvas_size.y -. 2.0 *. horz_border_dist(),
      1.0,
    ))
  let borders = [
    scene.mesh(
      id: "upperLine",
      geometry: horizontal_border_geometry,
      material: border_material,
      transform: transform.at(position: vec3.Vec3(0.0, upper_border(ctx), 0.0)),
      physics: option.None,
    ),
    scene.mesh(
      id: "downLine",
      geometry: horizontal_border_geometry,
      material: border_material,
      transform: transform.at(position: vec3.Vec3(0.0, down_border(ctx), 0.0)),
      physics: option.None,
    ),
    scene.mesh(
      id: "leftLine",
      geometry: vertical_border_geometry,
      material: border_material,
      transform: transform.at(position: vec3.Vec3(left_border(ctx), 0.0, 0.0)),
      physics: option.None,
    ),
    scene.mesh(
      id: "rightLine",
      geometry: vertical_border_geometry,
      material: border_material,
      transform: transform.at(position: vec3.Vec3(right_border(ctx), 0.0, 0.0)),
      physics: option.None,
    ),
  ]

  borders
}

fn create_game_over(model: Model) {
  let assert Ok(game_over_text_material) =
    material.new()
    |> material.with_color(color_hex(PrimeColor))
    |> material.with_metalness(0.0)
    |> material.with_roughness(0.7)
    |> material.build()
  let score_string =
    "GAME OVER FINAL SCORE: " <> int.to_string(model.score_info.current_score)
  let score_string_new_highscore = case model.score_info.new_high_score {
    True -> score_string <> " (New Highscore)"
    False -> score_string
  }
  let text_elements = case model.maybe_font {
    option.Some(font) -> {
      let assert Ok(game_over_text) =
        geometry.text(
          text: score_string_new_highscore,
          font: font,
          size: 50.0,
          depth: 0.2,
          curve_segments: 12,
          bevel_enabled: True,
          bevel_thickness: 0.05,
          bevel_size: 0.02,
          bevel_offset: 0.0,
          bevel_segments: 5,
        )

      let text_scene =
        scene.mesh(
          id: "game over display",
          geometry: game_over_text,
          material: game_over_text_material,
          transform: transform.at(position: vec3.Vec3(
            0.0 -. 9.0 *. box_width,
            0.0,
            0.0,
          )),
          physics: option.None,
        )

      let assert Ok(restart_hint_text) =
        geometry.text(
          text: "Press Enter or left Mouse for restart",
          font: font,
          size: 30.0,
          depth: 0.2,
          curve_segments: 12,
          bevel_enabled: True,
          bevel_thickness: 0.05,
          bevel_size: 0.02,
          bevel_offset: 0.0,
          bevel_segments: 5,
        )

      let assert Ok(restart_hint_material) =
        material.new()
        |> material.with_color(color_hex(SecColor))
        |> material.with_metalness(0.2)
        |> material.with_roughness(0.9)
        |> material.build()

      let restart_button_scene =
        scene.mesh(
          id: "restartHint",
          geometry: restart_hint_text,
          material: restart_hint_material,
          transform: transform.at(position: vec3.Vec3(
            0.0 -. 9.0 *. box_width,
            0.0 -. box_width,
            0.0,
          )),
          physics: option.None,
        )
      [text_scene, restart_button_scene]
    }
    _ -> []
  }
  text_elements
}

fn create_score_display(model: Model, ctx: tiramisu.Context) -> List(scene.Node) {
  let assert Ok(score_text_material) =
    material.new()
    |> material.with_color(color_hex(SecColor))
    |> material.with_metalness(0.0)
    |> material.with_roughness(0.7)
    |> material.build()
  let score_string = "Score: " <> int.to_string(model.score_info.current_score)
  let complete_score = case model.score_info.highscore {
    option.Some(val) ->
      score_string <> " (Highscore: " <> int.to_string(val) <> ")"
    _ -> score_string
  }
  let text_elements = case model.maybe_font {
    option.Some(font) -> {
      let assert Ok(text) =
        geometry.text(
          text: complete_score,
          font: font,
          size: 36.0,
          depth: 0.2,
          curve_segments: 12,
          bevel_enabled: True,
          bevel_thickness: 0.05,
          bevel_size: 0.02,
          bevel_offset: 0.0,
          bevel_segments: 5,
        )

      let text_scene =
        scene.mesh(
          id: "score_display",
          geometry: text,
          material: score_text_material,
          transform: transform.at(position: vec3.Vec3(
            0.0 -. 200.0,
            upper_border(ctx) +. horz_border_dist() /. 2.0,
            0.0,
          )),
          physics: option.None,
        )
      [text_scene]
    }
    _ -> []
  }
  text_elements
}

fn create_running_game_view(model: Model) -> List(scene.Node) {
  let assert Ok(cube_geometry) =
    geometry.box(size: vec3.Vec3(box_width, box_width, 1.0))
  let assert Ok(head_material) =
    material.new()
    |> material.with_color(color_hex(SnakeHeadColor))
    |> material.with_metalness(0.2)
    |> material.with_roughness(0.9)
    |> material.build()

  let assert Ok(beute_material) =
    material.new()
    |> material.with_color(color_hex(BeuteColor))
    |> material.with_metalness(0.2)
    |> material.with_roughness(0.9)
    |> material.build()

  let head_position = vec3.Vec3(model.head.x, model.head.y, 0.0)
  let dynamic_elements = [
    scene.mesh(
      id: "snakeHead",
      geometry: cube_geometry,
      material: head_material,
      transform: transform.at(position: head_position),
      physics: option.None,
    ),
    scene.mesh(
      id: "beute",
      geometry: cube_geometry,
      material: beute_material,
      transform: transform.at(position: vec3.Vec3(
        model.beute_pos.0,
        model.beute_pos.1,
        0.0,
      )),
      physics: option.None,
    ),
    ..tail_elements(model)
  ]
  dynamic_elements
}

fn tail_elements(model: Model) -> List(scene.Node) {
  let assert Ok(cube_geometry) =
    geometry.box(size: vec3.Vec3(box_width, box_width, 1.0))

  let assert Ok(tail_material) =
    material.new()
    |> material.with_color(color_hex(SnakeTailColor))
    |> material.with_metalness(0.2)
    |> material.with_roughness(0.9)
    |> material.build()
  model.tail
  |> list.map(fn(tail_element) { #(tail_element.x, tail_element.y) })
  |> list.index_map(fn(tuple, index) {
    create_box_cell_mesh(tuple.0, tuple.1, index, cube_geometry, tail_material)
  })
}

fn create_box_cell_mesh(
  x: Float,
  y: Float,
  index: Int,
  cube_geometry: geometry.Geometry,
  tail_material: material.Material,
) -> scene.Node {
  scene.mesh(
    id: string.append("TailElement", int.to_string(index)),
    geometry: cube_geometry,
    material: tail_material,
    transform: transform.at(position: vec3.Vec3(x, y, 0.0)),
    physics: option.None,
  )
}

// =============================================================================
// Border Calculations
// =============================================================================

fn left_border(ctx: tiramisu.Context) -> Float {
  0.0 -. half_vert_calc(ctx)
}

fn right_border(ctx: tiramisu.Context) -> Float {
  half_vert_calc(ctx)
}

fn half_vert_calc(ctx: tiramisu.Context) -> Float {
  ctx.canvas_size.x /. 2.0
}

fn upper_border(ctx: tiramisu.Context) -> Float {
  half_horz_calc(ctx)
}

fn down_border(ctx: tiramisu.Context) -> Float {
  0.0 -. half_horz_calc(ctx)
}

fn half_horz_calc(ctx: tiramisu.Context) -> Float {
  ctx.canvas_size.y /. 2.0 -. horz_border_dist()
}

fn horz_border_dist() -> Float {
  3.0 *. box_width
}

// =============================================================================
// FFI - Local Storage
// =============================================================================

@external(javascript, "./local_storage.ffi.mjs", "set_localstorage")
fn set_localstorage(_key: String, _value: String) -> Nil {
  Nil
}

@external(javascript, "./local_storage.ffi.mjs", "get_localstorage")
fn get_localstorage(_key: String) -> Result(Dynamic, Nil) {
  Error(Nil)
}
