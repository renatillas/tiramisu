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
import input
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
import tiramisu/scene
import tiramisu/tick
import tiramisu/transform
import vec/vec3

// =============================================================================
// Constants
// =============================================================================

const box_width = 50.0

/// Canvas width in world units (1 unit = 1 pixel with the orthographic camera).
const canvas_width = 1920.0

/// Canvas height in world units.
const canvas_height = 1080.0

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
    score_info: ScoreInfo,
    input: input.InputState,
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
  Tick(tick.TickContext)
  KeyDown(input.Key)
  KeyUp(input.Key)
  MouseDown(input.MouseButton)
  MouseUp(input.MouseButton)
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
  let assert Ok(_) = tiramisu.register([])
  let assert Ok(_) =
    lustre.application(init, update, view)
    |> lustre.start("#app", Nil)
  Nil
}

// =============================================================================
// Init
// =============================================================================

pub fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let pot_highscore =
    get_localstorage(highscore_key)
    |> result.try(fn(x) {
      let assert Ok(val) = decode.run(x, decode.int)
      Ok(val)
    })
  let highscore = option.from_result(pot_highscore)
  #(init_model(highscore), tick.subscribe("snake", Tick))
}

fn init_model(highscore: option.Option(Int)) -> Model {
  Model(
    time: 0.0,
    head: BoxData(x: 0.0, y: 0.0, direction: Right),
    tail: [],
    beute_pos: random_pos(),
    game_state: Running,
    score_info: ScoreInfo(
      current_score: 0,
      highscore: highscore,
      new_high_score: False,
    ),
    input: input.new(),
  )
}

// =============================================================================
// Update
// =============================================================================

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      let updated = case model.game_state {
        Running -> update_running_model(model, ctx)
        GameOver -> {
          case
            input.is_mouse_just_pressed(model.input, input.LeftButton)
            || input.is_just_pressed(model.input, input.Enter)
          {
            True -> init_model(model.score_info.highscore)
            False -> model
          }
        }
        NotStarted -> model
      }
      // Clear per-frame input state after game logic has consumed it
      #(Model(..updated, input: input.end_frame(updated.input)), effect.none())
    }
    KeyDown(key) -> #(
      Model(..model, input: input.key_down(model.input, key)),
      effect.none(),
    )
    KeyUp(key) -> #(
      Model(..model, input: input.key_up(model.input, key)),
      effect.none(),
    )
    MouseDown(btn) -> #(
      Model(..model, input: input.mouse_down(model.input, btn)),
      effect.none(),
    )
    MouseUp(btn) -> #(
      Model(..model, input: input.mouse_up(model.input, btn)),
      effect.none(),
    )
  }
}

fn update_running_model(model: Model, ctx: tick.TickContext) -> Model {
  case check_game_over(model) {
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

fn check_game_over(model: Model) -> Bool {
  let hx = model.head.x
  let hy = model.head.y
  let border_check =
    hx >. right_border()
    || hx <. left_border()
    || hy >. upper_border()
    || hy <. down_border()

  let own_tail_check = list.any(model.tail, fn(a) { a.x == hx && a.y == hy })
  border_check || own_tail_check
}

fn calculate_new_beute_pos(model: Model, safety: Int) -> #(Float, Float) {
  let new_pos_candidate = random_pos()
  let is_too_close =
    list.append([model.head], model.tail)
    |> list.any(spawn_too_close(_, new_pos_candidate))
  case is_too_close && safety < 5 {
    True -> calculate_new_beute_pos(model, safety + 1)
    False -> new_pos_candidate
  }
}

fn random_pos() -> #(Float, Float) {
  let abs_x = canvas_height -. 2.0 *. box_width
  let rand_x = float.round(abs_x /. box_width) - 1

  let abs_y = canvas_height -. 2.0 *. horz_border_dist() -. 2.0 *. box_width
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

fn update_snake_beute(model: Model, ctx: tick.TickContext) -> Model {
  let delta_seconds = duration.to_seconds(ctx.delta_time)
  let new_time = model.time +. delta_seconds /. 10.0
  let new_direction = parse_direction_from_key(model)

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
        _ -> calculate_new_beute_pos(model, 0)
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

fn parse_direction_from_key(model: Model) -> Direction {
  let is_left =
    input.is_just_pressed(model.input, input.ArrowLeft)
    || input.is_just_pressed(model.input, input.A)
  let is_right =
    input.is_just_pressed(model.input, input.ArrowRight)
    || input.is_just_pressed(model.input, input.D)
  let is_up =
    input.is_just_pressed(model.input, input.ArrowUp)
    || input.is_just_pressed(model.input, input.W)
  let is_down =
    input.is_just_pressed(model.input, input.ArrowDown)
    || input.is_just_pressed(model.input, input.S)
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

pub fn view(model: Model) -> Element(Msg) {
  tiramisu.scene(
    "snake",
    [
      // Canvas dimensions — match the canvas_width/canvas_height constants so
      // 1 world unit == 1 pixel in the orthographic camera below.
      attribute.attribute("width", "1920"),
      attribute.attribute("height", "1080"),
      scene.background_color(0x000000),
      // tabindex="0" allows the element to receive keyboard focus on click.
      attribute.attribute("tabindex", "0"),
      input.on_keydown(KeyDown),
      input.on_keyup(KeyUp),
      input.on_mousedown(MouseDown),
      input.on_mouseup(MouseUp),
    ],
    [
      tiramisu.camera(
        "camera",
        [
          camera.kind(camera.Orthographic),
          // Map world units 1:1 to pixels — left/right span the full canvas width,
          // top/bottom span the full canvas height.
          camera.left(-960.0),
          camera.right(960.0),
          camera.top(540.0),
          camera.bottom(-540.0),
          camera.near(0.1),
          camera.far(100.0),
          camera.active(True),
          transform.transform(transform.at(vec3.Vec3(0.0, 0.0, 20.0))),
        ],
        [],
      ),
      tiramisu.light(
        "ambient",
        [
          light.kind(light.Ambient),
          light.color(0xffffff),
          light.intensity(1.0),
        ],
        [],
      ),
      tiramisu.empty("rootNode", [transform.transform(transform.identity)], {
        list.flatten([
          create_static_view(),
          case model.game_state {
            Running -> create_running_game_view(model)
            // GameOver and NotStarted: only the static borders remain visible.
            // The player can restart by pressing Enter or clicking.
            GameOver | NotStarted -> []
          },
        ])
      }),
    ],
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

fn create_static_view() -> List(Element(Msg)) {
  let v_height = canvas_height -. 2.0 *. horz_border_dist()
  // Horizontal border: full canvas width, thin depth
  let h_border_geom =
    mesh.geometry_box(vec3.Vec3(canvas_width, box_width /. 5.0, 1.0))
  // Vertical border: narrow width, spans the play area height
  let v_border_geom =
    mesh.geometry_box(vec3.Vec3(box_width /. 5.0, v_height, 1.0))
  let border_color = mesh.color(color_hex(BorderColor))
  let border_metalness = material.metalness(0.2)
  let border_roughness = material.roughness(0.9)

  [
    tiramisu.mesh(
      "upperLine",
      [
        h_border_geom,
        border_color,
        border_metalness,
        border_roughness,
        transform.transform(transform.at(vec3.Vec3(0.0, upper_border(), 0.0))),
      ],
      [],
    ),
    tiramisu.mesh(
      "downLine",
      [
        h_border_geom,
        border_color,
        border_metalness,
        border_roughness,
        transform.transform(transform.at(vec3.Vec3(0.0, down_border(), 0.0))),
      ],
      [],
    ),
    tiramisu.mesh(
      "leftLine",
      [
        v_border_geom,
        border_color,
        border_metalness,
        border_roughness,
        transform.transform(transform.at(vec3.Vec3(left_border(), 0.0, 0.0))),
      ],
      [],
    ),
    tiramisu.mesh(
      "rightLine",
      [
        v_border_geom,
        border_color,
        border_metalness,
        border_roughness,
        transform.transform(transform.at(vec3.Vec3(right_border(), 0.0, 0.0))),
      ],
      [],
    ),
  ]
}

fn create_running_game_view(model: Model) -> List(Element(Msg)) {
  let cube_geom = mesh.geometry_box(vec3.Vec3(box_width, box_width, 1.0))
  let head_position = vec3.Vec3(model.head.x, model.head.y, 0.0)
  [
    tiramisu.mesh(
      "snakeHead",
      [
        cube_geom,
        mesh.color(color_hex(SnakeHeadColor)),
        material.metalness(0.2),
        material.roughness(0.9),
        transform.transform(transform.at(head_position)),
      ],
      [],
    ),
    tiramisu.mesh(
      "beute",
      [
        cube_geom,
        mesh.color(color_hex(BeuteColor)),
        material.metalness(0.2),
        material.roughness(0.9),
        transform.transform(
          transform.at(vec3.Vec3(model.beute_pos.0, model.beute_pos.1, 0.0)),
        ),
      ],
      [],
    ),
    ..tail_elements(model)
  ]
}

fn tail_elements(model: Model) -> List(Element(Msg)) {
  model.tail
  |> list.map(fn(tail_element) { #(tail_element.x, tail_element.y) })
  |> list.index_map(fn(tuple, index) {
    create_tail_element(tuple.0, tuple.1, index)
  })
}

fn create_tail_element(x: Float, y: Float, index: Int) -> Element(Msg) {
  tiramisu.mesh(
    string.append("TailElement", int.to_string(index)),
    [
      mesh.geometry_box(vec3.Vec3(box_width, box_width, 1.0)),
      mesh.color(color_hex(SnakeTailColor)),
      material.metalness(0.2),
      material.roughness(0.9),
      transform.transform(transform.at(vec3.Vec3(x, y, 0.0))),
    ],
    [],
  )
}

// =============================================================================
// Border Calculations
// =============================================================================

fn left_border() -> Float {
  0.0 -. canvas_width /. 2.0
}

fn right_border() -> Float {
  canvas_width /. 2.0
}

fn upper_border() -> Float {
  canvas_height /. 2.0 -. horz_border_dist()
}

fn down_border() -> Float {
  0.0 -. { canvas_height /. 2.0 -. horz_border_dist() }
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
