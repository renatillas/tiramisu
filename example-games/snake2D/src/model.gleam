//// Updates the model. 
//// Checks if game_over, if beute is eaten, etc. and reacts accordingly

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import snake_global.{box_width}
import tiramisu/asset
import tiramisu/effect.{type Effect}
import tiramisu/input

import tiramisu

// the key the highscore is stored in the local storage
const highscore_key = "Highscore"

// draw every <draw_frame>: low -> fast movement, high -> slow movement
const draw_frame = 8

pub type Model {
  Model(
    time: Float,
    head: BoxData,
    tail: List(BoxData),
    beute_pos: #(Float, Float),
    update_frame: Int,
    game_state: GameState,
    maybe_font: option.Option(asset.Font),
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
  FontLoaded(asset.Font)
  FontLoadFailed(asset.LoadError)
  Tick
}

pub fn init(
  ctx: tiramisu.Context(String),
) -> #(Model, Effect(Msg), option.Option(_)) {
  let load =
    asset.load_font("fonts/helvetiker_regular.typeface.json")
    |> promise.map(fn(result) {
      case result {
        Ok(font) -> FontLoaded(font)
        Error(err) -> FontLoadFailed(err)
      }
    })

  let pot_highscore =
    get_localstorage(highscore_key)
    |> result.try(fn(x) {
      let assert Ok(val) = decode.run(x, decode.int)
      Ok(val)
    })
  let highscore = option.from_result(pot_highscore)
  #(
    init_model(highscore, option.None, ctx),
    effect.batch([effect.tick(Tick), effect.from_promise(load)]),
    option.None,
  )
}

fn init_model(
  highscore: option.Option(Int),
  maybe_font: option.Option(asset.Font),
  ctx: tiramisu.Context(String),
) -> Model {
  let init_beute_pos = random_pos(ctx)
  Model(
    time: 0.0,
    head: BoxData(x: 0.0, y: 0.0, direction: Right),
    tail: [],
    beute_pos: init_beute_pos,
    update_frame: 0,
    game_state: Running,
    maybe_font: maybe_font,
    score_info: ScoreInfo(
      current_score: 0,
      highscore: highscore,
      new_high_score: False,
    ),
  )
}

pub fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(String),
) -> #(Model, Effect(Msg), option.Option(_)) {
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
    FontLoadFailed(err) -> {
      echo err
      model
    }
  }
  #(updatet_model, effect.tick(Tick), option.None)
}

fn update_running_model(model: Model, ctx: tiramisu.Context(String)) -> Model {
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

fn check_game_over(model: Model, ctx: tiramisu.Context(String)) -> Bool {
  let hx = model.head.x
  let hy = model.head.y
  let border_check =
    hx >. snake_global.right_border(ctx)
    || hx <. snake_global.left_border(ctx)
    || hy >. snake_global.upper_border(ctx)
    || hy <. snake_global.down_border(ctx)

  let own_tail_check = list.any(model.tail, fn(a) { a.x == hx && a.y == hy })
  border_check || own_tail_check
}

fn calculate_new_beute_pos(
  model: Model,
  safety: Int,
  ctx: tiramisu.Context(String),
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

fn random_pos(ctx: tiramisu.Context(String)) -> #(Float, Float) {
  let abs_x = ctx.canvas_height -. 2.0 *. box_width
  let rand_x = float.round(abs_x /. box_width) - 1

  let abs_y =
    ctx.canvas_height
    -. 2.0
    *. snake_global.horz_border_dist()
    -. 2.0
    *. box_width
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

fn update_snake_beute(model: Model, ctx: tiramisu.Context(String)) -> Model {
  let new_time = ctx.delta_time
  let new_direction = parse_direction_from_key(ctx, model)
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

  let new_tail = update_tail_pos(model.head, enhanced_tail, model.update_frame)

  let #(new_x, new_y) =
    update_head_pos(model.head, model.update_frame, new_direction)

  Model(
    ..model,
    time: new_time,
    head: BoxData(x: new_x, y: new_y, direction: new_direction),
    tail: new_tail,
    beute_pos: new_beute_pos,
    update_frame: { model.update_frame + 1 } % draw_frame,
    game_state: Running,
    score_info: ScoreInfo(..model.score_info, current_score: new_score),
  )
}

fn update_head_pos(
  box_data: BoxData,
  update_frame: Int,
  direction: Direction,
) -> #(Float, Float) {
  let update_movement = update_frame == 0

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
  case update_movement {
    True -> #(box_data.x +. horizontal_mov, box_data.y +. vertical_mov)
    False -> #(box_data.x, box_data.y)
  }
}

fn parse_direction_from_key(
  ctx: tiramisu.Context(String),
  model: Model,
) -> Direction {
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
        _ -> new_direction
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

fn update_tail_pos(
  head_pos: BoxData,
  tail_pos: List(BoxData),
  update_frame: Int,
) -> List(BoxData) {
  case update_frame == 0 {
    True -> {
      case tail_pos {
        [] -> tail_pos
        _ ->
          [head_pos]
          |> list.append(list.reverse(tail_pos) |> list.drop(1) |> list.reverse)
      }
    }
    False -> tail_pos
  }
}

@external(javascript, "./local_storage.ffi.mjs", "set_localstorage")
fn set_localstorage(_key: String, _value: String) -> Nil {
  Nil
}

@external(javascript, "./local_storage.ffi.mjs", "get_localstorage")
fn get_localstorage(_key: String) -> Result(Dynamic, Nil) {
  Error(Nil)
}
