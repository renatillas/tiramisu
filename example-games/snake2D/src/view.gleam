//// Updates the view: create_static_view (always), then either 
//// create_gameover() - view or create_running_game() -view 
//// depending on model.game_state

import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import tiramisu
import tiramisu/camera
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

import model.{type Model, GameOver, Running}
import snake_global.{box_width}

type Color {
  BeuteColor
  SnakeHeadColor
  SnakeTailColor
  PrimeColor
  SecColor
  BorderColor
}

fn color_hex(color: Color) -> Int {
  case color {
    BeuteColor -> 0xfcba03
    SnakeHeadColor -> 0x34eb4c
    SnakeTailColor -> 0x42f5b6
    //red
    PrimeColor -> 0xeb0933
    BorderColor -> 0xeb4034
    // slight Blue
    SecColor -> 0x34d0eb
  }
}

pub fn view(
  model: Model,
  ctx: tiramisu.Context(String),
) -> List(scene.Node(String)) {
  let cam =
    camera.camera_2d(
      width: float.round(ctx.canvas_width),
      height: float.round(ctx.canvas_height),
    )

  let init_elements = [
    scene.camera(
      id: "camera",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 20.0)),
      look_at: option.None,
      active: True,
      viewport: option.None,
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

  base_elements
  |> list.append(case model.game_state {
    Running -> create_running_game_view(model, ctx)
    GameOver -> create_game_over(model, ctx)
    _ -> []
  })
}

fn create_static_view(ctx: tiramisu.Context(String)) -> List(scene.Node(String)) {
  let assert Ok(border_material) =
    material.new()
    |> material.with_color(color_hex(BorderColor))
    |> material.with_metalness(0.2)
    |> material.with_roughness(0.9)
    |> material.build()

  let assert Ok(horizontal_border_geometry) =
    geometry.box(width: ctx.canvas_width, height: box_width /. 5.0, depth: 1.0)

  let assert Ok(vertical_border_geometry) =
    geometry.box(
      width: box_width /. 5.0,
      height: ctx.canvas_height -. 2.0 *. snake_global.horz_border_dist(),
      depth: 1.0,
    )
  let borders = [
    scene.mesh(
      id: "upperLine",
      geometry: horizontal_border_geometry,
      material: border_material,
      transform: transform.at(position: vec3.Vec3(
        0.0,
        snake_global.upper_border(ctx),
        0.0,
      )),
      physics: option.None,
    ),
    scene.mesh(
      id: "downLine",
      geometry: horizontal_border_geometry,
      material: border_material,
      transform: transform.at(position: vec3.Vec3(
        0.0,
        snake_global.down_border(ctx),
        0.0,
      )),
      physics: option.None,
    ),

    scene.mesh(
      id: "leftLine",
      geometry: vertical_border_geometry,
      material: border_material,
      transform: transform.at(position: vec3.Vec3(
        snake_global.left_border(ctx),
        0.0,
        0.0,
      )),
      physics: option.None,
    ),

    scene.mesh(
      id: "rightLine",
      geometry: vertical_border_geometry,
      material: border_material,
      transform: transform.at(position: vec3.Vec3(
        snake_global.right_border(ctx),
        0.0,
        0.0,
      )),
      physics: option.None,
    ),
  ]

  borders
}

pub fn create_game_over(model: Model, ctx: tiramisu.Context(String)) {
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

fn create_score_display(
  model: Model,
  ctx: tiramisu.Context(String),
) -> List(scene.Node(String)) {
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
            snake_global.upper_border(ctx)
              +. snake_global.horz_border_dist()
              /. 2.0,
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

fn create_running_game_view(
  model: Model,
  ctx: tiramisu.Context(String),
) -> List(scene.Node(String)) {
  let assert Ok(cube_geometry) =
    geometry.box(width: box_width, height: box_width, depth: 1.0)
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

fn tail_elements(model: Model) -> List(scene.Node(String)) {
  let assert Ok(cube_geometry) =
    geometry.box(width: box_width, height: box_width, depth: 1.0)

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
) -> scene.Node(String) {
  scene.mesh(
    id: string.append("TailElement", int.to_string(index)),
    geometry: cube_geometry,
    material: tail_material,
    transform: transform.at(position: vec3.Vec3(x, y, 0.0)),
    physics: option.None,
  )
}
