/// Text Geometry Example
///
/// Demonstrates TextGeometry with font loading:
/// - Loading fonts from CDN (typeface.json format)
/// - Creating 3D text with depth and beveling
/// - Creating flat 2D text (depth = 0)
/// - Animating text rotation
import gleam/io
import gleam/javascript/promise
import gleam/list
import gleam/option
import tiramisu
import tiramisu/asset
import tiramisu/background
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Model {
  Model(rotation: Float, font: option.Option(asset.Font))
}

pub type Msg {
  Tick
  FontLoaded(asset.Font)
  FontLoadError(asset.LoadError)
}

pub fn main() -> Nil {
  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x0a0a1a),
    init: init,
    update: update,
    view: view,
  )
}

fn init(
  _ctx: tiramisu.Context(String),
) -> #(Model, Effect(Msg), option.Option(_)) {
  // Load Helvetiker font from Three.js CDN
  let load_effect =
    asset.load_font(
      "https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/fonts/helvetiker_regular.typeface.json",
    )
    |> promise.map(fn(result) {
      case result {
        Ok(font) -> FontLoaded(font)
        Error(err) -> FontLoadError(err)
      }
    })
    |> effect.from_promise

  #(Model(rotation: 0.0, font: option.None), load_effect, option.None)
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(String),
) -> #(Model, Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.0003
      #(Model(..model, rotation: new_rotation), effect.tick(Tick), option.None)
    }

    FontLoaded(font) -> {
      io.println("Font loaded successfully!")
      #(
        Model(..model, font: option.Some(font)),
        effect.tick(Tick),
        option.None,
      )
    }

    FontLoadError(_err) -> {
      io.println("Failed to load font!")
      #(model, effect.none(), option.None)
    }
  }
}

fn view(model: Model, _) -> List(scene.Node(String)) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let camera =
    camera
    |> scene.camera(
      id: "main_camera",
      camera: _,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 15.0)),
      look_at: option.None,
      active: True,
      viewport: option.None,
    )
    |> list.wrap

  let lights = [
    scene.light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0xffffff, intensity: 0.5)
        light
      },
      transform: transform.identity,
    ),
    scene.light(
      id: "directional",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 1.0)
        light
      },
      transform: transform.at(position: vec3.Vec3(10.0, 10.0, 10.0)),
    ),
  ]

  // Create text meshes if font is loaded
  let text_meshes = case model.font {
    option.None -> []
    option.Some(font) -> [
      // 3D text with bevel (top)
      create_3d_text(
        "3D TEXT",
        font,
        0.0,
        3.0,
        model.rotation,
        0x00d4ff,
        True,
      ),
      // 3D text without bevel (middle)
      create_3d_text(
        "TIRAMISU",
        font,
        0.0,
        0.0,
        model.rotation,
        0xff6b9d,
        False,
      ),
      // Flat 2D text (bottom)
      create_flat_text("2D TEXT", font, 0.0, -3.0, model.rotation, 0x7cff6b),
    ]
  }

  list.flatten([camera, lights, text_meshes])
}

/// Create 3D text with depth and optional beveling
fn create_3d_text(
  text: String,
  font: asset.Font,
  x: Float,
  y: Float,
  rotation: Float,
  color: Int,
  bevel: Bool,
) -> scene.Node(String) {
  let assert Ok(text_geom) = geometry.text(
    text: text,
    font: font,
    size: 1.0,
    depth: 0.3,
    curve_segments: 12,
    bevel_enabled: bevel,
    bevel_thickness: 0.05,
    bevel_size: 0.02,
    bevel_offset: 0.0,
    bevel_segments: 5,
  )

  let assert Ok(mat) =
    material.new()
    |> material.with_color(color)
    |> material.with_metalness(0.7)
    |> material.with_roughness(0.3)
    |> material.build

  scene.mesh(
    id: "text_" <> text,
    geometry: text_geom,
    material: mat,
    transform: transform.at(position: vec3.Vec3(x -. 3.5, y, 0.0))
        |> transform.with_euler_rotation(vec3.Vec3(0.0, rotation, 0.0)),
    physics: option.None,
  )
}

/// Create flat 2D text (no depth)
fn create_flat_text(
  text: String,
  font: asset.Font,
  x: Float,
  y: Float,
  rotation: Float,
  color: Int,
) -> scene.Node(String) {
  let assert Ok(text_geom) = geometry.text(
    text: text,
    font: font,
    size: 1.0,
    depth: 0.0,
    // Flat text
    curve_segments: 8,
    bevel_enabled: False,
    bevel_thickness: 0.0,
    bevel_size: 0.0,
    bevel_offset: 0.0,
    bevel_segments: 1,
  )

  let assert Ok(mat) =
    material.new() |> material.with_color(color) |> material.build

  scene.mesh(
    id: "text_flat_" <> text,
    geometry: text_geom,
    material: mat,
    transform: transform.at(position: vec3.Vec3(x -. 2.5, y, 0.0))
        |> transform.with_euler_rotation(vec3.Vec3(0.0, rotation, 0.0)),
    physics: option.None,
  )
}
