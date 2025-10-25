import gleam/float
import gleam/int
import gleam/option.{None}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import paint as p
import paint/canvas
import tiramisu
import tiramisu/background
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Id {
  MainCamera
  Sun
  Cube
  CubeGroup
  CubeLabel
  Cube2
  CubeGroup2
  CubeLabel2
}

pub type Model {
  Model(rotation: Float, frame: Int)
}

pub type Msg {
  Tick
}

pub fn main() {
  // Initialize paint library
  canvas.define_web_component()

  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x87ceeb),
    init: init,
    update: update,
    view: view,
  )
}

fn css2d_label_element() -> Element(Msg) {
  html.div(
    [
      attribute.class(
        "bg-black/80 text-cyan-400 px-3 py-2 rounded font-sans text-sm font-medium shadow-lg",
      ),
    ],
    [element.text("CSS2D - Always On Top")],
  )
}

fn sprite_label_picture(rotation: Float, frame: Int) -> p.Picture {
  let rotation_text = "Rotation: " <> float.to_string(rotation)
  let frame_text = "Frame: " <> int.to_string(frame)

  p.combine([
    // Background with rounded corners (approximated with rectangle)
    p.rectangle(256.0, 64.0)
      |> p.fill(p.colour_rgb(0, 0, 0)),
    // Rotation text
    p.text(rotation_text, 12)
      |> p.translate_xy(12.0, 25.0)
      |> p.fill(p.colour_rgb(244, 107, 107)),
    // Frame text
    p.text(frame_text, 12)
      |> p.translate_xy(12.0, 45.0)
      |> p.fill(p.colour_rgb(244, 107, 107)),
  ])
}

fn init(_ctx: tiramisu.Context(Id)) {
  #(Model(rotation: 0.0, frame: 0), effect.tick(Tick), option.None)
}

fn update(model: Model, msg: Msg, ctx: tiramisu.Context(Id)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time /. 1000.0
      let new_frame = model.frame + 1
      #(
        Model(rotation: new_rotation, frame: new_frame),
        effect.tick(Tick),
        option.None,
      )
    }
  }
}

fn view(model: Model, _ctx: tiramisu.Context(Id)) {
  [
    // Camera
    scene.camera(
      id: MainCamera,
      camera: {
        let assert Ok(cam) =
          camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
        cam
      },
      transform: transform.at(vec3.Vec3(0.0, 2.0, 5.0)),
      active: True,
      look_at: None,
      viewport: None,
    ),
    // Light
    scene.light(
      id: Sun,
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 1.0)
        light
      },
      transform: transform.at(vec3.Vec3(5.0, 5.0, 5.0)),
    ),
    // Left Cube with CSS2D label (always on top)
    scene.group(
      id: CubeGroup,
      transform: transform.identity
        |> transform.with_position(vec3.Vec3(-2.0, 0.0, 0.0))
        |> transform.with_euler_rotation(vec3.Vec3(
          model.rotation,
          model.rotation *. 0.7,
          0.0,
        )),
      children: [
        scene.mesh(
          id: Cube,
          geometry: {
            let assert Ok(g) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
            g
          },
          material: {
            let assert Ok(m) =
              material.new()
              |> material.with_color(0x4ecdc4)
              |> material.build()
            m
          },
          transform: transform.identity,
          physics: None,
        ),
        // CSS2D Label - always visible on top
        scene.css2d_label(
          id: CubeLabel,
          html: element.to_string(css2d_label_element()),
          transform: transform.at(position: vec3.Vec3(0.0, 1.0, 0.0)),
        ),
      ],
    ),
    // Right Cube with Sprite label (depth-aware with occlusion)
    scene.group(
      id: CubeGroup2,
      transform: transform.identity
        |> transform.with_position(vec3.Vec3(2.0, 0.0, 0.0))
        |> transform.with_euler_rotation(vec3.Vec3(
          0.0,
          model.rotation *. 0.7,
          0.0,
        )),
      children: [
        scene.mesh(
          id: Cube2,
          geometry: {
            let assert Ok(g) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
            g
          },
          material: {
            let assert Ok(m) =
              material.new()
              |> material.with_color(0xff6b6b)
              |> material.build()
            m
          },
          transform: transform.identity,
          physics: None,
        ),
        // Sprite Label - hides behind objects with depth occlusion
        scene.sprite(
          id: CubeLabel2,
          picture: sprite_label_picture(model.rotation, model.frame),
          texture_width: 256,
          texture_height: 64,
          width: 2.0,
          height: 0.5,
          transform: transform.at(position: vec3.Vec3(0.0, 1.5, 0.0)),
        ),
      ],
    ),
  ]
}
