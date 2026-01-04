import gleam/float
import gleam/int
import gleam/option.{None}
import gleam/time/duration
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import paint as p
import paint/canvas
import tiramisu
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec2
import vec/vec3
import vec/vec3f

pub type Model {
  Model(rotation: Float, frame: Int)
}

pub type Msg {
  Tick
}

pub fn main() {
  // Initialize paint library
  canvas.define_web_component()

  tiramisu.application(init, update, view)
  |> tiramisu.start("body", tiramisu.FullScreen, option.None)
}

fn css2d_label_element(string) -> Element(Msg) {
  html.div(
    [
      attribute.class(
        "bg-white/80 text-black-400 px-3 py-2 rounded font-sans text-sm font-medium shadow-lg",
      ),
    ],
    [element.text("CSS2D - Always On Top - " <> string)],
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

fn init(_ctx: tiramisu.Context) {
  #(Model(rotation: 0.0, frame: 0), effect.dispatch(Tick), option.None)
}

fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. duration.to_seconds(ctx.delta_time)
      let new_frame = model.frame + 1
      #(
        Model(rotation: new_rotation, frame: new_frame),
        effect.dispatch(Tick),
        option.None,
      )
    }
  }
}

fn view(model: Model, _ctx: tiramisu.Context) -> scene.Node {
  scene.empty(id: "scene", transform: transform.identity, children: [
    // Camera
    scene.camera(
      id: "main-camera",
      camera: {
        let assert Ok(cam) =
          camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
        cam
      },
      transform: transform.at(position: vec3.Vec3(0.0, 2.0, 5.0)),
      active: True,
      viewport: None,
      postprocessing: option.None,
    ),
    // Light
    scene.light(
      id: "sun",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 1.0)
        light
      },
      transform: transform.at(position: vec3.Vec3(5.0, 5.0, 5.0)),
    ),
    // Left Cube with CSS2D label (always on top)
    scene.empty(
      id: "cube-group",
      transform: transform.identity
        |> transform.with_position(vec3.Vec3(-2.0, 0.0, 0.0))
        |> transform.with_euler_rotation(vec3.Vec3(
          model.rotation,
          model.rotation *. 0.7,
          0.0,
        )),
      children: [
        scene.mesh(
          id: "cube",
          geometry: {
            let assert Ok(g) = geometry.box(size: vec3f.one)
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
        scene.css2d(
          id: "cube-label",
          html: element.to_string(
            css2d_label_element(float.to_string(model.rotation)),
          ),
          transform: transform.at(position: vec3.Vec3(0.0, 1.0, 0.0)),
        ),
      ],
    ),
    // Right Cube with Sprite label (depth-aware with occlusion)
    scene.empty(
      id: "cube-group-2",
      transform: transform.identity
        |> transform.with_position(vec3.Vec3(2.0, 0.0, 0.0))
        |> transform.with_euler_rotation(vec3.Vec3(
          0.0,
          model.rotation *. 0.7,
          0.0,
        )),
      children: [
        scene.mesh(
          id: "cube-2",
          geometry: {
            let assert Ok(geometry) = geometry.box(size: vec3f.one)
            geometry
          },
          material: {
            let assert Ok(material) =
              material.new()
              |> material.with_color(0xff6b6b)
              |> material.build()
            material
          },
          transform: transform.identity,
          physics: None,
        ),
        // Sprite Label - hides behind objects with depth occlusion
        scene.canvas(
          id: "cube-label-2",
          picture: sprite_label_picture(model.rotation, model.frame),
          texture_size: vec2.Vec2(256, 64),
          size: vec2.Vec2(2.0, 0.5),
          transform: transform.at(position: vec3.Vec3(0.0, 1.5, 0.0)),
        ),
      ],
    ),
  ])
}
