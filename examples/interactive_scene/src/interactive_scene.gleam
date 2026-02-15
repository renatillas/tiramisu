//// Interactive Scene Example
////
//// Demonstrates interactive Tiramisu scenes using lustre.application:
//// - Switching between material types (standard, basic, phong, lambert, toon)
//// - Changing mesh colors dynamically
//// - Animating mesh rotation with requestAnimationFrame
//// - Toggling wireframe mode
//// - Adjusting material properties (metalness, roughness, emissive, shininess)
//// - Alpha test threshold for texture cutout
//// - Explicit transparency toggle
//// - Shadow casting and receiving on meshes
//// - Changing material side (front, back, double)
//// - Loading texture maps from URLs
//// - Mouse input: position display, left-click cycles color, right-click toggles
////   wireframe, scroll wheel adjusts displacement scale
//// - Keyboard input: arrow keys, digits, escape, enter

import gleam/float
import gleam/int
import gleam/list
import gleam/string
import gleam/time/duration

import lustre
import lustre/attribute.{class}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

import quaternion

import tiramisu
import tiramisu/camera
import input
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/tick
import tiramisu/transform

import vec/vec2
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// Application state
pub type Model {
  Model(
    /// Input state for keyboard and mouse
    input_state: input.InputState,
    /// Current cube color (hex integer)
    cube_color: Int,
    /// Current rotation angle in radians
    rotation: Float,
    /// Rotation speed multiplier
    rotation_speed: Float,
    /// Whether wireframe mode is enabled
    wireframe: Bool,
    /// Metalness value (0.0 - 1.0)
    metalness: Float,
    /// Roughness value (0.0 - 1.0)
    roughness: Float,
    /// Whether animation is running
    animating: Bool,
    /// Material type (standard, basic, phong, lambert, toon)
    material_type: String,
    /// Emissive color (hex integer)
    emissive_color: Int,
    /// Emissive intensity
    emissive_intensity: Float,
    /// Material side (front, back, double)
    side: String,
    /// Color map texture URL
    color_map_url: String,
    /// Normal map texture URL
    normal_map_url: String,
    /// AO map texture URL
    ao_map_url: String,
    /// Roughness map texture URL
    roughness_map_url: String,
    /// Metalness map texture URL
    metalness_map_url: String,
    /// Displacement map texture URL
    displacement_map_url: String,
    /// Displacement scale
    displacement_scale: Float,
    /// Displacement bias
    displacement_bias: Float,
    /// Shininess for phong material (specular highlight size)
    shininess: Float,
    /// Alpha test threshold (pixels below this alpha are discarded)
    alpha_test: Float,
    /// Explicit transparency (independent of opacity)
    transparent: Bool,
    /// Material opacity (0.0 - 1.0)
    opacity: Float,
    /// Whether the sphere casts shadows
    cast_shadow: Bool,
    /// Whether the ground receives shadows
    receive_shadow: Bool,
  )
}

/// Messages for state updates
pub type Msg {
  /// Animation tick with timing context
  Tick(tick.TickContext)
  /// Keyboard events
  KeyDown(input.Key)
  KeyUp(input.Key)
  /// Mouse events
  MouseMove(Float, Float)
  MouseDown(input.MouseButton)
  MouseUp(input.MouseButton)
  Wheel(Float)
  /// Change the cube color
  SetColor(Int)
  /// Toggle wireframe mode
  ToggleWireframe
  /// Toggle animation
  ToggleAnimation
  /// Set rotation speed
  SetRotationSpeed(Float)
  /// Set metalness
  SetMetalness(Float)
  /// Set roughness
  SetRoughness(Float)
  /// Set material type
  SetMaterialType(String)
  /// Set emissive glow color
  SetEmissiveColor(Int)
  /// Set emissive glow intensity
  SetEmissiveIntensity(Float)
  /// Set material side
  SetSide(String)
  /// Set color map texture URL
  SetColorMapUrl(String)
  /// Set normal map texture URL
  SetNormalMapUrl(String)
  /// Set AO map texture URL
  SetAoMapUrl(String)
  /// Set roughness map texture URL
  SetRoughnessMapUrl(String)
  /// Set metalness map texture URL
  SetMetalnessMapUrl(String)
  /// Set displacement map texture URL
  SetDisplacementMapUrl(String)
  /// Set displacement scale
  SetDisplacementScale(Float)
  /// Set displacement bias
  SetDisplacementBias(Float)
  /// Set shininess (phong)
  SetShininess(Float)
  /// Set alpha test threshold
  SetAlphaTest(Float)
  /// Set opacity
  SetOpacity(Float)
  /// Toggle explicit transparency
  ToggleTransparent
  /// Toggle shadow casting on sphere
  ToggleCastShadow
  /// Toggle shadow receiving on ground
  ToggleReceiveShadow
}

// MAIN ------------------------------------------------------------------------

pub fn main() -> Nil {
  // Register all Tiramisu web components
  let assert Ok(_) = tiramisu.register()

  // Start a Lustre app with effects support
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      input_state: input.new(),
      cube_color: 0xff6b6b,
      rotation: 0.0,
      rotation_speed: 1.0,
      wireframe: False,
      metalness: 0.3,
      roughness: 0.7,
      animating: True,
      material_type: "standard",
      emissive_color: 0x000000,
      emissive_intensity: 0.0,
      side: "front",
      color_map_url: "",
      normal_map_url: "",
      ao_map_url: "",
      roughness_map_url: "",
      metalness_map_url: "",
      displacement_map_url: "",
      displacement_scale: 0.2,
      displacement_bias: 0.0,
      shininess: 30.0,
      alpha_test: 0.0,
      transparent: False,
      opacity: 1.0,
      cast_shadow: True,
      receive_shadow: True,
    )

  // Subscribe to tick updates for animation
  #(model, tick.subscribe("main", Tick))
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      let new_model = case model.animating {
        True -> {
          let dt = duration.to_seconds(ctx.delta_time)
          let new_rotation = model.rotation +. dt *. model.rotation_speed
          Model(..model, rotation: new_rotation)
        }
        False -> model
      }
      #(
        Model(..new_model, input_state: input.end_frame(new_model.input_state)),
        effect.none(),
      )
    }

    // Keyboard input
    KeyDown(key) -> {
      let state = input.key_down(model.input_state, key)
      #(Model(..model, input_state: state), effect.none())
    }
    KeyUp(key) -> {
      let state = input.key_up(model.input_state, key)
      #(Model(..model, input_state: state), effect.none())
    }

    // Mouse input
    MouseMove(x, y) -> {
      let state = input.mouse_move(model.input_state, x, y)
      #(Model(..model, input_state: state), effect.none())
    }
    MouseDown(button) -> {
      let state = input.mouse_down(model.input_state, button)
      // Left-click cycles sphere color
      let new_color = case button {
        input.LeftButton -> next_color(model.cube_color)
        _ -> model.cube_color
      }
      // Right-click toggles wireframe
      let new_wireframe = case button {
        input.RightButton -> !model.wireframe
        _ -> model.wireframe
      }
      #(
        Model(
          ..model,
          input_state: state,
          cube_color: new_color,
          wireframe: new_wireframe,
        ),
        effect.none(),
      )
    }
    MouseUp(button) -> {
      let state = input.mouse_up(model.input_state, button)
      #(Model(..model, input_state: state), effect.none())
    }
    Wheel(delta) -> {
      let state = input.mouse_wheel_input(model.input_state, delta)
      // Scroll wheel adjusts displacement scale (clamped 0.0 - 2.0)
      let step = delta /. 500.0
      let new_scale = float.clamp(model.displacement_scale +. step, 0.0, 2.0)
      #(
        Model(..model, input_state: state, displacement_scale: new_scale),
        effect.none(),
      )
    }

    SetColor(color) -> #(Model(..model, cube_color: color), effect.none())

    ToggleWireframe -> #(
      Model(..model, wireframe: !model.wireframe),
      effect.none(),
    )

    ToggleAnimation -> #(
      Model(..model, animating: !model.animating),
      effect.none(),
    )

    SetRotationSpeed(speed) -> #(
      Model(..model, rotation_speed: speed),
      effect.none(),
    )

    SetMetalness(value) -> #(Model(..model, metalness: value), effect.none())

    SetRoughness(value) -> #(Model(..model, roughness: value), effect.none())

    SetMaterialType(mt) -> #(Model(..model, material_type: mt), effect.none())

    SetEmissiveColor(color) -> #(
      Model(..model, emissive_color: color),
      effect.none(),
    )

    SetEmissiveIntensity(value) -> #(
      Model(..model, emissive_intensity: value),
      effect.none(),
    )

    SetSide(s) -> #(Model(..model, side: s), effect.none())

    SetColorMapUrl(url) -> #(Model(..model, color_map_url: url), effect.none())

    SetNormalMapUrl(url) -> #(
      Model(..model, normal_map_url: url),
      effect.none(),
    )

    SetAoMapUrl(url) -> #(Model(..model, ao_map_url: url), effect.none())

    SetRoughnessMapUrl(url) -> #(
      Model(..model, roughness_map_url: url),
      effect.none(),
    )

    SetMetalnessMapUrl(url) -> #(
      Model(..model, metalness_map_url: url),
      effect.none(),
    )

    SetDisplacementMapUrl(url) -> #(
      Model(..model, displacement_map_url: url),
      effect.none(),
    )

    SetDisplacementScale(value) -> #(
      Model(..model, displacement_scale: value),
      effect.none(),
    )

    SetDisplacementBias(value) -> #(
      Model(..model, displacement_bias: value),
      effect.none(),
    )

    SetShininess(value) -> #(Model(..model, shininess: value), effect.none())

    SetAlphaTest(value) -> #(Model(..model, alpha_test: value), effect.none())

    SetOpacity(value) -> #(Model(..model, opacity: value), effect.none())

    ToggleTransparent -> #(
      Model(..model, transparent: !model.transparent),
      effect.none(),
    )

    ToggleCastShadow -> #(
      Model(..model, cast_shadow: !model.cast_shadow),
      effect.none(),
    )

    ToggleReceiveShadow -> #(
      Model(..model, receive_shadow: !model.receive_shadow),
      effect.none(),
    )
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div([class("container")], [
    // 3D Scene — with input event listeners
    renderer.renderer(
      [
        renderer.width(600),
        renderer.height(500),
        renderer.background("#1a1a2e"),
        renderer.scene_id("main"),
        // Focus support for keyboard events
        attribute.attribute("tabindex", "0"),
        // Keyboard
        input.on_keydown(KeyDown),
        input.on_keyup(KeyUp),
        // Mouse
        input.on_mousemove(MouseMove),
        input.on_mousedown(MouseDown)
          |> event.prevent_default(),
        input.on_mouseup(MouseUp),
        input.on_wheel(Wheel),
      ],
      [
        // Camera
        camera.camera(
          "main",
          [
            camera.fov(75.0),
            camera.transform(transform.at(vec3.Vec3(0.0, 3.0, 6.0))),
            camera.active(True),
          ],
          [],
        ),
        // Rotating sphere with dynamic material properties
        // (sphere has enough vertices for displacement maps to work)
        mesh.mesh(
          "sphere",
          [
            mesh.geometry_sphere(1.5, vec2.Vec2(64, 64)),
            mesh.color(model.cube_color),
            mesh.metalness(model.metalness),
            mesh.roughness(model.roughness),
            // Material type
            attribute.attribute("material-type", model.material_type),
            // Emissive glow
            material.emissive(model.emissive_color),
            material.emissive_intensity(model.emissive_intensity),
            // Material side
            attribute.attribute("side", model.side),
            // Displacement
            material.displacement_scale(model.displacement_scale),
            material.displacement_bias(model.displacement_bias),
            // Phong shininess
            material.shininess(model.shininess),
            // Alpha test, transparency & opacity
            material.alpha_test(model.alpha_test),
            mesh.opacity(model.opacity),
            mesh.transform(
              transform.at(vec3.Vec3(0.0, 2.0, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(0.0, model.rotation, 0.0)),
              ),
            ),
          ]
            |> list.append(case model.wireframe {
              True -> [mesh.wireframe()]
              False -> []
            })
            |> list.append(case model.transparent {
              True -> [material.transparent()]
              False -> []
            })
            |> list.append(case model.cast_shadow {
              True -> [mesh.cast_shadow()]
              False -> []
            })
            |> list.append(case model.color_map_url {
              "" -> []
              url -> [material.color_map(url)]
            })
            |> list.append(case model.normal_map_url {
              "" -> []
              url -> [material.normal_map(url)]
            })
            |> list.append(case model.ao_map_url {
              "" -> []
              url -> [material.ao_map(url)]
            })
            |> list.append(case model.roughness_map_url {
              "" -> []
              url -> [material.roughness_map(url)]
            })
            |> list.append(case model.metalness_map_url {
              "" -> []
              url -> [material.metalness_map(url)]
            })
            |> list.append(case model.displacement_map_url {
              "" -> []
              url -> [material.displacement_map(url)]
            }),
          [],
        ),
        // Ground plane — receives shadows from the sphere
        mesh.mesh(
          "ground",
          [
            mesh.geometry_plane(vec2.Vec2(15.0, 15.0)),
            mesh.color(0x2d3436),
            mesh.metalness(0.1),
            mesh.roughness(0.9),
            mesh.transform(
              transform.at(vec3.Vec3(0.0, 0.0, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
              ),
            ),
          ]
            |> list.append(case model.receive_shadow {
              True -> [mesh.receive_shadow()]
              False -> []
            }),
          [],
        ),
        // Lights
        light.light(
          "ambient",
          [
            light.light_type("ambient"),
            light.color(0xffffff),
            light.intensity(0.4),
          ],
          [],
        ),
        light.light(
          "sun",
          [
            light.light_type("directional"),
            light.color(0xffffff),
            light.intensity(1.0),
            light.cast_shadow(True),
            light.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
          ],
          [],
        ),
      ],
    ),
    // Mouse info overlay
    {
      let #(mx, my) = input.mouse_position(model.input_state)
      html.div(
        [
          attribute.styles([
            #("background", "rgba(0,0,0,0.7)"),
            #("color", "#4ecdc4"),
            #("padding", "8px 12px"),
            #("font-family", "monospace"),
            #("font-size", "12px"),
            #("border-radius", "4px"),
            #("margin-bottom", "8px"),
          ]),
        ],
        [
          html.text(
            "Mouse: "
            <> int.to_string(float.round(mx))
            <> ", "
            <> int.to_string(float.round(my))
            <> "  |  Left-click: cycle color  |  Right-click: wireframe  |  Scroll: displacement",
          ),
        ],
      )
    },
    // Control panel
    html.div([class("controls")], [
      // Material type selector
      html.h3([], [html.text("Material Type")]),
      html.div([class("btn-row")], [
        material_type_button("Standard", "standard", model.material_type),
        material_type_button("Basic", "basic", model.material_type),
        material_type_button("Phong", "phong", model.material_type),
        material_type_button("Lambert", "lambert", model.material_type),
        material_type_button("Toon", "toon", model.material_type),
      ]),
      // Color
      html.h3([attribute.style("margin-top", "15px")], [html.text("Color")]),
      html.div([class("btn-row")], [
        color_button("Red", 0xff6b6b, model.cube_color),
        color_button("Green", 0x4ecdc4, model.cube_color),
        color_button("Blue", 0x45b7d1, model.cube_color),
        color_button("Yellow", 0xf9ca24, model.cube_color),
        color_button("Purple", 0xa55eea, model.cube_color),
      ]),
      // PBR properties (only meaningful for standard material)
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("PBR Properties"),
      ]),
      slider("metalness", 0.0, 1.0, 0.05, model.metalness, SetMetalness),
      slider("roughness", 0.0, 1.0, 0.05, model.roughness, SetRoughness),
      // Shininess (only meaningful for phong material)
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Phong Shininess"),
      ]),
      slider("shininess", 1.0, 200.0, 1.0, model.shininess, SetShininess),
      html.div(
        [
          attribute.style("font-size", "11px"),
          attribute.style("color", "#666"),
        ],
        [html.text("Only applies to Phong material type")],
      ),
      // Emissive glow
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Emissive Glow"),
      ]),
      html.div([class("btn-row")], [
        emissive_button("None", 0x000000, model.emissive_color),
        emissive_button("Red", 0xff0000, model.emissive_color),
        emissive_button("Green", 0x00ff00, model.emissive_color),
        emissive_button("Blue", 0x0000ff, model.emissive_color),
        emissive_button("White", 0xffffff, model.emissive_color),
      ]),
      slider(
        "intensity",
        0.0,
        3.0,
        0.1,
        model.emissive_intensity,
        SetEmissiveIntensity,
      ),
      // Side
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Material Side"),
      ]),
      html.div([class("btn-row")], [
        side_button("Front", "front", model.side),
        side_button("Back", "back", model.side),
        side_button("Double", "double", model.side),
      ]),
      // Texture Maps
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Texture Maps"),
      ]),
      texture_input("Color Map", model.color_map_url, SetColorMapUrl),
      texture_input("Normal Map", model.normal_map_url, SetNormalMapUrl),
      texture_input("AO Map", model.ao_map_url, SetAoMapUrl),
      texture_input(
        "Roughness Map",
        model.roughness_map_url,
        SetRoughnessMapUrl,
      ),
      texture_input(
        "Metalness Map",
        model.metalness_map_url,
        SetMetalnessMapUrl,
      ),
      texture_input(
        "Displacement Map",
        model.displacement_map_url,
        SetDisplacementMapUrl,
      ),
      html.div(
        [
          attribute.style("font-size", "11px"),
          attribute.style("color", "#666"),
          attribute.style("margin-top", "4px"),
        ],
        [html.text("Paste any image URL (jpg, png, webp)")],
      ),
      // Displacement controls
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Displacement"),
      ]),
      slider(
        "scale",
        0.0,
        2.0,
        0.05,
        model.displacement_scale,
        SetDisplacementScale,
      ),
      slider(
        "bias",
        -1.0,
        1.0,
        0.05,
        model.displacement_bias,
        SetDisplacementBias,
      ),
      // Alpha & Transparency
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Alpha & Transparency"),
      ]),
      slider("opacity", 0.0, 1.0, 0.05, model.opacity, SetOpacity),
      toggle_button("Transparent", model.transparent, ToggleTransparent),
      html.div(
        [
          attribute.style("font-size", "11px"),
          attribute.style("color", "#666"),
          attribute.style("margin-top", "3px"),
          attribute.style("margin-bottom", "8px"),
        ],
        [
          html.text(
            "Lower opacity to see transparency (auto-enabled below 1.0)",
          ),
        ],
      ),
      slider(
        "alpha-test",
        0.0,
        1.0,
        0.05,
        model.alpha_test,
        SetAlphaTest,
      ),
      html.div(
        [
          attribute.style("font-size", "11px"),
          attribute.style("color", "#666"),
        ],
        [html.text("Discards pixels below this alpha (needs alpha-channel texture)")],
      ),
      // Shadows
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Shadows"),
      ]),
      html.div([class("btn-row")], [
        toggle_button("Cast Shadow", model.cast_shadow, ToggleCastShadow),
        toggle_button(
          "Receive Shadow",
          model.receive_shadow,
          ToggleReceiveShadow,
        ),
      ]),
      html.div(
        [
          attribute.style("font-size", "11px"),
          attribute.style("color", "#666"),
          attribute.style("margin-top", "3px"),
        ],
        [
          html.text(
            "Cast: sphere casts shadow | Receive: ground shows shadow",
          ),
        ],
      ),
      // Rotation
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Rotation Speed"),
      ]),
      slider("speed", 0.0, 5.0, 0.1, model.rotation_speed, SetRotationSpeed),
      // Options
      html.h3([attribute.style("margin-top", "15px")], [html.text("Options")]),
      html.button(
        [
          event.on_click(ToggleWireframe),
          attribute.styles([
            #("background", case model.wireframe {
              True -> "#4ecdc4"
              False -> "#666"
            }),
            #("color", "white"),
          ]),
        ],
        [
          html.text(case model.wireframe {
            True -> "Wireframe: ON"
            False -> "Wireframe: OFF"
          }),
        ],
      ),
      html.button(
        [
          event.on_click(ToggleAnimation),
          attribute.styles([
            #("background", case model.animating {
              True -> "#f9ca24"
              False -> "#666"
            }),
            #("color", case model.animating {
              True -> "#111"
              False -> "white"
            }),
            #("margin-top", "5px"),
          ]),
        ],
        [
          html.text(case model.animating {
            True -> "Animation: ON"
            False -> "Animation: OFF"
          }),
        ],
      ),
    ]),
  ])
}

// HELPERS ---------------------------------------------------------------------

/// Cycle to the next color in the palette
fn next_color(current: Int) -> Int {
  case current {
    0xff6b6b -> 0x4ecdc4
    0x4ecdc4 -> 0x45b7d1
    0x45b7d1 -> 0xf9ca24
    0xf9ca24 -> 0xa55eea
    _ -> 0xff6b6b
  }
}

fn material_type_button(
  label: String,
  mt: String,
  current: String,
) -> Element(Msg) {
  let is_selected = mt == current
  html.button(
    [
      class("type-btn"),
      event.on_click(SetMaterialType(mt)),
      attribute.styles([
        #("background", case is_selected {
          True -> "#e056fd"
          False -> "#444"
        }),
        #("color", "white"),
        #("font-size", "11px"),
        #("padding", "6px 8px"),
      ]),
    ],
    [html.text(label)],
  )
}

fn color_button(label: String, color: Int, current: Int) -> Element(Msg) {
  let hex = "#" <> int.to_base16(color)
  let is_selected = color == current
  html.button(
    [
      class("color-btn"),
      event.on_click(SetColor(color)),
      attribute.styles([
        #("background", hex),
        #("border", case is_selected {
          True -> "3px solid white"
          False -> "3px solid transparent"
        }),
      ]),
    ],
    [html.text(label)],
  )
}

fn emissive_button(label: String, color: Int, current: Int) -> Element(Msg) {
  let hex = "#" <> int.to_base16(color)
  let is_selected = color == current
  html.button(
    [
      class("type-btn"),
      event.on_click(SetEmissiveColor(color)),
      attribute.styles([
        #("background", case color {
          0 -> "#333"
          _ -> hex
        }),
        #("color", "white"),
        #("font-size", "11px"),
        #("padding", "6px 8px"),
        #("border", case is_selected {
          True -> "2px solid white"
          False -> "2px solid transparent"
        }),
      ]),
    ],
    [html.text(label)],
  )
}

fn side_button(label: String, s: String, current: String) -> Element(Msg) {
  let is_selected = s == current
  html.button(
    [
      class("type-btn"),
      event.on_click(SetSide(s)),
      attribute.styles([
        #("background", case is_selected {
          True -> "#6c5ce7"
          False -> "#444"
        }),
        #("color", "white"),
        #("font-size", "11px"),
        #("padding", "6px 10px"),
      ]),
    ],
    [html.text(label)],
  )
}

fn texture_input(
  label: String,
  value: String,
  on_change: fn(String) -> Msg,
) -> Element(Msg) {
  html.div([attribute.style("margin-bottom", "6px")], [
    html.div(
      [
        attribute.style("font-size", "11px"),
        attribute.style("color", "#aaa"),
        attribute.style("margin-bottom", "3px"),
      ],
      [html.text(label)],
    ),
    html.input([
      attribute.type_("text"),
      attribute.placeholder(label <> " URL..."),
      attribute.value(value),
      attribute.style("width", "100%"),
      attribute.style("box-sizing", "border-box"),
      attribute.style("padding", "6px"),
      attribute.style("border-radius", "4px"),
      attribute.style("border", "1px solid #444"),
      attribute.style("background", "#333"),
      attribute.style("color", "#fff"),
      attribute.style("font-size", "11px"),
      event.on_input(fn(str) { on_change(string.trim(str)) }),
    ]),
  ])
}

fn slider(
  id: String,
  min: Float,
  max: Float,
  step: Float,
  value: Float,
  on_change: fn(Float) -> Msg,
) -> Element(Msg) {
  html.label([], [
    html.text(id <> ": " <> float.to_string(value)),
    html.input([
      attribute.type_("range"),
      attribute.id(id),
      attribute.min(float.to_string(min)),
      attribute.max(float.to_string(max)),
      attribute.step(float.to_string(step)),
      attribute.value(float.to_string(value)),
      event.on_input(fn(str) {
        case float.parse(str) {
          Ok(v) -> on_change(v)
          Error(_) -> on_change(value)
        }
      }),
    ]),
  ])
}

fn toggle_button(label: String, enabled: Bool, msg: Msg) -> Element(Msg) {
  html.button(
    [
      event.on_click(msg),
      attribute.styles([
        #("background", case enabled {
          True -> "#4ecdc4"
          False -> "#666"
        }),
        #("color", "white"),
        #("font-size", "11px"),
        #("padding", "6px 10px"),
      ]),
    ],
    [
      html.text(label <> ": " <> case enabled {
        True -> "ON"
        False -> "OFF"
      }),
    ],
  )
}
