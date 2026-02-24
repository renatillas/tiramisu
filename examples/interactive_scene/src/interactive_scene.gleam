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
import tiramisu/scene

import lustre
import lustre/attribute.{class}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

import quaternion

import input
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
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
    /// Smoothed frames per second (exponential moving average)
    fps: Float,
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
  let assert Ok(_) =
    tiramisu.register([
      camera.extension(),
      light.extension(),
      mesh.extension(),
    ])

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
      fps: 0.0,
    )

  // Subscribe to tick updates for animation
  #(model, tick.subscribe("main-scene", Tick))
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      // Compute smoothed FPS using exponential moving average
      let current_fps = case dt >. 0.0 {
        True -> 1.0 /. dt
        False -> model.fps
      }
      let smooth_fps = case model.fps == 0.0 {
        True -> current_fps
        False -> model.fps *. 0.9 +. current_fps *. 0.1
      }
      let new_model = case model.animating {
        True -> {
          let new_rotation = model.rotation +. dt *. model.rotation_speed
          Model(..model, rotation: new_rotation, fps: smooth_fps)
        }
        False -> Model(..model, fps: smooth_fps)
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
  html.div(
    [class("flex flex-col bg-gray-950 text-white font-sans max-w-full")],
    [
      // 3D Scene — with input event listeners
      tiramisu.scene(
        "main-scene",
        [
          scene.background_color(0x1a1a2e),
          attribute.width(800),
          attribute.height(600),
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
          tiramisu.camera(
            "main",
            [
              camera.fov(75.0),
              camera.active(True),
              transform.transform(transform.at(vec3.Vec3(0.0, 3.0, 6.0))),
            ],
            [],
          ),
          // Rotating sphere with dynamic material properties
          // (sphere has enough vertices for displacement maps to work)
          tiramisu.mesh(
            "sphere",
            [
              mesh.sphere(1.5, vec2.Vec2(64, 64)),
              mesh.color(model.cube_color),
              material.metalness(model.metalness),
              material.roughness(model.roughness),
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
              material.opacity(model.opacity),
              transform.transform(
                transform.at(vec3.Vec3(0.0, 2.0, 0.0))
                |> transform.with_rotation(
                  quaternion.from_euler(vec3.Vec3(0.0, model.rotation, 0.0)),
                ),
              ),
              material.wireframe(model.wireframe),
              material.cast_shadow(model.cast_shadow),
              material.transparent(model.transparent),
            ]
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
                url -> [material.ambient_occlusion_map(url)]
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
          tiramisu.mesh(
            "ground",
            [
              mesh.plane(vec2.Vec2(15.0, 15.0)),
              mesh.color(0x2d3436),
              material.metalness(0.1),
              material.roughness(0.9),
              transform.transform(
                transform.at(vec3.Vec3(0.0, 0.0, 0.0))
                |> transform.with_rotation(
                  quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
                ),
              ),
              material.receive_shadow(model.receive_shadow),
            ],
            [],
          ),
          // Lights
          tiramisu.light(
            "ambient",
            [
              light.kind(light.Ambient),
              light.color(0xffffff),
              light.intensity(0.4),
            ],
            [],
          ),
          tiramisu.light(
            "sun",
            [
              light.kind(light.Directional),
              light.color(0xffffff),
              light.intensity(1.0),
              light.cast_shadow(True),
              transform.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
            ],
            [],
          ),
        ],
      ),
      // Mouse position info bar
      {
        let #(mx, my) = input.mouse_position(model.input_state)
        html.div(
          [
            class(
              "bg-black/70 text-teal-400 px-4 py-2 font-mono text-xs flex justify-between",
            ),
          ],
          [
            html.text(
              "Mouse: "
              <> int.to_string(float.round(mx))
              <> ", "
              <> int.to_string(float.round(my))
              <> "  |  Left-click: cycle color  |  Right-click: wireframe  |  Scroll: displacement",
            ),
            html.span([class("text-yellow-400 font-bold")], [
              html.text(int.to_string(float.round(model.fps)) <> " FPS"),
            ]),
          ],
        )
      },
      // Control panel — flex-wrap grid of sections
      html.div(
        [class("flex flex-wrap gap-6 p-5 bg-gray-900 border-t border-gray-800")],
        [
          // Material type
          html.div([class("flex flex-col gap-2")], [
            section_title("Material Type"),
            html.div([class("flex flex-wrap gap-1")], [
              material_type_button("Standard", "standard", model.material_type),
              material_type_button("Basic", "basic", model.material_type),
              material_type_button("Phong", "phong", model.material_type),
              material_type_button("Lambert", "lambert", model.material_type),
              material_type_button("Toon", "toon", model.material_type),
            ]),
          ]),
          // Color
          html.div([class("flex flex-col gap-2")], [
            section_title("Color"),
            html.div([class("flex flex-wrap gap-1")], [
              color_button("Red", 0xff6b6b, model.cube_color),
              color_button("Green", 0x4ecdc4, model.cube_color),
              color_button("Blue", 0x45b7d1, model.cube_color),
              color_button("Yellow", 0xf9ca24, model.cube_color),
              color_button("Purple", 0xa55eea, model.cube_color),
            ]),
          ]),
          // PBR properties
          html.div([class("flex flex-col gap-2 min-w-44")], [
            section_title("PBR Properties"),
            slider("metalness", 0.0, 1.0, 0.05, model.metalness, SetMetalness),
            slider("roughness", 0.0, 1.0, 0.05, model.roughness, SetRoughness),
          ]),
          // Phong shininess
          html.div([class("flex flex-col gap-2 min-w-44")], [
            section_title("Phong Shininess"),
            slider("shininess", 1.0, 200.0, 1.0, model.shininess, SetShininess),
            html.p([class("text-xs text-gray-500")], [
              html.text("Only applies to Phong material type"),
            ]),
          ]),
          // Emissive glow
          html.div([class("flex flex-col gap-2")], [
            section_title("Emissive Glow"),
            html.div([class("flex flex-wrap gap-1")], [
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
          ]),
          // Material side
          html.div([class("flex flex-col gap-2")], [
            section_title("Material Side"),
            html.div([class("flex gap-1")], [
              side_button("Front", "front", model.side),
              side_button("Back", "back", model.side),
              side_button("Double", "double", model.side),
            ]),
          ]),
          // Texture maps
          html.div([class("flex flex-col gap-2 min-w-56")], [
            section_title("Texture Maps"),
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
            html.p([class("text-xs text-gray-500 mt-1")], [
              html.text("Paste any image URL (jpg, png, webp)"),
            ]),
          ]),
          // Displacement
          html.div([class("flex flex-col gap-2 min-w-44")], [
            section_title("Displacement"),
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
          ]),
          // Alpha & transparency
          html.div([class("flex flex-col gap-2 min-w-44")], [
            section_title("Alpha & Transparency"),
            slider("opacity", 0.0, 1.0, 0.05, model.opacity, SetOpacity),
            toggle_button("Transparent", model.transparent, ToggleTransparent),
            html.p([class("text-xs text-gray-500")], [
              html.text(
                "Lower opacity to see transparency (auto-enabled below 1.0)",
              ),
            ]),
            slider("alpha-test", 0.0, 1.0, 0.05, model.alpha_test, SetAlphaTest),
            html.p([class("text-xs text-gray-500")], [
              html.text(
                "Discards pixels below this alpha (needs alpha-channel texture)",
              ),
            ]),
          ]),
          // Shadows
          html.div([class("flex flex-col gap-2")], [
            section_title("Shadows"),
            html.div([class("flex gap-1")], [
              toggle_button("Cast Shadow", model.cast_shadow, ToggleCastShadow),
              toggle_button(
                "Receive Shadow",
                model.receive_shadow,
                ToggleReceiveShadow,
              ),
            ]),
            html.p([class("text-xs text-gray-500")], [
              html.text(
                "Cast: sphere casts shadow | Receive: ground shows shadow",
              ),
            ]),
          ]),
          // Rotation speed
          html.div([class("flex flex-col gap-2 min-w-44")], [
            section_title("Rotation Speed"),
            slider(
              "speed",
              0.0,
              5.0,
              0.1,
              model.rotation_speed,
              SetRotationSpeed,
            ),
          ]),
          // Options
          html.div([class("flex flex-col gap-2")], [
            section_title("Options"),
            html.button(
              [
                event.on_click(ToggleWireframe),
                class(
                  "px-3 py-1.5 rounded text-xs font-medium cursor-pointer "
                  <> case model.wireframe {
                    True -> "bg-teal-500 text-white"
                    False -> "bg-gray-700 text-gray-300"
                  },
                ),
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
                class(
                  "px-3 py-1.5 rounded text-xs font-medium cursor-pointer "
                  <> case model.animating {
                    True -> "bg-yellow-400 text-gray-900"
                    False -> "bg-gray-700 text-gray-300"
                  },
                ),
              ],
              [
                html.text(case model.animating {
                  True -> "Animation: ON"
                  False -> "Animation: OFF"
                }),
              ],
            ),
          ]),
        ],
      ),
    ],
  )
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

fn section_title(text: String) -> Element(Msg) {
  html.h3(
    [class("text-xs font-semibold text-gray-400 uppercase tracking-wider")],
    [html.text(text)],
  )
}

fn material_type_button(
  label: String,
  mt: String,
  current: String,
) -> Element(Msg) {
  html.button(
    [
      event.on_click(SetMaterialType(mt)),
      class(
        "px-2 py-1.5 rounded text-white text-xs font-medium cursor-pointer "
        <> case mt == current {
          True -> "bg-purple-500"
          False -> "bg-gray-700 hover:bg-gray-600"
        },
      ),
    ],
    [html.text(label)],
  )
}

fn color_button(label: String, color: Int, current: Int) -> Element(Msg) {
  let hex = "#" <> int.to_base16(color)
  html.button(
    [
      event.on_click(SetColor(color)),
      attribute.style("background-color", hex),
      class(
        "px-3 py-1.5 rounded text-white text-xs font-semibold cursor-pointer "
        <> case color == current {
          True -> "ring-2 ring-white ring-offset-2 ring-offset-gray-900"
          False -> ""
        },
      ),
    ],
    [html.text(label)],
  )
}

fn emissive_button(label: String, color: Int, current: Int) -> Element(Msg) {
  let hex = "#" <> int.to_base16(color)
  html.button(
    [
      event.on_click(SetEmissiveColor(color)),
      attribute.style("background-color", case color {
        0 -> "#374151"
        _ -> hex
      }),
      class(
        "px-2 py-1.5 rounded text-white text-xs font-medium cursor-pointer "
        <> case color == current {
          True -> "ring-2 ring-white ring-offset-2 ring-offset-gray-900"
          False -> ""
        },
      ),
    ],
    [html.text(label)],
  )
}

fn side_button(label: String, s: String, current: String) -> Element(Msg) {
  html.button(
    [
      event.on_click(SetSide(s)),
      class(
        "px-3 py-1.5 rounded text-white text-xs font-medium cursor-pointer "
        <> case s == current {
          True -> "bg-indigo-600"
          False -> "bg-gray-700 hover:bg-gray-600"
        },
      ),
    ],
    [html.text(label)],
  )
}

fn texture_input(
  label: String,
  value: String,
  on_change: fn(String) -> Msg,
) -> Element(Msg) {
  html.div([class("flex flex-col gap-1")], [
    html.span([class("text-xs text-gray-400")], [html.text(label)]),
    html.input([
      attribute.type_("text"),
      attribute.placeholder(label <> " URL..."),
      attribute.value(value),
      class(
        "w-full px-2 py-1.5 rounded border border-gray-600 bg-gray-800 text-white text-xs placeholder-gray-500 focus:outline-none focus:border-teal-500",
      ),
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
  html.label([class("flex flex-col gap-1")], [
    html.span([class("text-xs text-gray-300")], [
      html.text(id <> ": " <> float.to_string(value)),
    ]),
    html.input([
      attribute.type_("range"),
      attribute.id(id),
      attribute.min(float.to_string(min)),
      attribute.max(float.to_string(max)),
      attribute.step(float.to_string(step)),
      attribute.value(float.to_string(value)),
      class("w-full accent-teal-400 cursor-pointer"),
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
      class(
        "px-3 py-1.5 rounded text-xs font-medium cursor-pointer "
        <> case enabled {
          True -> "bg-teal-500 text-white"
          False -> "bg-gray-700 text-gray-300"
        },
      ),
    ],
    [
      html.text(
        label
        <> ": "
        <> case enabled {
          True -> "ON"
          False -> "OFF"
        },
      ),
    ],
  )
}
