import gleam/int
import gleam/option
import gleam/time/duration
import lustre
import lustre/attribute
import lustre/effect
import lustre/element/html
import lustre/event
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
import tiramisu/primitive
import tiramisu/renderer
import tiramisu/scene
import tiramisu/transform
import vec/vec2
import vec/vec3

const autoplay_interval_seconds = 0.8

const lucy_model =
  "https://raw.githubusercontent.com/ginkogruen/lucy-openscad/df1607a929b26d53bcf90e7257426e33ea670db4/lucy.stl"

const walt_head_model = "https://threejs.org/examples/models/obj/walt/WaltHead.obj"

const missing_model = "https://example.com/never-there.stl"

const alpine_background =
  "https://images.unsplash.com/photo-1519681393784-d120267933ba?auto=format&fit=crop&w=1200&q=80"

const studio_background =
  "https://images.unsplash.com/photo-1500530855697-b586d89ba3ee?auto=format&fit=crop&w=1200&q=80"

const coast_background =
  "https://images.unsplash.com/photo-1506744038136-46273834b3fb?auto=format&fit=crop&w=1200&q=80"

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

type Model {
  Model(
    step_index: Int,
    autoplay: Bool,
    elapsed: Float,
    successful_loads: Int,
    failed_loads: Int,
    last_loaded: String,
    last_error: String,
  )
}

type Msg {
  Tick(scene.Tick)
  Advance
  Previous
  ToggleAutoplay
  MeshLoaded(String)
  MeshFailed(String)
}

type BackgroundSpec {
  BackgroundSpec(label: String, url: String)
}

type MeshSpec {
  MeshSpec(
    id: String,
    label: String,
    src: String,
    color: Int,
    scale: Float,
    rotation: vec3.Vec3(Float),
    position: vec3.Vec3(Float),
  )
}

type StepSpec {
  StepSpec(
    title: String,
    note: String,
    background: BackgroundSpec,
    mesh: option.Option(MeshSpec),
  )
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(
      step_index: 0,
      autoplay: True,
      elapsed: 0.0,
      successful_loads: 0,
      failed_loads: 0,
      last_loaded: "none yet",
      last_error: "none yet",
    ),
    effect.none(),
  )
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick(ctx) -> maybe_advance(model, duration.to_seconds(ctx.delta_time))
    Advance -> #(advance(model, 1), effect.none())
    Previous -> #(advance(model, -1), effect.none())
    ToggleAutoplay ->
      #(Model(..model, autoplay: !model.autoplay, elapsed: 0.0), effect.none())
    MeshLoaded(id) ->
      #(
        Model(
          ..model,
          successful_loads: model.successful_loads + 1,
          last_loaded: id,
        ),
        effect.none(),
      )
    MeshFailed(id) ->
      #(
        Model(
          ..model,
          failed_loads: model.failed_loads + 1,
          last_error: id,
        ),
        effect.none(),
      )
  }
}

fn maybe_advance(model: Model, dt: Float) -> #(Model, effect.Effect(Msg)) {
  let elapsed = model.elapsed +. dt

  case model.autoplay {
    True if elapsed >=. autoplay_interval_seconds ->
      #(advance(model, 1), effect.none())
    True -> #(Model(..model, elapsed: elapsed), effect.none())
    False -> #(model, effect.none())
  }
}

fn advance(model: Model, direction: Int) -> Model {
  let current = wrap_step(model.step_index + direction)
  Model(..model, step_index: current, elapsed: 0.0)
}

fn wrap_step(index: Int) -> Int {
  case index {
    -1 -> 3
    4 -> 0
    _ -> index
  }
}

fn current_step(model: Model) -> StepSpec {
  case model.step_index {
    0 ->
      StepSpec(
        title: "Step 1: good mesh request",
        note: "A valid STL starts loading while the scene background also changes.",
        background: BackgroundSpec("Alpine", alpine_background),
        mesh:
          option.Some(
            MeshSpec(
              id: "asset-a",
              label: "Lucy STL",
              src: lucy_model,
              color: 0xf97316,
              scale: 0.004,
              rotation: vec3.Vec3(-1.5708, 0.6, 0.0),
              position: vec3.Vec3(0.0, 0.4, 0.0),
            ),
          ),
      )

    1 ->
      StepSpec(
        title: "Step 2: same churn, different owner",
        note:
          "The first mesh disappears and a different node starts an OBJ request.",
        background: BackgroundSpec("Studio", studio_background),
        mesh:
          option.Some(
            MeshSpec(
              id: "asset-b",
              label: "WaltHead OBJ",
              src: walt_head_model,
              color: 0x38bdf8,
              scale: 0.85,
              rotation: vec3.Vec3(0.0, -0.5, 0.0),
              position: vec3.Vec3(0.0, -0.5, 0.0),
            ),
          ),
      )

    2 ->
      StepSpec(
        title: "Step 3: failure path",
        note:
          "This request should only produce an error event for the current node.",
        background: BackgroundSpec("Coast", coast_background),
        mesh:
          option.Some(
            MeshSpec(
              id: "asset-c",
              label: "Missing mesh",
              src: missing_model,
              color: 0xe11d48,
              scale: 0.5,
              rotation: vec3.Vec3(0.0, 0.0, 0.0),
              position: vec3.Vec3(0.0, 0.4, 0.0),
            ),
          ),
      )

    _ ->
      StepSpec(
        title: "Step 4: owner removed",
        note:
          "No mesh is rendered in this phase, so late results from earlier steps must be dropped.",
        background: BackgroundSpec("Alpine", alpine_background),
        mesh: option.None,
      )
  }
}

fn view(model: Model) {
  let step = current_step(model)
  let mesh_label = case step.mesh {
    option.Some(mesh) -> mesh.label
    option.None -> "none"
  }

  html.div(
    [attribute.styles([#("display", "grid"), #("gap", "1rem")])],
    [
      control_panel(model, step, mesh_label),
      tiramisu.renderer(
        "renderer",
        [renderer.width(980), renderer.height(580)],
        [
          tiramisu.scene(
            "scene",
            [
              scene.on_tick(Tick),
              scene.background_texture(step.background.url),
              scene.background_color_space_srgb(),
            ],
            scene_children(step),
          ),
        ],
      ),
    ],
  )
}

fn scene_children(step: StepSpec) {
  let mesh_nodes = case step.mesh {
    option.Some(mesh_spec) -> [active_mesh(mesh_spec)]
    option.None -> []
  }

  [
    tiramisu.camera(
      "camera",
      [
        camera.active(True),
        camera.fov(42.0),
        transform.position(vec3.Vec3(0.0, 1.8, 7.0)),
      ],
      [],
    ),
    tiramisu.light("ambient", [light.ambient(), light.intensity(0.55)], []),
    tiramisu.light(
      "sun",
      [
        light.directional(),
        light.intensity(1.3),
        transform.position(vec3.Vec3(5.0, 8.0, 6.0)),
      ],
      [],
    ),
    tiramisu.primitive(
      "platform",
      [
        primitive.cylinder(
          radius_top: 1.5,
          radius_bottom: 1.9,
          height: 0.35,
          segments: 48,
        ),
        material.standard(),
        material.color(0x0f172a),
        material.roughness(0.9),
        transform.position(vec3.Vec3(0.0, -1.0, 0.0)),
      ],
      [],
    ),
    tiramisu.primitive(
      "ground",
      [
        primitive.plane(vec2.Vec2(18.0, 12.0)),
        material.standard(),
        material.color(0x1e293b),
        material.roughness(1.0),
        transform.position(vec3.Vec3(0.0, -1.18, 0.0)),
        transform.rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
      ],
      [],
    ),
    ..mesh_nodes
  ]
}

fn active_mesh(spec: MeshSpec) {
  tiramisu.mesh(
    spec.id,
    [
      mesh.src(spec.src),
      mesh.center(True),
      mesh.cast_shadow(True),
      mesh.on_model_load(MeshLoaded),
      mesh.on_model_error(MeshFailed),
      transform.scale(vec3.splat(spec.scale)),
      transform.rotation(spec.rotation),
      transform.position(spec.position),
      material.color(spec.color),
    ],
    [],
  )
}

fn control_panel(model: Model, step: StepSpec, mesh_label: String) {
  let autoplay_label = case model.autoplay {
    True -> "Pause autoplay"
    False -> "Resume autoplay"
  }

  html.div(
    [
      attribute.styles([
        #("display", "grid"),
        #("gap", "0.75rem"),
        #("padding", "1rem"),
        #("border-radius", "16px"),
        #("background", "#e2e8f0"),
        #("color", "#0f172a"),
        #("font-family", "Iosevka, Menlo, monospace"),
      ]),
    ],
    [
      html.div(
        [attribute.styles([#("display", "flex"), #("gap", "0.75rem")])],
        [
          control_button("Previous", Previous),
          control_button("Next", Advance),
          control_button(autoplay_label, ToggleAutoplay),
        ],
      ),
      status_line("Active phase", step.title),
      status_line("Scene note", step.note),
      status_line("Requested mesh", mesh_label),
      status_line("Background", step.background.label),
      status_line("Successful load events", int.to_string(model.successful_loads)),
      status_line("Failed load events", int.to_string(model.failed_loads)),
      status_line("Last loaded node", model.last_loaded),
      status_line("Last error node", model.last_error),
      html.p(
        [attribute.styles([#("margin", "0"), #("font-size", "0.92rem")])],
        [
          html.text(
            "Let autoplay run for a few cycles. You should only see events for the currently rendered mesh node, not for owners that were already removed.",
          ),
        ],
      ),
    ],
  )
}

fn status_line(label: String, value: String) {
  html.p(
    [attribute.styles([#("margin", "0")])],
    [html.text(label <> ": " <> value)],
  )
}

fn control_button(label: String, message: Msg) {
  html.button(
    [
      event.on_click(message),
      attribute.styles([
        #("border", "0"),
        #("border-radius", "999px"),
        #("padding", "0.65rem 1rem"),
        #("background", "#0f172a"),
        #("color", "#f8fafc"),
        #("cursor", "pointer"),
      ]),
    ],
    [html.text(label)],
  )
}
