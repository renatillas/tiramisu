import gleam/io
import lustre
import lustre/attribute.{class, href, src, target}
import lustre/effect.{type Effect}
import lustre/element.{type Element, text}
import lustre/element/html.{
  a, div, footer, h1, h2, h3, h4, header, li, nav, p, section, ul,
}
import lustre/event

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  io.println("Tiramisu website started!")
}

pub type Model {
  Model(current_page: Page)
}

pub type Page {
  Home
  Tutorials
  Examples
  Mascarpone
}

pub type Msg {
  Navigate(Page)
}

fn init(_flags) -> #(Model, Effect(Msg)) {
  #(Model(current_page: Home), effect.none())
}

fn update(_model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Navigate(page) -> #(Model(current_page: page), effect.none())
  }
}

fn view(model: Model) -> Element(Msg) {
  div([class("container")], [
    render_header(),
    render_navigation(model.current_page),
    render_main(model.current_page),
    render_footer(),
  ])
}

fn render_header() -> Element(Msg) {
  header([class("header")], [
    h1([class("header-content")], [
      html.img([
        src("Tiramisu-logo-02.png"),
        attribute.attribute("alt", "Tiramisu"),
        class("logo"),
      ]),
    ]),
  ])
}

fn render_navigation(current: Page) -> Element(Msg) {
  nav([class("nav")], [
    ul([class("nav-list")], [
      nav_item("Home", Home, current),
      nav_item("Tutorials", Tutorials, current),
      nav_item("Examples", Examples, current),
      nav_item("🧀 Mascarpone", Mascarpone, current),
    ]),
  ])
}

fn nav_item(label: String, page: Page, current: Page) -> Element(Msg) {
  let is_active = page == current
  let active_class = case is_active {
    True -> " active"
    False -> ""
  }

  li([], [
    a(
      [
        class("nav-item" <> active_class),
        event.on_click(Navigate(page)),
        href("#"),
      ],
      [text(label)],
    ),
  ])
}

fn render_main(page: Page) -> Element(Msg) {
  html.main([class("main")], [
    case page {
      Home -> home_page()
      Tutorials -> tutorials_page()
      Examples -> examples_page()
      Mascarpone -> mascarpone_page()
    },
  ])
}

fn home_page() -> Element(Msg) {
  section([class("page")], [
    h2([class("page-title")], [text("Welcome to Tiramisu")]),
    p([class("lead")], [
      text(
        "Tiramisu brings the power of functional programming and static type safety to game development. ",
      ),
      text(
        "Built on Three.js for professional 3D rendering, wrapped in Gleam's elegant type system.",
      ),
    ]),
    div([class("features-grid")], [
      feature_card(
        "🔒",
        "Type-Safe",
        "Catch bugs at compile time with Gleam's expressive type system",
      ),
      feature_card(
        "🎮",
        "3D & 2D",
        "Full 3D capabilities powered by Three.js, with excellent 2D support",
      ),
      feature_card(
        "⚡",
        "Immutable",
        "Predictable game state management through functional updates",
      ),
      feature_card(
        "🎬",
        "Effect System",
        "MVU architecture inspired by Lustre for clean game loops",
      ),
      feature_card(
        "📦",
        "Rich API",
        "Scene graphs, materials, lighting, animations, physics, and more",
      ),
    ]),
    div([class("quick-start-section")], [
      h3([], [text("Quick Start")]),
      p([], [text("Install Tiramisu in your Gleam project:")]),
      html.pre([class("code-block")], [
        html.code([], [text("gleam add tiramisu")]),
      ]),
      p([], [
        text("Or use "),
        a([event.on_click(Navigate(Mascarpone)), href("#"), class("link")], [
          text("Mascarpone"),
        ]),
        text(" to scaffold a complete project in seconds!"),
      ]),
    ]),
    div([class("links-section")], [
      a(
        [
          href("https://hexdocs.pm/tiramisu/"),
          target("_blank"),
          class("btn btn-primary"),
        ],
        [text("📚 API Documentation")],
      ),
      a(
        [
          href("https://github.com/renatillas/tiramisu"),
          target("_blank"),
          class("btn btn-secondary"),
        ],
        [text("⭐ View on GitHub")],
      ),
    ]),
  ])
}

fn feature_card(
  _icon: String,
  title: String,
  description: String,
) -> Element(Msg) {
  div([class("feature-card")], [
    div([class("feature-card-image-container")], [
      html.img([
        src("tiramisu.jpg"),
        attribute.attribute("alt", "Tiramisu"),
        class("feature-card-image"),
      ]),
    ]),
    div([class("feature-card-content")], [
      h3([class("feature-title")], [text(title)]),
      p([class("feature-description")], [text(description)]),
    ]),
  ])
}

fn tutorials_page() -> Element(Msg) {
  section([class("page")], [
    h2([class("page-title")], [text("Tutorials")]),
    p([class("lead")], [
      text(
        "Learn Tiramisu step by step with our comprehensive tutorial series.",
      ),
    ]),
    div([class("tutorial-list")], [
      tutorial_card(
        "Getting Started",
        "Your first Tiramisu game",
        "Learn the basics of setting up a Tiramisu project and creating your first 3D scene with a rotating cube.",
      ),
      tutorial_card(
        "Understanding MVU",
        "Model-View-Update architecture",
        "Deep dive into Tiramisu's MVU pattern and how to structure your game logic for maintainability.",
      ),
      tutorial_card(
        "Working with Materials",
        "Lighting and materials",
        "Create stunning visuals with different material types, textures, and lighting setups.",
      ),
      tutorial_card(
        "Input Handling",
        "Keyboard, mouse, and touch",
        "Learn how to capture and respond to player input across different devices.",
      ),
      tutorial_card(
        "Physics Integration",
        "Rapier physics engine",
        "Add realistic physics simulation to your games with collision detection and rigid bodies.",
      ),
      tutorial_card(
        "Animation System",
        "Tweens and state machines",
        "Create smooth animations using tweens and manage complex animation states.",
      ),
      tutorial_card(
        "Scene Management",
        "Hierarchical transforms",
        "Build complex scenes with parent-child relationships and nested transformations.",
      ),
      tutorial_card(
        "Camera Systems",
        "Perspective and orthographic",
        "Master different camera types for 3D and 2D games, including split-screen setups.",
      ),
    ]),
  ])
}

fn tutorial_card(
  title: String,
  subtitle: String,
  description: String,
) -> Element(Msg) {
  div([class("tutorial-card")], [
    h3([class("tutorial-title")], [text(title)]),
    p([class("tutorial-subtitle")], [text(subtitle)]),
    p([class("tutorial-description")], [text(description)]),
  ])
}

fn examples_page() -> Element(Msg) {
  section([class("page")], [
    h2([class("page-title")], [text("Examples")]),
    p([class("lead")], [
      text(
        "Explore working examples showcasing different features of Tiramisu.",
      ),
    ]),
    div([class("examples-grid")], [
      example_card("Effects Showcase", "Post-processing effects"),
      example_card("Keyboard & Mouse Input", "Input handling"),
      example_card("Touch Input", "Mobile controls"),
      example_card("Materials & Lights", "Visual rendering"),
      example_card("Geometry Showcase", "3D primitives"),
      example_card("Animation Tweens", "Smooth animations"),
      example_card("2D Camera", "Orthographic view"),
      example_card("Multi-Camera", "Split-screen"),
      example_card("Debug Visualization", "Development tools"),
      example_card("Scene Hierarchy", "Parent-child transforms"),
      example_card("Asset Loading", "External resources"),
      example_card("Animated Models", "GLTF animations"),
      example_card("Character Controller", "Third-person controls"),
      example_card("Physics Demo", "Rapier integration"),
      example_card("Stress Test", "Performance testing"),
      example_card("UI Overlay", "Lustre integration"),
      example_card("Particle Effects", "Visual effects"),
      example_card("Audio Demo", "Sound system"),
      example_card("Spritesheet Animation", "2D sprite animation"),
      example_card("Text Geometry", "3D text rendering"),
      example_card("CSS2D Labels", "HTML labels in 3D space"),
    ]),
  ])
}

fn example_card(title: String, description: String) -> Element(Msg) {
  div([class("example-card")], [
    h3([class("example-title")], [text(title)]),
    p([class("example-description")], [text(description)]),
  ])
}

fn mascarpone_page() -> Element(Msg) {
  section([class("page")], [
    h2([class("page-title")], [text("🧀 Mascarpone")]),
    p([class("lead")], [
      text(
        "The fastest way to start building games with Tiramisu. Mascarpone is an interactive CLI tool that scaffolds complete game projects in seconds.",
      ),
    ]),
    div([class("mascarpone-hero")], [
      html.pre([class("code-block large")], [
        html.code([], [
          text("gleam add --dev mascarpone\n"),
          text("gleam run -m mascarpone"),
        ]),
      ]),
    ]),
    div([class("features-section")], [
      h3([], [text("Why Use Mascarpone?")]),
      div([class("mascarpone-features")], [
        mascarpone_feature(
          "🎨",
          "Beautiful TUI",
          "Interactive terminal interface with smooth keyboard navigation",
        ),
        mascarpone_feature(
          "🎮",
          "Multiple Templates",
          "Choose from 2D games, 3D games, physics demos, or UI-based projects",
        ),
        mascarpone_feature(
          "📦",
          "Automatic Setup",
          "Configured gleam.toml with all dependencies and CDN imports",
        ),
        mascarpone_feature(
          "🖥️",
          "Lustre Integration",
          "Optional UI overlays for menus, HUDs, and game interfaces",
        ),
        mascarpone_feature(
          "⚡",
          "Working Examples",
          "Start with a functional game, not an empty file",
        ),
        mascarpone_feature(
          "🎯",
          "Best Practices",
          "Follow recommended project structure and patterns",
        ),
      ]),
    ]),
    div([class("steps-section")], [
      h3([], [text("Get Started in 3 Steps")]),
      div([class("steps-grid")], [
        step_card("1", "Install", "gleam add --dev mascarpone"),
        step_card("2", "Run", "gleam run -m mascarpone"),
        step_card("3", "Play", "gleam run -m lustre/dev start"),
      ]),
    ]),
    div([class("templates-section")], [
      h3([], [text("Available Templates")]),
      div([class("templates-grid")], [
        template_card(
          "2D Game",
          "Perfect for platformers, top-down games, and pixel art projects with orthographic camera and sprite support.",
        ),
        template_card(
          "3D Game",
          "Full 3D experience with perspective camera, PBR materials, and multiple light sources.",
        ),
        template_card(
          "Physics Demo",
          "Showcase realistic physics simulation with Rapier, including rigid bodies and collision detection.",
        ),
        template_card(
          "UI Project",
          "Game with Lustre UI overlay for menus, HUD elements, and bidirectional messaging.",
        ),
      ]),
    ]),
    div([class("cta-section")], [
      a(
        [
          href("https://hexdocs.pm/mascarpone/"),
          target("_blank"),
          class("btn btn-primary"),
        ],
        [text("📖 Read the Docs")],
      ),
    ]),
  ])
}

fn mascarpone_feature(
  icon: String,
  title: String,
  description: String,
) -> Element(Msg) {
  div([class("mascarpone-feature")], [
    div([class("feature-icon")], [text(icon)]),
    h4([class("feature-title")], [text(title)]),
    p([class("feature-description")], [text(description)]),
  ])
}

fn step_card(number: String, title: String, command: String) -> Element(Msg) {
  div([class("step-card")], [
    div([class("step-number")], [text(number)]),
    h4([class("step-title")], [text(title)]),
    html.pre([class("code-block")], [html.code([], [text(command)])]),
  ])
}

fn template_card(title: String, description: String) -> Element(Msg) {
  div([class("template-card")], [
    h4([class("template-title")], [text(title)]),
    p([class("template-description")], [text(description)]),
  ])
}

fn render_footer() -> Element(Msg) {
  footer([class("footer")], [
    div([class("footer-content")], [
      p([], [
        text("Built with "),
        a([href("https://gleam.run"), target("_blank"), class("link")], [
          text("Gleam"),
        ]),
        text(" and "),
        a([href("https://lustre.build"), target("_blank"), class("link")], [
          text("Lustre"),
        ]),
      ]),
      p([], [
        a(
          [
            href("https://github.com/renatillas/tiramisu"),
            target("_blank"),
            class("link"),
          ],
          [text("GitHub")],
        ),
        text(" • "),
        a(
          [
            href("https://hexdocs.pm/tiramisu/"),
            target("_blank"),
            class("link"),
          ],
          [text("Hex Docs")],
        ),
        text(" • "),
        a(
          [
            href("https://hexdocs.pm/mascarpone/"),
            target("_blank"),
            class("link"),
          ],
          [text("Mascarpone")],
        ),
      ]),
    ]),
  ])
}
