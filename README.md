# 🍰 Tiramisu

**A type-safe 3D game engine for Gleam**

Tiramisu brings the power of functional programming and static type safety to game development, leveraging Three.js for professional-grade 3D rendering while maintaining Gleam's elegant, immutable design principles.

[![Package Version](https://img.shields.io/hexpm/v/tiramisu)](https://hex.pm/packages/tiramisu)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/tiramisu/)

---

## ✨ Features

- **🔒 Type-Safe**: Catch bugs at compile time with Gleam's expressive type system
- **🎮 3D & 2D**: Full 3D capabilities powered by Three.js, with excellent 2D support
- **⚡ Immutable**: Predictable game state management through functional updates
- **🎬 Effect System**: MVU architecture inspired by Lustre for clean game loops
- **📦 Rich API**: Scene graphs, materials, lighting, animations, physics, and more
- **🚀 Production Ready**: Built on battle-tested Three.js with WebGL acceleration
- **🎯 Zero Runtime Overhead**: Compiles directly to readable JavaScript

---

## 🚀 Quick Start

```gleam
import gleam/option
import tiramisu
import tiramisu/camera
import tiramisu/effect
import tiramisu/scene
import tiramisu/transform
import vec/vec3

type Model {
  Model(rotation: Float)
}

type Msg {
  Tick
}

pub fn main() {
  tiramisu.run(
    dimensions: option.None,  // Fullscreen mode
    background: 0x111111,
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) {
  #(Model(rotation: 0.0), effect.tick(Tick))
}

fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time
      #(Model(rotation: new_rotation), effect.tick(Tick))
    }
  }
}

fn view(model: Model) {
  let assert Ok(cam) = camera.perspective(
    field_of_view: 75.0,
    near: 0.1,
    far: 1000.0,
  )

  let assert Ok(geometry) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(material) = scene.standard_material(
    color: 0x00ff00,
    metalness: 0.3,
    roughness: 0.5,
    map: option.None,
    normal_map: option.None,
  )

  [
    scene.Camera(
      id: "main",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
      look_at: option.None,
      active: True,
      viewport: option.None,
    ),
    scene.Mesh(
      id: "cube",
      geometry: geometry,
      material: material,
      transform: transform.identity
        |> transform.rotate_y(model.rotation),
      physics: option.None,
    ),
  ]
}
```

---

## 📦 Installation

Add Tiramisu to your Gleam project:

```sh
gleam add tiramisu@1
```

### Configure your gleam.toml

Add Three.js, Rapier, and styling configuration:

```toml
name = "my_game"
version = "0.1.0"

# ...

[tools.lustre.html]
scripts = [
  { type = "importmap", content = "{ \"imports\": { \"three\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\", \"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\", \"@dimforge/rapier3d-compat\": \"https://cdn.jsdelivr.net/npm/@dimforge/rapier3d-compat@0.11.2/+esm\" } }" }
]
stylesheets = [
  { content = "body { margin: 0; padding: 0; overflow: hidden; }" }
]
```

**Note**: The `stylesheets` configuration removes default body margins and prevents scrollbars, which is essential for fullscreen games.

---

## 📚 Documentation

- [**Getting Started**](https://hexdocs.pm/tiramisu/getting_started.html) - Your first Tiramisu game
- [**Scene Graph Guide**](https://hexdocs.pm/tiramisu/scene_graph_guide.html) - Understanding scene hierarchies
- [**Performance Guide**](https://hexdocs.pm/tiramisu/performance_guide.html) - Optimization techniques
- [**API Reference**](https://hexdocs.pm/tiramisu/) - Complete API documentation

---

## 🏗️ Core Concepts

### MVU Architecture

Tiramisu follows the Model-View-Update pattern:

- **Model**: Your immutable game state
- **Update**: Pure function that transforms state based on messages
- **View**: Declarative scene description from current state

### Effect System

Side effects (network, timers, audio) are handled through a composable effect system, keeping your update logic pure and testable.

### Scene Graph

Hierarchical node system powered by Three.js, with functional updates and transforms. Support for meshes, lights, cameras, groups, and more.

---

## 🎮 What Can You Build?

- 3D games and simulations
- 2D platformers and top-down games
- Interactive visualizations
- Architectural walkthroughs
- Educational simulations
- Physics-based experiences
- Anything that runs in a browser!

---

## 🛠️ Built With

- **[Gleam](https://gleam.run)** - Type-safe functional language
- **[Three.js](https://threejs.org)** - 3D graphics library
- **[Lustre](https://lustre.build)** - Elm-inspired web framework

---

## 🤝 Contributing

Contributions are welcome! Whether it's bug reports, feature requests, or code contributions, please feel free to open an issue or pull request.

---

## 📄 License

MIT License - see [LICENSE.md](./LICENSE.md) for details.

---

## 🌟 Why Tiramisu?

> "Like the dessert, Tiramisu is made of layers - each one adding richness and depth. From the functional purity of Gleam to the rendering power of Three.js, every layer works together to create something delightful."

**Start building type-safe games today.** ✨

---

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
