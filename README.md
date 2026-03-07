# 🍰 Tiramisu

<img alt="Tiramisu-logo-03" src="https://github.com/user-attachments/assets/e4dff9c1-e132-4caa-82fc-37220990857b" />


**A type-safe 3D game engine for Gleam**

Tiramisu brings the power of functional programming and static type safety to game development, leveraging Three.js for professional-grade 3D rendering while maintaining Gleam's elegant, immutable design principles.

[![Package Version](https://img.shields.io/hexpm/v/tiramisu)](https://hex.pm/packages/tiramisu)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/tiramisu/)

---

## ✨ Features

TODO

---

## 🚀 Quick Start

TODO

---

## 🧀 Quick Project Setup with Mascarpone

**The fastest way to start a new Tiramisu project:**

```sh
gleam add --dev mascarpone
gleam run -m mascarpone
```

[Mascarpone](https://hexdocs.pm/mascarpone/) is an interactive CLI tool that scaffolds complete Tiramisu projects with:

- 🎨 **Beautiful TUI** - Interactive project configuration
- 🎮 **Multiple Templates** - Choose from 2D games, 3D games, or physics demos
- 📦 **Automatic Setup** - Configured `gleam.toml`, dependencies, and CDN imports
- 🖥️ **Lustre Integration** - Optional UI overlays for menus and HUDs
- ⚡ **Working Examples** - Start with a functional game, not an empty file

After creating your project, just run:

```sh
gleam run -m lustre/dev start
```

Then open http://localhost:1234 to see your game!

---

## 📦 Manual Installation

Alternatively, add Tiramisu to an existing Gleam project:

```sh
gleam add tiramisu
```

### Configure your gleam.toml

Add Three.js, Rapier, and styling configuration:

```toml
[tools.lustre.html]
scripts = [
  { type = "importmap", content = "{ \"imports\": { \"three\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\", \"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\", \"@dimforge/rapier3d-compat\": \"https://cdn.jsdelivr.net/npm/@dimforge/rapier3d-compat@0.11.2/+esm\" } }" }
]
```

---

## 🏗️ Core Concepts

TODO

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

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
