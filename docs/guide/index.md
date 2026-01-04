# Tiramisu Guide

Welcome to Tiramisu, a type-safe 3D game engine for Gleam.

Tiramisu takes a different approach to game development. Instead of mutable objects scattered across a scene editor, you describe your game as pure functions. State changes happen in one place. Rendering is declarative. The type system catches bugs at compile time.

This approach—borrowed from the Elm Architecture—might feel unusual at first if you're coming from Unity or Godot. But it makes games easier to reason about, test, and maintain as they grow.

## Getting started

If you're new to Tiramisu, start here:

1. **[Quickstart](01-quickstart.md)** - Get a spinning cube on screen in 5 minutes. Understand the basic structure of a Tiramisu game.

## Core concepts

These guides explain the fundamental patterns:

2. **[State Management](02-state-management.html)** - The Model-View-Update pattern. How to structure your game state, name your messages, and write update functions that scale.

3. **[Side Effects](03-side-effects.html)** - How Tiramisu handles the messy parts: timers, audio loading, browser APIs. Keep your logic pure while still doing useful things.

4. **[The Scene Graph](04-scene-graph.html)** - Declarative 3D rendering. Build scenes from meshes, lights, and cameras. Understand how Tiramisu diffs and patches efficiently.

5. **[Physics](05-physics.html)** - Add realistic simulation with Rapier. Rigid bodies, collision detection, forces, and raycasting.

6. **[Lustre Integration](06-lustre-integration.html)** - Connect Tiramisu with Lustre for HTML/CSS UI overlays. Build health bars, menus, inventories, and HUDs.

## Learning approach

These guides are designed to be read in order, but you can skip around based on what you need:

- **New to game dev?** Read everything in order.
- **Know MVU from Elm/Lustre?** Skim the state management guide, focus on scene graph and physics.
- **Want to build something now?** Start with quickstart, then jump to whichever guide covers what you're building.

## Philosophy

Tiramisu is opinionated. Here's what we believe:

**Immutability wins.** Mutable state seems convenient until you're debugging why your game randomly breaks. Immutable data with explicit updates is easier to reason about, test, and replay.

**Types prevent bugs.** A `Result(Geometry, GeometryError)` is better than a geometry that might be null. A `Msg` type that enumerates all possible events is better than stringly-typed callbacks.

**Declarative beats imperative.** Describing what you want (a scene tree) is clearer than describing how to build it (create this object, modify that property, destroy that one).

**Simple beats clever.** A flat model with obvious update logic beats a sophisticated ECS that's hard to debug. Start simple, add complexity only when you need it.

## Examples

The best way to learn is to read and modify working code. Check out the `examples/` folder in the repository:

- Simple demos showing individual features
- Complete mini-games demonstrating patterns
- Physics demos with collision handling

## Getting help

- **API Reference** - [hexdocs.pm/tiramisu](https://hexdocs.pm/tiramisu/)
- **Source Code** - [github.com/renatillas/tiramisu](https://github.com/renatillas/tiramisu)
- **Issues** - Report bugs or request features on GitHub

Let's make some games.
