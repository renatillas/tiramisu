# Tiramisu examples

These examples are a small guide to the library.
They are ordered so each example adds one new capability on top of the previous ones.

Run any example with:

```sh
gleam run -m lustre/dev start
```

Then open `http://localhost:1234`.

## 01-basics

- `01-renderer-and-scene`
  Minimal setup: register Tiramisu, mount a renderer, mount a scene, and add one active camera.

- `02-camera-primitive-transform`
  Adds geometry and transforms with a primitive cube and a positioned camera.

- `03-lights-materials-hierarchy`
  Adds lighting, materials, and `tiramisu.empty` for scene hierarchy.

## 02-effects

- `01-on-tick-animation`
  Uses `tiramisu.on_tick` to animate a scene from Lustre state.

## 03-assets

- `01-model-loading`
  Loads a remote GLB model with `tiramisu.mesh` and listens for model events.

## 04-layout

- `01-multiple-renderers`
  Demonstrates that multiple renderer instances can live on the same page.

## 05-server-components

- `01-basic-setup`
  Mirrors Lustre's basic server-component guide, but the server-side component
  renders a Tiramisu scene. This example is an Erlang app and has its own run
  steps in `examples/05-server-components/01-basic-setup/README.md`.
