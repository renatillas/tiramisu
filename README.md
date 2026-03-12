# Tiramisu

Tiramisu is a 3D renderer for Gleam built on top of Lustre web components and Three.js.

## What it gives you

- Declarative scene construction with `tiramisu.renderer` and `tiramisu.scene`
- Scene nodes as web components: camera, primitive, mesh, light, and empty
- A Lustre-friendly render loop through `tiramisu.on_tick`
- Asset loading through `tiramisu.mesh` and `attribute.src`
- Renderer backgrounds with solid colors, textures, panoramas, and cubemaps

## Install

```sh
gleam add tiramisu
```

Add a Three.js import map to `gleam.toml`:

```toml
[tools.lustre.html]
scripts = [
  { type = "importmap", content = "{ \"imports\": { \"three\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\", \"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\" } }" }
]
```

## Minimal example

```gleam
import lustre
import lustre/attribute
import tiramisu
import tiramisu/camera
import tiramisu/renderer
import tiramisu/transform
import vec/vec3

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
  let assert Ok(_) = lustre.start(lustre.element(view()), "#app", Nil)
  Nil
}

fn view() {
  tiramisu.renderer(
    "renderer",
    [renderer.width(800), renderer.height(480), renderer.background_color(0x111827)],
    [
      tiramisu.scene("scene", [], [
        tiramisu.camera(
          "camera",
          [camera.active(True), transform.position(vec3.Vec3(0.0, 0.0, 6.0))],
          [],
        ),
      ]),
    ],
  )
}
```

## Guide

The best place to learn the API now is the incremental examples in `examples/`.
Start with:

1. `examples/01-basics/01-renderer-and-scene`
2. `examples/01-basics/02-camera-primitive-transform`
3. `examples/01-basics/03-lights-materials-hierarchy`
4. `examples/02-effects/01-on-tick-animation`
5. `examples/03-assets/01-model-loading`
6. `examples/04-layout/01-multiple-renderers`
7. `examples/05-server-components/01-basic-setup`

See `examples/README.md` for the full walkthrough.


## Development

```sh
gleam build --target javascript
```
