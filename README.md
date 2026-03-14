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
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/primitive
import tiramisu/renderer
import tiramisu/scene
import tiramisu/transform
import vec/vec2
import vec/vec3

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
  let assert Ok(_) = lustre.start(lustre.element(view()), "#app", Nil)
  Nil
}

fn view() {
  tiramisu.renderer(
    "renderer",
    [renderer.width(800), renderer.height(480)],
    [
      tiramisu.scene("scene", [scene.background_color(0x111827)], [
        tiramisu.camera(
          "camera",
          [camera.active(True), transform.position(vec3.Vec3(0.0, 0.0, 6.0))],
          [],
        ),
        tiramisu.light("ambient", [light.ambient(), light.intensity(0.6)], []),
        tiramisu.primitive(
          "hero",
          [
            primitive.sphere(radius: 1.0, segments: vec2.Vec2(32, 16)),
            material.color(0x38bdf8),
          ],
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

1. `examples/01-basics/01-first-visible-scene`
2. `examples/01-basics/02-transforms-and-hierarchy`
3. `examples/01-basics/03-cameras`
4. `examples/01-basics/04-lighting-and-materials`
5. `examples/01-basics/05-primitive-gallery`
6. `examples/02-scene/01-backgrounds-and-fog`
7. `examples/02-effects/01-on-tick-animation`
8. `examples/03-assets/01-model-loading`
9. `examples/03-assets/02-material-textures`
10. `examples/04-layout/01-multiple-renderers`
11. `examples/05-server-components/01-basic-setup`

See `examples/README.md` for the full walkthrough.


## Development

```sh
gleam build --target javascript
```
