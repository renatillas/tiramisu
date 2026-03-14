# Tiramisu examples

These examples are a guide to the public API.
The first examples form a learnable path. Later sections are focused capability
demos and integrations.

Run most examples with:

```sh
gleam run -m lustre/dev start
```

Then open `http://localhost:1234`.

## Core path

- `01-basics/01-first-visible-scene`
  The smallest useful scene: renderer, scene, active camera, one visible object, and basic lighting.

- `01-basics/02-transforms-and-hierarchy`
  Uses `tiramisu.empty` to show how transforms compose through the scene graph.

- `01-basics/03-cameras`
  Compares perspective and orthographic cameras on the same subject.

- `01-basics/04-lighting-and-materials`
  Shows the main light types and the most important built-in material families.

- `01-basics/05-primitive-gallery`
  A visual reference for built-in geometry and visibility/shadow toggles.

## Scene and effects

- `02-scene/01-backgrounds-and-fog`
  Demonstrates texture and equirectangular backgrounds plus fog.

- `02-effects/01-on-tick-animation`
  Uses `scene.on_tick` to animate transforms from Lustre state.

## Assets and layout

- `03-assets/01-model-loading`
  Loads an external mesh and handles both success and error events.

- `03-assets/02-material-textures`
  Demonstrates texture-driven material attributes.

- `04-layout/01-multiple-renderers`
  Shows multiple independent renderer instances on the same page.

## Integrations

- `05-server-components/01-basic-setup`
  Renders a Tiramisu scene from a Lustre server component. See that example's
  README for run steps.
