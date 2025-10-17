# Tiramisu - Gleam Game Engine

## Gleam Language Overview

### Core Features
- **Static Type System**: Expressive type system catching bugs at compile time
- **Dual Compilation**: Compiles to both Erlang (BEAM) and JavaScript
- **Functional Programming**: Everything is immutable, algebraic data types (ADTs)
- **Integrated Tooling**: Single binary with compiler, build tool, package manager, formatter, LSP
- **JavaScript FFI**: Seamless interop with JavaScript/TypeScript with generated .d.ts files
- **Concurrency**: Actor-based on BEAM; Promise-based when targeting JavaScript
- **Human-Readable Output**: Generated JS code is readable and pretty-printed

### Key Strengths for Game Development
1. **Type Safety**: Eliminates entire classes of runtime errors
2. **Immutability**: Predictable game state management, easier debugging
3. **JavaScript Target**: Direct browser execution without runtime overhead
4. **FFI Support**: Can leverage existing Canvas/WebGL JavaScript libraries
5. **Pattern Matching**: Excellent for game state machines and entity systems
6. **Small Bundle Size**: No additional runtime for JS target

### Limitations & Constraints

#### Language Limitations
- **No Type Classes**: Simplified type system (no higher-kinded types)
- **No Native Mutation**: All data structures immutable (must use actors or JS FFI)
- **Limited Metaprogramming**: Minimal compile-time code generation
- **Impure Functional**: Allows side effects (not purely functional like Haskell)
- **No Hot Code Reloading**: Available but without type safety guarantees

#### Ecosystem Limitations
- **Young Language**: Small core team, ~6 years old
- **Limited Standard Library**: Only 19 modules, no built-in filesystem access
- **Small Package Ecosystem**: Far fewer libraries than JS/TypeScript
- **No Game Development Libraries**: Essentially zero existing game engines/frameworks
- **Setup Complexity**: Requires Erlang toolchain even for JS-only projects
- **Limited Documentation**: Sparse examples for advanced use cases

#### Performance Considerations
- **Immutability Overhead**: Potential GC pressure from structural sharing
- **No SIMD**: Can't directly use hardware acceleration features
- **FFI Boundary Costs**: Crossing JS FFI has overhead for hot paths
- **No WebAssembly Target**: Limited to JS performance characteristics

### Web Development Context
- **Lustre Framework**: Elm-like web framework (React-based), not game-focused
- **No Canvas/WebGL Libraries**: Must use JavaScript FFI for graphics
- **Promise-Based Concurrency**: Standard JS async model on browser target
- **TypeScript Definitions**: Generated for all Gleam code

## Game Engine Architecture Strategy

### Approach
Build a **type-safe, powerful game engine** that:
1. Leverages Gleam's type safety for game logic
2. Uses Three.js via FFI for production-ready 3D/2D rendering
3. Provides functional APIs for game state management
4. Wraps Three.js in type-safe Gleam abstractions

### Why Three.js over Canvas 2D?

#### Advantages
- **WebGL Performance**: Hardware-accelerated rendering out of the box
- **3D Capabilities**: Full 3D support with option for 2D games (orthographic camera)
- **Production Ready**: Mature, battle-tested library used in real games
- **Rich Features**: Built-in scene graph, materials, lighting, shadows, post-processing
- **Active Ecosystem**: Large community, extensive documentation, frequent updates
- **Cross-Platform**: Consistent performance across devices
- **Geometry Instancing**: Efficient rendering of many similar objects
- **Asset Pipeline**: Built-in loaders for various formats (GLTF, FBX, textures)

#### Tradeoffs
- **Larger Bundle**: ~600KB minified (vs minimal Canvas 2D)
- **Learning Curve**: More complex API surface to wrap
- **Overkill for Simple 2D**: Canvas 2D sufficient for basic pixel games
- **FFI Surface Area**: More JavaScript interop points

#### Decision
Three.js provides a **professional foundation** that:
- Scales from 2D to 3D games
- Handles WebGL complexity
- Offers production-grade performance
- Reduces need for custom rendering code
- Still maintains type safety through Gleam wrappers

### Design Principles
- **Hybrid Approach**: Pure Gleam for game logic, Three.js FFI for rendering
- **Entity-Component Pattern**: Functional implementation using records and modules
- **Immutable Game State**: Single source of truth updated functionally
- **Type-Safe Three.js Wrappers**: Opaque types wrapping Three.js objects
- **Thin FFI Layer**: Minimal overhead between Gleam and Three.js
- **Scene Graph Integration**: Leverage Three.js scene graph with functional updates

### Target Features
1. Core loop (update/render cycle)
2. Input handling (keyboard, mouse, touch)
3. 3D rendering (Three.js WebGL via FFI)
4. 2D sprite support (orthographic camera + planes)
5. Scene/entity management (integrate with Three.js scene graph)
6. Basic physics (collision detection, raycasting)
7. Asset loading (models, textures, audio via Three.js loaders)
8. Animation system (Three.js AnimationMixer + custom)
9. Camera controls (orbital, first-person, 2D)
10. Lighting and materials
11. Post-processing effects

### Non-Goals (Initially)
- Custom WebGL shaders (use Three.js built-ins first)
- Advanced physics engine (integrate with Rapier later)
- Networking/multiplayer
- Level editors
- Mobile-specific optimizations
- VR/AR support

## Technology Stack
- **Language**: Gleam (targeting JavaScript)
- **Rendering**: Three.js (WebGL) via FFI
- **Build Tool**: Gleam's built-in tooling + Vite for bundling
- **Type Safety**: Gleam's type system + opaque types for Three.js objects
- **Testing**: Gleam's built-in test framework
- **Future Physics**: Rapier physics engine (WASM)

## Project Configuration

### Standard gleam.toml Setup

All Tiramisu projects should include this configuration in their `gleam.toml`:

```toml
[tools.lustre.html]
scripts = [
  { type = "importmap", content = "{ \"imports\": { \"three\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\", \"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\", \"@dimforge/rapier3d-compat\": \"https://cdn.jsdelivr.net/npm/@dimforge/rapier3d-compat@0.11.2/+esm\" } }" }
]
stylesheets = [
  { content = "body { margin: 0; padding: 0; overflow: hidden; } canvas { display: block; }" }
]
```

**Key components:**
- `scripts` with importmap: Provides CDN imports for Three.js and Rapier physics
- `stylesheets`: Removes default body margin/padding, prevents scrollbars, and ensures canvas has block display (fixes touch coordinate alignment)

### Canvas Dimensions

When calling `tiramisu.run()`, use:
- `dimensions: None` for fullscreen mode (recommended for games)
- `dimensions: Some(tiramisu.Dimensions(width: 800.0, height: 600.0))` for fixed size

**Example:**
```gleam
import gleam/option.{None, Some}
import tiramisu

pub fn main() {
  // Fullscreen mode
  tiramisu.run(
    dimensions: None,
    background: 0x1a1a2e,
    init: init,
    update: update,
    view: view,
  )
}
```

### Camera Setup

Cameras automatically calculate aspect ratio from viewport or window dimensions:

```gleam
import tiramisu/camera

// Aspect ratio is calculated automatically
let assert Ok(cam) = camera.perspective(
  field_of_view: 75.0,
  near: 0.1,
  far: 1000.0,
)
```

### Game Context

The `Context` passed to `init()` and `update()` contains:
- `delta_time`: Time in seconds since last frame (for frame-rate independent movement)
- `input`: Current input state (keyboard, mouse, touch)
- `canvas_width`: Current canvas width in pixels
- `canvas_height`: Current canvas height in pixels

The canvas dimensions are automatically updated on window resize in fullscreen mode, making them ideal for coordinate conversion:

```gleam
fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  case msg {
    TouchMove(x, y) -> {
      // Convert screen coordinates to world space
      let world_x = { x -. ctx.canvas_width /. 2.0 } /. 100.0
      let world_y = { ctx.canvas_height /. 2.0 -. y } /. 100.0

      // Update model with converted coordinates
      Model(..model, position: vec3.Vec3(world_x, world_y, 0.0))
    }
  }
}
```

For cameras with custom viewports, the aspect ratio uses the viewport dimensions. For main cameras, it uses the window dimensions (which update automatically on resize in fullscreen mode).

## Migration Guide from v1.0.0

This section helps developers migrate from v1.0.0 to the next version.

### Breaking Changes

#### 1. Background API

The `background` parameter now uses a `Background` type instead of a simple hex color integer, allowing for color, texture, or cube texture (skybox) backgrounds.

**Old:**
```gleam
tiramisu.run(
  dimensions: None,
  background: 0x111111,  // Just a hex color
  // ...
)
```

**New:**
```gleam
import tiramisu
import tiramisu/background

// Solid color background (most common)
tiramisu.run(
  dimensions: None,
  background: background.Color(0x111111),
  // ...
)

// Texture background
tiramisu.run(
  dimensions: None,
  background: background.Texture("assets/sky.jpg"),
  // ...
)

// Skybox with cube texture (6 faces)
tiramisu.run(
  dimensions: None,
  background: background.CubeTexture([
    "assets/skybox/px.jpg",  // positive x
    "assets/skybox/nx.jpg",  // negative x
    "assets/skybox/py.jpg",  // positive y
    "assets/skybox/ny.jpg",  // negative y
    "assets/skybox/pz.jpg",  // positive z
    "assets/skybox/nz.jpg",  // negative z
  ]),
  // ...
)
```

**Migration:** Wrap all existing hex color backgrounds with `background.Color()`:
```gleam
// Change this:
background: 0x1a1a2e

// To this:
background: background.Color(0x1a1a2e)
```

#### 2. Canvas Dimensions API

**Old:**
```gleam
tiramisu.run(
  width: 800,
  height: 600,
  background: 0x111111,
  // ...
)
```

**New:**
```gleam
import gleam/option

// Fullscreen mode (recommended)
tiramisu.run(
  dimensions: option.None,
  background: background.Color(0x111111),
  // ...
)

// Or fixed size
tiramisu.run(
  dimensions: option.Some(tiramisu.Dimensions(width: 800.0, height: 600.0)),
  background: background.Color(0x111111),
  // ...
)
```

#### 3. Camera Perspective API

**Old:**
```gleam
let assert Ok(cam) = camera.perspective(
  field_of_view: 75.0,
  aspect: 16.0 /. 9.0,
  near: 0.1,
  far: 1000.0,
)
```

**New:**
```gleam
// Aspect ratio is calculated automatically at render time
let assert Ok(cam) = camera.perspective(
  field_of_view: 75.0,
  near: 0.1,
  far: 1000.0,
)
```

#### 4. Camera 2D API

**Old:**
```gleam
let cam = camera.camera_2d(width: 800, height: 600, distance: 5.0)
```

**New:**
```gleam
// Distance is now set via scene.Camera node's transform
let cam = camera.camera_2d(width: 800, height: 600)

scene.Camera(
  id: "main",
  camera: cam,
  transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
  // ...
)
```

#### 5. Camera Position and Look At

**Old:**
```gleam
let cam = camera.perspective(...)
  |> camera.set_position(vec3.Vec3(0.0, 5.0, 10.0))
  |> camera.set_look_at(vec3.Vec3(0.0, 0.0, 0.0))
```

**New:**
```gleam
// Position and look_at are now set on the scene.Camera node
let assert Ok(cam) = camera.perspective(
  field_of_view: 75.0,
  near: 0.1,
  far: 1000.0,
)

scene.Camera(
  id: "main",
  camera: cam,
  transform: transform.at(position: vec3.Vec3(0.0, 5.0, 10.0)),
  look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
  active: True,
  viewport: option.None,
)
```

#### 6. Context Type

The `Context` type now includes canvas dimensions:

**Old:**
```gleam
pub type Context {
  Context(delta_time: Float, input: input.InputState)
}
```

**New:**
```gleam
pub type Context {
  Context(
    delta_time: Float,
    input: input.InputState,
    canvas_width: Float,
    canvas_height: Float,
  )
}
```

**Impact:** If you pattern match on Context, update your patterns:

```gleam
// Old
fn update(model: Model, msg: Msg, Context(delta_time, input)) { ... }

// New
fn update(model: Model, msg: Msg, Context(delta_time, input, width, height)) { ... }

// Or just use record access
fn update(model: Model, msg: Msg, ctx: Context) {
  let x = ctx.canvas_width
  // ...
}
```

#### 7. Geometry and Material Types

Geometry and material types are now in separate modules with validated constructors and builder patterns:

**Old:**
```gleam
scene.Mesh(
  id: "cube",
  geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
  material: scene.StandardMaterial(color: 0xff0000),
  // ...
)
```

**New:**
```gleam
import tiramisu/geometry
import tiramisu/material

let assert Ok(geometry) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)

// Option 1: Builder pattern (recommended)
let assert Ok(material) =
  material.new()
  |> material.with_color(0xff0000)
  |> material.with_metalness(0.8)
  |> material.with_roughness(0.3)
  |> material.build()

// Option 2: Direct constructor with all parameters
let assert Ok(material2) = material.standard(
  color: 0xff0000,
  metalness: 0.8,
  roughness: 0.3,
  map: option.None,
  normal_map: option.None,
  ambient_oclusion_map: option.None,
  roughness_map: option.None,
  metalness_map: option.None,
)

scene.Mesh(
  id: "cube",
  geometry: geometry,
  material: material,
  // ...
)
```

#### 8. Light Constructors

Lights are now in a separate module with validated constructors:

**Old:**
```gleam
scene.Light(
  id: "sun",
  light_type: scene.DirectionalLight(color: 0xffffff, intensity: 1.0),
  transform: transform.identity,
)
```

**New:**
```gleam
import tiramisu/light

let assert Ok(sun) = light.directional(
  intensity: 1.0,
  color: 0xffffff,
)

scene.Light(
  id: "sun",
  light: sun,  // Note: renamed from light_type to light
  transform: transform.identity,
)
```

#### 9. State Machine Generic Types

State machines are now generic over state type:

**Old:**
```gleam
let machine = state_machine.new("idle")
  |> state_machine.add_state("idle", idle_anim, looping: True)
  |> state_machine.add_state("running", run_anim, looping: True)
  |> state_machine.add_transition(
    from: "idle",
    to: "running",
    condition: state_machine.Always,
    blend_duration: 0.3,
  )
```

**New:**
```gleam
// Define your state type
type State {
  Idle
  Running
}

let machine = state_machine.new(Idle)
  |> state_machine.add_state(Idle, idle_anim, looping: True)
  |> state_machine.add_state(Running, run_anim, looping: True)
  |> state_machine.add_transition(
    from: Idle,
    to: Running,
    condition: state_machine.Always,
    blend_duration: 0.3,
  )
```

### New Features

#### Canvas Dimensions in Context

You can now access canvas dimensions for coordinate conversion:

```gleam
fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  case msg {
    TouchMove(screen_x, screen_y) -> {
      // Convert screen to world coordinates using actual canvas size
      let world_x = { screen_x -. ctx.canvas_width /. 2.0 } /. 100.0
      let world_y = { ctx.canvas_height /. 2.0 -. screen_y } /. 100.0
      // ...
    }
  }
}
```

#### Fullscreen Mode

Easily create fullscreen games that automatically resize:

```gleam
tiramisu.run(
  dimensions: option.None,  // Fullscreen!
  background: 0x111111,
  // ...
)
```

#### Camera Look At

Cameras now support look-at targets directly in scene nodes:

```gleam
scene.Camera(
  id: "main",
  camera: cam,
  transform: transform.at(position: vec3.Vec3(10.0, 10.0, 10.0)),
  look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),  // Look at origin
  active: True,
  viewport: option.None,
)
```
- You cannot run the examples. Usually the user will run them in tandem to your conversation

## FFI Architecture (Post-Refactor)

### Overview

The library now uses a **layered FFI architecture** that separates pure bindings from business logic, making the codebase more testable, maintainable, and allowing more code to be written in Gleam.

### Architecture Layers

```
┌─────────────────────────────────────┐
│   Gleam Game Logic & API            │  ← User code
├─────────────────────────────────────┤
│   Tiramisu Gleam Modules            │  ← geometry.gleam, material.gleam, etc.
│   (Declarative, type-safe)          │     physics.gleam, scene.gleam
├─────────────────────────────────────┤
│   Pure FFI Bindings (1:1)           │  ← threejs.ffi.mjs, rapier.ffi.mjs
│   (No logic, just thin wrappers)    │     Direct library API calls
├─────────────────────────────────────┤
│   External Libraries                │  ← Three.js, Rapier3D
└─────────────────────────────────────┘
```

### Core FFI Files

#### `src/threejs.ffi.mjs`
**Purpose**: Pure 1:1 bindings to Three.js API
**Contents**: Direct wrappers around Three.js functions with no business logic
**Used by**:
- `tiramisu/geometry.gleam` - Geometry creation
- `tiramisu/material.gleam` - Material creation
- `tiramisu/light.gleam` - Light creation
- `tiramisu/camera.gleam` - Camera creation
- `tiramisu/object3d.gleam` - Animation utilities
- `tiramisu/internal/renderer.gleam` - Core rendering

**Example binding**:
```javascript
// Pure 1:1 binding - no logic
export function createBoxGeometry(width, height, depth) {
  return new THREE.BoxGeometry(width, height, depth);
}
```

**Gleam usage**:
```gleam
@external(javascript, "../threejs.ffi.mjs", "createBoxGeometry")
fn create_box_geometry(width: Float, height: Float, depth: Float) -> Nil
```

#### `src/rapier.ffi.mjs`
**Purpose**: Pure 1:1 bindings to Rapier3D physics API
**Contents**: Direct wrappers around Rapier functions with no business logic
**Used by**: `tiramisu/physics.gleam`

**Example binding**:
```javascript
// Pure 1:1 binding - no logic
export function createWorld(gravityX, gravityY, gravityZ) {
  const gravity = { x: gravityX, y: gravityY, z: gravityZ };
  return new RAPIER.World(gravity);
}
```

#### `src/tiramisu.ffi.mjs`
**Purpose**: Main consolidated FFI file containing all game engine business logic
**Contents**: Game loop, scene rendering, physics integration, audio system, input capture, asset loading, debug tools, UI integration, and effect system
**Why FFI**: Performance-critical code paths, DOM API integration, mutable state management, complex callback handling, async coordination

**Major subsystems**:
- **Game Loop**: Animation frame loop, delta time calculation, state updates
- **Scene Rendering**: Scene graph management, patch application, object caching, instance rendering
- **Physics Integration**: Rapier world management, collision detection, rigid body lifecycle
- **Audio System**: Web Audio API state management, group volume control, spatial audio
- **Input Capture**: Keyboard, mouse, touch, gamepad event listeners
- **Asset Loading**: Batch loading coordination, progress tracking, model/texture loaders
- **Debug Tools**: Performance monitoring, debug visualization, stats collection
- **UI Integration**: Lustre framework integration, bidirectional messaging
- **Effect System**: Scene manipulation effects, callback management

### Design Benefits

1. **Testability**: Pure FFI bindings are easier to mock and test
2. **Maintainability**: Clear separation between bindings and logic
3. **Expandability**: Easy to add new Three.js/Rapier functions
4. **Type Safety**: More logic in Gleam = more compile-time guarantees
5. **Documentation**: 1:1 bindings are self-documenting
6. **Performance**: Critical rendering logic stays in optimized JS

### Adding New Bindings

To add a new Three.js binding:

1. Add the 1:1 function to `src/threejs.ffi.mjs`:
```javascript
export function createSomeGeometry(param1, param2) {
  return new THREE.SomeGeometry(param1, param2);
}
```

2. Add the Gleam external declaration:
```gleam
@external(javascript, "../threejs.ffi.mjs", "createSomeGeometry")
fn create_some_geometry(param1: Float, param2: Float) -> Nil
```

3. Add Gleam validation/builder logic around it:
```gleam
pub fn some_geometry(
  param1 param1: Float,
  param2 param2: Float,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(param1 <=. 0.0, Error(InvalidParam))
  Ok(SomeGeometry(param1, param2))
}
```

### Migration Status

**Completed**:
- ✅ Created `src/threejs.ffi.mjs` with comprehensive Three.js bindings (pure 1:1 functions)
- ✅ Created `src/rapier.ffi.mjs` with comprehensive Rapier bindings (pure 1:1 functions)
- ✅ Consolidated all business logic FFI into `src/tiramisu.ffi.mjs` (game loop, rendering, physics, audio, input, assets, debug, UI, effects)
- ✅ Deleted `src/tiramisu/ffi/` directory and all individual FFI files
- ✅ Updated `geometry.gleam`, `material.gleam`, `light.gleam`, `camera.gleam` to use new structure
- ✅ Updated `physics.gleam` to use `rapier.ffi.mjs`
- ✅ Updated `internal/renderer.gleam` to use `tiramisu.ffi.mjs`
- ✅ Updated `audio.gleam` to use `tiramisu.ffi.mjs`
- ✅ Fixed all `@external` declarations to point to consolidated FFI files
- ✅ Library builds successfully
- ✅ Example projects compile and run

**Final Architecture**:
- `src/threejs.ffi.mjs` - Pure Three.js bindings (no logic)
- `src/rapier.ffi.mjs` - Pure Rapier bindings (no logic)
- `src/tiramisu.ffi.mjs` - All game engine business logic (consolidated)

### Future Improvements

1. **Further Gleam Migration**: Move more business logic from `tiramisu.ffi.mjs` to Gleam modules:
   - Scene graph operations could be pure Gleam using `threejs.ffi.mjs`
   - Asset batch coordination once Gleam gets better async support
   - Debug stats aggregation, keeping only browser API calls in FFI

2. **Particle System Integration**: Complete the particle system integration in renderer (currently marked as TODO)

3. **Performance Optimization**: Profile and optimize hot paths in the consolidated FFI file
