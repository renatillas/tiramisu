# Tiramisu Examples

This directory contains comprehensive examples demonstrating all major features of the Tiramisu game engine.

## Running Examples

Each example can be run with:

```bash
cd <example-name>
gleam run -m lustre/dev start
```

Then open http://localhost:1234 in your browser.

## Examples

### 1. **keyboard_mouse_input**
Demonstrates keyboard and mouse input handling.
- WASD keys for movement
- Mouse position tracking
- Mouse button clicks (left/right)
- Mouse wheel scrolling

**Key Features:** Keyboard input, Mouse position, Mouse buttons, Mouse wheel

---

### 2. **touch_input**
Demonstrates touch input and multi-touch gestures.
- Single touch to move objects
- Multi-touch for pinch-to-zoom
- Visual touch point indicators

**Key Features:** Touch events, Multi-touch, Pinch gestures

---

### 3. **geometry_showcase**
Displays all available geometry types in a rotating grid.
- Box, Sphere, Cone, Plane, Circle
- Cylinder, Torus, Tetrahedron, Icosahedron

**Key Features:** All geometry types, Transform rotations, Grid layout

---

### 4. **materials_and_lights**
Showcases different material types and lighting systems.
- **Materials:** Basic, Standard (PBR), Phong, Lambert, Toon
- **Lights:** Ambient, Directional, Point, Hemisphere

**Key Features:** Material types, Light types, Physically-based rendering

---

### 5. **animation_tweens**
Demonstrates the tween animation system with various easing functions.
- Position tweening between two points
- Cycles through all easing functions
- Visual start/end markers

**Key Features:** Tween animations, Easing functions (Linear, Quad, Cubic, Sine variants)

---

### 6. **camera_2d**
Shows 2D camera modes with orthographic projection.
- Centered 2D camera
- Grid of rotating squares in 2D space
- Orthographic projection

**Key Features:** 2D camera, Orthographic projection, 2D transforms

---

### 7. **scene_hierarchy**
Demonstrates scene graph with parent-child relationships.
- Solar system with sun and planets
- Nested groups (planet orbits, moon orbits)
- Dynamic scene updates (planets appear/disappear)

**Key Features:** Group nodes, Scene hierarchy, Nested transforms, Dynamic scene changes

---

### 8. **effects_system**
Shows the effect system with async operations.
- Continuous tick effects
- Scheduled effects (setTimeout)
- Dynamic object creation/removal
- Falling cubes with gravity

**Key Features:** Effect system, effect.tick, effect.from, Async operations, Dynamic scenes

---

### 9. **sprites_demo**
Demonstrates loading and displaying sprite textures from the internet.
- Async texture loading from URLs
- Multiple animated sprites with different motion patterns
- Loading state management
- Circular motion, rotation, and bouncing animations

**Key Features:** Texture loading, Sprites, Promises, effect.from_promise, Async resource loading

---

### 10. **stl_loader**
Demonstrates loading 3D models from STL files.
- Async STL file loading from URLs
- Custom geometry support (BufferGeometry)
- Loading states with error handling
- Rotating 3D model visualization
- Support for both ASCII and Binary STL formats

**Key Features:** STL loading, Custom geometries, 3D model import, BufferGeometry, PBR materials

---

### 11. **gltf_animated_model**
Demonstrates loading GLTF/GLB models with animations.
- Async GLTF/GLB file loading
- Animated 3D models support
- Multiple animations per model
- Declarative animation API
- Animation speed and loop control
- Animation switching support

**Key Features:** GLTF/GLB loading, Skeletal animations, AnimationMixer, Model3D scene node, Object3D

## Feature Coverage

| Feature | Examples |
|---------|----------|
| **Input** | keyboard_mouse_input, touch_input |
| **Geometries** | geometry_showcase, stl_loader (custom) |
| **Materials** | materials_and_lights, All examples |
| **Lights** | materials_and_lights, Most examples |
| **Animations** | animation_tweens, gltf_animated_model (skeletal) |
| **Camera 2D** | camera_2d |
| **Camera 3D** | All 3D examples |
| **Scene Hierarchy** | scene_hierarchy |
| **Effects** | effects_system, sprites_demo, stl_loader, gltf_animated_model, All examples (tick) |
| **Textures & Sprites** | sprites_demo |
| **STL Loading** | stl_loader |
| **GLTF Loading** | gltf_animated_model |
| **Custom Geometries** | stl_loader |
| **3D Model Animations** | gltf_animated_model |
| **Async Loading** | sprites_demo, stl_loader, gltf_animated_model |
| **Transforms** | All examples |
| **Immutable API** | All examples |

## Architecture

All examples follow the **Model-View-Update (MVU)** architecture:

1. **Model** - Immutable application state
2. **View** - Pure function that renders state to scene nodes
3. **Update** - Pure function that transforms state based on messages
4. **Effects** - Side effects (managed by the framework)

## Testing Coverage

These examples ensure all code paths are exercised:
- ✅ All 9 built-in geometry types + custom geometries
- ✅ All 7 material types
- ✅ All 5 light types
- ✅ All 9 easing functions
- ✅ Keyboard, mouse, and touch input
- ✅ 2D and 3D cameras
- ✅ Scene hierarchy with groups
- ✅ Effect system (tick, from, and from_promise)
- ✅ Texture loading and sprites
- ✅ STL file loading and 3D model import
- ✅ GLTF/GLB file loading with animations
- ✅ Custom BufferGeometry integration
- ✅ Skeletal animations with AnimationMixer
- ✅ Model3D scene nodes
- ✅ Async resource loading with promises
- ✅ Dynamic scene updates
- ✅ Transform operations
