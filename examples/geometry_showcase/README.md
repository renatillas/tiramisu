# Geometry Showcase Example

This example demonstrates all available geometry types in Tiramisu.

## Features Demonstrated

### Geometries
- **BoxGeometry** - Rectangular cuboid
- **SphereGeometry** - Smooth sphere with configurable segments
- **ConeGeometry** - Cone with circular base
- **PlaneGeometry** - Flat 2D plane
- **CircleGeometry** - Filled circle
- **CylinderGeometry** - Cylinder with configurable top/bottom radii
- **TorusGeometry** - Donut/torus shape
- **TetrahedronGeometry** - 4-sided polyhedron
- **IcosahedronGeometry** - 20-sided polyhedron

### Other Features
- Immutable scene graph with automatic diff/patch rendering
- StandardMaterial with metalness and roughness
- Ambient and directional lighting
- Transform rotations (each geometry rotates continuously)

## Running

```bash
gleam run -m lustre/dev start
```

Then open http://localhost:1234 in your browser.

## Layout

The geometries are arranged in a 3x3 grid, with each one rotating to showcase its shape.
