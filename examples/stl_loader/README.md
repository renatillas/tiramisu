# STL Loader Example

This example demonstrates loading 3D models from STL (Stereolithography) files.

## Features

- **Async STL Loading**: Load STL files using promises
- **Custom Geometry**: Display loaded 3D models as custom geometries
- **Loading States**: Visual feedback for loading, success, and error states
  - **Loading**: Shows a spinning cyan cube
  - **Error**: Shows a red cube if loading fails
  - **Success**: Shows your 3D model rotating
- **Model Rotation**: Animated rotation of the loaded model

## Quick Start

### 1. Place Your STL File

Put your STL file in the `priv` directory:

```bash
cd examples/stl_loader
# Place your STL file here
cp /path/to/your/model.stl priv/model.stl
```

### 2. Run the Example

```bash
gleam run -m lustre/dev start
```

Then open your browser to http://localhost:1234

You should see:
- A spinning **cyan cube** while your model loads
- Your **3D model** rotating once loaded
- A spinning **red cube** if loading failed

## What It Shows

The demo loads an STL file and displays it as a rotating 3D model with:

1. **File Loading**: Asynchronous loading of STL files
2. **Custom Geometries**: Using `CustomGeometry` to wrap Three.js BufferGeometry
3. **PBR Materials**: Standard material with metalness and roughness
4. **Multiple Lights**: Ambient and directional lighting for better visualization
5. **State Management**: Visual feedback for different loading states

## Using Your Own STL Files

### Option 1: Place in the Example Directory

1. Add your STL file to `examples/stl_loader/priv/` directory
2. Update the load URL to match your filename:
   ```gleam
   stl.load("./your-model.stl")
   ```

### Option 2: Use a Remote URL

Update the URL to point to any publicly accessible STL file:
```gleam
stl.load("https://example.com/path/to/model.stl")
```

## Key Concepts

### Loading STL Files

```gleam
import tiramisu/stl

let load_effect =
  effect.from_promise(
    promise.map(stl.load("./model.stl"), fn(result) {
      case result {
        Ok(geom) -> ModelLoaded(geom)
        Error(_) -> LoadingFailed
      }
    })
  )
```

### Using Custom Geometry

```gleam
scene.Mesh(
  id: "stl_model",
  geometry: scene.CustomGeometry(loaded_geometry),
  material: scene.StandardMaterial(
    color: 0x4ecdc4,
    metalness: 0.5,
    roughness: 0.5,
    map: option.None,
  ),
  transform: transform.identity(),
)
```

## STL File Format

STL (Stereolithography) is a file format native to 3D printing and CAD software. The loader supports both:
- **ASCII STL**: Text-based format
- **Binary STL**: Binary format (more common, smaller file size)

## Finding STL Files

You can find free STL files at:
- [Thingiverse](https://www.thingiverse.com/)
- [Printables](https://www.printables.com/)
- [MyMiniFactory](https://www.myminifactory.com/)
- [NASA 3D Resources](https://nasa3d.arc.nasa.gov/models)

## Error Handling

The loader handles various error cases:
- **Invalid URLs**: Empty or malformed URLs
- **Load Errors**: Network failures, 404 not found
- **Parse Errors**: Corrupted or invalid STL files

All errors are captured and can be handled in the update function.
