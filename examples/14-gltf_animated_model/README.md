# GLTF Animated Model Example

This example demonstrates loading 3D models with animations from GLTF/GLB files.

## Setup

1. Place your GLTF or GLB file in `priv/static/` and name it `model.glb` (or update the filename in the code)

2. Install dependencies and run the example:
   ```bash
   gleam run -m lustre/dev start
   ```

3. Open your browser to `http://localhost:1234`

## Features

- Loads GLTF/GLB models with animations
- Displays model with the first available animation playing
- Supports multiple animations per model
- Configurable animation speed and loop mode
- Declarative animation API

## Finding Free GLTF Models

You can find free GLTF models with animations at:
- [Sketchfab](https://sketchfab.com/features/gltf) - Search for "animated" models with "Downloadable" filter
- [mixamo.com](https://www.mixamo.com/) - Download as FBX and convert to GLB
- [Khronos GLTF Sample Models](https://github.com/KhronosGroup/glTF-Sample-Models)

## Example Model Recommendations

Good starter models:
- `RobotExpressive.glb` from Khronos samples (has multiple animations)
- `Fox.glb` from Khronos samples (simple animated character)
- Any rigged character from Sketchfab with CC license

## Controls (Future Enhancement)

The example currently shows the first animation. You can extend it to add:
- Keyboard controls to switch animations
- Speed controls
- Play/pause functionality
