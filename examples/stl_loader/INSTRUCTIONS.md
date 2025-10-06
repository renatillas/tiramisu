# How to Use the STL Loader Example

This example is ready to load YOUR STL file!

## Step-by-Step Instructions

### 1. Navigate to the example directory

```bash
cd examples/stl_loader
```

### 2. Place your STL file

Copy your STL file into the Lustre assets directory and name it `model.stl`.

The assets folder is typically at `.lustre/static/` after running the dev server once:

```bash
# Run the dev server first (it will fail to load, that's OK)
gleam run -m lustre/dev start

# In another terminal, copy your file
cp /path/to/your-file.stl .lustre/static/model.stl
```

Or you can place it before starting the server if you know where it should go.

### 3. Start the development server

```bash
gleam run -m lustre/dev start
```

### 4. Open your browser

Navigate to http://localhost:1234

## What You'll See

The example has three visual states:

1. **Loading** (Cyan spinning cube)
   - Appears immediately while your STL file loads
   - Spins in multiple directions

2. **Success** (Your 3D model)
   - Your STL model appears and rotates
   - Rendered with PBR materials (cyan metallic look)
   - Smooth shading with recomputed normals
   - Smooth rotation animation

3. **Error** (Red spinning cube)
   - Shows if the file can't be loaded
   - Check the browser console for error details
   - Make sure your file is at `priv/model.stl`

## Testing Without Your Own File

If you want to test before providing your own STL, you can:

1. Download a free STL file from [Thingiverse](https://www.thingiverse.com/)
2. Or use NASA's models: [NASA 3D Resources](https://nasa3d.arc.nasa.gov/models)
3. Place it at `priv/model.stl`

## Troubleshooting

### Red cube appears (loading failed)

**Check:**
- File exists in the assets folder (`.lustre/static/model.stl`)
- File is a valid STL (ASCII or Binary format)
- Check browser console (F12) for specific error message

**Common Errors:**

1. **"Invalid typed array length"** - Your STL file may be corrupted or have an invalid header
   - Try opening the file in a 3D viewer (like Blender, MeshLab, or online viewers)
   - Export it again from your 3D software
   - Make sure it's saved as STL (not a different format renamed to .stl)

2. **"File not found"** - The file isn't in the right location
   - Make sure it's in `.lustre/static/model.stl`
   - Check the file name exactly matches `model.stl`

3. **File loads but model doesn't appear** - Model might be very small or large
   - Check console for vertex count
   - Adjust camera distance or model scale

### Nothing appears

**Check:**
- Development server is running
- No errors in the terminal
- Browser console (F12) for JavaScript errors

### Model is too small/large

The example loads the model at its original scale. If your model is very large or small, you might need to adjust the camera position or model scale in the code.

To adjust scale, edit `src/stl_loader.gleam` and change:
```gleam
scale: vec3.Vec3(1.0, 1.0, 1.0),
```
to something like:
```gleam
scale: vec3.Vec3(0.1, 0.1, 0.1),  // Make it 10x smaller
```

## File Formats Supported

- **Binary STL** (most common, smaller file size) ✅
- **ASCII STL** (text-based, larger file size) ✅

Both formats are automatically detected and parsed!
