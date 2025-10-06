# Sprites Demo

This example demonstrates loading sprite textures from the internet and displaying them in a 3D scene.

## Features

- **Async Texture Loading**: Load textures from remote URLs using promises
- **Sprite Display**: Show 2D images (sprites) in a 3D world
- **Dynamic Animation**: Animate sprites with various motion patterns
- **Loading States**: Handle loading state with proper error handling

## What It Shows

The demo loads emoji images from the internet and displays them as animated sprites:

1. **Circular Motion**: Two sprites rotating in opposite circles
2. **Center Spinner**: A sprite rotating on its own axis in the center
3. **Bouncing Motion**: A sprite bouncing up and down

## Running the Example

```bash
cd examples/sprites_demo
gleam run -m lustre/dev start
```

Then open your browser to the URL shown (typically http://localhost:1234).

## Key Concepts

### Loading Textures

```gleam
import tiramisu/texture

let load_effect =
  effect.from_promise(
    promise.map(texture.load(url), fn(result) {
      case result {
        Ok(tex) -> TextureLoaded(name, tex)
        Error(_) -> LoadingFailed
      }
    })
  )
```

### Creating Sprites

```gleam
import tiramisu/graphics/sprite

sprite.mesh(
  id: "my_sprite",
  texture: loaded_texture,
  width: 2.0,
  height: 2.0
)
```

### Animating Sprites

Sprites can be animated by updating their transforms each frame based on the rotation value that increases over time.

## Image Sources

The example uses emoji images from [Twemoji](https://twemoji.twitter.com/) via their CDN. You can replace these with any PNG/JPG image URLs.
