# vec

[![Package Version](https://img.shields.io/hexpm/v/vec)](https://hex.pm/packages/vec)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/vec/)

This is a vectors library for the [Gleam programming language](https://gleam.run).

## Features

- Supported 2D/3D/4D vectors.
- Have StdLib's quality, from designs and naming convention, to documents and tests.
- Functionality comparable to the things you can do with vectors in [Godot](https://godotengine.org), [Phaser](https://phaser.io) and [Bevy](https://bevy.org).
- All vectors type are generic:
  - This giving user freedom to have any kind of vector (integer vector, float vector, rational vector, ...).
  - And and it's more convenient to handle (e.g: function-mapping).

## Installation

```sh
gleam add vec
```

```gleam
import vec/vec3.{Vec3}
import vec/vec3i

pub fn main() {
  Vec3(12, -34, 420)
  |> vec3i.add(Vec3(21, 45, -20))
  |> echo // -> Vec3(33, 11, 400)
  |> vec3i.scale(2)
  |> echo // -> Vec3(66, 22, 800)
}
```

Further documentation can be found at <https://hexdocs.pm/vec>.

## Credits

Special thanks to:

- [**Godot**](https://godotengine.org)
- [**Phaser**](https://phaser.io)
