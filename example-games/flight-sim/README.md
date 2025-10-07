# Physics Helicopter

A physics-based helicopter game where you must tilt and thrust to stay airborne! 🚁

## Running the Game

```bash
gleam run -m lustre/dev start
```

Then open your browser to the URL shown (typically http://localhost:1234)

## Controls

- **W/S**: Tilt forward/backward
- **A/D**: Tilt left/right
- **Space**: Apply upward thrust (in helicopter's current up direction)
- **R**: Reset to starting position

## Features

- **Physics-based flight**: Uses Rapier physics engine with gravity
- **Tilt mechanics**: Tilting the helicopter changes the direction of thrust
- **Dynamic controls**: The up force follows the helicopter's orientation
- **Third-person camera**: Follows helicopter from behind
- **Simple helicopter model**: Body, main rotor, tail, and tail rotor

## How It Works

- The helicopter is a dynamic physics rigid body affected by gravity
- Tilt inputs (W/A/S/D) rotate the helicopter's orientation vectors (forward/up/right)
- Pressing **Space** applies force in the helicopter's current "up" direction
- As you tilt, the up vector changes, so thrust pushes you in different directions
- This creates challenging physics where you must balance tilt and thrust to stay airborne
- Linear and angular damping simulate air resistance

## Physics Integration

This example demonstrates Tiramisu's physics integration:
- `physics.new_world()` creates a physics world with gravity enabled
- `physics.rigid_body()` creates physics bodies for meshes
- `physics.apply_force()` applies thrust forces in the helicopter's up direction
- `physics.step()` updates the physics simulation each frame
- `physics.get_transform()` retrieves updated positions/rotations

Try to keep the helicopter in the air! It's harder than it looks!
