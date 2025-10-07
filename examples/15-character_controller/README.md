# Character Controller Example

This example demonstrates the **Animation State Machine** system with smooth transitions between character animations.

## Features

- 🎮 **State Machine**: Manages animation transitions
- 🔄 **Smooth Blending**: Transitions smoothly between idle, walk, and run
- ⌨️ **Keyboard Controls**:
  - `W` key: Walk forward
  - `Shift` + `W`: Run
  - Release keys: Return to idle
- 🎨 **Declarative API**: Pure, immutable state machine configuration

## Setup

1. **Download a character model** with multiple animations:
   - [RobotExpressive.glb](https://github.com/KhronosGroup/glTF-Sample-Models/tree/master/2.0/RobotExpressive/glTF-Binary) (recommended)
   - [Fox.glb](https://github.com/KhronosGroup/glTF-Sample-Models/tree/master/2.0/Fox/glTF-Binary)
   - Any character from [Sketchfab](https://sketchfab.com) with idle/walk/run animations

2. **Place the model** in `priv/static/` and rename it to `character.glb`

3. **Run the example**:
   ```bash
   gleam run -m lustre/dev start
   ```

4. **Open your browser** to `http://localhost:1234`

## Controls

- Press `W` to make the character walk
- Hold `Shift` while pressing `W` to make the character run
- Release keys to return to idle animation

Watch the smooth blending between animation states!

## How It Works

### State Machine Definition

```gleam
state_machine.new("idle")
|> state_machine.add_state("idle", idle_animation, looping: True)
|> state_machine.add_state("walk", walk_animation, looping: True)
|> state_machine.add_transition(
  from: "idle",
  to: "walk",
  condition: Custom(fn() { input.is_key_pressed(ctx.input, KeyW) }),
  blend_duration: 0.3,
)
```

### Animation Output

The state machine returns one of:
- `Single(animation)` - Playing a single animation
- `Blend(from, to, factor)` - Blending between two animations (during transition)
- `None` - No animation

### Integration with Model3D

```gleam
scene.Model3D(
  id: "character",
  object: gltf.scene(data),
  animation: case state_machine.get_current_animation(machine) {
    Single(anim) -> Some(SingleAnimation(anim))
    Blend(from, to, factor) -> Some(BlendedAnimations(from, to, factor))
    None -> None
  },
  ...
)
```

## Architecture

This example demonstrates:

1. **Immutable State Machine**: The state machine is a pure value that gets updated each frame
2. **Declarative Transitions**: Conditions are defined declaratively
3. **Automatic Blending**: The renderer handles animation blending automatically
4. **MVU Pattern**: State machine updates happen in the `update` function

## Animation Requirements

The model should have at least 2-3 animations:
- **Idle**: Looping idle animation
- **Walk**: Looping walk cycle
- **Run**: (Optional) Looping run cycle

The example will automatically adapt based on available animations.

## Extending the Example

You can extend this to add:
- Jump animations (triggered by spacebar)
- Attack animations (triggered by mouse click)
- Direction-based animations (strafe left/right)
- Animation layers (upper/lower body separation)

## Troubleshooting

**Animations not blending smoothly?**
- Adjust `blend_duration` in transitions (0.2-0.5 seconds is usually good)

**Wrong animation playing?**
- Check animation order in the GLTF file
- Add console logging to see animation names
- Adjust state machine transitions

**Model not visible?**
- Check camera position and model scale
- Some models need to be scaled down (scale: 0.01 or 0.1)
- Check console for loading errors
