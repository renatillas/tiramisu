# Effects Showcase Example

This example demonstrates all the Time & Animation and System & Browser effects available in Tiramisu.

## Features Demonstrated

### Time & Animation Effects

- **Delay**: A welcome message appears 2 seconds after launch
- **Timeout**: Press `T` to trigger a 3-second timeout that changes the cube color
- **Interval**: Press `Space` to start a repeating 1-second interval that pulses the cube color
- **Cancel Interval**: Press `S` to stop the interval
- **Tween**: Press number keys `1-7` to animate the cube with different easing functions:
  - `1`: Linear
  - `2`: EaseInQuad
  - `3`: EaseOutQuad
  - `4`: EaseInOutQuad
  - `5`: EaseInCubic
  - `6`: EaseOutCubic
  - `7`: EaseInOutCubic

### System & Browser Effects

- **Fullscreen**: Press `F` to toggle fullscreen mode
- **Pointer Lock**: Press `L` to toggle pointer lock (useful for FPS games)
- **Vibration**: Touch the screen to trigger device vibration (mobile only)
- **Clipboard Write**: Press `C` to copy text to clipboard
- **Clipboard Read**: Press `P` to paste text from clipboard

## Running the Example

```bash
cd examples/24-effects_showcase
gleam run -t javascript
```

Then open your browser to the URL shown (typically http://localhost:1234).

## Controls

- **Space**: Start interval (cube color pulses)
- **S**: Stop interval
- **1-7**: Start tween animations with different easings
- **T**: Trigger 3-second timeout
- **F**: Toggle fullscreen
- **L**: Toggle pointer lock
- **Touch Screen**: Trigger vibration (mobile only)
- **C**: Copy to clipboard
- **P**: Paste from clipboard
- **ESC**: Exit pointer lock (browser default)

## Status Messages

Watch the console output and the cube's behavior to see the effects in action. Status messages appear in the browser console showing:
- When effects are triggered
- Interval tick counts
- Easing functions being used
- Success/failure of browser API requests

## Notes

- Some effects may require user interaction to work (browser security restrictions)
- Fullscreen API requires user gesture (key press counts)
- Pointer lock requires user gesture
- Vibration API only works on mobile devices with vibration support
- Clipboard API requires secure context (HTTPS or localhost)
- Gamepad vibration requires a connected gamepad with haptic support
