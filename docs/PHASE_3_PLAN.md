# Phase 3: Interactivity - Implementation Plan

## Goals
Add animation, advanced input handling, and camera controllers to enable interactive game experiences.

## Task Breakdown

### 3.1 Animation System (Priority 1)
**Goal**: Support smooth animations and transitions

#### Simple Tween System
- [ ] Add `Animation` type with easing functions
- [ ] `linear_tween()`, `ease_in_tween()`, `ease_out_tween()`, `ease_in_out_tween()`
- [ ] `tween_float()` - animate single values
- [ ] `tween_vec3()` - animate vectors
- [ ] `tween_transform()` - animate entire transforms
- [ ] Tests for easing functions

#### Animation State Management
- [ ] `AnimationState` type to track running animations
- [ ] `start_animation()` - begin new animation
- [ ] `update_animations()` - progress all active animations
- [ ] `stop_animation()` - cancel animation by ID
- [ ] Support for animation callbacks (on_complete)

#### Keyframe System (Optional)
- [ ] `Keyframe` type for multi-step animations
- [ ] `keyframe_animation()` - create from keyframes
- [ ] Automatic interpolation between keyframes

---

### 3.2 Enhanced Input (Priority 2)
**Goal**: Support more input methods beyond basic keyboard/mouse

#### Gamepad Support
- [ ] Add `Gamepad` FFI bindings
- [ ] `GamepadState` type with button/axis data
- [ ] `poll_gamepads()` - get connected gamepad state
- [ ] Helper functions for common mappings (Xbox, PlayStation)

#### Touch/Gesture Support
- [ ] `TouchEvent` type for multi-touch
- [ ] `get_touches()` - current touch points
- [ ] Gesture detection (pinch, swipe, tap)
- [ ] Touch-to-world raycasting for 3D picking

#### Input Utilities
- [ ] `is_key_pressed()`, `is_key_just_pressed()`, `is_key_released()`
- [ ] Input buffering for frame-perfect inputs
- [ ] Rebindable key mappings

---

### 3.3 Camera Controllers (Priority 3)
**Goal**: Pre-built camera behaviors for common game types

#### Orbital Camera (3D scene exploration)
- [ ] `OrbitalCameraState` type
- [ ] Mouse drag to rotate around target
- [ ] Mouse wheel to zoom in/out
- [ ] Auto-rotate option
- [ ] Min/max distance clamping
- [ ] Smooth damping

#### First-Person Camera (FPS games)
- [ ] `FPSCameraState` type
- [ ] Mouse look (pitch/yaw)
- [ ] WASD movement
- [ ] Collision detection (optional)
- [ ] Head bob effect (optional)

#### 2D Camera (Top-down, platformers)
- [ ] `Camera2DState` type
- [ ] Pan with arrow keys or mouse drag
- [ ] Zoom with mouse wheel
- [ ] Follow target with smoothing
- [ ] Screen shake effect

#### Camera Utilities
- [ ] `screen_to_world_ray()` - raycasting from screen coords
- [ ] `world_to_screen()` - project 3D point to 2D
- [ ] Camera transition/blend between controllers

---

## Implementation Order

1. **Animation System** (most foundational)
   - Start with simple tween system
   - Add animation state management
   - Defer keyframes if time-consuming

2. **Camera Controllers** (high user value)
   - Start with OrbitalCamera (most common)
   - Add FPSCamera and Camera2D
   - Add screen/world utilities

3. **Enhanced Input** (nice-to-have)
   - Add gamepad support
   - Add touch/gesture support
   - Defer input buffering/rebinding if complex

---

## Success Criteria

- [ ] Users can smoothly animate transforms using tweens
- [ ] At least 2 camera controllers work out of the box
- [ ] Gamepad input works for at least one controller type
- [ ] All new features have test coverage
- [ ] Examples demonstrate each new feature

---

## Deferred to Phase 4+
- Advanced physics-based camera collision
- Animation blending/state machines
- Input recording/playback
- VR/AR camera modes
