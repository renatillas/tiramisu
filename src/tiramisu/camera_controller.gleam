// import gleam/float
// import gleam_community/maths
// import tiramisu/input/keyboard
// import tiramisu/input/mouse
// import tiramisu/math/vec3
// import tiramisu/scene
//
// /// Orbital camera controller state
// pub type OrbitalController {
//   OrbitalController(
//     target: vec3.Vec3,
//     distance: Float,
//     // horizontal angle (radians)
//     azimuth_angle: Float,
//     // vertical angle (radians)
//     polar_angle: Float,
//     min_distance: Float,
//     max_distance: Float,
//     min_polar: Float,
//     max_polar: Float,
//     rotate_speed: Float,
//     zoom_speed: Float,
//   )
// }
//
// /// First-person camera controller state
// pub type FPSController {
//   FPSController(
//     position: vec3.Vec3,
//     yaw: Float,
//     pitch: Float,
//     move_speed: Float,
//     look_speed: Float,
//     min_pitch: Float,
//     max_pitch: Float,
//   )
// }
//
// /// 2D camera controller state (for top-down/side-scrolling games)
// pub type Camera2DController {
//   Camera2DController(
//     position: vec3.Vec3,
//     zoom: Float,
//     min_zoom: Float,
//     max_zoom: Float,
//     pan_speed: Float,
//     zoom_speed: Float,
//   )
// }
//
// /// Create a default orbital camera controller
// pub fn orbital(target: vec3.Vec3, distance: Float) -> OrbitalController {
//   OrbitalController(
//     target: target,
//     distance: distance,
//     azimuth_angle: 0.0,
//     polar_angle: 1.0,
//     min_distance: 1.0,
//     max_distance: 100.0,
//     min_polar: 0.1,
//     max_polar: 3.04,
//     // ~174 degrees
//     rotate_speed: 0.005,
//     zoom_speed: 1.0,
//   )
// }
//
// /// Update orbital controller with mouse input
// pub fn update_orbital(controller: OrbitalController) -> OrbitalController {
//   // Rotate with right mouse button
//   let azimuth_angle = case mouse.is_pressed(mouse.RightButton) {
//     True ->
//       controller.azimuth_angle +. mouse.get_delta_x() *. controller.rotate_speed
//     False -> controller.azimuth_angle
//   }
//
//   let polar_angle = case mouse.is_pressed(mouse.RightButton) {
//     True -> {
//       let new_polar =
//         controller.polar_angle +. mouse.get_delta_y() *. controller.rotate_speed
//       float.clamp(new_polar, controller.min_polar, controller.max_polar)
//     }
//     False -> controller.polar_angle
//   }
//
//   // Zoom with mouse wheel
//   let distance = {
//     let wheel = mouse.get_wheel_delta()
//     let new_distance = controller.distance -. wheel *. controller.zoom_speed
//     float.clamp(new_distance, controller.min_distance, controller.max_distance)
//   }
//
//   OrbitalController(..controller, azimuth_angle:, polar_angle:, distance:)
// }
//
// pub fn orbital_transform(controller: OrbitalController) -> scene.Transform {
//   let sin_polar = maths.sin(controller.polar_angle)
//   let cos_polar = maths.cos(controller.polar_angle)
//   let sin_azimuth = maths.sin(controller.azimuth_angle)
//   let cos_azimuth = maths.cos(controller.azimuth_angle)
//
//   let x = controller.target.x +. controller.distance *. sin_polar *. cos_azimuth
//   let y = controller.target.y +. controller.distance *. cos_polar
//   let z = controller.target.z +. controller.distance *. sin_polar *. sin_azimuth
//
//   scene.look_at_transform(
//     vec3.Vec3(x, y, z),
//     controller.target,
//     vec3.Vec3(0.0, 1.0, 0.0),
//   )
// }
//
// pub fn fps(position: vec3.Vec3) -> FPSController {
//   FPSController(
//     position: position,
//     yaw: 0.0,
//     pitch: 0.0,
//     move_speed: 5.0,
//     look_speed: 0.002,
//     min_pitch: -1.5,
//     max_pitch: 1.5,
//   )
// }
//
// pub fn update_fps(controller: FPSController, delta: Float) -> FPSController {
//   let yaw = case mouse.is_pressed(mouse.RightButton) {
//     True -> controller.yaw -. mouse.get_delta_x() *. controller.look_speed
//     False -> controller.yaw
//   }
//
//   let pitch = case mouse.is_pressed(mouse.RightButton) {
//     True -> {
//       let new_pitch =
//         controller.pitch -. mouse.get_delta_y() *. controller.look_speed
//       float.clamp(new_pitch, controller.min_pitch, controller.max_pitch)
//     }
//     False -> controller.pitch
//   }
//
//   // Calculate forward and right vectors
//   let cos_yaw = maths.cos(yaw)
//   let sin_yaw = maths.sin(yaw)
//
//   let forward = vec3.Vec3(sin_yaw, 0.0, cos_yaw)
//   let right = vec3.Vec3(cos_yaw, 0.0, 0.0 -. sin_yaw)
//
//   // Movement with WASD
//   let move_forward = case
//     keyboard.is_pressed(keyboard.KeyW),
//     keyboard.is_pressed(keyboard.KeyS)
//   {
//     True, False -> controller.move_speed *. delta
//     False, True -> -1.0 *. controller.move_speed *. delta
//     _, _ -> 0.0
//   }
//
//   let move_right = case
//     keyboard.is_pressed(keyboard.KeyD),
//     keyboard.is_pressed(keyboard.KeyA)
//   {
//     True, False -> controller.move_speed *. delta
//     False, True -> -1.0 *. controller.move_speed *. delta
//     _, _ -> 0.0
//   }
//
//   let position =
//     controller.position
//     |> vec3.add(vec3.scale(forward, move_forward))
//     |> vec3.add(vec3.scale(right, move_right))
//
//   FPSController(..controller, position: position, yaw: yaw, pitch: pitch)
// }
//
// /// Get camera transform from FPS controller
// pub fn fps_transform(controller: FPSController) -> scene.Transform {
//   scene.Transform(
//     position: controller.position,
//     rotation: vec3.Vec3(controller.pitch, controller.yaw, 0.0),
//     scale: vec3.one(),
//   )
// }
//
// /// Create a default 2D camera controller
// pub fn camera_2d(position: vec3.Vec3) -> Camera2DController {
//   Camera2DController(
//     position: position,
//     zoom: 1.0,
//     min_zoom: 0.1,
//     max_zoom: 10.0,
//     pan_speed: 5.0,
//     zoom_speed: 0.1,
//   )
// }
//
// pub fn update_2d(
//   controller: Camera2DController,
//   delta: Float,
// ) -> Camera2DController {
//   let pan_x = case
//     keyboard.is_pressed(keyboard.KeyD)
//     || keyboard.is_pressed(keyboard.ArrowRight),
//     keyboard.is_pressed(keyboard.KeyA)
//     || keyboard.is_pressed(keyboard.ArrowLeft)
//   {
//     True, False -> controller.pan_speed *. delta /. controller.zoom
//     False, True -> -1.0 *. controller.pan_speed *. delta /. controller.zoom
//     _, _ -> 0.0
//   }
//
//   let pan_y = case
//     keyboard.is_pressed(keyboard.KeyW) || keyboard.is_pressed(keyboard.ArrowUp),
//     keyboard.is_pressed(keyboard.KeyS)
//     || keyboard.is_pressed(keyboard.ArrowDown)
//   {
//     True, False -> controller.pan_speed *. delta /. controller.zoom
//     False, True -> -1.0 *. controller.pan_speed *. delta /. controller.zoom
//     _, _ -> 0.0
//   }
//
//   let position =
//     vec3.Vec3(
//       controller.position.x +. pan_x,
//       controller.position.y +. pan_y,
//       controller.position.z,
//     )
//
//   let zoom = {
//     let wheel = mouse.get_wheel_delta()
//     let new_zoom = controller.zoom +. wheel *. controller.zoom_speed
//     float.clamp(new_zoom, controller.min_zoom, controller.max_zoom)
//   }
//
//   Camera2DController(..controller, position: position, zoom: zoom)
// }
//
// pub fn camera_2d_transform(controller: Camera2DController) -> scene.Transform {
//   scene.Transform(
//     position: controller.position,
//     rotation: vec3.Vec3(0.0, 0.0, 0.0),
//     scale: vec3.Vec3(controller.zoom, controller.zoom, 1.0),
//   )
// }
