import gleam/bool
import gleam/int
import tiramisu/vec3

pub opaque type Camera {
  Camera(
    position: vec3.Vec3,
    look_at_target: vec3.Vec3,
    projection: CameraProjection,
  )
}

type CameraProjection {
  Perspective(fov: Float, aspect: Float, near: Float, far: Float)
  Orthographic(
    left: Float,
    right: Float,
    top: Float,
    bottom: Float,
    near: Float,
    far: Float,
  )
}

pub type CameraError {
  InvalidFieldOfView(Float)
  InvalidAspectRatio(Float)
  InvalidNearPlane(Float)
  InvalidFarPlane(Float)
  NearFarConflict(near: Float, far: Float)
}

pub fn perspective(
  field_of_view fov: Float,
  aspect aspect: Float,
  near near: Float,
  far far: Float,
) -> Result(Camera, CameraError) {
  use <- bool.guard(
    fov <=. 0.0 || fov >=. 180.0,
    Error(InvalidFieldOfView(fov)),
  )
  use <- bool.guard(aspect <=. 0.0, Error(InvalidAspectRatio(aspect)))
  use <- bool.guard(near <=. 0.0, Error(InvalidNearPlane(near)))
  use <- bool.guard(far <=. 0.0, Error(InvalidFarPlane(far)))
  use <- bool.guard(near >=. far, Error(NearFarConflict(near, far)))

  Ok(Camera(
    position: vec3.zero(),
    look_at_target: vec3.zero(),
    projection: Perspective(fov: fov, aspect: aspect, near: near, far: far),
  ))
}

pub fn orthographic(
  left left: Float,
  right right: Float,
  top top: Float,
  bottom bottom: Float,
  near near: Float,
  far far: Float,
) -> Camera {
  Camera(
    position: vec3.zero(),
    look_at_target: vec3.zero(),
    projection: Orthographic(
      left: left,
      right: right,
      top: top,
      bottom: bottom,
      near: near,
      far: far,
    ),
  )
}

pub fn camera_2d(width: Int, height: Int, distance: Float) -> Camera {
  let w = int.to_float(width)
  let h = int.to_float(height)
  let half_w = w /. 2.0
  let half_h = h /. 2.0

  orthographic(
    left: 0.0 -. half_w,
    right: half_w,
    top: half_h,
    bottom: 0.0 -. half_h,
    near: 0.1,
    far: 1000.0,
  )
  |> set_position(vec3.Vec3(0.0, 0.0, distance))
}

pub fn camera_2d_screen_space(
  width: Int,
  height: Int,
  distance: Float,
) -> Camera {
  let w = int.to_float(width)
  let h = int.to_float(height)

  orthographic(
    left: 0.0,
    right: w,
    top: 0.0,
    bottom: 0.0 -. h,
    near: 0.1,
    far: 1000.0,
  )
  |> set_position(vec3.Vec3(w /. 2.0, 0.0 -. h /. 2.0, distance))
}

pub fn camera_2d_with_bounds(
  left: Float,
  right: Float,
  top: Float,
  bottom: Float,
  distance: Float,
) -> Camera {
  orthographic(
    left: left,
    right: right,
    top: top,
    bottom: bottom,
    near: 0.1,
    far: 1000.0,
  )
  |> set_position(vec3.Vec3(0.0, 0.0, distance))
}

pub fn set_position(camera: Camera, position position: vec3.Vec3) -> Camera {
  Camera(..camera, position:)
}

pub fn look(camera: Camera, at look_at_target: vec3.Vec3) -> Camera {
  Camera(..camera, look_at_target:)
}
