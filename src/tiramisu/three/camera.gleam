import gleam/bool

pub type Camera

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

  Ok(create_perspective_camera(fov, aspect, near, far))
}

@external(javascript, "./ffi/camera.mjs", "createPerspectiveCamera")
fn create_perspective_camera(
  fov: Float,
  aspect: Float,
  near: Float,
  far: Float,
) -> Camera

@external(javascript, "./ffi/camera.mjs", "createOrthographicCamera")
pub fn orthographic(
  left left: Float,
  right right: Float,
  top top: Float,
  bottom bottom: Float,
  near near: Float,
  far far: Float,
) -> Camera

@external(javascript, "./ffi/camera.mjs", "setCameraPosition")
pub fn set_position(
  camera: Camera,
  x x: Float,
  y y: Float,
  z z: Float,
) -> Camera

@external(javascript, "./ffi/camera.mjs", "lookAt")
pub fn look_at(camera: Camera, x x: Float, y y: Float, z z: Float) -> Camera

@external(javascript, "./ffi/camera.mjs", "updateProjectionMatrix")
pub fn update_projection_matrix(camera: Camera) -> Camera
