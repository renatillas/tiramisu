import tiramisu/three/mesh

/// 2D position type
pub type Position2D {
  Position2D(x: Float, y: Float)
}

/// Set 2D position (Z is set to 0)
pub fn set_position(sprite: mesh.Mesh, pos: Position2D) -> mesh.Mesh {
  mesh.set_position(sprite, pos.x, pos.y, 0.0)
}

/// Set 2D position from x, y coordinates
pub fn set_position_xy(sprite: mesh.Mesh, x: Float, y: Float) -> mesh.Mesh {
  mesh.set_position(sprite, x, y, 0.0)
}

/// Set 2D rotation (rotates around Z axis)
pub fn set_rotation(sprite: mesh.Mesh, angle: Float) -> mesh.Mesh {
  mesh.set_rotation(sprite, 0.0, 0.0, angle)
}

/// Rotate incrementally around Z axis
pub fn rotate(sprite: mesh.Mesh, angle: Float) -> mesh.Mesh {
  mesh.rotate_z(sprite, angle)
}

/// Set uniform 2D scale
pub fn set_scale_uniform(sprite: mesh.Mesh, scale: Float) -> mesh.Mesh {
  mesh.set_scale(sprite, scale, scale, 1.0)
}

/// Set 2D scale with separate X and Y
pub fn set_scale_xy(sprite: mesh.Mesh, x: Float, y: Float) -> mesh.Mesh {
  mesh.set_scale(sprite, x, y, 1.0)
}

/// Flip sprite horizontally
pub fn flip_x(sprite: mesh.Mesh) -> mesh.Mesh {
  mesh.set_scale(sprite, -1.0, 1.0, 1.0)
}

/// Flip sprite vertically
pub fn flip_y(sprite: mesh.Mesh) -> mesh.Mesh {
  mesh.set_scale(sprite, 1.0, -1.0, 1.0)
}
