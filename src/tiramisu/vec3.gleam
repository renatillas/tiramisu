import gleam/float
import gleam_community/maths

/// Math utilities and vector types for Tiramisu
/// 3D Vector
pub type Vec3 {
  Vec3(x: Float, y: Float, z: Float)
}

/// Create a zero vector
pub fn zero() -> Vec3 {
  Vec3(0.0, 0.0, 0.0)
}

/// Create a vector with all components set to 1
pub fn one() -> Vec3 {
  Vec3(1.0, 1.0, 1.0)
}

/// Vector magnitude (length)
pub fn magnitude(v: Vec3) -> Float {
  let assert Ok(result) =
    float.square_root(v.x *. v.x +. v.y *. v.y +. v.z *. v.z)
  result
}

/// Scale a vector by a scalar
pub fn scale(v: Vec3, scale: Float) -> Vec3 {
  Vec3(v.x *. scale, v.y *. scale, v.z *. scale)
}

/// Add two vectors
pub fn add(a: Vec3, b: Vec3) -> Vec3 {
  Vec3(a.x +. b.x, a.y +. b.y, a.z +. b.z)
}

/// Subtract two vectors
pub fn subtract(a: Vec3, b: Vec3) -> Vec3 {
  Vec3(a.x -. b.x, a.y -. b.y, a.z -. b.z)
}

/// Normalize a vector (make it unit length)
pub fn normalize(v: Vec3) -> Vec3 {
  let mag = magnitude(v)
  case mag {
    0.0 -> Vec3(0.0, 0.0, 0.0)
    _ -> scale(v, 1.0 /. mag)
  }
}

/// Dot product of two vectors
pub fn dot(a: Vec3, b: Vec3) -> Float {
  a.x *. b.x +. a.y *. b.y +. a.z *. b.z
}

/// Cross product of two vectors
pub fn cross(a: Vec3, b: Vec3) -> Vec3 {
  Vec3(
    a.y *. b.z -. a.z *. b.y,
    a.z *. b.x -. a.x *. b.z,
    a.x *. b.y -. a.y *. b.x,
  )
}

/// Distance between two vectors
pub fn distance(a: Vec3, b: Vec3) -> Float {
  magnitude(subtract(b, a))
}

/// Linear interpolation between two vectors
pub fn lerp(a: Vec3, b: Vec3, t: Float) -> Vec3 {
  Vec3(
    a.x +. { b.x -. a.x } *. t,
    a.y +. { b.y -. a.y } *. t,
    a.z +. { b.z -. a.z } *. t,
  )
}

/// Angle between two vectors in radians
/// Returns Ok(angle) or Error if vectors are zero length
pub fn angle(a: Vec3, b: Vec3) -> Result(Float, Nil) {
  let mag_a = magnitude(a)
  let mag_b = magnitude(b)

  case mag_a == 0.0 || mag_b == 0.0 {
    True -> Error(Nil)
    False -> {
      let dot_product = dot(a, b)
      let cos_angle = dot_product /. { mag_a *. mag_b }
      // Clamp to [-1, 1] to handle floating point errors
      let clamped = float.clamp(cos_angle, -1.0, 1.0)
      maths.acos(clamped)
    }
  }
}
