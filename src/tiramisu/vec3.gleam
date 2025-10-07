import gleam/float
import gleam_community/maths

pub type Vec3 {
  Vec3(x: Float, y: Float, z: Float)
}

pub fn zero() -> Vec3 {
  Vec3(0.0, 0.0, 0.0)
}

pub fn one() -> Vec3 {
  Vec3(1.0, 1.0, 1.0)
}

pub fn magnitude(v: Vec3) -> Float {
  let assert Ok(result) =
    float.square_root(v.x *. v.x +. v.y *. v.y +. v.z *. v.z)
  result
}

pub fn scale(v: Vec3, scale: Float) -> Vec3 {
  Vec3(v.x *. scale, v.y *. scale, v.z *. scale)
}

pub fn add(a: Vec3, b: Vec3) -> Vec3 {
  Vec3(a.x +. b.x, a.y +. b.y, a.z +. b.z)
}

pub fn subtract(a: Vec3, b: Vec3) -> Vec3 {
  Vec3(a.x -. b.x, a.y -. b.y, a.z -. b.z)
}

pub fn normalize(v: Vec3) -> Vec3 {
  let mag = magnitude(v)
  case mag {
    0.0 -> Vec3(0.0, 0.0, 0.0)
    _ -> scale(v, 1.0 /. mag)
  }
}

pub fn dot(a: Vec3, b: Vec3) -> Float {
  a.x *. b.x +. a.y *. b.y +. a.z *. b.z
}

pub fn cross(a: Vec3, b: Vec3) -> Vec3 {
  Vec3(
    a.y *. b.z -. a.z *. b.y,
    a.z *. b.x -. a.x *. b.z,
    a.x *. b.y -. a.y *. b.x,
  )
}

pub fn distance(a: Vec3, b: Vec3) -> Float {
  magnitude(subtract(b, a))
}

pub fn lerp(a: Vec3, b: Vec3, t: Float) -> Vec3 {
  Vec3(
    a.x +. { b.x -. a.x } *. t,
    a.y +. { b.y -. a.y } *. t,
    a.z +. { b.z -. a.z } *. t,
  )
}

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
