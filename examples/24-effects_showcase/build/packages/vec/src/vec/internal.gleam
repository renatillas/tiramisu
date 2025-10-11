/// The mathematical constant Pi.
///
pub const pi = 3.1415926535897932

/// The Sine function.
///
pub fn sin(x: Float) -> Float {
  cos(pi /. 2.0 -. x)
}

/// The Cosine function.
///
pub fn cos(x: Float) -> Float {
  do_cos(x)
}

@external(erlang, "math", "cos")
@external(javascript, "../maths.mjs", "cos")
fn do_cos(a: Float) -> Float

/// The inverse Cosine function.
///
pub fn acos(x: Float) -> Result(Float, Nil) {
  case x >=. -1.0 && x <=. 1.0 {
    True -> Ok(do_acos(x))
    False -> Error(Nil)
  }
}

@external(erlang, "math", "acos")
@external(javascript, "../maths.mjs", "acos")
fn do_acos(a: Float) -> Float
