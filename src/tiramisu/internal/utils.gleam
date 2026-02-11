pub fn set_changed(value: String, f: fn(Bool) -> a) -> Result(a, Nil) {
  case value {
    "" | "true" -> Ok(f(True))
    "false" -> Ok(f(False))
    _ -> Error(Nil)
  }
}
