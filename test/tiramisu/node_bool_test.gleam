import gleam/dict
import tiramisu/dev/extension

pub fn get_bool_test() {
  let attrs =
    dict.new()
    |> dict.insert("hidden", "")
    |> dict.insert("cast-shadow", "true")
    |> dict.insert("receive-shadow", "false")

  assert extension.get_bool(attrs, "hidden")
  assert extension.get_bool(attrs, "cast-shadow")
  assert extension.get_bool(attrs, "receive-shadow")
  assert !extension.get_bool(attrs, "missing")
}

pub fn get_bool_respects_html_boolean_semantics_test() {
  let attrs =
    dict.new()
    |> dict.insert("active", "false")
    |> dict.insert("transparent", "0")
    |> dict.insert("wireframe", "definitely-not-bool")

  assert extension.get_bool(attrs, "active")
  assert extension.get_bool(attrs, "transparent")
  assert extension.get_bool(attrs, "wireframe")
  assert !extension.get_bool(dict.new(), "active")
}

pub fn bool_change_test() {
  let changes =
    dict.new()
    |> dict.insert("active", extension.Added(""))
    |> dict.insert("hidden", extension.Removed)
    |> dict.insert("wireframe", extension.Updated("false"))

  let assert Ok(True) = extension.bool_change(changes, "active")
  let assert Ok(False) = extension.bool_change(changes, "hidden")
  let assert Ok(True) = extension.bool_change(changes, "wireframe")
  let assert Error(Nil) = extension.bool_change(changes, "missing")
}

pub fn attribute_change_helpers_test() {
  let changes =
    dict.new()
    |> dict.insert("hidden", extension.Removed)
    |> dict.insert("active", extension.Added(""))
    |> dict.insert("wireframe", extension.Updated("false"))

  assert extension.has_change(changes, "hidden")
  assert extension.has_change(changes, "active")
  assert !extension.has_change(changes, "missing")

  assert extension.was_removed(changes, "hidden")
  assert !extension.was_removed(changes, "active")

  assert extension.has_any_change(changes, ["missing", "active"])
  assert !extension.has_any_change(changes, ["missing", "absent"])

  let assert Ok(extension.Removed) = extension.change(changes, "hidden")
  let assert Ok(extension.Added("")) = extension.change(changes, "active")
  let assert Ok(extension.Updated("false")) =
    extension.change(changes, "wireframe")
  let assert Error(Nil) = extension.change(changes, "missing")
}
