import gleam/list
import gleam/set
import gleam/string
import snag

/// Constraint type for verifying flag values
///
pub type Constraint(a) =
  fn(a) -> snag.Result(a)

/// Returns a Constraint that ensures the parsed flag value is one of the allowed values.
///
/// ```gleam
/// import glint
/// import glint/constraint
/// ...
/// glint.int_flag("my_flag")
/// |> glint.constraint(constraint.one_of([1, 2, 3, 4]))
/// ```
///
pub fn one_of(allowed: List(a)) -> Constraint(a) {
  let allowed_set = set.from_list(allowed)
  fn(val: a) -> snag.Result(a) {
    case set.contains(allowed_set, val) {
      True -> Ok(val)
      False ->
        snag.error(
          "invalid value '"
          <> string.inspect(val)
          <> "', must be one of: ["
          <> {
            allowed
            |> list.map(fn(a) { "'" <> string.inspect(a) <> "'" })
            |> string.join(", ")
          }
          <> "]",
        )
    }
  }
}

/// Returns a Constraint that ensures the parsed flag value is not one of the disallowed values.
///
/// ```gleam
/// import glint
/// import glint/constraint
/// ...
/// glint.int_flag("my_flag")
/// |> glint.constraint(constraint.none_of([1, 2, 3, 4]))
/// ```
///
pub fn none_of(disallowed: List(a)) -> Constraint(a) {
  let disallowed_set = set.from_list(disallowed)
  fn(val: a) -> snag.Result(a) {
    case set.contains(disallowed_set, val) {
      False -> Ok(val)
      True ->
        snag.error(
          "invalid value '"
          <> string.inspect(val)
          <> "', must not be one of: ["
          <> {
            {
              disallowed
              |> list.map(fn(a) { "'" <> string.inspect(a) <> "'" })
              |> string.join(", ")
              <> "]"
            }
          },
        )
    }
  }
}

/// This is a convenience function for applying a Constraint(a) to a List(a).
/// This is useful because the default behaviour for constraints on lists is that they will apply to the list as a whole.
///
/// For example, to apply one_of to all items in a `List(Int)`:
///
/// Via `use`:
/// ```gleam
/// import glint
/// import glint/constraint
/// ...
/// use li <- glint.flag_constraint(glint.int_flag("my_flag"))
/// use i <- constraint.each()
/// i |> one_of([1, 2, 3, 4])
/// ```
///
/// via a pipe:
/// ```gleam
/// import glint
/// import glint/constraint
/// ...
/// glint.int_flag("my_flag")
/// |> glint.flag_constraint(
///   constraint.one_of([1,2,3,4])
///   |> constraint.each
/// )
/// ```
///
pub fn each(constraint: Constraint(a)) -> Constraint(List(a)) {
  list.try_map(_, constraint)
}
