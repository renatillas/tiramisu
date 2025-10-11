import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $snag from "../../snag/snag.mjs";
import { Ok } from "../gleam.mjs";

/**
 * Returns a Constraint that ensures the parsed flag value is one of the allowed values.
 *
 * ```gleam
 * import glint
 * import glint/constraint
 * ...
 * glint.int_flag("my_flag")
 * |> glint.constraint(constraint.one_of([1, 2, 3, 4]))
 * ```
 */
export function one_of(allowed) {
  let allowed_set = $set.from_list(allowed);
  return (val) => {
    let $ = $set.contains(allowed_set, val);
    if ($) {
      return new Ok(val);
    } else {
      return $snag.error(
        ((("invalid value '" + $string.inspect(val)) + "', must be one of: [") + (() => {
          let _pipe = allowed;
          let _pipe$1 = $list.map(
            _pipe,
            (a) => { return ("'" + $string.inspect(a)) + "'"; },
          );
          return $string.join(_pipe$1, ", ");
        })()) + "]",
      );
    }
  };
}

/**
 * Returns a Constraint that ensures the parsed flag value is not one of the disallowed values.
 *
 * ```gleam
 * import glint
 * import glint/constraint
 * ...
 * glint.int_flag("my_flag")
 * |> glint.constraint(constraint.none_of([1, 2, 3, 4]))
 * ```
 */
export function none_of(disallowed) {
  let disallowed_set = $set.from_list(disallowed);
  return (val) => {
    let $ = $set.contains(disallowed_set, val);
    if ($) {
      return $snag.error(
        (("invalid value '" + $string.inspect(val)) + "', must not be one of: [") + ((() => {
          let _pipe = disallowed;
          let _pipe$1 = $list.map(
            _pipe,
            (a) => { return ("'" + $string.inspect(a)) + "'"; },
          );
          return $string.join(_pipe$1, ", ");
        })() + "]"),
      );
    } else {
      return new Ok(val);
    }
  };
}

/**
 * This is a convenience function for applying a Constraint(a) to a List(a).
 * This is useful because the default behaviour for constraints on lists is that they will apply to the list as a whole.
 *
 * For example, to apply one_of to all items in a `List(Int)`:
 *
 * Via `use`:
 * ```gleam
 * import glint
 * import glint/constraint
 * ...
 * use li <- glint.flag_constraint(glint.int_flag("my_flag"))
 * use i <- constraint.each()
 * i |> one_of([1, 2, 3, 4])
 * ```
 *
 * via a pipe:
 * ```gleam
 * import glint
 * import glint/constraint
 * ...
 * glint.int_flag("my_flag")
 * |> glint.flag_constraint(
 *   constraint.one_of([1,2,3,4])
 *   |> constraint.each
 * )
 * ```
 */
export function each(constraint) {
  return (_capture) => { return $list.try_map(_capture, constraint); };
}
