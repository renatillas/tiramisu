import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";

export class Some extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class None extends $CustomType {}

function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix instanceof $Empty) {
      return suffix;
    } else {
      let first = prefix.head;
      let rest = prefix.tail;
      loop$prefix = rest;
      loop$suffix = listPrepend(first, suffix);
    }
  }
}

function reverse(list) {
  return reverse_and_prepend(list, toList([]));
}

function all_loop(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list instanceof $Empty) {
      return new Some(reverse(acc));
    } else {
      let $ = list.head;
      if ($ instanceof Some) {
        let rest = list.tail;
        let first = $[0];
        loop$list = rest;
        loop$acc = listPrepend(first, acc);
      } else {
        return new None();
      }
    }
  }
}

/**
 * Combines a list of `Option`s into a single `Option`.
 * If all elements in the list are `Some` then returns a `Some` holding the list of values.
 * If any element is `None` then returns`None`.
 *
 * ## Examples
 *
 * ```gleam
 * all([Some(1), Some(2)])
 * // -> Some([1, 2])
 * ```
 *
 * ```gleam
 * all([Some(1), None])
 * // -> None
 * ```
 */
export function all(list) {
  return all_loop(list, toList([]));
}

/**
 * Checks whether the `Option` is a `Some` value.
 *
 * ## Examples
 *
 * ```gleam
 * is_some(Some(1))
 * // -> True
 * ```
 *
 * ```gleam
 * is_some(None)
 * // -> False
 * ```
 */
export function is_some(option) {
  return !isEqual(option, new None());
}

/**
 * Checks whether the `Option` is a `None` value.
 *
 * ## Examples
 *
 * ```gleam
 * is_none(Some(1))
 * // -> False
 * ```
 *
 * ```gleam
 * is_none(None)
 * // -> True
 * ```
 */
export function is_none(option) {
  return isEqual(option, new None());
}

/**
 * Converts an `Option` type to a `Result` type.
 *
 * ## Examples
 *
 * ```gleam
 * to_result(Some(1), "some_error")
 * // -> Ok(1)
 * ```
 *
 * ```gleam
 * to_result(None, "some_error")
 * // -> Error("some_error")
 * ```
 */
export function to_result(option, e) {
  if (option instanceof Some) {
    let a = option[0];
    return new Ok(a);
  } else {
    return new Error(e);
  }
}

/**
 * Converts a `Result` type to an `Option` type.
 *
 * ## Examples
 *
 * ```gleam
 * from_result(Ok(1))
 * // -> Some(1)
 * ```
 *
 * ```gleam
 * from_result(Error("some_error"))
 * // -> None
 * ```
 */
export function from_result(result) {
  if (result instanceof Ok) {
    let a = result[0];
    return new Some(a);
  } else {
    return new None();
  }
}

/**
 * Extracts the value from an `Option`, returning a default value if there is none.
 *
 * ## Examples
 *
 * ```gleam
 * unwrap(Some(1), 0)
 * // -> 1
 * ```
 *
 * ```gleam
 * unwrap(None, 0)
 * // -> 0
 * ```
 */
export function unwrap(option, default$) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return default$;
  }
}

/**
 * Extracts the value from an `Option`, evaluating the default function if the option is `None`.
 *
 * ## Examples
 *
 * ```gleam
 * lazy_unwrap(Some(1), fn() { 0 })
 * // -> 1
 * ```
 *
 * ```gleam
 * lazy_unwrap(None, fn() { 0 })
 * // -> 0
 * ```
 */
export function lazy_unwrap(option, default$) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return default$();
  }
}

/**
 * Updates a value held within the `Some` of an `Option` by calling a given function
 * on it.
 *
 * If the `Option` is a `None` rather than `Some`, the function is not called and the
 * `Option` stays the same.
 *
 * ## Examples
 *
 * ```gleam
 * map(over: Some(1), with: fn(x) { x + 1 })
 * // -> Some(2)
 * ```
 *
 * ```gleam
 * map(over: None, with: fn(x) { x + 1 })
 * // -> None
 * ```
 */
export function map(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return new Some(fun(x));
  } else {
    return option;
  }
}

/**
 * Merges a nested `Option` into a single layer.
 *
 * ## Examples
 *
 * ```gleam
 * flatten(Some(Some(1)))
 * // -> Some(1)
 * ```
 *
 * ```gleam
 * flatten(Some(None))
 * // -> None
 * ```
 *
 * ```gleam
 * flatten(None)
 * // -> None
 * ```
 */
export function flatten(option) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return option;
  }
}

/**
 * Updates a value held within the `Some` of an `Option` by calling a given function
 * on it, where the given function also returns an `Option`. The two options are
 * then merged together into one `Option`.
 *
 * If the `Option` is a `None` rather than `Some` the function is not called and the
 * option stays the same.
 *
 * This function is the equivalent of calling `map` followed by `flatten`, and
 * it is useful for chaining together multiple functions that return `Option`.
 *
 * ## Examples
 *
 * ```gleam
 * then(Some(1), fn(x) { Some(x + 1) })
 * // -> Some(2)
 * ```
 *
 * ```gleam
 * then(Some(1), fn(x) { Some(#("a", x)) })
 * // -> Some(#("a", 1))
 * ```
 *
 * ```gleam
 * then(Some(1), fn(_) { None })
 * // -> None
 * ```
 *
 * ```gleam
 * then(None, fn(x) { Some(x + 1) })
 * // -> None
 * ```
 */
export function then$(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return fun(x);
  } else {
    return option;
  }
}

/**
 * Returns the first value if it is `Some`, otherwise returns the second value.
 *
 * ## Examples
 *
 * ```gleam
 * or(Some(1), Some(2))
 * // -> Some(1)
 * ```
 *
 * ```gleam
 * or(Some(1), None)
 * // -> Some(1)
 * ```
 *
 * ```gleam
 * or(None, Some(2))
 * // -> Some(2)
 * ```
 *
 * ```gleam
 * or(None, None)
 * // -> None
 * ```
 */
export function or(first, second) {
  if (first instanceof Some) {
    return first;
  } else {
    return second;
  }
}

/**
 * Returns the first value if it is `Some`, otherwise evaluates the given function for a fallback value.
 *
 * ## Examples
 *
 * ```gleam
 * lazy_or(Some(1), fn() { Some(2) })
 * // -> Some(1)
 * ```
 *
 * ```gleam
 * lazy_or(Some(1), fn() { None })
 * // -> Some(1)
 * ```
 *
 * ```gleam
 * lazy_or(None, fn() { Some(2) })
 * // -> Some(2)
 * ```
 *
 * ```gleam
 * lazy_or(None, fn() { None })
 * // -> None
 * ```
 */
export function lazy_or(first, second) {
  if (first instanceof Some) {
    return first;
  } else {
    return second();
  }
}

function values_loop(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list instanceof $Empty) {
      return reverse(acc);
    } else {
      let $ = list.head;
      if ($ instanceof Some) {
        let rest = list.tail;
        let first = $[0];
        loop$list = rest;
        loop$acc = listPrepend(first, acc);
      } else {
        let rest = list.tail;
        loop$list = rest;
        loop$acc = acc;
      }
    }
  }
}

/**
 * Given a list of `Option`s,
 * returns only the values inside `Some`.
 *
 * ## Examples
 *
 * ```gleam
 * values([Some(1), None, Some(3)])
 * // -> [1, 3]
 * ```
 */
export function values(options) {
  return values_loop(options, toList([]));
}
