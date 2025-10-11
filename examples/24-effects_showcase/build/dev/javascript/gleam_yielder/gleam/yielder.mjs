import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";

class Stop extends $CustomType {}

class Continue extends $CustomType {
  constructor($0, $1) {
    super();
    this[0] = $0;
    this[1] = $1;
  }
}

class Yielder extends $CustomType {
  constructor(continuation) {
    super();
    this.continuation = continuation;
  }
}

export class Next extends $CustomType {
  constructor(element, accumulator) {
    super();
    this.element = element;
    this.accumulator = accumulator;
  }
}

export class Done extends $CustomType {}

class AnotherBy extends $CustomType {
  constructor($0, $1, $2, $3) {
    super();
    this[0] = $0;
    this[1] = $1;
    this[2] = $2;
    this[3] = $3;
  }
}

class LastBy extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Another extends $CustomType {
  constructor($0, $1) {
    super();
    this[0] = $0;
    this[1] = $1;
  }
}

class Last extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class NoMore extends $CustomType {}

function stop() {
  return new Stop();
}

function unfold_loop(initial, f) {
  return () => {
    let $ = f(initial);
    if ($ instanceof Next) {
      let x = $.element;
      let acc = $.accumulator;
      return new Continue(x, unfold_loop(acc, f));
    } else {
      return new Stop();
    }
  };
}

/**
 * Creates an yielder from a given function and accumulator.
 *
 * The function is called on the accumulator and returns either `Done`,
 * indicating the yielder has no more elements, or `Next` which contains a
 * new element and accumulator. The element is yielded by the yielder and the
 * new accumulator is used with the function to compute the next element in
 * the sequence.
 *
 * ## Examples
 *
 * ```gleam
 * unfold(from: 5, with: fn(n) {
 *  case n {
 *    0 -> Done
 *    n -> Next(element: n, accumulator: n - 1)
 *  }
 * })
 * |> to_list
 * // -> [5, 4, 3, 2, 1]
 * ```
 */
export function unfold(initial, f) {
  let _pipe = initial;
  let _pipe$1 = unfold_loop(_pipe, f);
  return new Yielder(_pipe$1);
}

/**
 * Creates an yielder that yields values created by calling a given function
 * repeatedly.
 *
 * ```gleam
 * repeatedly(fn() { 7 })
 * |> take(3)
 * |> to_list
 * // -> [7, 7, 7]
 * ```
 */
export function repeatedly(f) {
  return unfold(undefined, (_) => { return new Next(f(), undefined); });
}

/**
 * Creates an yielder that returns the same value infinitely.
 *
 * ## Examples
 *
 * ```gleam
 * repeat(10)
 * |> take(4)
 * |> to_list
 * // -> [10, 10, 10, 10]
 * ```
 */
export function repeat(x) {
  return repeatedly(() => { return x; });
}

/**
 * Creates an yielder that yields each element from the given list.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 4])
 * |> to_list
 * // -> [1, 2, 3, 4]
 * ```
 */
export function from_list(list) {
  let yield$1 = (acc) => {
    if (acc instanceof $Empty) {
      return new Done();
    } else {
      let head = acc.head;
      let tail = acc.tail;
      return new Next(head, tail);
    }
  };
  return unfold(list, yield$1);
}

function transform_loop(continuation, state, f) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let el = $[0];
      let next = $[1];
      let $1 = f(state, el);
      if ($1 instanceof Next) {
        let yield$1 = $1.element;
        let next_state = $1.accumulator;
        return new Continue(yield$1, transform_loop(next, next_state, f));
      } else {
        return new Stop();
      }
    }
  };
}

/**
 * Creates an yielder from an existing yielder
 * and a stateful function that may short-circuit.
 *
 * `f` takes arguments `acc` for current state and `el` for current element from underlying yielder,
 * and returns either `Next` with yielded element and new state value, or `Done` to halt the yielder.
 *
 * ## Examples
 *
 * Approximate implementation of `index` in terms of `transform`:
 *
 * ```gleam
 * from_list(["a", "b", "c"])
 * |> transform(0, fn(i, el) { Next(#(i, el), i + 1) })
 * |> to_list
 * // -> [#(0, "a"), #(1, "b"), #(2, "c")]
 * ```
 */
export function transform(yielder, initial, f) {
  let _pipe = transform_loop(yielder.continuation, initial, f);
  return new Yielder(_pipe);
}

function fold_loop(loop$continuation, loop$f, loop$accumulator) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let accumulator = loop$accumulator;
    let $ = continuation();
    if ($ instanceof Stop) {
      return accumulator;
    } else {
      let elem = $[0];
      let next = $[1];
      loop$continuation = next;
      loop$f = f;
      loop$accumulator = f(accumulator, elem);
    }
  }
}

/**
 * Reduces an yielder of elements into a single value by calling a given
 * function on each element in turn.
 *
 * If called on an yielder of infinite length then this function will never
 * return.
 *
 * If you do not care about the end value and only wish to evaluate the
 * yielder for side effects consider using the `run` function instead.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 4])
 * |> fold(from: 0, with: fn(acc, element) { element + acc })
 * // -> 10
 * ```
 */
export function fold(yielder, initial, f) {
  let _pipe = yielder.continuation;
  return fold_loop(_pipe, f, initial);
}

/**
 * Evaluates all elements emitted by the given yielder. This function is useful for when
 * you wish to trigger any side effects that would occur when evaluating
 * the yielder.
 */
export function run(yielder) {
  return fold(yielder, undefined, (_, _1) => { return undefined; });
}

/**
 * Evaluates an yielder and returns all the elements as a list.
 *
 * If called on an yielder of infinite length then this function will never
 * return.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3])
 * |> map(fn(x) { x * 2 })
 * |> to_list
 * // -> [2, 4, 6]
 * ```
 */
export function to_list(yielder) {
  let _pipe = yielder;
  let _pipe$1 = fold(
    _pipe,
    toList([]),
    (acc, e) => { return listPrepend(e, acc); },
  );
  return $list.reverse(_pipe$1);
}

/**
 * Eagerly accesses the first value of an yielder, returning a `Next`
 * that contains the first value and the rest of the yielder.
 *
 * If called on an empty yielder, `Done` is returned.
 *
 * ## Examples
 *
 * ```gleam
 * let assert Next(first, rest) = from_list([1, 2, 3, 4]) |> step
 *
 * first
 * // -> 1
 *
 * rest |> to_list
 * // -> [2, 3, 4]
 * ```
 *
 * ```gleam
 * empty() |> step
 * // -> Done
 * ```
 */
export function step(yielder) {
  let $ = yielder.continuation();
  if ($ instanceof Stop) {
    return new Done();
  } else {
    let e = $[0];
    let a = $[1];
    return new Next(e, new Yielder(a));
  }
}

function take_loop(continuation, desired) {
  return () => {
    let $ = desired > 0;
    if ($) {
      let $1 = continuation();
      if ($1 instanceof Stop) {
        return $1;
      } else {
        let e = $1[0];
        let next = $1[1];
        return new Continue(e, take_loop(next, desired - 1));
      }
    } else {
      return new Stop();
    }
  };
}

/**
 * Creates an yielder that only yields the first `desired` elements.
 *
 * If the yielder does not have enough elements all of them are yielded.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 4, 5])
 * |> take(up_to: 3)
 * |> to_list
 * // -> [1, 2, 3]
 * ```
 *
 * ```gleam
 * from_list([1, 2])
 * |> take(up_to: 3)
 * |> to_list
 * // -> [1, 2]
 * ```
 */
export function take(yielder, desired) {
  let _pipe = yielder.continuation;
  let _pipe$1 = take_loop(_pipe, desired);
  return new Yielder(_pipe$1);
}

function drop_loop(loop$continuation, loop$desired) {
  while (true) {
    let continuation = loop$continuation;
    let desired = loop$desired;
    let $ = continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = desired > 0;
      if ($1) {
        loop$continuation = next;
        loop$desired = desired - 1;
      } else {
        return new Continue(e, next);
      }
    }
  }
}

/**
 * Evaluates and discards the first N elements in an yielder, returning a new
 * yielder.
 *
 * If the yielder does not have enough elements an empty yielder is
 * returned.
 *
 * This function does not evaluate the elements of the yielder, the
 * computation is performed when the yielder is later run.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 4, 5])
 * |> drop(up_to: 3)
 * |> to_list
 * // -> [4, 5]
 * ```
 *
 * ```gleam
 * from_list([1, 2])
 * |> drop(up_to: 3)
 * |> to_list
 * // -> []
 * ```
 */
export function drop(yielder, desired) {
  let _pipe = () => { return drop_loop(yielder.continuation, desired); };
  return new Yielder(_pipe);
}

function map_loop(continuation, f) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let e = $[0];
      let continuation$1 = $[1];
      return new Continue(f(e), map_loop(continuation$1, f));
    }
  };
}

/**
 * Creates an yielder from an existing yielder and a transformation function.
 *
 * Each element in the new yielder will be the result of calling the given
 * function on the elements in the given yielder.
 *
 * This function does not evaluate the elements of the yielder, the
 * computation is performed when the yielder is later run.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3])
 * |> map(fn(x) { x * 2 })
 * |> to_list
 * // -> [2, 4, 6]
 * ```
 */
export function map(yielder, f) {
  let _pipe = yielder.continuation;
  let _pipe$1 = map_loop(_pipe, f);
  return new Yielder(_pipe$1);
}

function map2_loop(continuation1, continuation2, fun) {
  return () => {
    let $ = continuation1();
    if ($ instanceof Stop) {
      return $;
    } else {
      let a = $[0];
      let next_a = $[1];
      let $1 = continuation2();
      if ($1 instanceof Stop) {
        return $1;
      } else {
        let b = $1[0];
        let next_b = $1[1];
        return new Continue(fun(a, b), map2_loop(next_a, next_b, fun));
      }
    }
  };
}

/**
 * Combines two yielders into a single one using the given function.
 *
 * If an yielder is longer than the other the extra elements are dropped.
 *
 * This function does not evaluate the elements of the two yielders, the
 * computation is performed when the resulting yielder is later run.
 *
 * ## Examples
 *
 * ```gleam
 * let first = from_list([1, 2, 3])
 * let second = from_list([4, 5, 6])
 * map2(first, second, fn(x, y) { x + y }) |> to_list
 * // -> [5, 7, 9]
 * ```
 *
 * ```gleam
 * let first = from_list([1, 2])
 * let second = from_list(["a", "b", "c"])
 * map2(first, second, fn(i, x) { #(i, x) }) |> to_list
 * // -> [#(1, "a"), #(2, "b")]
 * ```
 */
export function map2(yielder1, yielder2, fun) {
  let _pipe = map2_loop(yielder1.continuation, yielder2.continuation, fun);
  return new Yielder(_pipe);
}

function append_loop(first, second) {
  let $ = first();
  if ($ instanceof Stop) {
    return second();
  } else {
    let e = $[0];
    let first$1 = $[1];
    return new Continue(e, () => { return append_loop(first$1, second); });
  }
}

/**
 * Appends two yielders, producing a new yielder.
 *
 * This function does not evaluate the elements of the yielders, the
 * computation is performed when the resulting yielder is later run.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2])
 * |> append(from_list([3, 4]))
 * |> to_list
 * // -> [1, 2, 3, 4]
 * ```
 */
export function append(first, second) {
  let _pipe = () => {
    return append_loop(first.continuation, second.continuation);
  };
  return new Yielder(_pipe);
}

function flatten_loop(flattened) {
  let $ = flattened();
  if ($ instanceof Stop) {
    return $;
  } else {
    let it = $[0];
    let next_yielder = $[1];
    return append_loop(
      it.continuation,
      () => { return flatten_loop(next_yielder); },
    );
  }
}

/**
 * Flattens an yielder of yielders, creating a new yielder.
 *
 * This function does not evaluate the elements of the yielder, the
 * computation is performed when the yielder is later run.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([[1, 2], [3, 4]])
 * |> map(from_list)
 * |> flatten
 * |> to_list
 * // -> [1, 2, 3, 4]
 * ```
 */
export function flatten(yielder) {
  let _pipe = () => { return flatten_loop(yielder.continuation); };
  return new Yielder(_pipe);
}

/**
 * Joins a list of yielders into a single yielder.
 *
 * This function does not evaluate the elements of the yielder, the
 * computation is performed when the yielder is later run.
 *
 * ## Examples
 *
 * ```gleam
 * [[1, 2], [3, 4]]
 * |> map(from_list)
 * |> concat
 * |> to_list
 * // -> [1, 2, 3, 4]
 * ```
 */
export function concat(yielders) {
  return flatten(from_list(yielders));
}

/**
 * Creates an yielder from an existing yielder and a transformation function.
 *
 * Each element in the new yielder will be the result of calling the given
 * function on the elements in the given yielder and then flattening the
 * results.
 *
 * This function does not evaluate the elements of the yielder, the
 * computation is performed when the yielder is later run.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2])
 * |> flat_map(fn(x) { from_list([x, x + 1]) })
 * |> to_list
 * // -> [1, 2, 2, 3]
 * ```
 */
export function flat_map(yielder, f) {
  let _pipe = yielder;
  let _pipe$1 = map(_pipe, f);
  return flatten(_pipe$1);
}

function filter_loop(loop$continuation, loop$predicate) {
  while (true) {
    let continuation = loop$continuation;
    let predicate = loop$predicate;
    let $ = continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let e = $[0];
      let yielder = $[1];
      let $1 = predicate(e);
      if ($1) {
        return new Continue(
          e,
          () => { return filter_loop(yielder, predicate); },
        );
      } else {
        loop$continuation = yielder;
        loop$predicate = predicate;
      }
    }
  }
}

/**
 * Creates an yielder from an existing yielder and a predicate function.
 *
 * The new yielder will contain elements from the first yielder for which
 * the given function returns `True`.
 *
 * This function does not evaluate the elements of the yielder, the
 * computation is performed when the yielder is later run.
 *
 * ## Examples
 *
 * ```gleam
 * import gleam/int
 *
 * from_list([1, 2, 3, 4])
 * |> filter(int.is_even)
 * |> to_list
 * // -> [2, 4]
 * ```
 */
export function filter(yielder, predicate) {
  let _pipe = () => { return filter_loop(yielder.continuation, predicate); };
  return new Yielder(_pipe);
}

function filter_map_loop(loop$continuation, loop$f) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let $ = continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = f(e);
      if ($1 instanceof Ok) {
        let e$1 = $1[0];
        return new Continue(e$1, () => { return filter_map_loop(next, f); });
      } else {
        loop$continuation = next;
        loop$f = f;
      }
    }
  }
}

/**
 * Creates an yielder from an existing yielder and a transforming predicate function.
 *
 * The new yielder will contain elements from the first yielder for which
 * the given function returns `Ok`, transformed to the value inside the `Ok`.
 *
 * This function does not evaluate the elements of the yielder, the
 * computation is performed when the yielder is later run.
 *
 * ## Examples
 *
 * ```gleam
 * import gleam/string
 * import gleam/int
 *
 * "a1b2c3d4e5f"
 * |> string.to_graphemes
 * |> from_list
 * |> filter_map(int.parse)
 * |> to_list
 * // -> [1, 2, 3, 4, 5]
 * ```
 */
export function filter_map(yielder, f) {
  let _pipe = () => { return filter_map_loop(yielder.continuation, f); };
  return new Yielder(_pipe);
}

/**
 * Creates an yielder that repeats a given yielder infinitely.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2])
 * |> cycle
 * |> take(6)
 * |> to_list
 * // -> [1, 2, 1, 2, 1, 2]
 * ```
 */
export function cycle(yielder) {
  let _pipe = repeat(yielder);
  return flatten(_pipe);
}

function find_loop(loop$continuation, loop$f) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Error(undefined);
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = f(e);
      if ($1) {
        return new Ok(e);
      } else {
        loop$continuation = next;
        loop$f = f;
      }
    }
  }
}

/**
 * Finds the first element in a given yielder for which the given function returns
 * `True`.
 *
 * Returns `Error(Nil)` if the function does not return `True` for any of the
 * elements.
 *
 * ## Examples
 *
 * ```gleam
 * find(from_list([1, 2, 3]), fn(x) { x > 2 })
 * // -> Ok(3)
 * ```
 *
 * ```gleam
 * find(from_list([1, 2, 3]), fn(x) { x > 4 })
 * // -> Error(Nil)
 * ```
 *
 * ```gleam
 * find(empty(), fn(_) { True })
 * // -> Error(Nil)
 * ```
 */
export function find(haystack, is_desired) {
  let _pipe = haystack.continuation;
  return find_loop(_pipe, is_desired);
}

function find_map_loop(loop$continuation, loop$f) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Error(undefined);
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = f(e);
      if ($1 instanceof Ok) {
        return $1;
      } else {
        loop$continuation = next;
        loop$f = f;
      }
    }
  }
}

/**
 * Finds the first element in a given yielder
 * for which the given function returns `Ok(new_value)`,
 * then returns the wrapped `new_value`.
 *
 * Returns `Error(Nil)` if no such element is found.
 *
 * ## Examples
 *
 * ```gleam
 * find_map(from_list(["a", "1", "2"]), int.parse)
 * // -> Ok(1)
 * ```
 *
 * ```gleam
 * find_map(from_list(["a", "b", "c"]), int.parse)
 * // -> Error(Nil)
 * ```
 *
 * ```gleam
 * find_map(from_list([]), int.parse)
 * // -> Error(Nil)
 * ```
 */
export function find_map(haystack, is_desired) {
  let _pipe = haystack.continuation;
  return find_map_loop(_pipe, is_desired);
}

function index_loop(continuation, next) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let e = $[0];
      let continuation$1 = $[1];
      return new Continue([e, next], index_loop(continuation$1, next + 1));
    }
  };
}

/**
 * Wraps values yielded from an yielder with indices, starting from 0.
 *
 * ## Examples
 *
 * ```gleam
 * from_list(["a", "b", "c"]) |> index |> to_list
 * // -> [#("a", 0), #("b", 1), #("c", 2)]
 * ```
 */
export function index(yielder) {
  let _pipe = yielder.continuation;
  let _pipe$1 = index_loop(_pipe, 0);
  return new Yielder(_pipe$1);
}

/**
 * Creates an yielder that infinitely applies a function to a value.
 *
 * ## Examples
 *
 * ```gleam
 * iterate(1, fn(n) { n * 3 }) |> take(5) |> to_list
 * // -> [1, 3, 9, 27, 81]
 * ```
 */
export function iterate(initial, f) {
  return unfold(initial, (element) => { return new Next(element, f(element)); });
}

function take_while_loop(continuation, predicate) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = predicate(e);
      if ($1) {
        return new Continue(e, take_while_loop(next, predicate));
      } else {
        return new Stop();
      }
    }
  };
}

/**
 * Creates an yielder that yields elements while the predicate returns `True`.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 2, 4])
 * |> take_while(satisfying: fn(x) { x < 3 })
 * |> to_list
 * // -> [1, 2]
 * ```
 */
export function take_while(yielder, predicate) {
  let _pipe = yielder.continuation;
  let _pipe$1 = take_while_loop(_pipe, predicate);
  return new Yielder(_pipe$1);
}

function drop_while_loop(loop$continuation, loop$predicate) {
  while (true) {
    let continuation = loop$continuation;
    let predicate = loop$predicate;
    let $ = continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = predicate(e);
      if ($1) {
        loop$continuation = next;
        loop$predicate = predicate;
      } else {
        return new Continue(e, next);
      }
    }
  }
}

/**
 * Creates an yielder that drops elements while the predicate returns `True`,
 * and then yields the remaining elements.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 4, 2, 5])
 * |> drop_while(satisfying: fn(x) { x < 4 })
 * |> to_list
 * // -> [4, 2, 5]
 * ```
 */
export function drop_while(yielder, predicate) {
  let _pipe = () => { return drop_while_loop(yielder.continuation, predicate); };
  return new Yielder(_pipe);
}

function scan_loop(continuation, f, accumulator) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let el = $[0];
      let next = $[1];
      let accumulated = f(accumulator, el);
      return new Continue(accumulated, scan_loop(next, f, accumulated));
    }
  };
}

/**
 * Creates an yielder from an existing yielder and a stateful function.
 *
 * Specifically, this behaves like `fold`, but yields intermediate results.
 *
 * ## Examples
 *
 * ```gleam
 * // Generate a sequence of partial sums
 * from_list([1, 2, 3, 4, 5])
 * |> scan(from: 0, with: fn(acc, el) { acc + el })
 * |> to_list
 * // -> [1, 3, 6, 10, 15]
 * ```
 */
export function scan(yielder, initial, f) {
  let _pipe = yielder.continuation;
  let _pipe$1 = scan_loop(_pipe, f, initial);
  return new Yielder(_pipe$1);
}

function zip_loop(left, right) {
  return () => {
    let $ = left();
    if ($ instanceof Stop) {
      return $;
    } else {
      let el_left = $[0];
      let next_left = $[1];
      let $1 = right();
      if ($1 instanceof Stop) {
        return $1;
      } else {
        let el_right = $1[0];
        let next_right = $1[1];
        return new Continue(
          [el_left, el_right],
          zip_loop(next_left, next_right),
        );
      }
    }
  };
}

/**
 * Zips two yielders together, emitting values from both
 * until the shorter one runs out.
 *
 * ## Examples
 *
 * ```gleam
 * from_list(["a", "b", "c"])
 * |> zip(range(20, 30))
 * |> to_list
 * // -> [#("a", 20), #("b", 21), #("c", 22)]
 * ```
 */
export function zip(left, right) {
  let _pipe = zip_loop(left.continuation, right.continuation);
  return new Yielder(_pipe);
}

function next_chunk(
  loop$continuation,
  loop$f,
  loop$previous_key,
  loop$current_chunk
) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let previous_key = loop$previous_key;
    let current_chunk = loop$current_chunk;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new LastBy($list.reverse(current_chunk));
    } else {
      let e = $[0];
      let next = $[1];
      let key = f(e);
      let $1 = isEqual(key, previous_key);
      if ($1) {
        loop$continuation = next;
        loop$f = f;
        loop$previous_key = key;
        loop$current_chunk = listPrepend(e, current_chunk);
      } else {
        return new AnotherBy($list.reverse(current_chunk), key, e, next);
      }
    }
  }
}

function chunk_loop(continuation, f, previous_key, previous_element) {
  let $ = next_chunk(continuation, f, previous_key, toList([previous_element]));
  if ($ instanceof AnotherBy) {
    let chunk$1 = $[0];
    let key = $[1];
    let el = $[2];
    let next = $[3];
    return new Continue(chunk$1, () => { return chunk_loop(next, f, key, el); });
  } else {
    let chunk$1 = $[0];
    return new Continue(chunk$1, stop);
  }
}

/**
 * Creates an yielder that emits chunks of elements
 * for which `f` returns the same value.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 2, 3, 4, 4, 6, 7, 7])
 * |> chunk(by: fn(n) { n % 2 })
 * |> to_list
 * // -> [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
 * ```
 */
export function chunk(yielder, f) {
  let _pipe = () => {
    let $ = yielder.continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let e = $[0];
      let next = $[1];
      return chunk_loop(next, f, f(e), e);
    }
  };
  return new Yielder(_pipe);
}

function next_sized_chunk(loop$continuation, loop$left, loop$current_chunk) {
  while (true) {
    let continuation = loop$continuation;
    let left = loop$left;
    let current_chunk = loop$current_chunk;
    let $ = continuation();
    if ($ instanceof Stop) {
      if (current_chunk instanceof $Empty) {
        return new NoMore();
      } else {
        let remaining = current_chunk;
        return new Last($list.reverse(remaining));
      }
    } else {
      let e = $[0];
      let next = $[1];
      let chunk$1 = listPrepend(e, current_chunk);
      let $1 = left > 1;
      if ($1) {
        loop$continuation = next;
        loop$left = left - 1;
        loop$current_chunk = chunk$1;
      } else {
        return new Another($list.reverse(chunk$1), next);
      }
    }
  }
}

function sized_chunk_loop(continuation, count) {
  return () => {
    let $ = next_sized_chunk(continuation, count, toList([]));
    if ($ instanceof Another) {
      let chunk$1 = $[0];
      let next_element = $[1];
      return new Continue(chunk$1, sized_chunk_loop(next_element, count));
    } else if ($ instanceof Last) {
      let chunk$1 = $[0];
      return new Continue(chunk$1, stop);
    } else {
      return new Stop();
    }
  };
}

/**
 * Creates an yielder that emits chunks of given size.
 *
 * If the last chunk does not have `count` elements, it is yielded
 * as a partial chunk, with less than `count` elements.
 *
 * For any `count` less than 1 this function behaves as if it was set to 1.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 4, 5, 6])
 * |> sized_chunk(into: 2)
 * |> to_list
 * // -> [[1, 2], [3, 4], [5, 6]]
 * ```
 *
 * ```gleam
 * from_list([1, 2, 3, 4, 5, 6, 7, 8])
 * |> sized_chunk(into: 3)
 * |> to_list
 * // -> [[1, 2, 3], [4, 5, 6], [7, 8]]
 * ```
 */
export function sized_chunk(yielder, count) {
  let _pipe = yielder.continuation;
  let _pipe$1 = sized_chunk_loop(_pipe, count);
  return new Yielder(_pipe$1);
}

function intersperse_loop(continuation, separator) {
  let $ = continuation();
  if ($ instanceof Stop) {
    return $;
  } else {
    let e = $[0];
    let next = $[1];
    let next_interspersed = () => { return intersperse_loop(next, separator); };
    return new Continue(
      separator,
      () => { return new Continue(e, next_interspersed); },
    );
  }
}

/**
 * Creates an yielder that yields the given `elem` element
 * between elements emitted by the underlying yielder.
 *
 * ## Examples
 *
 * ```gleam
 * empty()
 * |> intersperse(with: 0)
 * |> to_list
 * // -> []
 * ```
 *
 * ```gleam
 * from_list([1])
 * |> intersperse(with: 0)
 * |> to_list
 * // -> [1]
 * ```
 *
 * ```gleam
 * from_list([1, 2, 3, 4, 5])
 * |> intersperse(with: 0)
 * |> to_list
 * // -> [1, 0, 2, 0, 3, 0, 4, 0, 5]
 * ```
 */
export function intersperse(yielder, elem) {
  let _pipe = () => {
    let $ = yielder.continuation();
    if ($ instanceof Stop) {
      return $;
    } else {
      let e = $[0];
      let next = $[1];
      return new Continue(e, () => { return intersperse_loop(next, elem); });
    }
  };
  return new Yielder(_pipe);
}

function any_loop(loop$continuation, loop$predicate) {
  while (true) {
    let continuation = loop$continuation;
    let predicate = loop$predicate;
    let $ = continuation();
    if ($ instanceof Stop) {
      return false;
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = predicate(e);
      if ($1) {
        return $1;
      } else {
        loop$continuation = next;
        loop$predicate = predicate;
      }
    }
  }
}

/**
 * Returns `True` if any element emitted by the yielder satisfies the given predicate,
 * `False` otherwise.
 *
 * This function short-circuits once it finds a satisfying element.
 *
 * An empty yielder results in `False`.
 *
 * ## Examples
 *
 * ```gleam
 * empty()
 * |> any(fn(n) { n % 2 == 0 })
 * // -> False
 * ```
 *
 * ```gleam
 * from_list([1, 2, 5, 7, 9])
 * |> any(fn(n) { n % 2 == 0 })
 * // -> True
 * ```
 *
 * ```gleam
 * from_list([1, 3, 5, 7, 9])
 * |> any(fn(n) { n % 2 == 0 })
 * // -> False
 * ```
 */
export function any(yielder, predicate) {
  let _pipe = yielder.continuation;
  return any_loop(_pipe, predicate);
}

function all_loop(loop$continuation, loop$predicate) {
  while (true) {
    let continuation = loop$continuation;
    let predicate = loop$predicate;
    let $ = continuation();
    if ($ instanceof Stop) {
      return true;
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = predicate(e);
      if ($1) {
        loop$continuation = next;
        loop$predicate = predicate;
      } else {
        return $1;
      }
    }
  }
}

/**
 * Returns `True` if all elements emitted by the yielder satisfy the given predicate,
 * `False` otherwise.
 *
 * This function short-circuits once it finds a non-satisfying element.
 *
 * An empty yielder results in `True`.
 *
 * ## Examples
 *
 * ```gleam
 * empty()
 * |> all(fn(n) { n % 2 == 0 })
 * // -> True
 * ```
 *
 * ```gleam
 * from_list([2, 4, 6, 8])
 * |> all(fn(n) { n % 2 == 0 })
 * // -> True
 * ```
 *
 * ```gleam
 * from_list([2, 4, 5, 8])
 * |> all(fn(n) { n % 2 == 0 })
 * // -> False
 * ```
 */
export function all(yielder, predicate) {
  let _pipe = yielder.continuation;
  return all_loop(_pipe, predicate);
}

function update_group_with(el) {
  return (maybe_group) => {
    if (maybe_group instanceof Some) {
      let group$1 = maybe_group[0];
      return listPrepend(el, group$1);
    } else {
      return toList([el]);
    }
  };
}

function group_updater(f) {
  return (groups, elem) => {
    let _pipe = groups;
    return $dict.upsert(_pipe, f(elem), update_group_with(elem));
  };
}

/**
 * Returns a `Dict(k, List(element))` of elements from the given yielder
 * grouped with the given key function.
 *
 * The order within each group is preserved from the yielder.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 4, 5, 6])
 * |> group(by: fn(n) { n % 3 })
 * // -> dict.from_list([#(0, [3, 6]), #(1, [1, 4]), #(2, [2, 5])])
 * ```
 */
export function group(yielder, key) {
  let _pipe = yielder;
  let _pipe$1 = fold(_pipe, $dict.new$(), group_updater(key));
  return $dict.map_values(
    _pipe$1,
    (_, group) => { return $list.reverse(group); },
  );
}

/**
 * This function acts similar to fold, but does not take an initial state.
 * Instead, it starts from the first yielded element
 * and combines it with each subsequent element in turn using the given function.
 * The function is called as `f(accumulator, current_element)`.
 *
 * Returns `Ok` to indicate a successful run, and `Error` if called on an empty yielder.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([])
 * |> reduce(fn(acc, x) { acc + x })
 * // -> Error(Nil)
 * ```
 *
 * ```gleam
 * from_list([1, 2, 3, 4, 5])
 * |> reduce(fn(acc, x) { acc + x })
 * // -> Ok(15)
 * ```
 */
export function reduce(yielder, f) {
  let $ = yielder.continuation();
  if ($ instanceof Stop) {
    return new Error(undefined);
  } else {
    let e = $[0];
    let next = $[1];
    let _pipe = fold_loop(next, f, e);
    return new Ok(_pipe);
  }
}

/**
 * Returns the last element in the given yielder.
 *
 * Returns `Error(Nil)` if the yielder is empty.
 *
 * This function runs in linear time.
 *
 * ## Examples
 *
 * ```gleam
 * empty() |> last
 * // -> Error(Nil)
 * ```
 *
 * ```gleam
 * range(1, 10) |> last
 * // -> Ok(10)
 * ```
 */
export function last(yielder) {
  let _pipe = yielder;
  return reduce(_pipe, (_, elem) => { return elem; });
}

/**
 * Creates an yielder that yields no elements.
 *
 * ## Examples
 *
 * ```gleam
 * empty() |> to_list
 * // -> []
 * ```
 */
export function empty() {
  return new Yielder(stop);
}

/**
 * Creates an yielder that yields exactly one element provided by calling the given function.
 *
 * ## Examples
 *
 * ```gleam
 * once(fn() { 1 }) |> to_list
 * // -> [1]
 * ```
 */
export function once(f) {
  let _pipe = () => { return new Continue(f(), stop); };
  return new Yielder(_pipe);
}

/**
 * Creates an yielder of ints, starting at a given start int and stepping by
 * one to a given end int.
 *
 * ## Examples
 *
 * ```gleam
 * range(from: 1, to: 5) |> to_list
 * // -> [1, 2, 3, 4, 5]
 * ```
 *
 * ```gleam
 * range(from: 1, to: -2) |> to_list
 * // -> [1, 0, -1, -2]
 * ```
 *
 * ```gleam
 * range(from: 0, to: 0) |> to_list
 * // -> [0]
 * ```
 */
export function range(start, stop) {
  let $ = $int.compare(start, stop);
  if ($ instanceof $order.Lt) {
    return unfold(
      start,
      (current) => {
        let $1 = current > stop;
        if ($1) {
          return new Done();
        } else {
          return new Next(current, current + 1);
        }
      },
    );
  } else if ($ instanceof $order.Eq) {
    return once(() => { return start; });
  } else {
    return unfold(
      start,
      (current) => {
        let $1 = current < stop;
        if ($1) {
          return new Done();
        } else {
          return new Next(current, current - 1);
        }
      },
    );
  }
}

/**
 * Creates an yielder that yields the given element exactly once.
 *
 * ## Examples
 *
 * ```gleam
 * single(1) |> to_list
 * // -> [1]
 * ```
 */
export function single(elem) {
  return once(() => { return elem; });
}

function interleave_loop(current, next) {
  let $ = current();
  if ($ instanceof Stop) {
    return next();
  } else {
    let e = $[0];
    let next_other = $[1];
    return new Continue(e, () => { return interleave_loop(next, next_other); });
  }
}

/**
 * Creates an yielder that alternates between the two given yielders
 * until both have run out.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 4])
 * |> interleave(from_list([11, 12, 13, 14]))
 * |> to_list
 * // -> [1, 11, 2, 12, 3, 13, 4, 14]
 * ```
 *
 * ```gleam
 * from_list([1, 2, 3, 4])
 * |> interleave(from_list([100]))
 * |> to_list
 * // -> [1, 100, 2, 3, 4]
 * ```
 */
export function interleave(left, right) {
  let _pipe = () => {
    return interleave_loop(left.continuation, right.continuation);
  };
  return new Yielder(_pipe);
}

function fold_until_loop(loop$continuation, loop$f, loop$accumulator) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let accumulator = loop$accumulator;
    let $ = continuation();
    if ($ instanceof Stop) {
      return accumulator;
    } else {
      let elem = $[0];
      let next = $[1];
      let $1 = f(accumulator, elem);
      if ($1 instanceof $list.Continue) {
        let accumulator$1 = $1[0];
        loop$continuation = next;
        loop$f = f;
        loop$accumulator = accumulator$1;
      } else {
        let accumulator$1 = $1[0];
        return accumulator$1;
      }
    }
  }
}

/**
 * Like `fold`, `fold_until` reduces an yielder of elements into a single value by calling a given
 * function on each element in turn, but uses `list.ContinueOrStop` to determine
 * whether or not to keep iterating.
 *
 * If called on an yielder of infinite length then this function will only ever
 * return if the function returns `list.Stop`.
 *
 * ## Examples
 *
 * ```gleam
 * import gleam/list
 *
 * let f = fn(acc, e) {
 *   case e {
 *     _ if e < 4 -> list.Continue(e + acc)
 *     _ -> list.Stop(acc)
 *   }
 * }
 *
 * from_list([1, 2, 3, 4])
 * |> fold_until(from: 0, with: f)
 * // -> 6
 * ```
 */
export function fold_until(yielder, initial, f) {
  let _pipe = yielder.continuation;
  return fold_until_loop(_pipe, f, initial);
}

function try_fold_loop(loop$continuation, loop$f, loop$accumulator) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let accumulator = loop$accumulator;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Ok(accumulator);
    } else {
      let elem = $[0];
      let next = $[1];
      let $1 = f(accumulator, elem);
      if ($1 instanceof Ok) {
        let result = $1[0];
        loop$continuation = next;
        loop$f = f;
        loop$accumulator = result;
      } else {
        return $1;
      }
    }
  }
}

/**
 * A variant of fold that might fail.
 *
 * The folding function should return `Result(accumulator, error)`.
 * If the returned value is `Ok(accumulator)` try_fold will try the next value in the yielder.
 * If the returned value is `Error(error)` try_fold will stop and return that error.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 4])
 * |> try_fold(0, fn(acc, i) {
 *   case i < 3 {
 *     True -> Ok(acc + i)
 *     False -> Error(Nil)
 *   }
 * })
 * // -> Error(Nil)
 * ```
 */
export function try_fold(yielder, initial, f) {
  let _pipe = yielder.continuation;
  return try_fold_loop(_pipe, f, initial);
}

/**
 * Returns the first element yielded by the given yielder, if it exists,
 * or `Error(Nil)` otherwise.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3]) |> first
 * // -> Ok(1)
 * ```
 *
 * ```gleam
 * empty() |> first
 * // -> Error(Nil)
 * ```
 */
export function first(yielder) {
  let $ = yielder.continuation();
  if ($ instanceof Stop) {
    return new Error(undefined);
  } else {
    let e = $[0];
    return new Ok(e);
  }
}

/**
 * Returns nth element yielded by the given yielder, where `0` means the first element.
 *
 * If there are not enough elements in the yielder, `Error(Nil)` is returned.
 *
 * For any `index` less than `0` this function behaves as if it was set to `0`.
 *
 * ## Examples
 *
 * ```gleam
 * from_list([1, 2, 3, 4]) |> at(2)
 * // -> Ok(3)
 * ```
 *
 * ```gleam
 * from_list([1, 2, 3, 4]) |> at(4)
 * // -> Error(Nil)
 * ```
 *
 * ```gleam
 * empty() |> at(0)
 * // -> Error(Nil)
 * ```
 */
export function at(yielder, index) {
  let _pipe = yielder;
  let _pipe$1 = drop(_pipe, index);
  return first(_pipe$1);
}

function length_loop(loop$continuation, loop$length) {
  while (true) {
    let continuation = loop$continuation;
    let length = loop$length;
    let $ = continuation();
    if ($ instanceof Stop) {
      return length;
    } else {
      let next = $[1];
      loop$continuation = next;
      loop$length = length + 1;
    }
  }
}

/**
 * Counts the number of elements in the given yielder.
 *
 * This function has to traverse the entire yielder to count its elements,
 * so it runs in linear time.
 *
 * ## Examples
 *
 * ```gleam
 * empty() |> length
 * // -> 0
 * ```
 *
 * ```gleam
 * from_list([1, 2, 3, 4]) |> length
 * // -> 4
 * ```
 */
export function length(yielder) {
  let _pipe = yielder.continuation;
  return length_loop(_pipe, 0);
}

/**
 * Traverse an yielder, calling a function on each element.
 *
 * ## Examples
 *
 * ```gleam
 * empty() |> each(io.println)
 * // -> Nil
 * ```
 *
 * ```gleam
 * from_list(["Tom", "Malory", "Louis"]) |> each(io.println)
 * // -> Nil
 * // Tom
 * // Malory
 * // Louis
 * ```
 */
export function each(yielder, f) {
  let _pipe = yielder;
  let _pipe$1 = map(_pipe, f);
  return run(_pipe$1);
}

/**
 * Add a new element to the start of an yielder.
 *
 * This function is for use with `use` expressions, to replicate the behaviour
 * of the `yield` keyword found in other languages.
 *
 * If you only need to prepend an element and don't require the `use` syntax,
 * use `prepend`.
 *
 * ## Examples
 *
 * ```gleam
 * let yielder = {
 *   use <- yield(1)
 *   use <- yield(2)
 *   use <- yield(3)
 *   empty()
 * }
 *
 * yielder |> to_list
 * // -> [1, 2, 3]
 * ```
 */
export function yield$(element, next) {
  return new Yielder(
    () => {
      return new Continue(element, () => { return next().continuation(); });
    },
  );
}

/**
 * Add a new element to the start of an yielder.
 *
 * ## Examples
 *
 * ```gleam
 * let yielder = from_list([1, 2, 3]) |> prepend(0)
 *
 * yielder.to_list
 * // -> [0, 1, 2, 3]
 * ```
 */
export function prepend(yielder, element) {
  return yield$(element, () => { return yielder; });
}
