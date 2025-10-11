import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order

// Internal private representation of an Yielder
type Action(element) {
  // Dedicated to Electric Six
  // https://youtu.be/_30t2dzEgiw?t=162
  Stop
  Continue(element, fn() -> Action(element))
}

/// An yielder is a lazily evaluated sequence of element.
///
/// Yielders are useful when working with collections that are too large to
/// fit in memory (or those that are infinite in size) as they only require the
/// elements currently being processed to be in memory.
///
/// As a lazy data structure no work is done when an yielder is filtered,
/// mapped, etc, instead a new yielder is returned with these transformations
/// applied to the stream. Once the stream has all the required transformations
/// applied it can be evaluated using functions such as `fold` and `to_list`.
///
pub opaque type Yielder(element) {
  Yielder(continuation: fn() -> Action(element))
}

// Public API for iteration
pub type Step(element, accumulator) {
  Next(element: element, accumulator: accumulator)
  Done
}

// Shortcut for an empty yielder.
fn stop() -> Action(element) {
  Stop
}

/// Creates an yielder from a given function and accumulator.
///
/// The function is called on the accumulator and returns either `Done`,
/// indicating the yielder has no more elements, or `Next` which contains a
/// new element and accumulator. The element is yielded by the yielder and the
/// new accumulator is used with the function to compute the next element in
/// the sequence.
///
/// ## Examples
///
/// ```gleam
/// unfold(from: 5, with: fn(n) {
///  case n {
///    0 -> Done
///    n -> Next(element: n, accumulator: n - 1)
///  }
/// })
/// |> to_list
/// // -> [5, 4, 3, 2, 1]
/// ```
///
pub fn unfold(
  from initial: acc,
  with f: fn(acc) -> Step(element, acc),
) -> Yielder(element) {
  initial
  |> unfold_loop(f)
  |> Yielder
}

// Creating Yielders
fn unfold_loop(
  initial: acc,
  f: fn(acc) -> Step(element, acc),
) -> fn() -> Action(element) {
  fn() {
    case f(initial) {
      Next(x, acc) -> Continue(x, unfold_loop(acc, f))
      Done -> Stop
    }
  }
}

/// Creates an yielder that yields values created by calling a given function
/// repeatedly.
///
/// ```gleam
/// repeatedly(fn() { 7 })
/// |> take(3)
/// |> to_list
/// // -> [7, 7, 7]
/// ```
///
pub fn repeatedly(f: fn() -> element) -> Yielder(element) {
  unfold(Nil, fn(_) { Next(f(), Nil) })
}

/// Creates an yielder that returns the same value infinitely.
///
/// ## Examples
///
/// ```gleam
/// repeat(10)
/// |> take(4)
/// |> to_list
/// // -> [10, 10, 10, 10]
/// ```
///
pub fn repeat(x: element) -> Yielder(element) {
  repeatedly(fn() { x })
}

/// Creates an yielder that yields each element from the given list.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4])
/// |> to_list
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn from_list(list: List(element)) -> Yielder(element) {
  let yield = fn(acc) {
    case acc {
      [] -> Done
      [head, ..tail] -> Next(head, tail)
    }
  }
  unfold(list, yield)
}

// Consuming Yielders
fn transform_loop(
  continuation: fn() -> Action(a),
  state: acc,
  f: fn(acc, a) -> Step(b, acc),
) -> fn() -> Action(b) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(el, next) ->
        case f(state, el) {
          Done -> Stop
          Next(yield, next_state) ->
            Continue(yield, transform_loop(next, next_state, f))
        }
    }
  }
}

/// Creates an yielder from an existing yielder
/// and a stateful function that may short-circuit.
///
/// `f` takes arguments `acc` for current state and `el` for current element from underlying yielder,
/// and returns either `Next` with yielded element and new state value, or `Done` to halt the yielder.
///
/// ## Examples
///
/// Approximate implementation of `index` in terms of `transform`:
///
/// ```gleam
/// from_list(["a", "b", "c"])
/// |> transform(0, fn(i, el) { Next(#(i, el), i + 1) })
/// |> to_list
/// // -> [#(0, "a"), #(1, "b"), #(2, "c")]
/// ```
///
pub fn transform(
  over yielder: Yielder(a),
  from initial: acc,
  with f: fn(acc, a) -> Step(b, acc),
) -> Yielder(b) {
  transform_loop(yielder.continuation, initial, f)
  |> Yielder
}

/// Reduces an yielder of elements into a single value by calling a given
/// function on each element in turn.
///
/// If called on an yielder of infinite length then this function will never
/// return.
///
/// If you do not care about the end value and only wish to evaluate the
/// yielder for side effects consider using the `run` function instead.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4])
/// |> fold(from: 0, with: fn(acc, element) { element + acc })
/// // -> 10
/// ```
///
pub fn fold(
  over yielder: Yielder(e),
  from initial: acc,
  with f: fn(acc, e) -> acc,
) -> acc {
  yielder.continuation
  |> fold_loop(f, initial)
}

fn fold_loop(
  continuation: fn() -> Action(e),
  f: fn(acc, e) -> acc,
  accumulator: acc,
) -> acc {
  case continuation() {
    Continue(elem, next) -> fold_loop(next, f, f(accumulator, elem))
    Stop -> accumulator
  }
}

// TODO: test
/// Evaluates all elements emitted by the given yielder. This function is useful for when
/// you wish to trigger any side effects that would occur when evaluating
/// the yielder.
///
pub fn run(yielder: Yielder(e)) -> Nil {
  fold(yielder, Nil, fn(_, _) { Nil })
}

/// Evaluates an yielder and returns all the elements as a list.
///
/// If called on an yielder of infinite length then this function will never
/// return.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3])
/// |> map(fn(x) { x * 2 })
/// |> to_list
/// // -> [2, 4, 6]
/// ```
///
pub fn to_list(yielder: Yielder(element)) -> List(element) {
  yielder
  |> fold([], fn(acc, e) { [e, ..acc] })
  |> list.reverse
}

/// Eagerly accesses the first value of an yielder, returning a `Next`
/// that contains the first value and the rest of the yielder.
///
/// If called on an empty yielder, `Done` is returned.
///
/// ## Examples
///
/// ```gleam
/// let assert Next(first, rest) = from_list([1, 2, 3, 4]) |> step
///
/// first
/// // -> 1
///
/// rest |> to_list
/// // -> [2, 3, 4]
/// ```
///
/// ```gleam
/// empty() |> step
/// // -> Done
/// ```
///
pub fn step(yielder: Yielder(e)) -> Step(e, Yielder(e)) {
  case yielder.continuation() {
    Stop -> Done
    Continue(e, a) -> Next(e, Yielder(a))
  }
}

/// Creates an yielder that only yields the first `desired` elements.
///
/// If the yielder does not have enough elements all of them are yielded.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5])
/// |> take(up_to: 3)
/// |> to_list
/// // -> [1, 2, 3]
/// ```
///
/// ```gleam
/// from_list([1, 2])
/// |> take(up_to: 3)
/// |> to_list
/// // -> [1, 2]
/// ```
///
pub fn take(from yielder: Yielder(e), up_to desired: Int) -> Yielder(e) {
  yielder.continuation
  |> take_loop(desired)
  |> Yielder
}

fn take_loop(continuation: fn() -> Action(e), desired: Int) -> fn() -> Action(e) {
  fn() {
    case desired > 0 {
      False -> Stop
      True ->
        case continuation() {
          Stop -> Stop
          Continue(e, next) -> Continue(e, take_loop(next, desired - 1))
        }
    }
  }
}

/// Evaluates and discards the first N elements in an yielder, returning a new
/// yielder.
///
/// If the yielder does not have enough elements an empty yielder is
/// returned.
///
/// This function does not evaluate the elements of the yielder, the
/// computation is performed when the yielder is later run.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5])
/// |> drop(up_to: 3)
/// |> to_list
/// // -> [4, 5]
/// ```
///
/// ```gleam
/// from_list([1, 2])
/// |> drop(up_to: 3)
/// |> to_list
/// // -> []
/// ```
///
pub fn drop(from yielder: Yielder(e), up_to desired: Int) -> Yielder(e) {
  fn() { drop_loop(yielder.continuation, desired) }
  |> Yielder
}

fn drop_loop(continuation: fn() -> Action(e), desired: Int) -> Action(e) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) ->
      case desired > 0 {
        True -> drop_loop(next, desired - 1)
        False -> Continue(e, next)
      }
  }
}

/// Creates an yielder from an existing yielder and a transformation function.
///
/// Each element in the new yielder will be the result of calling the given
/// function on the elements in the given yielder.
///
/// This function does not evaluate the elements of the yielder, the
/// computation is performed when the yielder is later run.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3])
/// |> map(fn(x) { x * 2 })
/// |> to_list
/// // -> [2, 4, 6]
/// ```
///
pub fn map(over yielder: Yielder(a), with f: fn(a) -> b) -> Yielder(b) {
  yielder.continuation
  |> map_loop(f)
  |> Yielder
}

fn map_loop(continuation: fn() -> Action(a), f: fn(a) -> b) -> fn() -> Action(b) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(e, continuation) -> Continue(f(e), map_loop(continuation, f))
    }
  }
}

/// Combines two yielders into a single one using the given function.
///
/// If an yielder is longer than the other the extra elements are dropped.
///
/// This function does not evaluate the elements of the two yielders, the
/// computation is performed when the resulting yielder is later run.
///
/// ## Examples
///
/// ```gleam
/// let first = from_list([1, 2, 3])
/// let second = from_list([4, 5, 6])
/// map2(first, second, fn(x, y) { x + y }) |> to_list
/// // -> [5, 7, 9]
/// ```
///
/// ```gleam
/// let first = from_list([1, 2])
/// let second = from_list(["a", "b", "c"])
/// map2(first, second, fn(i, x) { #(i, x) }) |> to_list
/// // -> [#(1, "a"), #(2, "b")]
/// ```
///
pub fn map2(
  yielder1: Yielder(a),
  yielder2: Yielder(b),
  with fun: fn(a, b) -> c,
) -> Yielder(c) {
  map2_loop(yielder1.continuation, yielder2.continuation, fun)
  |> Yielder
}

fn map2_loop(
  continuation1: fn() -> Action(a),
  continuation2: fn() -> Action(b),
  with fun: fn(a, b) -> c,
) -> fn() -> Action(c) {
  fn() {
    case continuation1() {
      Stop -> Stop
      Continue(a, next_a) ->
        case continuation2() {
          Stop -> Stop
          Continue(b, next_b) ->
            Continue(fun(a, b), map2_loop(next_a, next_b, fun))
        }
    }
  }
}

/// Appends two yielders, producing a new yielder.
///
/// This function does not evaluate the elements of the yielders, the
/// computation is performed when the resulting yielder is later run.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2])
/// |> append(from_list([3, 4]))
/// |> to_list
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn append(to first: Yielder(a), suffix second: Yielder(a)) -> Yielder(a) {
  fn() { append_loop(first.continuation, second.continuation) }
  |> Yielder
}

fn append_loop(first: fn() -> Action(a), second: fn() -> Action(a)) -> Action(a) {
  case first() {
    Continue(e, first) -> Continue(e, fn() { append_loop(first, second) })
    Stop -> second()
  }
}

/// Flattens an yielder of yielders, creating a new yielder.
///
/// This function does not evaluate the elements of the yielder, the
/// computation is performed when the yielder is later run.
///
/// ## Examples
///
/// ```gleam
/// from_list([[1, 2], [3, 4]])
/// |> map(from_list)
/// |> flatten
/// |> to_list
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn flatten(yielder: Yielder(Yielder(a))) -> Yielder(a) {
  fn() { flatten_loop(yielder.continuation) }
  |> Yielder
}

fn flatten_loop(flattened: fn() -> Action(Yielder(a))) -> Action(a) {
  case flattened() {
    Stop -> Stop
    Continue(it, next_yielder) ->
      append_loop(it.continuation, fn() { flatten_loop(next_yielder) })
  }
}

/// Joins a list of yielders into a single yielder.
///
/// This function does not evaluate the elements of the yielder, the
/// computation is performed when the yielder is later run.
///
/// ## Examples
///
/// ```gleam
/// [[1, 2], [3, 4]]
/// |> map(from_list)
/// |> concat
/// |> to_list
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn concat(yielders: List(Yielder(a))) -> Yielder(a) {
  flatten(from_list(yielders))
}

/// Creates an yielder from an existing yielder and a transformation function.
///
/// Each element in the new yielder will be the result of calling the given
/// function on the elements in the given yielder and then flattening the
/// results.
///
/// This function does not evaluate the elements of the yielder, the
/// computation is performed when the yielder is later run.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2])
/// |> flat_map(fn(x) { from_list([x, x + 1]) })
/// |> to_list
/// // -> [1, 2, 2, 3]
/// ```
///
pub fn flat_map(
  over yielder: Yielder(a),
  with f: fn(a) -> Yielder(b),
) -> Yielder(b) {
  yielder
  |> map(f)
  |> flatten
}

/// Creates an yielder from an existing yielder and a predicate function.
///
/// The new yielder will contain elements from the first yielder for which
/// the given function returns `True`.
///
/// This function does not evaluate the elements of the yielder, the
/// computation is performed when the yielder is later run.
///
/// ## Examples
///
/// ```gleam
/// import gleam/int
///
/// from_list([1, 2, 3, 4])
/// |> filter(int.is_even)
/// |> to_list
/// // -> [2, 4]
/// ```
///
pub fn filter(
  yielder: Yielder(a),
  keeping predicate: fn(a) -> Bool,
) -> Yielder(a) {
  fn() { filter_loop(yielder.continuation, predicate) }
  |> Yielder
}

fn filter_loop(
  continuation: fn() -> Action(e),
  predicate: fn(e) -> Bool,
) -> Action(e) {
  case continuation() {
    Stop -> Stop
    Continue(e, yielder) ->
      case predicate(e) {
        True -> Continue(e, fn() { filter_loop(yielder, predicate) })
        False -> filter_loop(yielder, predicate)
      }
  }
}

/// Creates an yielder from an existing yielder and a transforming predicate function.
///
/// The new yielder will contain elements from the first yielder for which
/// the given function returns `Ok`, transformed to the value inside the `Ok`.
///
/// This function does not evaluate the elements of the yielder, the
/// computation is performed when the yielder is later run.
///
/// ## Examples
///
/// ```gleam
/// import gleam/string
/// import gleam/int
///
/// "a1b2c3d4e5f"
/// |> string.to_graphemes
/// |> from_list
/// |> filter_map(int.parse)
/// |> to_list
/// // -> [1, 2, 3, 4, 5]
/// ```
///
pub fn filter_map(
  yielder: Yielder(a),
  keeping_with f: fn(a) -> Result(b, c),
) -> Yielder(b) {
  fn() { filter_map_loop(yielder.continuation, f) }
  |> Yielder
}

fn filter_map_loop(
  continuation: fn() -> Action(a),
  f: fn(a) -> Result(b, c),
) -> Action(b) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) ->
      case f(e) {
        Ok(e) -> Continue(e, fn() { filter_map_loop(next, f) })
        Error(_) -> filter_map_loop(next, f)
      }
  }
}

/// Creates an yielder that repeats a given yielder infinitely.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2])
/// |> cycle
/// |> take(6)
/// |> to_list
/// // -> [1, 2, 1, 2, 1, 2]
/// ```
///
pub fn cycle(yielder: Yielder(a)) -> Yielder(a) {
  repeat(yielder)
  |> flatten
}

/// Creates an yielder of ints, starting at a given start int and stepping by
/// one to a given end int.
///
/// ## Examples
///
/// ```gleam
/// range(from: 1, to: 5) |> to_list
/// // -> [1, 2, 3, 4, 5]
/// ```
///
/// ```gleam
/// range(from: 1, to: -2) |> to_list
/// // -> [1, 0, -1, -2]
/// ```
///
/// ```gleam
/// range(from: 0, to: 0) |> to_list
/// // -> [0]
/// ```
///
pub fn range(from start: Int, to stop: Int) -> Yielder(Int) {
  case int.compare(start, stop) {
    order.Eq -> once(fn() { start })
    order.Gt ->
      unfold(from: start, with: fn(current) {
        case current < stop {
          False -> Next(current, current - 1)
          True -> Done
        }
      })

    order.Lt ->
      unfold(from: start, with: fn(current) {
        case current > stop {
          False -> Next(current, current + 1)
          True -> Done
        }
      })
  }
}

/// Finds the first element in a given yielder for which the given function returns
/// `True`.
///
/// Returns `Error(Nil)` if the function does not return `True` for any of the
/// elements.
///
/// ## Examples
///
/// ```gleam
/// find(from_list([1, 2, 3]), fn(x) { x > 2 })
/// // -> Ok(3)
/// ```
///
/// ```gleam
/// find(from_list([1, 2, 3]), fn(x) { x > 4 })
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// find(empty(), fn(_) { True })
/// // -> Error(Nil)
/// ```
///
pub fn find(
  in haystack: Yielder(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(a, Nil) {
  haystack.continuation
  |> find_loop(is_desired)
}

fn find_loop(
  continuation: fn() -> Action(a),
  f: fn(a) -> Bool,
) -> Result(a, Nil) {
  case continuation() {
    Stop -> Error(Nil)
    Continue(e, next) ->
      case f(e) {
        True -> Ok(e)
        False -> find_loop(next, f)
      }
  }
}

/// Finds the first element in a given yielder
/// for which the given function returns `Ok(new_value)`,
/// then returns the wrapped `new_value`.
///
/// Returns `Error(Nil)` if no such element is found.
///
/// ## Examples
///
/// ```gleam
/// find_map(from_list(["a", "1", "2"]), int.parse)
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// find_map(from_list(["a", "b", "c"]), int.parse)
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// find_map(from_list([]), int.parse)
/// // -> Error(Nil)
/// ```
///
pub fn find_map(
  in haystack: Yielder(a),
  one_that is_desired: fn(a) -> Result(b, c),
) -> Result(b, Nil) {
  haystack.continuation
  |> find_map_loop(is_desired)
}

fn find_map_loop(
  continuation: fn() -> Action(a),
  f: fn(a) -> Result(b, c),
) -> Result(b, Nil) {
  case continuation() {
    Stop -> Error(Nil)
    Continue(e, next) ->
      case f(e) {
        Ok(e) -> Ok(e)
        Error(_) -> find_map_loop(next, f)
      }
  }
}

/// Wraps values yielded from an yielder with indices, starting from 0.
///
/// ## Examples
///
/// ```gleam
/// from_list(["a", "b", "c"]) |> index |> to_list
/// // -> [#("a", 0), #("b", 1), #("c", 2)]
/// ```
///
pub fn index(over yielder: Yielder(element)) -> Yielder(#(element, Int)) {
  yielder.continuation
  |> index_loop(0)
  |> Yielder
}

fn index_loop(
  continuation: fn() -> Action(element),
  next: Int,
) -> fn() -> Action(#(element, Int)) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(e, continuation) ->
        Continue(#(e, next), index_loop(continuation, next + 1))
    }
  }
}

/// Creates an yielder that infinitely applies a function to a value.
///
/// ## Examples
///
/// ```gleam
/// iterate(1, fn(n) { n * 3 }) |> take(5) |> to_list
/// // -> [1, 3, 9, 27, 81]
/// ```
///
pub fn iterate(
  from initial: element,
  with f: fn(element) -> element,
) -> Yielder(element) {
  unfold(initial, fn(element) { Next(element, f(element)) })
}

/// Creates an yielder that yields elements while the predicate returns `True`.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 2, 4])
/// |> take_while(satisfying: fn(x) { x < 3 })
/// |> to_list
/// // -> [1, 2]
/// ```
///
pub fn take_while(
  in yielder: Yielder(element),
  satisfying predicate: fn(element) -> Bool,
) -> Yielder(element) {
  yielder.continuation
  |> take_while_loop(predicate)
  |> Yielder
}

fn take_while_loop(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> fn() -> Action(element) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(e, next) ->
        case predicate(e) {
          False -> Stop
          True -> Continue(e, take_while_loop(next, predicate))
        }
    }
  }
}

/// Creates an yielder that drops elements while the predicate returns `True`,
/// and then yields the remaining elements.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4, 2, 5])
/// |> drop_while(satisfying: fn(x) { x < 4 })
/// |> to_list
/// // -> [4, 2, 5]
/// ```
///
pub fn drop_while(
  in yielder: Yielder(element),
  satisfying predicate: fn(element) -> Bool,
) -> Yielder(element) {
  fn() { drop_while_loop(yielder.continuation, predicate) }
  |> Yielder
}

fn drop_while_loop(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> Action(element) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) ->
      case predicate(e) {
        False -> Continue(e, next)
        True -> drop_while_loop(next, predicate)
      }
  }
}

/// Creates an yielder from an existing yielder and a stateful function.
///
/// Specifically, this behaves like `fold`, but yields intermediate results.
///
/// ## Examples
///
/// ```gleam
/// // Generate a sequence of partial sums
/// from_list([1, 2, 3, 4, 5])
/// |> scan(from: 0, with: fn(acc, el) { acc + el })
/// |> to_list
/// // -> [1, 3, 6, 10, 15]
/// ```
///
pub fn scan(
  over yielder: Yielder(element),
  from initial: acc,
  with f: fn(acc, element) -> acc,
) -> Yielder(acc) {
  yielder.continuation
  |> scan_loop(f, initial)
  |> Yielder
}

fn scan_loop(
  continuation: fn() -> Action(element),
  f: fn(acc, element) -> acc,
  accumulator: acc,
) -> fn() -> Action(acc) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(el, next) -> {
        let accumulated = f(accumulator, el)
        Continue(accumulated, scan_loop(next, f, accumulated))
      }
    }
  }
}

/// Zips two yielders together, emitting values from both
/// until the shorter one runs out.
///
/// ## Examples
///
/// ```gleam
/// from_list(["a", "b", "c"])
/// |> zip(range(20, 30))
/// |> to_list
/// // -> [#("a", 20), #("b", 21), #("c", 22)]
/// ```
///
pub fn zip(left: Yielder(a), right: Yielder(b)) -> Yielder(#(a, b)) {
  zip_loop(left.continuation, right.continuation)
  |> Yielder
}

fn zip_loop(
  left: fn() -> Action(a),
  right: fn() -> Action(b),
) -> fn() -> Action(#(a, b)) {
  fn() {
    case left() {
      Stop -> Stop
      Continue(el_left, next_left) ->
        case right() {
          Stop -> Stop
          Continue(el_right, next_right) ->
            Continue(#(el_left, el_right), zip_loop(next_left, next_right))
        }
    }
  }
}

// Result of collecting a single chunk by key
type Chunk(element, key) {
  AnotherBy(List(element), key, element, fn() -> Action(element))
  LastBy(List(element))
}

/// Creates an yielder that emits chunks of elements
/// for which `f` returns the same value.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 2, 3, 4, 4, 6, 7, 7])
/// |> chunk(by: fn(n) { n % 2 })
/// |> to_list
/// // -> [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
/// ```
///
pub fn chunk(
  over yielder: Yielder(element),
  by f: fn(element) -> key,
) -> Yielder(List(element)) {
  fn() {
    case yielder.continuation() {
      Stop -> Stop
      Continue(e, next) -> chunk_loop(next, f, f(e), e)
    }
  }
  |> Yielder
}

fn chunk_loop(
  continuation: fn() -> Action(element),
  f: fn(element) -> key,
  previous_key: key,
  previous_element: element,
) -> Action(List(element)) {
  case next_chunk(continuation, f, previous_key, [previous_element]) {
    LastBy(chunk) -> Continue(chunk, stop)
    AnotherBy(chunk, key, el, next) ->
      Continue(chunk, fn() { chunk_loop(next, f, key, el) })
  }
}

fn next_chunk(
  continuation: fn() -> Action(element),
  f: fn(element) -> key,
  previous_key: key,
  current_chunk: List(element),
) -> Chunk(element, key) {
  case continuation() {
    Stop -> LastBy(list.reverse(current_chunk))
    Continue(e, next) -> {
      let key = f(e)
      case key == previous_key {
        True -> next_chunk(next, f, key, [e, ..current_chunk])
        False -> AnotherBy(list.reverse(current_chunk), key, e, next)
      }
    }
  }
}

/// Creates an yielder that emits chunks of given size.
///
/// If the last chunk does not have `count` elements, it is yielded
/// as a partial chunk, with less than `count` elements.
///
/// For any `count` less than 1 this function behaves as if it was set to 1.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5, 6])
/// |> sized_chunk(into: 2)
/// |> to_list
/// // -> [[1, 2], [3, 4], [5, 6]]
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5, 6, 7, 8])
/// |> sized_chunk(into: 3)
/// |> to_list
/// // -> [[1, 2, 3], [4, 5, 6], [7, 8]]
/// ```
///
pub fn sized_chunk(
  over yielder: Yielder(element),
  into count: Int,
) -> Yielder(List(element)) {
  yielder.continuation
  |> sized_chunk_loop(count)
  |> Yielder
}

fn sized_chunk_loop(
  continuation: fn() -> Action(element),
  count: Int,
) -> fn() -> Action(List(element)) {
  fn() {
    case next_sized_chunk(continuation, count, []) {
      NoMore -> Stop
      Last(chunk) -> Continue(chunk, stop)
      Another(chunk, next_element) ->
        Continue(chunk, sized_chunk_loop(next_element, count))
    }
  }
}

// Result of collecting a single sized chunk
type SizedChunk(element) {
  Another(List(element), fn() -> Action(element))
  Last(List(element))
  NoMore
}

fn next_sized_chunk(
  continuation: fn() -> Action(element),
  left: Int,
  current_chunk: List(element),
) -> SizedChunk(element) {
  case continuation() {
    Stop ->
      case current_chunk {
        [] -> NoMore
        remaining -> Last(list.reverse(remaining))
      }
    Continue(e, next) -> {
      let chunk = [e, ..current_chunk]
      case left > 1 {
        False -> Another(list.reverse(chunk), next)
        True -> next_sized_chunk(next, left - 1, chunk)
      }
    }
  }
}

/// Creates an yielder that yields the given `elem` element
/// between elements emitted by the underlying yielder.
///
/// ## Examples
///
/// ```gleam
/// empty()
/// |> intersperse(with: 0)
/// |> to_list
/// // -> []
/// ```
///
/// ```gleam
/// from_list([1])
/// |> intersperse(with: 0)
/// |> to_list
/// // -> [1]
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5])
/// |> intersperse(with: 0)
/// |> to_list
/// // -> [1, 0, 2, 0, 3, 0, 4, 0, 5]
/// ```
///
pub fn intersperse(
  over yielder: Yielder(element),
  with elem: element,
) -> Yielder(element) {
  fn() {
    case yielder.continuation() {
      Stop -> Stop
      Continue(e, next) -> Continue(e, fn() { intersperse_loop(next, elem) })
    }
  }
  |> Yielder
}

fn intersperse_loop(
  continuation: fn() -> Action(element),
  separator: element,
) -> Action(element) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) -> {
      let next_interspersed = fn() { intersperse_loop(next, separator) }
      Continue(separator, fn() { Continue(e, next_interspersed) })
    }
  }
}

/// Returns `True` if any element emitted by the yielder satisfies the given predicate,
/// `False` otherwise.
///
/// This function short-circuits once it finds a satisfying element.
///
/// An empty yielder results in `False`.
///
/// ## Examples
///
/// ```gleam
/// empty()
/// |> any(fn(n) { n % 2 == 0 })
/// // -> False
/// ```
///
/// ```gleam
/// from_list([1, 2, 5, 7, 9])
/// |> any(fn(n) { n % 2 == 0 })
/// // -> True
/// ```
///
/// ```gleam
/// from_list([1, 3, 5, 7, 9])
/// |> any(fn(n) { n % 2 == 0 })
/// // -> False
/// ```
///
pub fn any(
  in yielder: Yielder(element),
  satisfying predicate: fn(element) -> Bool,
) -> Bool {
  yielder.continuation
  |> any_loop(predicate)
}

fn any_loop(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> Bool {
  case continuation() {
    Stop -> False
    Continue(e, next) ->
      case predicate(e) {
        True -> True
        False -> any_loop(next, predicate)
      }
  }
}

/// Returns `True` if all elements emitted by the yielder satisfy the given predicate,
/// `False` otherwise.
///
/// This function short-circuits once it finds a non-satisfying element.
///
/// An empty yielder results in `True`.
///
/// ## Examples
///
/// ```gleam
/// empty()
/// |> all(fn(n) { n % 2 == 0 })
/// // -> True
/// ```
///
/// ```gleam
/// from_list([2, 4, 6, 8])
/// |> all(fn(n) { n % 2 == 0 })
/// // -> True
/// ```
///
/// ```gleam
/// from_list([2, 4, 5, 8])
/// |> all(fn(n) { n % 2 == 0 })
/// // -> False
/// ```
///
pub fn all(
  in yielder: Yielder(element),
  satisfying predicate: fn(element) -> Bool,
) -> Bool {
  yielder.continuation
  |> all_loop(predicate)
}

fn all_loop(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> Bool {
  case continuation() {
    Stop -> True
    Continue(e, next) ->
      case predicate(e) {
        True -> all_loop(next, predicate)
        False -> False
      }
  }
}

/// Returns a `Dict(k, List(element))` of elements from the given yielder
/// grouped with the given key function.
///
/// The order within each group is preserved from the yielder.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5, 6])
/// |> group(by: fn(n) { n % 3 })
/// // -> dict.from_list([#(0, [3, 6]), #(1, [1, 4]), #(2, [2, 5])])
/// ```
///
pub fn group(
  in yielder: Yielder(element),
  by key: fn(element) -> key,
) -> Dict(key, List(element)) {
  yielder
  |> fold(dict.new(), group_updater(key))
  |> dict.map_values(fn(_, group) { list.reverse(group) })
}

fn group_updater(
  f: fn(element) -> key,
) -> fn(Dict(key, List(element)), element) -> Dict(key, List(element)) {
  fn(groups, elem) {
    groups
    |> dict.upsert(f(elem), update_group_with(elem))
  }
}

fn update_group_with(el: element) -> fn(Option(List(element))) -> List(element) {
  fn(maybe_group) {
    case maybe_group {
      Some(group) -> [el, ..group]
      None -> [el]
    }
  }
}

/// This function acts similar to fold, but does not take an initial state.
/// Instead, it starts from the first yielded element
/// and combines it with each subsequent element in turn using the given function.
/// The function is called as `f(accumulator, current_element)`.
///
/// Returns `Ok` to indicate a successful run, and `Error` if called on an empty yielder.
///
/// ## Examples
///
/// ```gleam
/// from_list([])
/// |> reduce(fn(acc, x) { acc + x })
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5])
/// |> reduce(fn(acc, x) { acc + x })
/// // -> Ok(15)
/// ```
///
pub fn reduce(over yielder: Yielder(e), with f: fn(e, e) -> e) -> Result(e, Nil) {
  case yielder.continuation() {
    Stop -> Error(Nil)
    Continue(e, next) ->
      fold_loop(next, f, e)
      |> Ok
  }
}

/// Returns the last element in the given yielder.
///
/// Returns `Error(Nil)` if the yielder is empty.
///
/// This function runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// empty() |> last
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// range(1, 10) |> last
/// // -> Ok(10)
/// ```
///
pub fn last(yielder: Yielder(element)) -> Result(element, Nil) {
  yielder
  |> reduce(fn(_, elem) { elem })
}

/// Creates an yielder that yields no elements.
///
/// ## Examples
///
/// ```gleam
/// empty() |> to_list
/// // -> []
/// ```
///
pub fn empty() -> Yielder(element) {
  Yielder(stop)
}

/// Creates an yielder that yields exactly one element provided by calling the given function.
///
/// ## Examples
///
/// ```gleam
/// once(fn() { 1 }) |> to_list
/// // -> [1]
/// ```
///
pub fn once(f: fn() -> element) -> Yielder(element) {
  fn() { Continue(f(), stop) }
  |> Yielder
}

/// Creates an yielder that yields the given element exactly once.
///
/// ## Examples
///
/// ```gleam
/// single(1) |> to_list
/// // -> [1]
/// ```
///
pub fn single(elem: element) -> Yielder(element) {
  once(fn() { elem })
}

/// Creates an yielder that alternates between the two given yielders
/// until both have run out.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4])
/// |> interleave(from_list([11, 12, 13, 14]))
/// |> to_list
/// // -> [1, 11, 2, 12, 3, 13, 4, 14]
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4])
/// |> interleave(from_list([100]))
/// |> to_list
/// // -> [1, 100, 2, 3, 4]
/// ```
///
pub fn interleave(
  left: Yielder(element),
  with right: Yielder(element),
) -> Yielder(element) {
  fn() { interleave_loop(left.continuation, right.continuation) }
  |> Yielder
}

fn interleave_loop(
  current: fn() -> Action(element),
  next: fn() -> Action(element),
) -> Action(element) {
  case current() {
    Stop -> next()
    Continue(e, next_other) ->
      Continue(e, fn() { interleave_loop(next, next_other) })
  }
}

/// Like `fold`, `fold_until` reduces an yielder of elements into a single value by calling a given
/// function on each element in turn, but uses `list.ContinueOrStop` to determine
/// whether or not to keep iterating.
///
/// If called on an yielder of infinite length then this function will only ever
/// return if the function returns `list.Stop`.
///
/// ## Examples
///
/// ```gleam
/// import gleam/list
///
/// let f = fn(acc, e) {
///   case e {
///     _ if e < 4 -> list.Continue(e + acc)
///     _ -> list.Stop(acc)
///   }
/// }
///
/// from_list([1, 2, 3, 4])
/// |> fold_until(from: 0, with: f)
/// // -> 6
/// ```
///
pub fn fold_until(
  over yielder: Yielder(e),
  from initial: acc,
  with f: fn(acc, e) -> list.ContinueOrStop(acc),
) -> acc {
  yielder.continuation
  |> fold_until_loop(f, initial)
}

fn fold_until_loop(
  continuation: fn() -> Action(e),
  f: fn(acc, e) -> list.ContinueOrStop(acc),
  accumulator: acc,
) -> acc {
  case continuation() {
    Stop -> accumulator
    Continue(elem, next) ->
      case f(accumulator, elem) {
        list.Continue(accumulator) -> fold_until_loop(next, f, accumulator)
        list.Stop(accumulator) -> accumulator
      }
  }
}

/// A variant of fold that might fail.
///
/// The folding function should return `Result(accumulator, error)`.
/// If the returned value is `Ok(accumulator)` try_fold will try the next value in the yielder.
/// If the returned value is `Error(error)` try_fold will stop and return that error.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4])
/// |> try_fold(0, fn(acc, i) {
///   case i < 3 {
///     True -> Ok(acc + i)
///     False -> Error(Nil)
///   }
/// })
/// // -> Error(Nil)
/// ```
///
pub fn try_fold(
  over yielder: Yielder(e),
  from initial: acc,
  with f: fn(acc, e) -> Result(acc, err),
) -> Result(acc, err) {
  yielder.continuation
  |> try_fold_loop(f, initial)
}

fn try_fold_loop(
  over continuation: fn() -> Action(a),
  with f: fn(acc, a) -> Result(acc, err),
  from accumulator: acc,
) -> Result(acc, err) {
  case continuation() {
    Stop -> Ok(accumulator)
    Continue(elem, next) -> {
      case f(accumulator, elem) {
        Ok(result) -> try_fold_loop(next, f, result)
        Error(_) as error -> error
      }
    }
  }
}

/// Returns the first element yielded by the given yielder, if it exists,
/// or `Error(Nil)` otherwise.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3]) |> first
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// empty() |> first
/// // -> Error(Nil)
/// ```
pub fn first(from yielder: Yielder(e)) -> Result(e, Nil) {
  case yielder.continuation() {
    Stop -> Error(Nil)
    Continue(e, _) -> Ok(e)
  }
}

/// Returns nth element yielded by the given yielder, where `0` means the first element.
///
/// If there are not enough elements in the yielder, `Error(Nil)` is returned.
///
/// For any `index` less than `0` this function behaves as if it was set to `0`.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4]) |> at(2)
/// // -> Ok(3)
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4]) |> at(4)
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// empty() |> at(0)
/// // -> Error(Nil)
/// ```
///
pub fn at(in yielder: Yielder(e), get index: Int) -> Result(e, Nil) {
  yielder
  |> drop(index)
  |> first
}

/// Counts the number of elements in the given yielder.
///
/// This function has to traverse the entire yielder to count its elements,
/// so it runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// empty() |> length
/// // -> 0
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4]) |> length
/// // -> 4
/// ```
///
pub fn length(over yielder: Yielder(e)) -> Int {
  yielder.continuation
  |> length_loop(0)
}

fn length_loop(over continuation: fn() -> Action(e), with length: Int) -> Int {
  case continuation() {
    Stop -> length
    Continue(_, next) -> length_loop(next, length + 1)
  }
}

/// Traverse an yielder, calling a function on each element.
///
/// ## Examples
///
/// ```gleam
/// empty() |> each(io.println)
/// // -> Nil
/// ```
///
/// ```gleam
/// from_list(["Tom", "Malory", "Louis"]) |> each(io.println)
/// // -> Nil
/// // Tom
/// // Malory
/// // Louis
/// ```
///
pub fn each(over yielder: Yielder(a), with f: fn(a) -> b) -> Nil {
  yielder
  |> map(f)
  |> run
}

/// Add a new element to the start of an yielder.
///
/// This function is for use with `use` expressions, to replicate the behaviour
/// of the `yield` keyword found in other languages.
///
/// If you only need to prepend an element and don't require the `use` syntax,
/// use `prepend`.
///
/// ## Examples
///
/// ```gleam
/// let yielder = {
///   use <- yield(1)
///   use <- yield(2)
///   use <- yield(3)
///   empty()
/// }
///
/// yielder |> to_list
/// // -> [1, 2, 3]
/// ```
///
pub fn yield(element: a, next: fn() -> Yielder(a)) -> Yielder(a) {
  Yielder(fn() { Continue(element, fn() { next().continuation() }) })
}

/// Add a new element to the start of an yielder.
///
/// ## Examples
///
/// ```gleam
/// let yielder = from_list([1, 2, 3]) |> prepend(0)
///
/// yielder.to_list
/// // -> [0, 1, 2, 3]
/// ```
///
pub fn prepend(yielder: Yielder(a), element: a) -> Yielder(a) {
  use <- yield(element)
  yielder
}
