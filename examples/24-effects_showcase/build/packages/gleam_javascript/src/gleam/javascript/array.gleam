/// A JavaScript array, in Gleam!
///
/// Unlike most data structures in Gleam this one is mutable.
///
pub type Array(element)

/// Convert a JavaScript array to a Gleam list.
///
/// Runs in linear time.
///
pub fn to_list(items: Array(element)) -> List(element) {
  fold_right(over: items, from: [], with: fn(list, item) { [item, ..list] })
}

/// Convert a Gleam list to a JavaScript array.
///
/// Runs in linear time.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "toArray")
pub fn from_list(a: List(element)) -> Array(element)

/// Get the number of elements in the array.
///
/// Runs in constant time.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "length")
pub fn size(a: Array(element)) -> Int

/// Returns a new array containing only the elements of the first array after
/// the function has been applied to each one.
///
/// Runs in linear time.
///
/// # Examples
///
/// ```gleam
/// > map(from_list([2, 4, 6]), fn(x) { x * 2 })
/// from_list([4, 8, 12])
/// ```
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "map")
pub fn map(a: Array(a), with with: fn(a) -> b) -> Array(b)

/// Reduces a list of elements into a single value by calling a given function
/// on each element, going from left to right.
///
/// `fold(from_list([1, 2, 3]), 0, add)` is the equivalent of
/// `add(add(add(0, 1), 2), 3)`.
///
/// Runs in linear time.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "reduce")
pub fn fold(over over: Array(e), from from: a, with with: fn(a, e) -> a) -> a

/// Reduces a list of elements into a single value by calling a given function
/// on each element, going from right to left.
///
/// `fold_right(from_list([1, 2, 3]), 0, add)` is the equivalent of
/// `add(add(add(0, 3), 2), 1)`.
///
/// Runs in linear time.
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "reduceRight")
pub fn fold_right(
  over over: Array(e),
  from from: a,
  with with: fn(a, e) -> a,
) -> a

/// Get the element at the given index.
///
/// # Examples
///
/// ```gleam
/// > get(from_list([2, 4, 6]), 1)
/// Ok(4)
/// ```
///
/// ```gleam
/// > get(from_list([2, 4, 6]), 4)
/// Error(Nil)
/// ```
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "index")
pub fn get(a: Array(e), b: Int) -> Result(e, Nil)
