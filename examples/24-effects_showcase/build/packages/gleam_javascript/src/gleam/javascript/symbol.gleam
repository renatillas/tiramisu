/// Symbols are special unique values in JavaScript.
///
/// For further information view the MDN documentation: 
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol>
///
pub type Symbol

/// Creates a new symbol with the given description.
///
/// Symbols created with this function are "local", they are not in the global
/// symbol registry.
///
/// For further information see the MDN documentation:
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/Symbol>
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "new_symbol")
pub fn new(description: String) -> Symbol

/// Returns the symbol for the given key from the global symbol registry,
/// creating and registering a new one if one did not already exist.
///
/// Uses the JavaScript `Symbol.for` internally.
///
/// For further information see the MDN documentation:
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/for>
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "get_symbol")
pub fn get_or_create_global(key: String) -> Symbol

/// Get the description of the symbol, if it has one.
///
/// # Examples
///
/// ```gleam
/// symbol.new("wibble")
/// |> symbol.description
/// // -> Ok("wibble")
/// ```
///
@external(javascript, "../../gleam_javascript_ffi.mjs", "symbol_description")
pub fn description(symbol: Symbol) -> Result(String, Nil)
