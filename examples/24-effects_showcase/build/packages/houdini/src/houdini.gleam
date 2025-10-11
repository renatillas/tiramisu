@target(javascript)
import houdini/internal/escape_js as escape

@target(erlang)
import houdini/internal/escape_erl as escape

/// Escapes a string to be safely used inside an HTML document by escaping
/// the following characters:
///   - `<` becomes `&lt;`
///   - `>` becomes `&gt;`
///   - `&` becomes `&amp;`
///   - `"` becomes `&quot;`
///   - `'` becomes `&#39;`.
///
/// ## Examples
///
/// ```gleam
/// assert escape("wibble & wobble") == "wibble &amp; wobble"
/// assert escape("wibble > wobble") == "wibble &gt; wobble"
/// ```
///
pub fn escape(string: String) -> String {
  escape.escape(string)
}
