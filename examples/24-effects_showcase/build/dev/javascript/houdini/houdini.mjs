import * as $escape from "./houdini/internal/escape_js.mjs";

/**
 * Escapes a string to be safely used inside an HTML document by escaping
 * the following characters:
 *   - `<` becomes `&lt;`
 *   - `>` becomes `&gt;`
 *   - `&` becomes `&amp;`
 *   - `"` becomes `&quot;`
 *   - `'` becomes `&#39;`.
 *
 * ## Examples
 *
 * ```gleam
 * assert escape("wibble & wobble") == "wibble &amp; wobble"
 * assert escape("wibble > wobble") == "wibble &gt; wobble"
 * ```
 */
export function escape(string) {
  return $escape.escape(string);
}
