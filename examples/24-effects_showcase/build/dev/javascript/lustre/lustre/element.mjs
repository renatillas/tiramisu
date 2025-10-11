import * as $function from "../../gleam_stdlib/gleam/function.mjs";
import { identity as coerce } from "../../gleam_stdlib/gleam/function.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $string_tree from "../../gleam_stdlib/gleam/string_tree.mjs";
import { toList } from "../gleam.mjs";
import * as $attribute from "../lustre/attribute.mjs";
import * as $mutable_map from "../lustre/internals/mutable_map.mjs";
import * as $events from "../lustre/vdom/events.mjs";
import * as $vnode from "../lustre/vdom/vnode.mjs";
import { Element, Fragment, Text, UnsafeInnerHtml } from "../lustre/vdom/vnode.mjs";

/**
 * A general function for constructing any kind of element. In most cases you
 * will want to use the [`lustre/element/html`](./element/html.html) instead but this
 * function is particularly handy when constructing custom elements, either
 * from your own Lustre components or from external JavaScript libraries.
 *
 * > **Note**: Because Lustre is primarily used to create HTML, this function
 * > special-cases the following tags which render as
 * > [void elements](https://developer.mozilla.org/en-US/docs/Glossary/Void_element):
 * >
 * >   - area
 * >   - base
 * >   - br
 * >   - col
 * >   - embed
 * >   - hr
 * >   - img
 * >   - input
 * >   - link
 * >   - meta
 * >   - param
 * >   - source
 * >   - track
 * >   - wbr
 * >
 * > This will only affect the output of `to_string` and `to_string_builder`!
 * > If you need to render any of these tags with children, *or* you want to
 * > render some other tag as self-closing or void, use [`advanced`](#advanced)
 * > to construct the element instead.
 */
export function element(tag, attributes, children) {
  return $vnode.element(
    "",
    $function.identity,
    "",
    tag,
    attributes,
    children,
    $mutable_map.new$(),
    false,
    false,
  );
}

/**
 * A function for constructing elements in a specific XML namespace. This can
 * be used to construct SVG or MathML elements, for example.
 */
export function namespaced(namespace, tag, attributes, children) {
  return $vnode.element(
    "",
    $function.identity,
    namespace,
    tag,
    attributes,
    children,
    $mutable_map.new$(),
    false,
    false,
  );
}

/**
 * A function for constructing elements with more control over how the element
 * is rendered when converted to a string. This is necessary because some HTML,
 * SVG, and MathML elements are self-closing or void elements, and Lustre needs
 * to know how to render them correctly!
 */
export function advanced(
  namespace,
  tag,
  attributes,
  children,
  self_closing,
  void$
) {
  return $vnode.element(
    "",
    $function.identity,
    namespace,
    tag,
    attributes,
    children,
    $mutable_map.new$(),
    self_closing,
    void$,
  );
}

/**
 * A function for turning a Gleam string into a text node. Gleam doesn't have
 * union types like some other languages you may be familiar with, like TypeScript.
 * Instead, we need a way to take a `String` and turn it into an `Element` somehow:
 * this function is exactly that!
 */
export function text(content) {
  return $vnode.text("", $function.identity, content);
}

/**
 * A function for rendering nothing. This is mostly useful for conditional
 * rendering, where you might want to render something only if a certain
 * condition is met.
 */
export function none() {
  return $vnode.text("", $function.identity, "");
}

/**
 * A function for constructing a wrapper element with no tag name. This is
 * useful for wrapping a list of elements together without adding an extra
 * `<div>` or other container element, or returning multiple elements in places
 * where only one `Element` is expected.
 */
export function fragment(children) {
  return $vnode.fragment("", $function.identity, children, $mutable_map.new$());
}

/**
 * A function for constructing a wrapper element with custom raw HTML as its
 * content. Lustre will render the provided HTML verbatim, and will not touch
 * its children except when replacing the entire inner html on changes.
 *
 * > **Note:** The provided HTML will not be escaped automatically and may expose
 * > your applications to XSS attacks! Make sure you absolutely trust the HTML you
 * > pass to this function. In particular, never use this to display un-sanitised
 * > user HTML!
 */
export function unsafe_raw_html(namespace, tag, attributes, inner_html) {
  return $vnode.unsafe_inner_html(
    "",
    $function.identity,
    namespace,
    tag,
    attributes,
    inner_html,
  );
}

/**
 * The `Element` type is parameterised by the type of messages it can produce
 * from events. Sometimes you might end up with a fragment of HTML from another
 * library or module that produces a different type of message: this function lets
 * you map the messages produced from one type to another.
 *
 * Think of it like `list.map` or `result.map` but for HTML events!
 */
export function map(element, f) {
  let mapper = coerce($events.compose_mapper(coerce(f), element.mapper));
  if (element instanceof Fragment) {
    let children = element.children;
    let keyed_children = element.keyed_children;
    return new Fragment(
      element.kind,
      element.key,
      mapper,
      coerce(children),
      coerce(keyed_children),
    );
  } else if (element instanceof Element) {
    let attributes = element.attributes;
    let children = element.children;
    let keyed_children = element.keyed_children;
    return new Element(
      element.kind,
      element.key,
      mapper,
      element.namespace,
      element.tag,
      coerce(attributes),
      coerce(children),
      coerce(keyed_children),
      element.self_closing,
      element.void,
    );
  } else if (element instanceof Text) {
    return coerce(element);
  } else {
    let attributes = element.attributes;
    return new UnsafeInnerHtml(
      element.kind,
      element.key,
      mapper,
      element.namespace,
      element.tag,
      coerce(attributes),
      element.inner_html,
    );
  }
}

/**
 * Convert a Lustre `Element` to a string. This is _not_ pretty-printed, so
 * there are no newlines or indentation. If you need to pretty-print an element,
 * reach out on the [Gleam Discord](https://discord.gg/Fm8Pwmy) or
 * [open an issue](https://github.com/lustre-labs/lustre/issues/new) with your
 * use case and we'll see what we can do!
 */
export function to_string(element) {
  return $vnode.to_string(element);
}

/**
 * Converts an element to a string like [`to_string`](#to_string), but prepends
 * a `<!doctype html>` declaration to the string. This is useful for rendering
 * complete HTML documents.
 *
 * If the provided element is not an `html` element, it will be wrapped in both
 * a `html` and `body` element.
 */
export function to_document_string(el) {
  let _pipe = $vnode.to_string(
    (() => {
      if (el instanceof Element) {
        let $ = el.tag;
        if ($ === "html") {
          return el;
        } else if ($ === "head") {
          return element("html", toList([]), toList([el]));
        } else if ($ === "body") {
          return element("html", toList([]), toList([el]));
        } else {
          return element(
            "html",
            toList([]),
            toList([element("body", toList([]), toList([el]))]),
          );
        }
      } else {
        return element(
          "html",
          toList([]),
          toList([element("body", toList([]), toList([el]))]),
        );
      }
    })(),
  );
  return ((_capture) => { return $string.append("<!doctype html>\n", _capture); })(
    _pipe,
  );
}

/**
 * Convert a Lustre `Element` to a `StringTree`. This is _not_ pretty-printed,
 * so there are no newlines or indentation. If you need to pretty-print an element,
 * reach out on the [Gleam Discord](https://discord.gg/Fm8Pwmy) or
 * [open an issue](https://github.com/lustre-labs/lustre/issues/new) with your
 * use case and we'll see what we can do!
 */
export function to_string_tree(element) {
  return $vnode.to_string_tree(element);
}

/**
 * Converts an element to a `StringTree` like [`to_string_builder`](#to_string_builder),
 * but prepends a `<!doctype html>` declaration. This is useful for rendering
 * complete HTML documents.
 *
 * If the provided element is not an `html` element, it will be wrapped in both
 * a `html` and `body` element.
 */
export function to_document_string_tree(el) {
  let _pipe = $vnode.to_string_tree(
    (() => {
      if (el instanceof Element) {
        let $ = el.tag;
        if ($ === "html") {
          return el;
        } else if ($ === "head") {
          return element("html", toList([]), toList([el]));
        } else if ($ === "body") {
          return element("html", toList([]), toList([el]));
        } else {
          return element(
            "html",
            toList([]),
            toList([element("body", toList([]), toList([el]))]),
          );
        }
      } else {
        return element(
          "html",
          toList([]),
          toList([element("body", toList([]), toList([el]))]),
        );
      }
    })(),
  );
  return $string_tree.prepend(_pipe, "<!doctype html>\n");
}

/**
 * Converts a Lustre `Element` to a human-readable string by inserting new lines
 * and indentation where appropriate. This is useful for debugging and testing,
 * but for production code you should use [`to_string`](#to_string) or
 * [`to_document_string`](#to_document_string) instead.
 *
 * 💡 This function works great with the snapshot testing library
 *    [birdie](https://hexdocs.pm/birdie)!
 *
 * ## Using `to_string`:
 *
 * ```html
 * <header><h1>Hello, world!</h1></header>
 * ```
 *
 * ## Using `to_readable_string`
 *
 * ```html
 * <header>
 *   <h1>
 *     Hello, world!
 *   </h1>
 * </header>
 * ```
 */
export function to_readable_string(el) {
  return $vnode.to_snapshot(el);
}
