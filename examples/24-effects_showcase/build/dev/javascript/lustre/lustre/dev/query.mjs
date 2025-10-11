import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../../gleam_stdlib/gleam/order.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "../../gleam.mjs";
import * as $attribute from "../../lustre/attribute.mjs";
import * as $element from "../../lustre/element.mjs";
import * as $path from "../../lustre/vdom/path.mjs";
import * as $vattr from "../../lustre/vdom/vattr.mjs";
import { Attribute } from "../../lustre/vdom/vattr.mjs";
import * as $vnode from "../../lustre/vdom/vnode.mjs";
import { Element, Fragment, Text, UnsafeInnerHtml } from "../../lustre/vdom/vnode.mjs";

const FILEPATH = "src/lustre/dev/query.gleam";

class FindElement extends $CustomType {
  constructor(matching) {
    super();
    this.matching = matching;
  }
}

class FindChild extends $CustomType {
  constructor(of, matching) {
    super();
    this.of = of;
    this.matching = matching;
  }
}

class FindDescendant extends $CustomType {
  constructor(of, matching) {
    super();
    this.of = of;
    this.matching = matching;
  }
}

class All extends $CustomType {
  constructor(of) {
    super();
    this.of = of;
  }
}

class Type extends $CustomType {
  constructor(namespace, tag) {
    super();
    this.namespace = namespace;
    this.tag = tag;
  }
}

class HasAttribute extends $CustomType {
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
}

class HasClass extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

class HasStyle extends $CustomType {
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
}

class Contains extends $CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
}

/**
 * Find any elements in a view that match the given [`Selector`](#Selector).
 */
export function element(selector) {
  return new FindElement(selector);
}

/**
 * Given a `Query` that finds an element, find any of that element's _direct_
 * children that match the given [`Selector`](#Selector). This is similar to the
 * CSS `>` combinator.
 */
export function child(parent, selector) {
  return new FindChild(parent, selector);
}

/**
 * Given a `Query` that finds an element, find any of that element's _descendants_
 * that match the given [`Selector`](#Selector). This will walk the entire tree
 * from the matching parent.
 */
export function descendant(parent, selector) {
  return new FindDescendant(parent, selector);
}

/**
 * Combine two selectors into one that must match both. For example, if you have
 * a selector for div elements and a selector for elements with the class "wibble"
 * then they can be combined into a selector that matches only div elements with
 * the class "wibble".
 *
 * ```gleam
 * import lustre/dev/query
 *
 * pub fn example() {
 *   let div = query.tag("div")
 *   let wibble = query.class("wibble")
 *
 *   query.element(matching: div |> query.and(wibble))
 * }
 * ```
 *
 * You can chain multiple `and` calls together to combine many selectors into
 * something more specific.
 *
 * ```gleam
 * import lustre/dev/query
 *
 * pub fn example() {
 *   query.tag("div")
 *   |> query.and(query.class("wibble"))
 *   |> query.and(query.data("open", "true"))
 * }
 * ```
 *
 * > **Note**: if you find yourself crafting complex selectors, consider using
 * > a test id on the element(s) you want to find instead.
 */
export function and(first, second) {
  if (first instanceof All) {
    let $ = first.of;
    if ($ instanceof $Empty) {
      return new All(toList([second]));
    } else {
      let others = $;
      return new All(listPrepend(second, others));
    }
  } else {
    return new All(toList([first, second]));
  }
}

/**
 * Select elements based on their tag name, like `"div"`, `"span"`, or `"a"`.
 * To select elements with an XML namespace - such as SVG elements - use the
 * [`namespaced`](#namespaced) selector instead.
 */
export function tag(value) {
  return new Type("", value);
}

/**
 * Select elements based on their tag name and XML namespace. This is useful
 * for selecting SVG elements or other XML elements that have a namespace.
 * For example, to select an SVG circle element, you would use:
 *
 * ```gleam
 * import lustre/dev/query
 *
 * pub fn example() {
 *   let svg = "http://www.w3.org/2000/svg"
 *
 *   query.element(matching: query.namespaced(svg, "circle"))
 * }
 * ```
 */
export function namespaced(namespace, tag) {
  return new Type(namespace, tag);
}

/**
 * Select elements that have the specified attribute with the given value. If
 * the value is left blank, this selector will match any element that has the
 * attribute, _regardless of its value_.
 *
 * For example, to select a form input with the name "username", you would
 * use:
 *
 * ```gleam
 * import lustre/dev/query
 *
 * pub fn example() {
 *   query.element(matching: query.attribute("name", "username"))
 * }
 * ```
 *
 * Or to select elements with the `disabled` attribute:
 *
 * ```gleam
 * import lustre/dev/query
 *
 * pub fn example() {
 *   query.element(matching: query.attribute("disabled", ""))
 * }
 * ```
 */
export function attribute(name, value) {
  return new HasAttribute(name, value);
}

/**
 * Select elements that include the given space-separated class name(s). For
 * example given the element `<div class="foo bar baz">`, the following selectors
 * would match:
 *
 * - `query.class("foo")`
 *
 * - `query.class("bar baz")`
 *
 * If you need to match the class attribute exactly, you can use the [`attribute`](#attribute)
 * selector instead.
 */
export function class$(name) {
  return new HasClass(name);
}

/**
 * Select elements that have the specified inline style with the given value.
 * If the value is left blank, this selector will match any element that has
 * the given style, _regardless of its value_.
 */
export function style(name, value) {
  return new HasStyle(name, value);
}

/**
 * Select an element based on its `id` attribute. Well-formed HTML means that
 * only one element should have a given id.
 */
export function id(name) {
  return new HasAttribute("id", name);
}

/**
 * Select elements that have the given `data-*` attribute. For example you can
 * select a custom disclosure element that is currently open with:
 *
 * ```gleam
 * import lustre/dev/query
 *
 * pub fn example() {
 *   query.element(matching: query.data("open", "true"))
 * }
 * ```
 */
export function data(name, value) {
  return new HasAttribute("data-" + name, value);
}

/**
 * It is a common convention to use the `data-test-id` attribute to mark elements
 * for easy selection in tests. This function is a shorthand for writing
 * `query.data("test-id", value)`
 */
export function test_id(value) {
  return data("test-id", value);
}

/**
 * Select elements that have the given `aria-*` attribute. For example you can
 * select the trigger of a dropdown menu with:
 *
 * ```gleam
 * import lustre/dev/query
 *
 * pub fn example() {
 *   query.element(matching: query.aria("expanded", "true"))
 * }
 * ```
 */
export function aria(name, value) {
  return new HasAttribute("aria-" + name, value);
}

/**
 * Select elements whose text content matches the given string exactly. This
 * includes text from **inline** children, but not from **block** children. For
 * example, given the following HTML:
 *
 * ```html
 * <p>Hello, <span class="font-bold">Joe</span>!</p>
 * ```
 *
 * The selector `query.text("Hello, Joe!")` would match the `<p>` element because
 * the text content of the inline `<span>` element is included in the paragraph's
 * text content.
 *
 * Whitespace must match exactly, so the selector `query.text("Hello, Joe!")`
 * would not match an element like:
 *
 * ```gleam
 * html.p([], [html.text("Hello,     Joe!")])
 * ```
 *
 * > **Note**: while this selector makes a best-effort attempt to include the
 * > text content of inline children, this cannot account for block elements that
 * > are styled as inline by CSS stylesheets.
 *
 * > **Note**: often it is better to use more precise selectors such as
 * > [`id`](#id), [`class`](#class), or [`test_id`](#test_id). You should reach
 * > for this selector only when you want to assert that an element contains
 * > some specific text, such as in a hero banner or a copyright notice.
 */
export function text(content) {
  return new Contains(content);
}

function text_content(element, inline, content) {
  if (element instanceof Fragment) {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element) {
    if (!inline || (element.namespace !== "")) {
      return $list.fold(
        element.children,
        content,
        (content, child) => { return text_content(child, true, content); },
      );
    } else {
      let $ = element.tag;
      if ($ === "a") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "abbr") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "acronym") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "b") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "bdo") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "big") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "br") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "button") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "cite") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "code") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "dfn") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "em") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "i") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "img") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "input") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "kbd") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "label") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "map") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "object") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "output") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "q") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "samp") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "script") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "select") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "small") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "span") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "strong") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "sub") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "sup") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "textarea") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "time") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "tt") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else if ($ === "var") {
        return $list.fold(
          element.children,
          content,
          (content, child) => { return text_content(child, true, content); },
        );
      } else {
        let rule = "display:inline";
        let is_inline = $list.any(
          element.attributes,
          (attribute) => {
            if (attribute instanceof Attribute) {
              let $1 = attribute.name;
              if ($1 === "style") {
                let value = attribute.value;
                return $string.contains(value, rule);
              } else {
                return false;
              }
            } else {
              return false;
            }
          },
        );
        if (is_inline) {
          return $list.fold(
            element.children,
            content,
            (content, child) => { return text_content(child, true, content); },
          );
        } else {
          return content;
        }
      }
    }
  } else if (element instanceof Text) {
    return content + element.content;
  } else {
    return content;
  }
}

/**
 * Check if the given target element matches the given [`Selector`](#Selector).
 */
export function matches(element, selector) {
  if (selector instanceof All) {
    let selectors = selector.of;
    return $list.all(
      selectors,
      (_capture) => { return matches(element, _capture); },
    );
  } else if (selector instanceof Type) {
    if (element instanceof Element) {
      let namespace = element.namespace;
      let tag$1 = element.tag;
      return (namespace === selector.namespace) && (tag$1 === selector.tag);
    } else if (element instanceof UnsafeInnerHtml) {
      let namespace = element.namespace;
      let tag$1 = element.tag;
      return (namespace === selector.namespace) && (tag$1 === selector.tag);
    } else {
      return false;
    }
  } else if (selector instanceof HasAttribute) {
    if (element instanceof Element) {
      let $ = selector.value;
      if ($ === "") {
        let name = selector.name;
        let attributes = element.attributes;
        return $list.any(
          attributes,
          (attribute) => {
            if (attribute instanceof Attribute) {
              return attribute.name === name;
            } else {
              return false;
            }
          },
        );
      } else {
        let name = selector.name;
        let value = $;
        let attributes = element.attributes;
        return $list.contains(attributes, $attribute.attribute(name, value));
      }
    } else if (element instanceof UnsafeInnerHtml) {
      let $ = selector.value;
      if ($ === "") {
        let name = selector.name;
        let attributes = element.attributes;
        return $list.any(
          attributes,
          (attribute) => {
            if (attribute instanceof Attribute) {
              return attribute.name === name;
            } else {
              return false;
            }
          },
        );
      } else {
        let name = selector.name;
        let value = $;
        let attributes = element.attributes;
        return $list.contains(attributes, $attribute.attribute(name, value));
      }
    } else {
      return false;
    }
  } else if (selector instanceof HasClass) {
    if (element instanceof Element) {
      let name = selector.name;
      let attributes = element.attributes;
      return $list.fold_until(
        $string.split(name, " "),
        true,
        (_, class$) => {
          let name$1 = $string.trim_end(class$);
          let matches$1 = $list.any(
            attributes,
            (attribute) => {
              if (attribute instanceof Attribute) {
                let $ = attribute.name;
                if ($ === "class") {
                  let value = attribute.value;
                  return (((value === name$1) || $string.starts_with(
                    value,
                    name$1 + " ",
                  )) || $string.ends_with(value, " " + name$1)) || $string.contains(
                    value,
                    (" " + name$1) + " ",
                  );
                } else {
                  return false;
                }
              } else {
                return false;
              }
            },
          );
          if (matches$1) {
            return new $list.Continue(true);
          } else {
            return new $list.Stop(false);
          }
        },
      );
    } else if (element instanceof UnsafeInnerHtml) {
      let name = selector.name;
      let attributes = element.attributes;
      return $list.fold_until(
        $string.split(name, " "),
        true,
        (_, class$) => {
          let name$1 = $string.trim_end(class$);
          let matches$1 = $list.any(
            attributes,
            (attribute) => {
              if (attribute instanceof Attribute) {
                let $ = attribute.name;
                if ($ === "class") {
                  let value = attribute.value;
                  return (((value === name$1) || $string.starts_with(
                    value,
                    name$1 + " ",
                  )) || $string.ends_with(value, " " + name$1)) || $string.contains(
                    value,
                    (" " + name$1) + " ",
                  );
                } else {
                  return false;
                }
              } else {
                return false;
              }
            },
          );
          if (matches$1) {
            return new $list.Continue(true);
          } else {
            return new $list.Stop(false);
          }
        },
      );
    } else {
      return false;
    }
  } else if (selector instanceof HasStyle) {
    if (element instanceof Element) {
      let name = selector.name;
      let value = selector.value;
      let attributes = element.attributes;
      let rule = ((name + ":") + value) + ";";
      return $list.any(
        attributes,
        (attribute) => {
          if (attribute instanceof Attribute) {
            let $ = attribute.name;
            if ($ === "style") {
              let value$1 = attribute.value;
              return $string.contains(value$1, rule);
            } else {
              return false;
            }
          } else {
            return false;
          }
        },
      );
    } else if (element instanceof UnsafeInnerHtml) {
      let name = selector.name;
      let value = selector.value;
      let attributes = element.attributes;
      let rule = ((name + ":") + value) + ";";
      return $list.any(
        attributes,
        (attribute) => {
          if (attribute instanceof Attribute) {
            let $ = attribute.name;
            if ($ === "style") {
              let value$1 = attribute.value;
              return $string.contains(value$1, rule);
            } else {
              return false;
            }
          } else {
            return false;
          }
        },
      );
    } else {
      return false;
    }
  } else if (element instanceof Element) {
    let content = selector.content;
    let _pipe = element;
    let _pipe$1 = text_content(_pipe, false, "");
    return $string.contains(_pipe$1, content);
  } else {
    return false;
  }
}

function find_matching_in_list(
  loop$elements,
  loop$selector,
  loop$path,
  loop$index
) {
  while (true) {
    let elements = loop$elements;
    let selector = loop$selector;
    let path = loop$path;
    let index = loop$index;
    if (elements instanceof $Empty) {
      return new Error(undefined);
    } else {
      let $ = elements.head;
      if ($ instanceof Fragment) {
        let first = $;
        let rest = elements.tail;
        loop$elements = $list.append(first.children, rest);
        loop$selector = selector;
        loop$path = $path.add(path, index, first.key);
        loop$index = 0;
      } else {
        let first = $;
        let rest = elements.tail;
        let $1 = matches(first, selector);
        if ($1) {
          return new Ok([first, $path.add(path, index, first.key)]);
        } else {
          loop$elements = rest;
          loop$selector = selector;
          loop$path = path;
          loop$index = index + 1;
        }
      }
    }
  }
}

function find_direct_child(parent, selector, path) {
  if (parent instanceof Fragment) {
    let children = parent.children;
    return find_matching_in_list(children, selector, path, 0);
  } else if (parent instanceof Element) {
    let children = parent.children;
    return find_matching_in_list(children, selector, path, 0);
  } else if (parent instanceof Text) {
    return new Error(undefined);
  } else {
    return new Error(undefined);
  }
}

function find_all_matching_in_list(loop$elements, loop$selector) {
  while (true) {
    let elements = loop$elements;
    let selector = loop$selector;
    if (elements instanceof $Empty) {
      return elements;
    } else {
      let first = elements.head;
      let rest = elements.tail;
      let $ = matches(first, selector);
      if ($) {
        return listPrepend(first, find_all_matching_in_list(rest, selector));
      } else {
        loop$elements = rest;
        loop$selector = selector;
      }
    }
  }
}

function find_all_direct_children(parent, selector) {
  if (parent instanceof Fragment) {
    let children = parent.children;
    return find_all_matching_in_list(children, selector);
  } else if (parent instanceof Element) {
    let children = parent.children;
    return find_all_matching_in_list(children, selector);
  } else if (parent instanceof Text) {
    return toList([]);
  } else {
    return toList([]);
  }
}

function sort_selectors(selectors) {
  return $list.sort(
    $list.flat_map(
      selectors,
      (selector) => {
        if (selector instanceof All) {
          let selectors$1 = selector.of;
          return selectors$1;
        } else {
          return toList([selector]);
        }
      },
    ),
    (a, b) => {
      if (a instanceof All) {
        throw makeError(
          "panic",
          FILEPATH,
          "lustre/dev/query",
          756,
          "sort_selectors",
          "`All` selectors should be flattened",
          {}
        )
      } else if (a instanceof Type) {
        if (b instanceof All) {
          throw makeError(
            "panic",
            FILEPATH,
            "lustre/dev/query",
            756,
            "sort_selectors",
            "`All` selectors should be flattened",
            {}
          )
        } else if (b instanceof Type) {
          let $ = $string.compare(a.namespace, b.namespace);
          if ($ instanceof $order.Eq) {
            return $string.compare(a.tag, b.tag);
          } else {
            return $;
          }
        } else if (b instanceof HasAttribute) {
          return new $order.Lt();
        } else if (b instanceof HasClass) {
          return new $order.Lt();
        } else if (b instanceof HasStyle) {
          return new $order.Lt();
        } else {
          return new $order.Lt();
        }
      } else if (a instanceof HasAttribute) {
        if (b instanceof All) {
          throw makeError(
            "panic",
            FILEPATH,
            "lustre/dev/query",
            756,
            "sort_selectors",
            "`All` selectors should be flattened",
            {}
          )
        } else if (b instanceof Type) {
          return new $order.Gt();
        } else if (b instanceof HasAttribute) {
          let $ = b.name;
          if ($ === "id") {
            let $1 = a.name;
            if ($1 === "id") {
              return $string.compare(a.value, b.value);
            } else {
              return new $order.Gt();
            }
          } else {
            let $1 = a.name;
            if ($1 === "id") {
              return new $order.Lt();
            } else {
              let $2 = $string.compare(a.name, b.name);
              if ($2 instanceof $order.Eq) {
                return $string.compare(a.value, b.value);
              } else {
                return $2;
              }
            }
          }
        } else if (b instanceof HasClass) {
          let $ = a.name;
          if ($ === "id") {
            return new $order.Lt();
          } else {
            return new $order.Lt();
          }
        } else if (b instanceof HasStyle) {
          let $ = a.name;
          if ($ === "id") {
            return new $order.Lt();
          } else {
            return new $order.Lt();
          }
        } else {
          let $ = a.name;
          if ($ === "id") {
            return new $order.Lt();
          } else {
            return new $order.Lt();
          }
        }
      } else if (a instanceof HasClass) {
        if (b instanceof All) {
          throw makeError(
            "panic",
            FILEPATH,
            "lustre/dev/query",
            756,
            "sort_selectors",
            "`All` selectors should be flattened",
            {}
          )
        } else if (b instanceof Type) {
          return new $order.Gt();
        } else if (b instanceof HasAttribute) {
          let $ = b.name;
          if ($ === "id") {
            return new $order.Gt();
          } else {
            return new $order.Gt();
          }
        } else if (b instanceof HasClass) {
          return $string.compare(a.name, b.name);
        } else if (b instanceof HasStyle) {
          return new $order.Gt();
        } else {
          return new $order.Lt();
        }
      } else if (a instanceof HasStyle) {
        if (b instanceof All) {
          throw makeError(
            "panic",
            FILEPATH,
            "lustre/dev/query",
            756,
            "sort_selectors",
            "`All` selectors should be flattened",
            {}
          )
        } else if (b instanceof Type) {
          return new $order.Gt();
        } else if (b instanceof HasAttribute) {
          let $ = b.name;
          if ($ === "id") {
            return new $order.Gt();
          } else {
            return new $order.Gt();
          }
        } else if (b instanceof HasClass) {
          return new $order.Lt();
        } else if (b instanceof HasStyle) {
          return $string.compare(a.name, b.name);
        } else {
          return new $order.Lt();
        }
      } else if (b instanceof All) {
        throw makeError(
          "panic",
          FILEPATH,
          "lustre/dev/query",
          756,
          "sort_selectors",
          "`All` selectors should be flattened",
          {}
        )
      } else if (b instanceof Type) {
        return new $order.Gt();
      } else if (b instanceof HasAttribute) {
        let $ = b.name;
        if ($ === "id") {
          return new $order.Gt();
        } else {
          return new $order.Gt();
        }
      } else if (b instanceof HasClass) {
        return new $order.Gt();
      } else if (b instanceof HasStyle) {
        return new $order.Gt();
      } else {
        return $string.compare(a.content, b.content);
      }
    },
  );
}

function selector_to_readable_string(selector) {
  if (selector instanceof All) {
    let $ = selector.of;
    if ($ instanceof $Empty) {
      return "";
    } else {
      let selectors = $;
      let _pipe = selectors;
      let _pipe$1 = sort_selectors(_pipe);
      let _pipe$2 = $list.map(_pipe$1, selector_to_readable_string);
      return $string.concat(_pipe$2);
    }
  } else if (selector instanceof Type) {
    let $ = selector.namespace;
    if ($ === "") {
      let $1 = selector.tag;
      if ($1 === "") {
        return "";
      } else {
        let tag$1 = $1;
        return tag$1;
      }
    } else {
      let namespace = $;
      let tag$1 = selector.tag;
      return (namespace + ":") + tag$1;
    }
  } else if (selector instanceof HasAttribute) {
    let $ = selector.name;
    if ($ === "") {
      return "";
    } else if ($ === "id") {
      let value = selector.value;
      return "#" + value;
    } else {
      let $1 = selector.value;
      if ($1 === "") {
        let name = $;
        return ("[" + name) + "]";
      } else {
        let name = $;
        let value = $1;
        return ((("[" + name) + "=\"") + value) + "\"]";
      }
    }
  } else if (selector instanceof HasClass) {
    let $ = selector.name;
    if ($ === "") {
      return "";
    } else {
      let name = $;
      return "." + name;
    }
  } else if (selector instanceof HasStyle) {
    let $ = selector.name;
    if ($ === "") {
      return "";
    } else {
      let $1 = selector.value;
      if ($1 === "") {
        return "";
      } else {
        let name = $;
        let value = $1;
        return ((("[style*=\"" + name) + ":") + value) + "\"]";
      }
    }
  } else {
    let $ = selector.content;
    if ($ === "") {
      return "";
    } else {
      let content = $;
      return (":contains(\"" + content) + "\")";
    }
  }
}

/**
 * Print a `Query` as a human-readable string similar to a CSS selector. This
 * function is primarily intended for debugging and testing purposes: for example,
 * you might use this to include the selector in a snapshot test for easier
 * review.
 *
 * > **Note**: while similar, this function is not guaranteed to produce a valid
 * > CSS selector. Specifically, queries that use the [`text`](#text) selector
 * > will not be valid CSS selectors as they use the `:contains` pseudo-class,
 * > which is not part of the CSS spec!
 */
export function to_readable_string(query) {
  if (query instanceof FindElement) {
    let selector = query.matching;
    return selector_to_readable_string(selector);
  } else if (query instanceof FindChild) {
    let parent = query.of;
    let selector = query.matching;
    return (to_readable_string(parent) + " > ") + selector_to_readable_string(
      selector,
    );
  } else {
    let parent = query.of;
    let selector = query.matching;
    return (to_readable_string(parent) + " ") + selector_to_readable_string(
      selector,
    );
  }
}

function find_all_in_list(elements, query) {
  if (elements instanceof $Empty) {
    return elements;
  } else {
    let first = elements.head;
    let rest = elements.tail;
    let first_matches = find_all(first, query);
    let rest_matches = find_all_in_list(rest, query);
    return $list.append(first_matches, rest_matches);
  }
}

/**
 * Like [`find`](#find) but returns every element in the view that matches the
 * given query.
 */
export function find_all(root, query) {
  if (query instanceof FindElement) {
    let selector = query.matching;
    let $ = matches(root, selector);
    if ($) {
      return listPrepend(root, find_all_in_children(root, query));
    } else {
      return find_all_in_children(root, query);
    }
  } else if (query instanceof FindChild) {
    let parent = query.of;
    let selector = query.matching;
    let _pipe = root;
    let _pipe$1 = find_all(_pipe, parent);
    return $list.flat_map(
      _pipe$1,
      (_capture) => { return find_all_direct_children(_capture, selector); },
    );
  } else {
    let parent = query.of;
    let selector = query.matching;
    let _pipe = root;
    let _pipe$1 = find_all(_pipe, parent);
    return $list.flat_map(
      _pipe$1,
      (_capture) => { return find_all_descendants(_capture, selector); },
    );
  }
}

function find_all_in_children(element, query) {
  if (element instanceof Fragment) {
    let children = element.children;
    return find_all_in_list(children, query);
  } else if (element instanceof Element) {
    let children = element.children;
    return find_all_in_list(children, query);
  } else if (element instanceof Text) {
    return toList([]);
  } else {
    return toList([]);
  }
}

function find_all_descendants_in_list(elements, selector) {
  if (elements instanceof $Empty) {
    return elements;
  } else {
    let first = elements.head;
    let rest = elements.tail;
    let first_matches = find_all_descendants(first, selector);
    let rest_matches = find_all_descendants_in_list(rest, selector);
    return $list.append(first_matches, rest_matches);
  }
}

function find_all_descendants(parent, selector) {
  let direct_matches = find_all_direct_children(parent, selector);
  let _block;
  if (parent instanceof Fragment) {
    let children = parent.children;
    _block = find_all_descendants_in_list(children, selector);
  } else if (parent instanceof Element) {
    let children = parent.children;
    _block = find_all_descendants_in_list(children, selector);
  } else if (parent instanceof Text) {
    _block = toList([]);
  } else {
    _block = toList([]);
  }
  let descendant_matches = _block;
  return $list.append(direct_matches, descendant_matches);
}

function find_in_list(loop$elements, loop$query, loop$path, loop$index) {
  while (true) {
    let elements = loop$elements;
    let query = loop$query;
    let path = loop$path;
    let index = loop$index;
    if (elements instanceof $Empty) {
      return new Error(undefined);
    } else {
      let first = elements.head;
      let rest = elements.tail;
      let $ = find_path(first, query, index, path);
      if ($ instanceof Ok) {
        return $;
      } else {
        loop$elements = rest;
        loop$query = query;
        loop$path = path;
        loop$index = index + 1;
      }
    }
  }
}

/**
 *
 * 
 * @ignore
 */
export function find_path(root, query, index, path) {
  if (query instanceof FindElement) {
    let selector = query.matching;
    let $ = matches(root, selector);
    if ($) {
      return new Ok(
        [
          root,
          (() => {
            let _pipe = path;
            return $path.add(_pipe, index, root.key);
          })(),
        ],
      );
    } else {
      return find_in_children(root, query, index, path);
    }
  } else if (query instanceof FindChild) {
    let parent = query.of;
    let selector = query.matching;
    let $ = find_path(root, parent, index, path);
    if ($ instanceof Ok) {
      let element$1 = $[0][0];
      let path$1 = $[0][1];
      return find_direct_child(element$1, selector, path$1);
    } else {
      return new Error(undefined);
    }
  } else {
    let parent = query.of;
    let selector = query.matching;
    let $ = find_path(root, parent, index, path);
    if ($ instanceof Ok) {
      let element$1 = $[0][0];
      let path$1 = $[0][1];
      return find_descendant(element$1, selector, path$1);
    } else {
      return new Error(undefined);
    }
  }
}

function find_in_children(element, query, index, path) {
  if (element instanceof Fragment) {
    let children = element.children;
    return find_in_list(
      children,
      query,
      (() => {
        let _pipe = path;
        return $path.add(_pipe, index, element.key);
      })(),
      0,
    );
  } else if (element instanceof Element) {
    let children = element.children;
    return find_in_list(
      children,
      query,
      (() => {
        let _pipe = path;
        return $path.add(_pipe, index, element.key);
      })(),
      0,
    );
  } else if (element instanceof Text) {
    return new Error(undefined);
  } else {
    return new Error(undefined);
  }
}

/**
 * Find the first element in a view that matches the given [`Query`](#Query).
 * This is useful for tests when combined with [`element.to_readable_string`](../element.html#to_readable_string),
 * allowing you to render large views but take more precise snapshots.
 */
export function find(root, query) {
  let $ = find_path(root, query, 0, $path.root);
  if ($ instanceof Ok) {
    let element$1 = $[0][0];
    return new Ok(element$1);
  } else {
    return new Error(undefined);
  }
}

/**
 * Check if an element or any of its descendants match the given
 * [`Selector`](#Selector).
 */
export function has(element, selector) {
  let $ = find(element, new FindElement(selector));
  if ($ instanceof Ok) {
    return true;
  } else {
    return false;
  }
}

function find_descendant_in_list(
  loop$elements,
  loop$selector,
  loop$path,
  loop$index
) {
  while (true) {
    let elements = loop$elements;
    let selector = loop$selector;
    let path = loop$path;
    let index = loop$index;
    if (elements instanceof $Empty) {
      return new Error(undefined);
    } else {
      let first = elements.head;
      let rest = elements.tail;
      let $ = matches(first, selector);
      if ($) {
        return new Ok([first, $path.add(path, index, first.key)]);
      } else {
        let child$1 = $path.add(path, index, first.key);
        let $1 = find_descendant(first, selector, child$1);
        if ($1 instanceof Ok) {
          return $1;
        } else {
          loop$elements = rest;
          loop$selector = selector;
          loop$path = path;
          loop$index = index + 1;
        }
      }
    }
  }
}

function find_descendant(parent, selector, path) {
  let $ = find_direct_child(parent, selector, path);
  if ($ instanceof Ok) {
    return $;
  } else {
    if (parent instanceof Fragment) {
      let children = parent.children;
      return find_descendant_in_list(children, selector, path, 0);
    } else if (parent instanceof Element) {
      let children = parent.children;
      return find_descendant_in_list(children, selector, path, 0);
    } else if (parent instanceof Text) {
      return new Error(undefined);
    } else {
      return new Error(undefined);
    }
  }
}
