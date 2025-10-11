import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import { Empty as $Empty, prepend as listPrepend } from "../../gleam.mjs";
import * as $attribute from "../../lustre/attribute.mjs";
import * as $element from "../../lustre/element.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";
import * as $mutable_map from "../../lustre/internals/mutable_map.mjs";
import * as $vnode from "../../lustre/vdom/vnode.mjs";

function do_extract_keyed_children(
  loop$key_children_pairs,
  loop$keyed_children,
  loop$children
) {
  while (true) {
    let key_children_pairs = loop$key_children_pairs;
    let keyed_children = loop$keyed_children;
    let children = loop$children;
    if (key_children_pairs instanceof $Empty) {
      return [keyed_children, $list.reverse(children)];
    } else {
      let rest = key_children_pairs.tail;
      let key = key_children_pairs.head[0];
      let element$1 = key_children_pairs.head[1];
      let keyed_element = $vnode.to_keyed(key, element$1);
      let _block;
      if (key === "") {
        _block = keyed_children;
      } else {
        _block = $mutable_map.insert(keyed_children, key, keyed_element);
      }
      let keyed_children$1 = _block;
      let children$1 = listPrepend(keyed_element, children);
      loop$key_children_pairs = rest;
      loop$keyed_children = keyed_children$1;
      loop$children = children$1;
    }
  }
}

function extract_keyed_children(children) {
  return do_extract_keyed_children(
    children,
    $mutable_map.new$(),
    $constants.empty_list,
  );
}

/**
 * Render a _keyed_ element with the given tag. Each child is assigned a unique
 * key, which Lustre uses to identify the element in the DOM. This is useful when
 * a single child can be moved around such as in a to-do list, or when elements
 * are frequently added or removed.
 *
 * > **Note**: the key for each child must be unique within the list of children,
 * > but it doesn't have to be unique across the whole application. It's fine to
 * > use the same key in different lists.
 */
export function element(tag, attributes, children) {
  let $ = extract_keyed_children(children);
  let keyed_children;
  let children$1;
  keyed_children = $[0];
  children$1 = $[1];
  return $vnode.element(
    "",
    $function.identity,
    "",
    tag,
    attributes,
    children$1,
    keyed_children,
    false,
    false,
  );
}

/**
 * Render a _keyed_ element with the given namespace and tag. Each child is
 * assigned a unique key, which Lustre uses to identify the element in the DOM.
 * This is useful when a single child can be moved around such as in a to-do
 * list, or when elements are frequently added or removed.
 *
 * > **Note**: the key for each child must be unique within the list of children,
 * > but it doesn't have to be unique across the whole application. It's fine to
 * > use the same key in different lists.
 */
export function namespaced(namespace, tag, attributes, children) {
  let $ = extract_keyed_children(children);
  let keyed_children;
  let children$1;
  keyed_children = $[0];
  children$1 = $[1];
  return $vnode.element(
    "",
    $function.identity,
    namespace,
    tag,
    attributes,
    children$1,
    keyed_children,
    false,
    false,
  );
}

/**
 * Render a _keyed_ fragment. Each child is assigned a unique key, which Lustre
 * uses to identify the element in the DOM. This is useful when a single child
 * can be moved around such as in a to-do list, or when elements are frequently
 * added or removed.
 *
 * > **Note**: the key for each child must be unique within the list of children,
 * > but it doesn't have to be unique across the whole application. It's fine to
 * > use the same key in different lists.
 */
export function fragment(children) {
  let $ = extract_keyed_children(children);
  let keyed_children;
  let children$1;
  keyed_children = $[0];
  children$1 = $[1];
  return $vnode.fragment("", $function.identity, children$1, keyed_children);
}

export function ul(attributes, children) {
  return element("ul", attributes, children);
}

export function ol(attributes, children) {
  return element("ol", attributes, children);
}

export function div(attributes, children) {
  return element("div", attributes, children);
}

export function tbody(attributes, children) {
  return element("tbody", attributes, children);
}

export function dl(attributes, children) {
  return element("dl", attributes, children);
}
