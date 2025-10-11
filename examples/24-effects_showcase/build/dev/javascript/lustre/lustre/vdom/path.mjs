import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import {
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "../../gleam.mjs";

class Root extends $CustomType {}

class Key extends $CustomType {
  constructor(key, parent) {
    super();
    this.key = key;
    this.parent = parent;
  }
}

class Index extends $CustomType {
  constructor(index, parent) {
    super();
    this.index = index;
    this.parent = parent;
  }
}

function do_matches(loop$path, loop$candidates) {
  while (true) {
    let path = loop$path;
    let candidates = loop$candidates;
    if (candidates instanceof $Empty) {
      return false;
    } else {
      let candidate = candidates.head;
      let rest = candidates.tail;
      let $ = $string.starts_with(path, candidate);
      if ($) {
        return $;
      } else {
        loop$path = path;
        loop$candidates = rest;
      }
    }
  }
}

/**
 *
 */
export function add(parent, index, key) {
  if (key === "") {
    return new Index(index, parent);
  } else {
    return new Key(key, parent);
  }
}

/**
 *
 */
export const root = /* @__PURE__ */ new Root();

/**
 *
 */
export const separator_element = "\t";

function do_to_string(loop$path, loop$acc) {
  while (true) {
    let path = loop$path;
    let acc = loop$acc;
    if (path instanceof Root) {
      if (acc instanceof $Empty) {
        return "";
      } else {
        let segments = acc.tail;
        return $string.concat(segments);
      }
    } else if (path instanceof Key) {
      let key = path.key;
      let parent = path.parent;
      loop$path = parent;
      loop$acc = listPrepend(separator_element, listPrepend(key, acc));
    } else {
      let index = path.index;
      let parent = path.parent;
      loop$path = parent;
      loop$acc = listPrepend(
        separator_element,
        listPrepend($int.to_string(index), acc),
      );
    }
  }
}

/**
 *
 */
export function to_string(path) {
  return do_to_string(path, toList([]));
}

/**
 *
 */
export function matches(path, candidates) {
  if (candidates instanceof $Empty) {
    return false;
  } else {
    return do_matches(to_string(path), candidates);
  }
}

/**
 *
 */
export const separator_event = "\n";

/**
 * Convert a path to a resolved string with an event name appended to it.
 */
export function event(path, event) {
  return do_to_string(path, toList([separator_event, event]));
}
