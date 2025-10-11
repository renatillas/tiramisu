import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import { identity as coerce } from "../../../gleam_stdlib/gleam/function.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "../../gleam.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";
import * as $mutable_map from "../../lustre/internals/mutable_map.mjs";
import * as $path from "../../lustre/vdom/path.mjs";
import * as $vattr from "../../lustre/vdom/vattr.mjs";
import { Event, Handler } from "../../lustre/vdom/vattr.mjs";
import * as $vnode from "../../lustre/vdom/vnode.mjs";
import { Element, Fragment, Text, UnsafeInnerHtml } from "../../lustre/vdom/vnode.mjs";
import { isReferenceEqual as is_reference_equal } from "../internals/equals.ffi.mjs";

class Events extends $CustomType {
  constructor(handlers, dispatched_paths, next_dispatched_paths) {
    super();
    this.handlers = handlers;
    this.dispatched_paths = dispatched_paths;
    this.next_dispatched_paths = next_dispatched_paths;
  }
}

/**
 *
 */
export function new$() {
  return new Events(
    $mutable_map.new$(),
    $constants.empty_list,
    $constants.empty_list,
  );
}

export function tick(events) {
  return new Events(
    events.handlers,
    events.next_dispatched_paths,
    $constants.empty_list,
  );
}

function do_remove_event(handlers, path, name) {
  return $mutable_map.delete$(handlers, $path.event(path, name));
}

export function remove_event(events, path, name) {
  let handlers = do_remove_event(events.handlers, path, name);
  return new Events(
    handlers,
    events.dispatched_paths,
    events.next_dispatched_paths,
  );
}

function remove_attributes(handlers, path, attributes) {
  return $list.fold(
    attributes,
    handlers,
    (events, attribute) => {
      if (attribute instanceof Event) {
        let name = attribute.name;
        return do_remove_event(events, path, name);
      } else {
        return events;
      }
    },
  );
}

/**
 *
 */
export function handle(events, path, name, event) {
  let next_dispatched_paths = listPrepend(path, events.next_dispatched_paths);
  let events$1 = new Events(
    events.handlers,
    events.dispatched_paths,
    next_dispatched_paths,
  );
  let $ = $mutable_map.get(
    events$1.handlers,
    (path + $path.separator_event) + name,
  );
  if ($ instanceof Ok) {
    let handler = $[0];
    return [events$1, $decode.run(event, handler)];
  } else {
    return [events$1, new Error(toList([]))];
  }
}

export function has_dispatched_events(events, path) {
  return $path.matches(path, events.dispatched_paths);
}

function do_add_event(handlers, mapper, path, name, handler) {
  return $mutable_map.insert(
    handlers,
    $path.event(path, name),
    $decode.map(
      handler,
      (handler) => {
        return new Handler(
          handler.prevent_default,
          handler.stop_propagation,
          coerce(mapper)(handler.message),
        );
      },
    ),
  );
}

export function add_event(events, mapper, path, name, handler) {
  let handlers = do_add_event(events.handlers, mapper, path, name, handler);
  return new Events(
    handlers,
    events.dispatched_paths,
    events.next_dispatched_paths,
  );
}

function add_attributes(handlers, mapper, path, attributes) {
  return $list.fold(
    attributes,
    handlers,
    (events, attribute) => {
      if (attribute instanceof Event) {
        let name = attribute.name;
        let handler = attribute.handler;
        return do_add_event(events, mapper, path, name, handler);
      } else {
        return events;
      }
    },
  );
}

export function compose_mapper(mapper, child_mapper) {
  let $ = is_reference_equal(mapper, $function.identity);
  let $1 = is_reference_equal(child_mapper, $function.identity);
  if ($1) {
    return mapper;
  } else if ($) {
    return child_mapper;
  } else {
    return (msg) => { return mapper(child_mapper(msg)); };
  }
}

function do_remove_children(
  loop$handlers,
  loop$path,
  loop$child_index,
  loop$children
) {
  while (true) {
    let handlers = loop$handlers;
    let path = loop$path;
    let child_index = loop$child_index;
    let children = loop$children;
    if (children instanceof $Empty) {
      return handlers;
    } else {
      let child = children.head;
      let rest = children.tail;
      let _pipe = handlers;
      let _pipe$1 = do_remove_child(_pipe, path, child_index, child);
      loop$handlers = _pipe$1;
      loop$path = path;
      loop$child_index = child_index + 1;
      loop$children = rest;
    }
  }
}

function do_remove_child(handlers, parent, child_index, child) {
  if (child instanceof Fragment) {
    let children = child.children;
    let path = $path.add(parent, child_index, child.key);
    return do_remove_children(handlers, path, 0, children);
  } else if (child instanceof Element) {
    let attributes = child.attributes;
    let children = child.children;
    let path = $path.add(parent, child_index, child.key);
    let _pipe = handlers;
    let _pipe$1 = remove_attributes(_pipe, path, attributes);
    return do_remove_children(_pipe$1, path, 0, children);
  } else if (child instanceof Text) {
    return handlers;
  } else {
    let attributes = child.attributes;
    let path = $path.add(parent, child_index, child.key);
    return remove_attributes(handlers, path, attributes);
  }
}

export function remove_child(events, parent, child_index, child) {
  let handlers = do_remove_child(events.handlers, parent, child_index, child);
  return new Events(
    handlers,
    events.dispatched_paths,
    events.next_dispatched_paths,
  );
}

function do_add_children(
  loop$handlers,
  loop$mapper,
  loop$path,
  loop$child_index,
  loop$children
) {
  while (true) {
    let handlers = loop$handlers;
    let mapper = loop$mapper;
    let path = loop$path;
    let child_index = loop$child_index;
    let children = loop$children;
    if (children instanceof $Empty) {
      return handlers;
    } else {
      let child = children.head;
      let rest = children.tail;
      let _pipe = handlers;
      let _pipe$1 = do_add_child(_pipe, mapper, path, child_index, child);
      loop$handlers = _pipe$1;
      loop$mapper = mapper;
      loop$path = path;
      loop$child_index = child_index + 1;
      loop$children = rest;
    }
  }
}

function do_add_child(handlers, mapper, parent, child_index, child) {
  if (child instanceof Fragment) {
    let children = child.children;
    let path = $path.add(parent, child_index, child.key);
    let composed_mapper = compose_mapper(mapper, child.mapper);
    return do_add_children(handlers, composed_mapper, path, 0, children);
  } else if (child instanceof Element) {
    let attributes = child.attributes;
    let children = child.children;
    let path = $path.add(parent, child_index, child.key);
    let composed_mapper = compose_mapper(mapper, child.mapper);
    let _pipe = handlers;
    let _pipe$1 = add_attributes(_pipe, composed_mapper, path, attributes);
    return do_add_children(_pipe$1, composed_mapper, path, 0, children);
  } else if (child instanceof Text) {
    return handlers;
  } else {
    let attributes = child.attributes;
    let path = $path.add(parent, child_index, child.key);
    let composed_mapper = compose_mapper(mapper, child.mapper);
    return add_attributes(handlers, composed_mapper, path, attributes);
  }
}

export function add_child(events, mapper, parent, index, child) {
  let handlers = do_add_child(events.handlers, mapper, parent, index, child);
  return new Events(
    handlers,
    events.dispatched_paths,
    events.next_dispatched_paths,
  );
}

export function from_node(root) {
  return add_child(new$(), $function.identity, $path.root, 0, root);
}

/**
 *
 */
export function add_children(events, mapper, path, child_index, children) {
  let handlers = do_add_children(
    events.handlers,
    mapper,
    path,
    child_index,
    children,
  );
  return new Events(
    handlers,
    events.dispatched_paths,
    events.next_dispatched_paths,
  );
}
