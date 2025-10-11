import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import * as $order from "../../../gleam_stdlib/gleam/order.mjs";
import { Eq, Gt, Lt } from "../../../gleam_stdlib/gleam/order.mjs";
import {
  Ok,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "../../gleam.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";
import * as $mutable_map from "../../lustre/internals/mutable_map.mjs";
import * as $events from "../../lustre/vdom/events.mjs";
import * as $patch from "../../lustre/vdom/patch.mjs";
import { Patch } from "../../lustre/vdom/patch.mjs";
import * as $path from "../../lustre/vdom/path.mjs";
import * as $vattr from "../../lustre/vdom/vattr.mjs";
import { Attribute, Event, Property } from "../../lustre/vdom/vattr.mjs";
import * as $vnode from "../../lustre/vdom/vnode.mjs";
import { Element, Fragment, Text, UnsafeInnerHtml } from "../../lustre/vdom/vnode.mjs";
import { isEqual as property_value_equal } from "../internals/equals.ffi.mjs";

export class Diff extends $CustomType {
  constructor(patch, events) {
    super();
    this.patch = patch;
    this.events = events;
  }
}

class AttributeChange extends $CustomType {
  constructor(added, removed, events) {
    super();
    this.added = added;
    this.removed = removed;
    this.events = events;
  }
}

function is_controlled(events, namespace, tag, path) {
  if (tag === "input" && namespace === "") {
    return $events.has_dispatched_events(events, path);
  } else if (tag === "select" && namespace === "") {
    return $events.has_dispatched_events(events, path);
  } else if (tag === "textarea" && namespace === "") {
    return $events.has_dispatched_events(events, path);
  } else {
    return false;
  }
}

function diff_attributes(
  loop$controlled,
  loop$path,
  loop$mapper,
  loop$events,
  loop$old,
  loop$new,
  loop$added,
  loop$removed
) {
  while (true) {
    let controlled = loop$controlled;
    let path = loop$path;
    let mapper = loop$mapper;
    let events = loop$events;
    let old = loop$old;
    let new$ = loop$new;
    let added = loop$added;
    let removed = loop$removed;
    if (new$ instanceof $Empty) {
      if (old instanceof $Empty) {
        return new AttributeChange(added, removed, events);
      } else {
        let $ = old.head;
        if ($ instanceof Event) {
          let prev = $;
          let old$1 = old.tail;
          let name = $.name;
          let removed$1 = listPrepend(prev, removed);
          let events$1 = $events.remove_event(events, path, name);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events$1;
          loop$old = old$1;
          loop$new = new$;
          loop$added = added;
          loop$removed = removed$1;
        } else {
          let prev = $;
          let old$1 = old.tail;
          let removed$1 = listPrepend(prev, removed);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events;
          loop$old = old$1;
          loop$new = new$;
          loop$added = added;
          loop$removed = removed$1;
        }
      }
    } else if (old instanceof $Empty) {
      let $ = new$.head;
      if ($ instanceof Event) {
        let next = $;
        let new$1 = new$.tail;
        let name = $.name;
        let handler = $.handler;
        let added$1 = listPrepend(next, added);
        let events$1 = $events.add_event(events, mapper, path, name, handler);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events$1;
        loop$old = old;
        loop$new = new$1;
        loop$added = added$1;
        loop$removed = removed;
      } else {
        let next = $;
        let new$1 = new$.tail;
        let added$1 = listPrepend(next, added);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events;
        loop$old = old;
        loop$new = new$1;
        loop$added = added$1;
        loop$removed = removed;
      }
    } else {
      let next = new$.head;
      let remaining_new = new$.tail;
      let prev = old.head;
      let remaining_old = old.tail;
      let $ = $vattr.compare(prev, next);
      if ($ instanceof Lt) {
        if (prev instanceof Event) {
          let name = prev.name;
          let removed$1 = listPrepend(prev, removed);
          let events$1 = $events.remove_event(events, path, name);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events$1;
          loop$old = remaining_old;
          loop$new = new$;
          loop$added = added;
          loop$removed = removed$1;
        } else {
          let removed$1 = listPrepend(prev, removed);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events;
          loop$old = remaining_old;
          loop$new = new$;
          loop$added = added;
          loop$removed = removed$1;
        }
      } else if ($ instanceof Eq) {
        if (next instanceof Attribute) {
          if (prev instanceof Attribute) {
            let _block;
            let $1 = next.name;
            if ($1 === "value") {
              _block = controlled || (prev.value !== next.value);
            } else if ($1 === "checked") {
              _block = controlled || (prev.value !== next.value);
            } else if ($1 === "selected") {
              _block = controlled || (prev.value !== next.value);
            } else {
              _block = prev.value !== next.value;
            }
            let has_changes = _block;
            let _block$1;
            if (has_changes) {
              _block$1 = listPrepend(next, added);
            } else {
              _block$1 = added;
            }
            let added$1 = _block$1;
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed;
          } else if (prev instanceof Event) {
            let name = prev.name;
            let added$1 = listPrepend(next, added);
            let removed$1 = listPrepend(prev, removed);
            let events$1 = $events.remove_event(events, path, name);
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events$1;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed$1;
          } else {
            let added$1 = listPrepend(next, added);
            let removed$1 = listPrepend(prev, removed);
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed$1;
          }
        } else if (next instanceof Property) {
          if (prev instanceof Property) {
            let _block;
            let $1 = next.name;
            if ($1 === "scrollLeft") {
              _block = true;
            } else if ($1 === "scrollRight") {
              _block = true;
            } else if ($1 === "value") {
              _block = controlled || !property_value_equal(
                prev.value,
                next.value,
              );
            } else if ($1 === "checked") {
              _block = controlled || !property_value_equal(
                prev.value,
                next.value,
              );
            } else if ($1 === "selected") {
              _block = controlled || !property_value_equal(
                prev.value,
                next.value,
              );
            } else {
              _block = !property_value_equal(prev.value, next.value);
            }
            let has_changes = _block;
            let _block$1;
            if (has_changes) {
              _block$1 = listPrepend(next, added);
            } else {
              _block$1 = added;
            }
            let added$1 = _block$1;
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed;
          } else if (prev instanceof Event) {
            let name = prev.name;
            let added$1 = listPrepend(next, added);
            let removed$1 = listPrepend(prev, removed);
            let events$1 = $events.remove_event(events, path, name);
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events$1;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed$1;
          } else {
            let added$1 = listPrepend(next, added);
            let removed$1 = listPrepend(prev, removed);
            loop$controlled = controlled;
            loop$path = path;
            loop$mapper = mapper;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed$1;
          }
        } else if (prev instanceof Event) {
          let name = next.name;
          let handler = next.handler;
          let has_changes = ((((prev.prevent_default.kind !== next.prevent_default.kind) || (prev.stop_propagation.kind !== next.stop_propagation.kind)) || (prev.immediate !== next.immediate)) || (prev.debounce !== next.debounce)) || (prev.throttle !== next.throttle);
          let _block;
          if (has_changes) {
            _block = listPrepend(next, added);
          } else {
            _block = added;
          }
          let added$1 = _block;
          let events$1 = $events.add_event(events, mapper, path, name, handler);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events$1;
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = added$1;
          loop$removed = removed;
        } else {
          let name = next.name;
          let handler = next.handler;
          let added$1 = listPrepend(next, added);
          let removed$1 = listPrepend(prev, removed);
          let events$1 = $events.add_event(events, mapper, path, name, handler);
          loop$controlled = controlled;
          loop$path = path;
          loop$mapper = mapper;
          loop$events = events$1;
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = added$1;
          loop$removed = removed$1;
        }
      } else if (next instanceof Event) {
        let name = next.name;
        let handler = next.handler;
        let added$1 = listPrepend(next, added);
        let events$1 = $events.add_event(events, mapper, path, name, handler);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events$1;
        loop$old = old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed;
      } else {
        let added$1 = listPrepend(next, added);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events;
        loop$old = old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed;
      }
    }
  }
}

function do_diff(
  loop$old,
  loop$old_keyed,
  loop$new,
  loop$new_keyed,
  loop$moved,
  loop$moved_offset,
  loop$removed,
  loop$node_index,
  loop$patch_index,
  loop$path,
  loop$changes,
  loop$children,
  loop$mapper,
  loop$events
) {
  while (true) {
    let old = loop$old;
    let old_keyed = loop$old_keyed;
    let new$ = loop$new;
    let new_keyed = loop$new_keyed;
    let moved = loop$moved;
    let moved_offset = loop$moved_offset;
    let removed = loop$removed;
    let node_index = loop$node_index;
    let patch_index = loop$patch_index;
    let path = loop$path;
    let changes = loop$changes;
    let children = loop$children;
    let mapper = loop$mapper;
    let events = loop$events;
    if (new$ instanceof $Empty) {
      if (old instanceof $Empty) {
        return new Diff(
          new Patch(patch_index, removed, changes, children),
          events,
        );
      } else {
        let prev = old.head;
        let old$1 = old.tail;
        let _block;
        let $ = (prev.key === "") || !$mutable_map.has_key(moved, prev.key);
        if ($) {
          _block = removed + 1;
        } else {
          _block = removed;
        }
        let removed$1 = _block;
        let events$1 = $events.remove_child(events, path, node_index, prev);
        loop$old = old$1;
        loop$old_keyed = old_keyed;
        loop$new = new$;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset;
        loop$removed = removed$1;
        loop$node_index = node_index;
        loop$patch_index = patch_index;
        loop$path = path;
        loop$changes = changes;
        loop$children = children;
        loop$mapper = mapper;
        loop$events = events$1;
      }
    } else if (old instanceof $Empty) {
      let events$1 = $events.add_children(
        events,
        mapper,
        path,
        node_index,
        new$,
      );
      let insert = $patch.insert(new$, node_index - moved_offset);
      let changes$1 = listPrepend(insert, changes);
      return new Diff(
        new Patch(patch_index, removed, changes$1, children),
        events$1,
      );
    } else {
      let next = new$.head;
      let prev = old.head;
      if (prev.key !== next.key) {
        let new_remaining = new$.tail;
        let old_remaining = old.tail;
        let next_did_exist = $mutable_map.get(old_keyed, next.key);
        let prev_does_exist = $mutable_map.has_key(new_keyed, prev.key);
        if (next_did_exist instanceof Ok) {
          if (prev_does_exist) {
            let match = next_did_exist[0];
            let $ = $mutable_map.has_key(moved, prev.key);
            if ($) {
              loop$old = old_remaining;
              loop$old_keyed = old_keyed;
              loop$new = new$;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset - 1;
              loop$removed = removed;
              loop$node_index = node_index;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = changes;
              loop$children = children;
              loop$mapper = mapper;
              loop$events = events;
            } else {
              let before = node_index - moved_offset;
              let changes$1 = listPrepend(
                $patch.move(next.key, before),
                changes,
              );
              let moved$1 = $mutable_map.insert(moved, next.key, undefined);
              let moved_offset$1 = moved_offset + 1;
              loop$old = listPrepend(match, old);
              loop$old_keyed = old_keyed;
              loop$new = new$;
              loop$new_keyed = new_keyed;
              loop$moved = moved$1;
              loop$moved_offset = moved_offset$1;
              loop$removed = removed;
              loop$node_index = node_index;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = changes$1;
              loop$children = children;
              loop$mapper = mapper;
              loop$events = events;
            }
          } else {
            let index = node_index - moved_offset;
            let changes$1 = listPrepend($patch.remove(index), changes);
            let events$1 = $events.remove_child(events, path, node_index, prev);
            let moved_offset$1 = moved_offset - 1;
            loop$old = old_remaining;
            loop$old_keyed = old_keyed;
            loop$new = new$;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset$1;
            loop$removed = removed;
            loop$node_index = node_index;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = changes$1;
            loop$children = children;
            loop$mapper = mapper;
            loop$events = events$1;
          }
        } else if (prev_does_exist) {
          let before = node_index - moved_offset;
          let events$1 = $events.add_child(
            events,
            mapper,
            path,
            node_index,
            next,
          );
          let insert = $patch.insert(toList([next]), before);
          let changes$1 = listPrepend(insert, changes);
          loop$old = old;
          loop$old_keyed = old_keyed;
          loop$new = new_remaining;
          loop$new_keyed = new_keyed;
          loop$moved = moved;
          loop$moved_offset = moved_offset + 1;
          loop$removed = removed;
          loop$node_index = node_index + 1;
          loop$patch_index = patch_index;
          loop$path = path;
          loop$changes = changes$1;
          loop$children = children;
          loop$mapper = mapper;
          loop$events = events$1;
        } else {
          let change = $patch.replace(node_index - moved_offset, next);
          let _block;
          let _pipe = events;
          let _pipe$1 = $events.remove_child(_pipe, path, node_index, prev);
          _block = $events.add_child(_pipe$1, mapper, path, node_index, next);
          let events$1 = _block;
          loop$old = old_remaining;
          loop$old_keyed = old_keyed;
          loop$new = new_remaining;
          loop$new_keyed = new_keyed;
          loop$moved = moved;
          loop$moved_offset = moved_offset;
          loop$removed = removed;
          loop$node_index = node_index + 1;
          loop$patch_index = patch_index;
          loop$path = path;
          loop$changes = listPrepend(change, changes);
          loop$children = children;
          loop$mapper = mapper;
          loop$events = events$1;
        }
      } else {
        let $ = old.head;
        if ($ instanceof Fragment) {
          let $1 = new$.head;
          if ($1 instanceof Fragment) {
            let next$1 = $1;
            let new$1 = new$.tail;
            let prev$1 = $;
            let old$1 = old.tail;
            let composed_mapper = $events.compose_mapper(mapper, next$1.mapper);
            let child_path = $path.add(path, node_index, next$1.key);
            let child = do_diff(
              prev$1.children,
              prev$1.keyed_children,
              next$1.children,
              next$1.keyed_children,
              $mutable_map.new$(),
              0,
              0,
              0,
              node_index,
              child_path,
              $constants.empty_list,
              $constants.empty_list,
              composed_mapper,
              events,
            );
            let _block;
            let $2 = child.patch;
            let $3 = $2.children;
            if ($3 instanceof $Empty) {
              let $4 = $2.changes;
              if ($4 instanceof $Empty) {
                let $5 = $2.removed;
                if ($5 === 0) {
                  _block = children;
                } else {
                  _block = listPrepend(child.patch, children);
                }
              } else {
                _block = listPrepend(child.patch, children);
              }
            } else {
              _block = listPrepend(child.patch, children);
            }
            let children$1 = _block;
            loop$old = old$1;
            loop$old_keyed = old_keyed;
            loop$new = new$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = changes;
            loop$children = children$1;
            loop$mapper = mapper;
            loop$events = child.events;
          } else {
            let next$1 = $1;
            let new_remaining = new$.tail;
            let prev$1 = $;
            let old_remaining = old.tail;
            let change = $patch.replace(node_index - moved_offset, next$1);
            let _block;
            let _pipe = events;
            let _pipe$1 = $events.remove_child(_pipe, path, node_index, prev$1);
            _block = $events.add_child(
              _pipe$1,
              mapper,
              path,
              node_index,
              next$1,
            );
            let events$1 = _block;
            loop$old = old_remaining;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = listPrepend(change, changes);
            loop$children = children;
            loop$mapper = mapper;
            loop$events = events$1;
          }
        } else if ($ instanceof Element) {
          let $1 = new$.head;
          if ($1 instanceof Element) {
            let next$1 = $1;
            let prev$1 = $;
            if (
              (prev$1.namespace === next$1.namespace) && (prev$1.tag === next$1.tag)
            ) {
              let new$1 = new$.tail;
              let old$1 = old.tail;
              let composed_mapper = $events.compose_mapper(
                mapper,
                next$1.mapper,
              );
              let child_path = $path.add(path, node_index, next$1.key);
              let controlled = is_controlled(
                events,
                next$1.namespace,
                next$1.tag,
                child_path,
              );
              let $2 = diff_attributes(
                controlled,
                child_path,
                composed_mapper,
                events,
                prev$1.attributes,
                next$1.attributes,
                $constants.empty_list,
                $constants.empty_list,
              );
              let added_attrs;
              let removed_attrs;
              let events$1;
              added_attrs = $2.added;
              removed_attrs = $2.removed;
              events$1 = $2.events;
              let _block;
              if (
                removed_attrs instanceof $Empty &&
                added_attrs instanceof $Empty
              ) {
                _block = $constants.empty_list;
              } else {
                _block = toList([$patch.update(added_attrs, removed_attrs)]);
              }
              let initial_child_changes = _block;
              let child = do_diff(
                prev$1.children,
                prev$1.keyed_children,
                next$1.children,
                next$1.keyed_children,
                $mutable_map.new$(),
                0,
                0,
                0,
                node_index,
                child_path,
                initial_child_changes,
                $constants.empty_list,
                composed_mapper,
                events$1,
              );
              let _block$1;
              let $3 = child.patch;
              let $4 = $3.children;
              if ($4 instanceof $Empty) {
                let $5 = $3.changes;
                if ($5 instanceof $Empty) {
                  let $6 = $3.removed;
                  if ($6 === 0) {
                    _block$1 = children;
                  } else {
                    _block$1 = listPrepend(child.patch, children);
                  }
                } else {
                  _block$1 = listPrepend(child.patch, children);
                }
              } else {
                _block$1 = listPrepend(child.patch, children);
              }
              let children$1 = _block$1;
              loop$old = old$1;
              loop$old_keyed = old_keyed;
              loop$new = new$1;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = changes;
              loop$children = children$1;
              loop$mapper = mapper;
              loop$events = child.events;
            } else {
              let next$2 = $1;
              let new_remaining = new$.tail;
              let prev$2 = $;
              let old_remaining = old.tail;
              let change = $patch.replace(node_index - moved_offset, next$2);
              let _block;
              let _pipe = events;
              let _pipe$1 = $events.remove_child(
                _pipe,
                path,
                node_index,
                prev$2,
              );
              _block = $events.add_child(
                _pipe$1,
                mapper,
                path,
                node_index,
                next$2,
              );
              let events$1 = _block;
              loop$old = old_remaining;
              loop$old_keyed = old_keyed;
              loop$new = new_remaining;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = listPrepend(change, changes);
              loop$children = children;
              loop$mapper = mapper;
              loop$events = events$1;
            }
          } else {
            let next$1 = $1;
            let new_remaining = new$.tail;
            let prev$1 = $;
            let old_remaining = old.tail;
            let change = $patch.replace(node_index - moved_offset, next$1);
            let _block;
            let _pipe = events;
            let _pipe$1 = $events.remove_child(_pipe, path, node_index, prev$1);
            _block = $events.add_child(
              _pipe$1,
              mapper,
              path,
              node_index,
              next$1,
            );
            let events$1 = _block;
            loop$old = old_remaining;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = listPrepend(change, changes);
            loop$children = children;
            loop$mapper = mapper;
            loop$events = events$1;
          }
        } else if ($ instanceof Text) {
          let $1 = new$.head;
          if ($1 instanceof Text) {
            let next$1 = $1;
            let prev$1 = $;
            if (prev$1.content === next$1.content) {
              let new$1 = new$.tail;
              let old$1 = old.tail;
              loop$old = old$1;
              loop$old_keyed = old_keyed;
              loop$new = new$1;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = changes;
              loop$children = children;
              loop$mapper = mapper;
              loop$events = events;
            } else {
              let next$2 = $1;
              let new$1 = new$.tail;
              let old$1 = old.tail;
              let child = $patch.new$(
                node_index,
                0,
                toList([$patch.replace_text(next$2.content)]),
                $constants.empty_list,
              );
              loop$old = old$1;
              loop$old_keyed = old_keyed;
              loop$new = new$1;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$path = path;
              loop$changes = changes;
              loop$children = listPrepend(child, children);
              loop$mapper = mapper;
              loop$events = events;
            }
          } else {
            let next$1 = $1;
            let new_remaining = new$.tail;
            let prev$1 = $;
            let old_remaining = old.tail;
            let change = $patch.replace(node_index - moved_offset, next$1);
            let _block;
            let _pipe = events;
            let _pipe$1 = $events.remove_child(_pipe, path, node_index, prev$1);
            _block = $events.add_child(
              _pipe$1,
              mapper,
              path,
              node_index,
              next$1,
            );
            let events$1 = _block;
            loop$old = old_remaining;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = listPrepend(change, changes);
            loop$children = children;
            loop$mapper = mapper;
            loop$events = events$1;
          }
        } else {
          let $1 = new$.head;
          if ($1 instanceof UnsafeInnerHtml) {
            let next$1 = $1;
            let new$1 = new$.tail;
            let prev$1 = $;
            let old$1 = old.tail;
            let composed_mapper = $events.compose_mapper(mapper, next$1.mapper);
            let child_path = $path.add(path, node_index, next$1.key);
            let $2 = diff_attributes(
              false,
              child_path,
              composed_mapper,
              events,
              prev$1.attributes,
              next$1.attributes,
              $constants.empty_list,
              $constants.empty_list,
            );
            let added_attrs;
            let removed_attrs;
            let events$1;
            added_attrs = $2.added;
            removed_attrs = $2.removed;
            events$1 = $2.events;
            let _block;
            if (removed_attrs instanceof $Empty && added_attrs instanceof $Empty) {
              _block = $constants.empty_list;
            } else {
              _block = toList([$patch.update(added_attrs, removed_attrs)]);
            }
            let child_changes = _block;
            let _block$1;
            let $3 = prev$1.inner_html === next$1.inner_html;
            if ($3) {
              _block$1 = child_changes;
            } else {
              _block$1 = listPrepend(
                $patch.replace_inner_html(next$1.inner_html),
                child_changes,
              );
            }
            let child_changes$1 = _block$1;
            let _block$2;
            if (child_changes$1 instanceof $Empty) {
              _block$2 = children;
            } else {
              _block$2 = listPrepend(
                $patch.new$(node_index, 0, child_changes$1, toList([])),
                children,
              );
            }
            let children$1 = _block$2;
            loop$old = old$1;
            loop$old_keyed = old_keyed;
            loop$new = new$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = changes;
            loop$children = children$1;
            loop$mapper = mapper;
            loop$events = events$1;
          } else {
            let next$1 = $1;
            let new_remaining = new$.tail;
            let prev$1 = $;
            let old_remaining = old.tail;
            let change = $patch.replace(node_index - moved_offset, next$1);
            let _block;
            let _pipe = events;
            let _pipe$1 = $events.remove_child(_pipe, path, node_index, prev$1);
            _block = $events.add_child(
              _pipe$1,
              mapper,
              path,
              node_index,
              next$1,
            );
            let events$1 = _block;
            loop$old = old_remaining;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$path = path;
            loop$changes = listPrepend(change, changes);
            loop$children = children;
            loop$mapper = mapper;
            loop$events = events$1;
          }
        }
      }
    }
  }
}

export function diff(events, old, new$) {
  return do_diff(
    toList([old]),
    $mutable_map.new$(),
    toList([new$]),
    $mutable_map.new$(),
    $mutable_map.new$(),
    0,
    0,
    0,
    0,
    $path.root,
    $constants.empty_list,
    $constants.empty_list,
    $function.identity,
    $events.tick(events),
  );
}
