import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../../gleam_stdlib/gleam/order.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $string_tree from "../../../gleam_stdlib/gleam/string_tree.mjs";
import * as $houdini from "../../../houdini/houdini.mjs";
import { Empty as $Empty, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";
import * as $json_object_builder from "../../lustre/internals/json_object_builder.mjs";
import { compare } from "./vattr.ffi.mjs";

export { compare };

export class Attribute extends $CustomType {
  constructor(kind, name, value) {
    super();
    this.kind = kind;
    this.name = name;
    this.value = value;
  }
}

export class Property extends $CustomType {
  constructor(kind, name, value) {
    super();
    this.kind = kind;
    this.name = name;
    this.value = value;
  }
}

export class Event extends $CustomType {
  constructor(kind, name, handler, include, prevent_default, stop_propagation, immediate, debounce, throttle) {
    super();
    this.kind = kind;
    this.name = name;
    this.handler = handler;
    this.include = include;
    this.prevent_default = prevent_default;
    this.stop_propagation = stop_propagation;
    this.immediate = immediate;
    this.debounce = debounce;
    this.throttle = throttle;
  }
}

export class Handler extends $CustomType {
  constructor(prevent_default, stop_propagation, message) {
    super();
    this.prevent_default = prevent_default;
    this.stop_propagation = stop_propagation;
    this.message = message;
  }
}

export class Never extends $CustomType {
  constructor(kind) {
    super();
    this.kind = kind;
  }
}

export class Possible extends $CustomType {
  constructor(kind) {
    super();
    this.kind = kind;
  }
}

export class Always extends $CustomType {
  constructor(kind) {
    super();
    this.kind = kind;
  }
}

export function merge(loop$attributes, loop$merged) {
  while (true) {
    let attributes = loop$attributes;
    let merged = loop$merged;
    if (attributes instanceof $Empty) {
      return merged;
    } else {
      let $ = attributes.head;
      if ($ instanceof Attribute) {
        let $1 = $.name;
        if ($1 === "") {
          let rest = attributes.tail;
          loop$attributes = rest;
          loop$merged = merged;
        } else if ($1 === "class") {
          let $2 = $.value;
          if ($2 === "") {
            let rest = attributes.tail;
            loop$attributes = rest;
            loop$merged = merged;
          } else {
            let $3 = attributes.tail;
            if ($3 instanceof $Empty) {
              let attribute$1 = $;
              let rest = $3;
              loop$attributes = rest;
              loop$merged = listPrepend(attribute$1, merged);
            } else {
              let $4 = $3.head;
              if ($4 instanceof Attribute) {
                let $5 = $4.name;
                if ($5 === "class") {
                  let kind = $.kind;
                  let class1 = $2;
                  let rest = $3.tail;
                  let class2 = $4.value;
                  let value = (class1 + " ") + class2;
                  let attribute$1 = new Attribute(kind, "class", value);
                  loop$attributes = listPrepend(attribute$1, rest);
                  loop$merged = merged;
                } else {
                  let attribute$1 = $;
                  let rest = $3;
                  loop$attributes = rest;
                  loop$merged = listPrepend(attribute$1, merged);
                }
              } else {
                let attribute$1 = $;
                let rest = $3;
                loop$attributes = rest;
                loop$merged = listPrepend(attribute$1, merged);
              }
            }
          }
        } else if ($1 === "style") {
          let $2 = $.value;
          if ($2 === "") {
            let rest = attributes.tail;
            loop$attributes = rest;
            loop$merged = merged;
          } else {
            let $3 = attributes.tail;
            if ($3 instanceof $Empty) {
              let attribute$1 = $;
              let rest = $3;
              loop$attributes = rest;
              loop$merged = listPrepend(attribute$1, merged);
            } else {
              let $4 = $3.head;
              if ($4 instanceof Attribute) {
                let $5 = $4.name;
                if ($5 === "style") {
                  let kind = $.kind;
                  let style1 = $2;
                  let rest = $3.tail;
                  let style2 = $4.value;
                  let value = (style1 + ";") + style2;
                  let attribute$1 = new Attribute(kind, "style", value);
                  loop$attributes = listPrepend(attribute$1, rest);
                  loop$merged = merged;
                } else {
                  let attribute$1 = $;
                  let rest = $3;
                  loop$attributes = rest;
                  loop$merged = listPrepend(attribute$1, merged);
                }
              } else {
                let attribute$1 = $;
                let rest = $3;
                loop$attributes = rest;
                loop$merged = listPrepend(attribute$1, merged);
              }
            }
          }
        } else {
          let attribute$1 = $;
          let rest = attributes.tail;
          loop$attributes = rest;
          loop$merged = listPrepend(attribute$1, merged);
        }
      } else {
        let attribute$1 = $;
        let rest = attributes.tail;
        loop$attributes = rest;
        loop$merged = listPrepend(attribute$1, merged);
      }
    }
  }
}

export function prepare(attributes) {
  if (attributes instanceof $Empty) {
    return attributes;
  } else {
    let $ = attributes.tail;
    if ($ instanceof $Empty) {
      return attributes;
    } else {
      let _pipe = attributes;
      let _pipe$1 = $list.sort(_pipe, (a, b) => { return compare(b, a); });
      return merge(_pipe$1, $constants.empty_list);
    }
  }
}

function attribute_to_json(kind, name, value) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "name", name);
  let _pipe$2 = $json_object_builder.string(_pipe$1, "value", value);
  return $json_object_builder.build(_pipe$2);
}

function property_to_json(kind, name, value) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "name", name);
  let _pipe$2 = $json_object_builder.json(_pipe$1, "value", value);
  return $json_object_builder.build(_pipe$2);
}

export const attribute_kind = 0;

export function attribute(name, value) {
  return new Attribute(attribute_kind, name, value);
}

export function to_string_tree(key, namespace, attributes) {
  let _block;
  let $ = key !== "";
  if ($) {
    _block = listPrepend(attribute("data-lustre-key", key), attributes);
  } else {
    _block = attributes;
  }
  let attributes$1 = _block;
  let _block$1;
  let $1 = namespace !== "";
  if ($1) {
    _block$1 = listPrepend(attribute("xmlns", namespace), attributes$1);
  } else {
    _block$1 = attributes$1;
  }
  let attributes$2 = _block$1;
  return $list.fold(
    attributes$2,
    $string_tree.new$(),
    (html, attr) => {
      if (attr instanceof Attribute) {
        let $2 = attr.name;
        if ($2 === "virtual:defaultValue") {
          let value = attr.value;
          return $string_tree.append(
            html,
            (" value=\"" + $houdini.escape(value)) + "\"",
          );
        } else if ($2 === "") {
          return html;
        } else {
          let $3 = attr.value;
          if ($3 === "") {
            let name = $2;
            return $string_tree.append(html, " " + name);
          } else {
            let name = $2;
            let value = $3;
            return $string_tree.append(
              html,
              ((((" " + name) + "=\"") + $houdini.escape(value)) + "\""),
            );
          }
        }
      } else {
        return html;
      }
    },
  );
}

export const property_kind = 1;

export function property(name, value) {
  return new Property(property_kind, name, value);
}

export const event_kind = 2;

export function event(
  name,
  handler,
  include,
  prevent_default,
  stop_propagation,
  immediate,
  debounce,
  throttle
) {
  return new Event(
    event_kind,
    name,
    handler,
    include,
    prevent_default,
    stop_propagation,
    immediate,
    debounce,
    throttle,
  );
}

export const never_kind = 0;

function event_behaviour_to_json_builder(behaviour) {
  if (behaviour instanceof Never) {
    let kind = behaviour.kind;
    return $json_object_builder.tagged(kind);
  } else if (behaviour instanceof Possible) {
    return $json_object_builder.tagged(never_kind);
  } else {
    let kind = behaviour.kind;
    return $json_object_builder.tagged(kind);
  }
}

function event_to_json(
  kind,
  name,
  include,
  prevent_default,
  stop_propagation,
  immediate,
  debounce,
  throttle
) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "name", name);
  let _pipe$2 = $json_object_builder.list(
    _pipe$1,
    "include",
    include,
    $json.string,
  );
  let _pipe$3 = $json_object_builder.object(
    _pipe$2,
    "prevent_default",
    event_behaviour_to_json_builder(prevent_default),
  );
  let _pipe$4 = $json_object_builder.object(
    _pipe$3,
    "stop_propagation",
    event_behaviour_to_json_builder(stop_propagation),
  );
  let _pipe$5 = $json_object_builder.bool(_pipe$4, "immediate", immediate);
  let _pipe$6 = $json_object_builder.int(_pipe$5, "debounce", debounce);
  let _pipe$7 = $json_object_builder.int(_pipe$6, "throttle", throttle);
  return $json_object_builder.build(_pipe$7);
}

export function to_json(attribute) {
  if (attribute instanceof Attribute) {
    let kind = attribute.kind;
    let name = attribute.name;
    let value = attribute.value;
    return attribute_to_json(kind, name, value);
  } else if (attribute instanceof Property) {
    let kind = attribute.kind;
    let name = attribute.name;
    let value = attribute.value;
    return property_to_json(kind, name, value);
  } else {
    let kind = attribute.kind;
    let name = attribute.name;
    let include = attribute.include;
    let prevent_default = attribute.prevent_default;
    let stop_propagation = attribute.stop_propagation;
    let immediate = attribute.immediate;
    let debounce = attribute.debounce;
    let throttle = attribute.throttle;
    return event_to_json(
      kind,
      name,
      include,
      prevent_default,
      stop_propagation,
      immediate,
      debounce,
      throttle,
    );
  }
}

export const never = /* @__PURE__ */ new Never(never_kind);

export const possible_kind = 1;

export const possible = /* @__PURE__ */ new Possible(possible_kind);

export const always_kind = 2;

export const always = /* @__PURE__ */ new Always(always_kind);
