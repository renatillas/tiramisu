import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import { toList, CustomType as $CustomType } from "../../gleam.mjs";
import * as $patch from "../../lustre/vdom/patch.mjs";
import * as $vnode from "../../lustre/vdom/vnode.mjs";

export class Mount extends $CustomType {
  constructor(kind, open_shadow_root, will_adopt_styles, observed_attributes, observed_properties, requested_contexts, provided_contexts, vdom) {
    super();
    this.kind = kind;
    this.open_shadow_root = open_shadow_root;
    this.will_adopt_styles = will_adopt_styles;
    this.observed_attributes = observed_attributes;
    this.observed_properties = observed_properties;
    this.requested_contexts = requested_contexts;
    this.provided_contexts = provided_contexts;
    this.vdom = vdom;
  }
}

export class Reconcile extends $CustomType {
  constructor(kind, patch) {
    super();
    this.kind = kind;
    this.patch = patch;
  }
}

export class Emit extends $CustomType {
  constructor(kind, name, data) {
    super();
    this.kind = kind;
    this.name = name;
    this.data = data;
  }
}

export class Provide extends $CustomType {
  constructor(kind, key, value) {
    super();
    this.kind = kind;
    this.key = key;
    this.value = value;
  }
}

export class Batch extends $CustomType {
  constructor(kind, messages) {
    super();
    this.kind = kind;
    this.messages = messages;
  }
}

export class AttributeChanged extends $CustomType {
  constructor(kind, name, value) {
    super();
    this.kind = kind;
    this.name = name;
    this.value = value;
  }
}

export class PropertyChanged extends $CustomType {
  constructor(kind, name, value) {
    super();
    this.kind = kind;
    this.name = name;
    this.value = value;
  }
}

export class EventFired extends $CustomType {
  constructor(kind, path, name, event) {
    super();
    this.kind = kind;
    this.path = path;
    this.name = name;
    this.event = event;
  }
}

export class ContextProvided extends $CustomType {
  constructor(kind, key, value) {
    super();
    this.kind = kind;
    this.key = key;
    this.value = value;
  }
}

function mount_to_json(
  kind,
  open_shadow_root,
  will_adopt_styles,
  observed_attributes,
  observed_properties,
  requested_contexts,
  provided_contexts,
  vdom
) {
  return $json.object(
    toList([
      ["kind", $json.int(kind)],
      ["open_shadow_root", $json.bool(open_shadow_root)],
      ["will_adopt_styles", $json.bool(will_adopt_styles)],
      ["observed_attributes", $json.array(observed_attributes, $json.string)],
      ["observed_properties", $json.array(observed_properties, $json.string)],
      ["requested_contexts", $json.array(requested_contexts, $json.string)],
      [
        "provided_contexts",
        $json.dict(provided_contexts, $function.identity, $function.identity),
      ],
      ["vdom", $vnode.to_json(vdom)],
    ]),
  );
}

function reconcile_to_json(kind, patch) {
  return $json.object(
    toList([["kind", $json.int(kind)], ["patch", $patch.to_json(patch)]]),
  );
}

function emit_to_json(kind, name, data) {
  return $json.object(
    toList([
      ["kind", $json.int(kind)],
      ["name", $json.string(name)],
      ["data", data],
    ]),
  );
}

function provide_to_json(kind, key, value) {
  return $json.object(
    toList([
      ["kind", $json.int(kind)],
      ["key", $json.string(key)],
      ["value", value],
    ]),
  );
}

export function client_message_to_json(message) {
  if (message instanceof Mount) {
    let kind = message.kind;
    let open_shadow_root = message.open_shadow_root;
    let will_adopt_styles = message.will_adopt_styles;
    let observed_attributes = message.observed_attributes;
    let observed_properties = message.observed_properties;
    let requested_contexts = message.requested_contexts;
    let provided_contexts = message.provided_contexts;
    let vdom = message.vdom;
    return mount_to_json(
      kind,
      open_shadow_root,
      will_adopt_styles,
      observed_attributes,
      observed_properties,
      requested_contexts,
      provided_contexts,
      vdom,
    );
  } else if (message instanceof Reconcile) {
    let kind = message.kind;
    let patch = message.patch;
    return reconcile_to_json(kind, patch);
  } else if (message instanceof Emit) {
    let kind = message.kind;
    let name = message.name;
    let data = message.data;
    return emit_to_json(kind, name, data);
  } else {
    let kind = message.kind;
    let key = message.key;
    let value = message.value;
    return provide_to_json(kind, key, value);
  }
}

export const mount_kind = 0;

export function mount(
  open_shadow_root,
  will_adopt_styles,
  observed_attributes,
  observed_properties,
  requested_contexts,
  provided_contexts,
  vdom
) {
  return new Mount(
    mount_kind,
    open_shadow_root,
    will_adopt_styles,
    observed_attributes,
    observed_properties,
    requested_contexts,
    provided_contexts,
    vdom,
  );
}

export const reconcile_kind = 1;

export function reconcile(patch) {
  return new Reconcile(reconcile_kind, patch);
}

export const emit_kind = 2;

export function emit(name, data) {
  return new Emit(emit_kind, name, data);
}

export const provide_kind = 3;

export function provide(key, value) {
  return new Provide(provide_kind, key, value);
}

export const attribute_changed_kind = 0;

export function attribute_changed(name, value) {
  return new AttributeChanged(attribute_changed_kind, name, value);
}

function attribute_changed_decoder() {
  return $decode.field(
    "name",
    $decode.string,
    (name) => {
      return $decode.field(
        "value",
        $decode.string,
        (value) => { return $decode.success(attribute_changed(name, value)); },
      );
    },
  );
}

export const event_fired_kind = 1;

export function event_fired(path, name, event) {
  return new EventFired(event_fired_kind, path, name, event);
}

function event_fired_decoder() {
  return $decode.field(
    "path",
    $decode.string,
    (path) => {
      return $decode.field(
        "name",
        $decode.string,
        (name) => {
          return $decode.field(
            "event",
            $decode.dynamic,
            (event) => {
              return $decode.success(event_fired(path, name, event));
            },
          );
        },
      );
    },
  );
}

export const property_changed_kind = 2;

export function property_changed(name, value) {
  return new PropertyChanged(property_changed_kind, name, value);
}

function property_changed_decoder() {
  return $decode.field(
    "name",
    $decode.string,
    (name) => {
      return $decode.field(
        "value",
        $decode.dynamic,
        (value) => { return $decode.success(property_changed(name, value)); },
      );
    },
  );
}

export const batch_kind = 3;

export function batch(messages) {
  return new Batch(batch_kind, messages);
}

export const context_provided_kind = 4;

export function context_provided(key, value) {
  return new ContextProvided(context_provided_kind, key, value);
}

export function context_provided_decoder() {
  return $decode.field(
    "key",
    $decode.string,
    (key) => {
      return $decode.field(
        "value",
        $decode.dynamic,
        (value) => { return $decode.success(context_provided(key, value)); },
      );
    },
  );
}

function batch_decoder() {
  return $decode.field(
    "messages",
    $decode.list(server_message_decoder()),
    (messages) => { return $decode.success(batch(messages)); },
  );
}

export function server_message_decoder() {
  return $decode.field(
    "kind",
    $decode.int,
    (kind) => {
      if (kind === 0) {
        return attribute_changed_decoder();
      } else if (kind === 2) {
        return property_changed_decoder();
      } else if (kind === 1) {
        return event_fired_decoder();
      } else if (kind === 3) {
        return batch_decoder();
      } else {
        return $decode.failure(batch(toList([])), "");
      }
    },
  );
}
