import * as $process from "../../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $json from "../../../../gleam_json/gleam/json.mjs";
import * as $actor from "../../../../gleam_otp/gleam/otp/actor.mjs";
import * as $dict from "../../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $list from "../../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../../gleam_stdlib/gleam/result.mjs";
import * as $set from "../../../../gleam_stdlib/gleam/set.mjs";
import { Ok, Error, CustomType as $CustomType } from "../../../gleam.mjs";
import * as $effect from "../../../lustre/effect.mjs";
import * as $transport from "../../../lustre/runtime/transport.mjs";
import * as $diff from "../../../lustre/vdom/diff.mjs";
import { Diff, diff } from "../../../lustre/vdom/diff.mjs";
import * as $events from "../../../lustre/vdom/events.mjs";
import * as $vnode from "../../../lustre/vdom/vnode.mjs";
import {
  throw_server_component_error as handle_effect,
  throw_server_component_error as broadcast,
  throw_server_component_error as loop,
  throw_server_component_error as start,
} from "../client/runtime.ffi.mjs";

export { start };

export class State extends $CustomType {
  constructor(self, selector, base_selector, model, update, view, config, vdom, events, providers, subscribers, callbacks) {
    super();
    this.self = self;
    this.selector = selector;
    this.base_selector = base_selector;
    this.model = model;
    this.update = update;
    this.view = view;
    this.config = config;
    this.vdom = vdom;
    this.events = events;
    this.providers = providers;
    this.subscribers = subscribers;
    this.callbacks = callbacks;
  }
}

export class Config extends $CustomType {
  constructor(open_shadow_root, adopt_styles, attributes, properties, contexts) {
    super();
    this.open_shadow_root = open_shadow_root;
    this.adopt_styles = adopt_styles;
    this.attributes = attributes;
    this.properties = properties;
    this.contexts = contexts;
  }
}

export class ClientDispatchedMessage extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

export class ClientRegisteredSubject extends $CustomType {
  constructor(client) {
    super();
    this.client = client;
  }
}

export class ClientDeregisteredSubject extends $CustomType {
  constructor(client) {
    super();
    this.client = client;
  }
}

export class ClientRegisteredCallback extends $CustomType {
  constructor(callback) {
    super();
    this.callback = callback;
  }
}

export class ClientDeregisteredCallback extends $CustomType {
  constructor(callback) {
    super();
    this.callback = callback;
  }
}

export class EffectAddedSelector extends $CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
}

export class EffectDispatchedMessage extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

export class EffectEmitEvent extends $CustomType {
  constructor(name, data) {
    super();
    this.name = name;
    this.data = data;
  }
}

export class EffectProvidedValue extends $CustomType {
  constructor(key, value) {
    super();
    this.key = key;
    this.value = value;
  }
}

export class MonitorReportedDown extends $CustomType {
  constructor(monitor) {
    super();
    this.monitor = monitor;
  }
}

export class SystemRequestedShutdown extends $CustomType {}

function handle_attribute_change(attributes, name, value) {
  let $ = $dict.get(attributes, name);
  if ($ instanceof Ok) {
    let handler = $[0];
    return handler(value);
  } else {
    return new Error(undefined);
  }
}

function handle_property_change(properties, name, value) {
  let $ = $dict.get(properties, name);
  if ($ instanceof Ok) {
    let decoder = $[0];
    let _pipe = $decode.run(value, decoder);
    return $result.replace_error(_pipe, undefined);
  } else {
    return new Error(undefined);
  }
}

function handle_client_message(state, message) {
  if (message instanceof $transport.Batch) {
    let messages = message.messages;
    return $list.fold(messages, state, handle_client_message);
  } else if (message instanceof $transport.AttributeChanged) {
    let name = message.name;
    let value = message.value;
    let $ = handle_attribute_change(state.config.attributes, name, value);
    if ($ instanceof Ok) {
      let msg = $[0];
      let $1 = state.update(state.model, msg);
      let model;
      let effect;
      model = $1[0];
      effect = $1[1];
      let vdom = state.view(model);
      handle_effect(state.self, effect);
      return new State(
        state.self,
        state.selector,
        state.base_selector,
        model,
        state.update,
        state.view,
        state.config,
        vdom,
        state.events,
        state.providers,
        state.subscribers,
        state.callbacks,
      );
    } else {
      return state;
    }
  } else if (message instanceof $transport.PropertyChanged) {
    let name = message.name;
    let value = message.value;
    let $ = handle_property_change(state.config.properties, name, value);
    if ($ instanceof Ok) {
      let msg = $[0];
      let $1 = state.update(state.model, msg);
      let model;
      let effect;
      model = $1[0];
      effect = $1[1];
      let vdom = state.view(model);
      handle_effect(state.self, effect);
      return new State(
        state.self,
        state.selector,
        state.base_selector,
        model,
        state.update,
        state.view,
        state.config,
        vdom,
        state.events,
        state.providers,
        state.subscribers,
        state.callbacks,
      );
    } else {
      return state;
    }
  } else if (message instanceof $transport.EventFired) {
    let path = message.path;
    let name = message.name;
    let event = message.event;
    let $ = $events.handle(state.events, path, name, event);
    let $1 = $[1];
    if ($1 instanceof Ok) {
      let events = $[0];
      let handler = $1[0];
      let $2 = state.update(state.model, handler.message);
      let model;
      let effect;
      model = $2[0];
      effect = $2[1];
      let vdom = state.view(model);
      handle_effect(state.self, effect);
      return new State(
        state.self,
        state.selector,
        state.base_selector,
        model,
        state.update,
        state.view,
        state.config,
        vdom,
        events,
        state.providers,
        state.subscribers,
        state.callbacks,
      );
    } else {
      let events = $[0];
      return new State(
        state.self,
        state.selector,
        state.base_selector,
        state.model,
        state.update,
        state.view,
        state.config,
        state.vdom,
        events,
        state.providers,
        state.subscribers,
        state.callbacks,
      );
    }
  } else {
    let key = message.key;
    let value = message.value;
    let $ = $dict.get(state.config.contexts, key);
    if ($ instanceof Ok) {
      let decoder = $[0];
      let $1 = $decode.run(value, decoder);
      if ($1 instanceof Ok) {
        let context = $1[0];
        let $2 = state.update(state.model, context);
        let model;
        let effect;
        model = $2[0];
        effect = $2[1];
        let vdom = state.view(model);
        handle_effect(state.self, effect);
        return new State(
          state.self,
          state.selector,
          state.base_selector,
          model,
          state.update,
          state.view,
          state.config,
          vdom,
          state.events,
          state.providers,
          state.subscribers,
          state.callbacks,
        );
      } else {
        return state;
      }
    } else {
      return state;
    }
  }
}
