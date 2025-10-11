import * as $process from "../../gleam_erlang/gleam/erlang/process.mjs";
import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { toList, CustomType as $CustomType } from "../gleam.mjs";

class Effect extends $CustomType {
  constructor(synchronous, before_paint, after_paint) {
    super();
    this.synchronous = synchronous;
    this.before_paint = before_paint;
    this.after_paint = after_paint;
  }
}

class Actions extends $CustomType {
  constructor(dispatch, emit, select, root, provide) {
    super();
    this.dispatch = dispatch;
    this.emit = emit;
    this.select = select;
    this.root = root;
    this.provide = provide;
  }
}

function do_comap_select(_, _1, _2) {
  return undefined;
}

function do_comap_actions(actions, f) {
  return new Actions(
    (msg) => { return actions.dispatch(f(msg)); },
    actions.emit,
    (selector) => { return do_comap_select(actions, selector, f); },
    actions.root,
    actions.provide,
  );
}

function do_map(effects, f) {
  return $list.map(
    effects,
    (effect) => {
      return (actions) => { return effect(do_comap_actions(actions, f)); };
    },
  );
}

/**
 * Transform the result of an effect. This is useful for mapping over effects
 * produced by other libraries or modules.
 *
 * > **Note**: Remember that effects are not _required_ to dispatch any messages.
 * > Your mapping function may never be called!
 */
export function map(effect, f) {
  return new Effect(
    do_map(effect.synchronous, f),
    do_map(effect.before_paint, f),
    do_map(effect.after_paint, f),
  );
}

/**
 * Perform a side effect by supplying your own `dispatch` and `emit`functions.
 * This is primarily used internally by the server component runtime, but it is
 * may also useful for testing.
 *
 * Because this is run outside of the runtime, timing-related effects scheduled
 * by `before_paint` and `after_paint` will **not** be run.
 *
 * > **Note**: For now, you should **not** consider this function a part of the
 * > public API. It may be removed in a future minor or patch release. If you have
 * > a specific use case for this function, we'd love to hear about it! Please
 * > reach out on the [Gleam Discord](https://discord.gg/Fm8Pwmy) or
 * > [open an issue](https://github.com/lustre-labs/lustre/issues/new)!
 * 
 * @ignore
 */
export function perform(effect, dispatch, emit, select, root, provide) {
  let actions = new Actions(dispatch, emit, select, root, provide);
  return $list.each(effect.synchronous, (run) => { return run(actions); });
}

const empty = /* @__PURE__ */ new Effect(
  /* @__PURE__ */ toList([]),
  /* @__PURE__ */ toList([]),
  /* @__PURE__ */ toList([]),
);

/**
 * Most Lustre applications need to return a tuple of `#(model, Effect(msg))`
 * from their `init` and `update` functions. If you don't want to perform any
 * side effects, you can use `none` to tell the runtime there's no work to do.
 */
export function none() {
  return empty;
}

/**
 * Construct your own reusable effect from a custom callback. This callback is
 * called with a `dispatch` function you can use to send messages back to your
 * application's `update` function.
 *
 * Example using the `window` module from the `plinth` library to dispatch a
 * message on the browser window object's `"visibilitychange"` event.
 *
 * ```gleam
 * import lustre/effect.{type Effect}
 * import plinth/browser/window
 *
 * type Model {
 *   Model(Int)
 * }
 *
 * type Msg {
 *   FetchState
 * }
 *
 * fn init(_flags) -> #(Model, Effect(Msg)) {
 *   #(
 *     Model(0),
 *     effect.from(fn(dispatch) {
 *       window.add_event_listener("visibilitychange", fn(_event) {
 *         dispatch(FetchState)
 *       })
 *     }),
 *   )
 * }
 * ```
 */
export function from(effect) {
  let task = (actions) => {
    let dispatch = actions.dispatch;
    return effect(dispatch);
  };
  return new Effect(toList([task]), empty.before_paint, empty.after_paint);
}

/**
 * Schedule a side effect that is guaranteed to run after your `view` function
 * is called and the DOM has been updated, but **before** the browser has
 * painted the screen. This effect is useful when you need to read from the DOM
 * or perform other operations that might affect the layout of your application.
 *
 * In addition to the `dispatch` function, your callback will also be provided
 * with root element of your app or component. This is especially useful inside
 * of components, giving you a reference to the [Shadow Root](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot).
 *
 * Messages dispatched immediately in this effect will trigger a second re-render
 * of your application before the browser paints the screen. This let's you read
 * the state of the DOM, update your model, and then render a second time with
 * the additional information.
 *
 * > **Note**: dispatching messages synchronously in this effect can lead to
 * > degraded performance if not used correctly. In the worst case you can lock
 * > up the browser and prevent it from painting the screen _at all_.
 *
 * > **Note**: There is no concept of a "paint" for server components. These
 * > effects will be ignored in those contexts and never run.
 */
export function before_paint(effect) {
  let task = (actions) => {
    let root = actions.root();
    let dispatch = actions.dispatch;
    return effect(dispatch, root);
  };
  return new Effect(empty.synchronous, toList([task]), empty.after_paint);
}

/**
 * Schedule a side effect that is guaranteed to run after the browser has painted
 * the screen.
 *
 * In addition to the `dispatch` function, your callback will also be provided
 * with root element of your app or component. This is especially useful inside
 * of components, giving you a reference to the [Shadow Root](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot).
 *
 * > **Note**: There is no concept of a "paint" for server components. These
 * > effects will be ignored in those contexts and never run.
 */
export function after_paint(effect) {
  let task = (actions) => {
    let root = actions.root();
    let dispatch = actions.dispatch;
    return effect(dispatch, root);
  };
  return new Effect(empty.synchronous, empty.before_paint, toList([task]));
}

/**
 * Emit a custom event from a component as an effect. Parents can listen to these
 * events in their `view` function like any other HTML event. Any data you pass
 * to `effect.emit` can be accessed by event listeners through the `detail` property
 * of the event object.
 * 
 * @ignore
 */
export function event(name, data) {
  let task = (actions) => { return actions.emit(name, data); };
  return new Effect(toList([task]), empty.before_paint, empty.after_paint);
}

export function select(_) {
  return empty;
}

export function provide(key, value) {
  let task = (actions) => { return actions.provide(key, value); };
  return new Effect(toList([task]), empty.before_paint, empty.after_paint);
}

/**
 * Batch multiple effects to be performed at the same time.
 *
 * > **Note**: The runtime makes no guarantees about the order on which effects
 * > are performed! If you need to chain or sequence effects together, you have
 * > two broad options:
 * >
 * > 1. Create variants of your `msg` type to represent each step in the sequence
 * >    and fire off the next effect in response to the previous one.
 * >
 * > 2. If you're defining effects yourself, consider whether or not you can handle
 * >    the sequencing inside the effect itself.
 */
export function batch(effects) {
  return $list.fold(
    effects,
    empty,
    (acc, eff) => {
      return new Effect(
        $list.fold(eff.synchronous, acc.synchronous, $list.prepend),
        $list.fold(eff.before_paint, acc.before_paint, $list.prepend),
        $list.fold(eff.after_paint, acc.after_paint, $list.prepend),
      );
    },
  );
}
