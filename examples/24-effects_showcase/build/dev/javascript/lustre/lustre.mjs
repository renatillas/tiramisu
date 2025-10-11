import * as $actor from "../gleam_otp/gleam/otp/actor.mjs";
import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import { identity as coerce } from "../gleam_stdlib/gleam/function.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import { Error, CustomType as $CustomType } from "./gleam.mjs";
import * as $component from "./lustre/component.mjs";
import * as $effect from "./lustre/effect.mjs";
import * as $element from "./lustre/element.mjs";
import * as $constants from "./lustre/internals/constants.mjs";
import { make_component as register } from "./lustre/runtime/client/component.ffi.mjs";
import { send, is_browser, is_registered } from "./lustre/runtime/client/runtime.ffi.mjs";
import { start as do_start } from "./lustre/runtime/client/spa.ffi.mjs";
import { start as start_server_component } from "./lustre/runtime/server/runtime.ffi.mjs";
import * as $runtime from "./lustre/runtime/server/runtime.mjs";

export { is_browser, is_registered, register, send, start_server_component };

class App extends $CustomType {
  constructor(init, update, view, config) {
    super();
    this.init = init;
    this.update = update;
    this.view = view;
    this.config = config;
  }
}

export class ActorError extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class BadComponentName extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class ComponentAlreadyRegistered extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class ElementNotFound extends $CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
}

export class NotABrowser extends $CustomType {}

/**
 * A `component` is a type of Lustre application designed to be embedded within
 * another application and has its own encapsulated update loop. This constructor
 * is almost identical to the [`application`](#application) constructor, but it
 * also allows you to specify a dictionary of attribute names and decoders.
 *
 * When a component is rendered in a parent application, it can receive data from
 * the parent application through HTML attributes and properties just like any
 * other HTML element. This dictionary of decoders allows you to specify how to
 * decode those attributes into messages your component's update loop can handle.
 *
 * > **Note**: Lustre components are conceptually a lot "heavier" than components
 * > in frameworks like React. They should be used for more complex UI widgets
 * > like a combobox with complex keyboard interactions rather than simple things
 * > like buttons or text inputs. Where possible try to think about how to build
 * > your UI with simple view functions (functions that return [Elements](./lustre/element.html#Element))
 * > and only reach for components when you really need to encapsulate that update
 * > loop.
 */
export function component(init, update, view, options) {
  return new App(init, update, view, $component.new$(options));
}

/**
 * A complete Lustre application that follows the Model-View-Update architecture
 * and can handle side effects like HTTP requests or querying the DOM. Most real
 * Lustre applications will use this constructor.
 *
 * To learn more about effects and their purpose, take a look at the
 * [`effect`](./lustre/effect.html) module or the
 * [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).
 */
export function application(init, update, view) {
  return new App(init, update, view, $component.new$($constants.empty_list));
}

/**
 * The simplest type of Lustre application. The `element` application is
 * primarily used for demonstration purposes. It renders a static Lustre `Element`
 * on the page and does not have any state or update logic.
 */
export function element(view) {
  return application(
    (_) => { return [undefined, $effect.none()]; },
    (_, _1) => { return [undefined, $effect.none()]; },
    (_) => { return view; },
  );
}

/**
 * A `simple` application has the basic Model-View-Update building blocks present
 * in all Lustre applications, but it cannot handle effects. This is a great way
 * to learn the basics of Lustre and its architecture.
 *
 * Once you're comfortable with the Model-View-Update loop and want to start
 * building more complex applications that can communicate with the outside world,
 * you'll want to use the [`application`](#application) constructor instead.
 */
export function simple(init, update, view) {
  let init$1 = (start_args) => { return [init(start_args), $effect.none()]; };
  let update$1 = (model, msg) => { return [update(model, msg), $effect.none()]; };
  return application(init$1, update$1, view);
}

/**
 * Dispatch a message to a running application's `update` function. This can be
 * used as a way for the outside world to communicate with a Lustre app without
 * the app needing to initiate things with an effect.
 */
export function dispatch(msg) {
  return new $runtime.EffectDispatchedMessage(msg);
}

/**
 * Instruct a running application to shut down. For client SPAs this will stop
 * the runtime and unmount the app from the DOM. For server components, this will
 * stop the runtime and prevent any further patches from being sent to connected
 * clients.
 */
export function shutdown() {
  return new $runtime.SystemRequestedShutdown();
}

/**
 * Start a constructed application as a client-side single-page application (SPA).
 * This is the most typical way to start a Lustre application and will *only* work
 * in the browser
 *
 * The second argument is a [CSS selector](https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector)
 * used to locate the DOM element where the application will be mounted on to.
 * The most common selectors are `"#app"` to target an element with an id of `app`
 * or `[data-lustre-app]` to target an element with a `data-lustre-app` attribute.
 *
 * The third argument is the starting data for the application. This is passed
 * to the application's `init` function.
 */
export function start(app, selector, start_args) {
  return $bool.guard(
    !is_browser(),
    new Error(new NotABrowser()),
    () => { return do_start(app, selector, start_args); },
  );
}
