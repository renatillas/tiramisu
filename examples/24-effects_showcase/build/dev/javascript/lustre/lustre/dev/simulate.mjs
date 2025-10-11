import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import { identity as erase } from "../../../gleam_stdlib/gleam/function.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $pair from "../../../gleam_stdlib/gleam/pair.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { Ok, toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import * as $query from "../../lustre/dev/query.mjs";
import * as $effect from "../../lustre/effect.mjs";
import * as $element from "../../lustre/element.mjs";
import * as $events from "../../lustre/vdom/events.mjs";
import * as $path from "../../lustre/vdom/path.mjs";

class App extends $CustomType {
  constructor(init, update, view) {
    super();
    this.init = init;
    this.update = update;
    this.view = view;
  }
}

class Simulation extends $CustomType {
  constructor(update, view, history, model, html) {
    super();
    this.update = update;
    this.view = view;
    this.history = history;
    this.model = model;
    this.html = html;
  }
}

export class Dispatch extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

export class Event extends $CustomType {
  constructor(target, name, data) {
    super();
    this.target = target;
    this.name = name;
    this.data = data;
  }
}

export class Problem extends $CustomType {
  constructor(name, message) {
    super();
    this.name = name;
    this.message = message;
  }
}

/**
 * Construct a simulated simple Lustre application. The simulation can be started
 * with the [`start`](#start) function by providing the initial arguments for
 * your app's `init` function.
 *
 * DOM events and messages dispatched by effects can be simulated using the
 * [`event`](#event) and [`messgae`](#message) functions.
 */
export function simple(init, update, view) {
  return new App(
    (args) => { return [init(args), $effect.none()]; },
    (model, msg) => { return [update(model, msg), $effect.none()]; },
    view,
  );
}

/**
 * Construct a simulated Lustre application. The simulation can be started
 * with the [`start`](#start) function by providing the initial arguments for
 * your app's `init` function.
 *
 * DOM events and messages dispatched by effects can be simulated using the
 * [`event`](#event) and [`messgae`](#message) functions.
 *
 * > **Note**: simulated apps do not run any effects! You can simulate the result
 * > of an effect by using the [`message`](#message) function, but to test side
 * > effects you should test your application in a real environment.
 */
export function application(init, update, view) {
  return new App(init, update, view);
}

/**
 * Start a simulated Lustre application. Once a simulation is running you can
 * use the [`message`](#message) and [`event`](#event) functions to simulate
 * events
 */
export function start(app, args) {
  let $ = app.init(args);
  let model$1;
  model$1 = $[0];
  let html = app.view(model$1);
  return new Simulation(app.update, app.view, toList([]), model$1, html);
}

/**
 * Simulate a message sent directly to the runtime. This is often used to mimic
 * the result of some effect you would have run in a real environment. For example,
 * you might simulate a click event on a login button and then simulate the
 * successful response from the server by calling this function with the message
 * you would dispatch from the effect:
 *
 * ```gleam
 * import birdie
 * import lustre/dev/simulate
 * import lustre/dev/query
 * import lustre/element
 *
 * pub fn login_test() {
 *   let app = simulate.application(init:, update:, view:)
 *   let login_button = query.element(matching: query.id("login"))
 *   let user = User(name: "Lucy")
 *
 *   simulate.start(app, Nil)
 *   |> simulate.event(on: login_button, name: "click", data: [])
 *   // Simulate a successful response from the server
 *   |> simulate.message(ApiReturnedUser(Ok(user)))
 *   |> simulate.view
 *   |> element.to_readable_string
 *   |> birdie.snap("Successful login")
 * }
 * ```
 *
 * > **Note**: your app's `view` function will probably be rendering quite a lot
 * > of HTML! To make your snapshots more meaningful, you might want to couple
 * > this with the [`query`](./query.html) module to only snapshot parts of the
 * > page that are relevant to the test.
 */
export function message(simulation, msg) {
  let $ = simulation.update(simulation.model, msg);
  let model$1;
  model$1 = $[0];
  let html = simulation.view(model$1);
  let history$1 = listPrepend(new Dispatch(msg), simulation.history);
  return new Simulation(
    simulation.update,
    simulation.view,
    history$1,
    model$1,
    html,
  );
}

/**
 * Log a problem that occured during the simulation. This function is useful for
 * external packages that want to provide functions to simulate certain effects
 * that may fail in the real world. For example, a routing package may log a
 * problem if a link has an invalid `href` attribute that would cause no message
 * to be dispatched.
 *
 * > **Note**: logging a problem will not stop the simulation from running, just
 * > like a real application!
 */
export function problem(simulation, name, message) {
  let history$1 = listPrepend(new Problem(name, message), simulation.history);
  return new Simulation(
    simulation.update,
    simulation.view,
    history$1,
    simulation.model,
    simulation.html,
  );
}

/**
 * Introspect the current `model` of a running simulation. This can be useful
 * to debug why a simulation is not producing the view you expect.
 */
export function model(simulation) {
  return simulation.model;
}

/**
 * Introspect the current `view` of a running simulation. Typically you would
 * use this with a snapshot testing library like [`birdie`](https://hexdocs.pm/birdie/index.html)
 * and/or with the [`query`](./query.html) api to make assertions about the state
 * of the page.
 */
export function view(simulation) {
  return simulation.html;
}

/**
 * Receive the current [`Event`](#Event) log of a running simulation. You can
 * use this to produce more detailed snapshots by also rendering the sequence of
 * events that produced the given view.
 *
 * In addition to simulated DOM events and message dispatch, the event log will
 * also include entries for when the queried event target could not be found in
 * the view and cases where an event was fired but not handled by your application.
 */
export function history(simulation) {
  let _pipe = simulation.history;
  return $list.reverse(_pipe);
}

/**
 * Simulate a DOM event on the first element that matches the given query. The
 * payload represents a simulated event object, and should be used to pass data
 * you expect your event handlers to decode.
 *
 * If no element matches the query, an [`EventTargetNotFound`](#Event) event is
 * logged in the simulation history. If an element is found, but the application
 * has no handler for the event, the [`EventHandlerNotFound`](#Event) event is
 * logged instead.
 *
 * > **Note**: this is not a perfect simulation of a real DOM event. There is no
 * > capture phase of a simulated event and simulated events will not bubble up
 * > to parent elements.
 */
export function event(simulation, query, event, payload) {
  return $result.unwrap_both(
    $result.try$(
      $result.replace_error(
        $query.find_path(simulation.html, query, 0, $path.root),
        problem(
          simulation,
          "EventTargetNotFound",
          "No element matching " + $query.to_readable_string(query),
        ),
      ),
      (_use0) => {
        let path;
        path = _use0[1];
        let events = $events.from_node(simulation.html);
        let data = $json.object(payload);
        return $result.try$(
          $result.replace_error(
            $pair.second(
              $events.handle(
                events,
                $path.to_string(path),
                event,
                (() => {
                  let _pipe = data;
                  let _pipe$1 = $json.to_string(_pipe);
                  let _pipe$2 = $json.parse(_pipe$1, $decode.dynamic);
                  return $result.unwrap(_pipe$2, erase(undefined));
                })(),
              ),
            ),
            problem(
              simulation,
              "EventHandlerNotFound",
              (("No " + event) + " handler for element matching ") + $query.to_readable_string(
                query,
              ),
            ),
          ),
          (handler) => {
            let $ = simulation.update(simulation.model, handler.message);
            let model$1;
            model$1 = $[0];
            let html = simulation.view(model$1);
            let history$1 = listPrepend(
              new Event(query, event, data),
              simulation.history,
            );
            return new Ok(
              new Simulation(
                simulation.update,
                simulation.view,
                history$1,
                model$1,
                html,
              ),
            );
          },
        );
      },
    ),
  );
}

/**
 * A convenience function that simulates a click event on the first element
 * matching the given query. This event will have no payload and is only
 * appropriate for event handlers that use Lustre's `on_click` handler or custom
 * handlers that do not decode the event payload.
 */
export function click(simulation, query) {
  return event(simulation, query, "click", toList([]));
}

/**
 * Simulate an input event on the first element matching the given query. This
 * helper has an event payload that looks like this:
 *
 * ```json
 * {
 *   "target": {
 *     "value": value
 *   }
 * }
 * ```
 *
 * and is appropriate for event handlers that use Lustre's `on_input` handler
 * or custom handlers that only decode the event target value.
 */
export function input(simulation, query, value) {
  return event(
    simulation,
    query,
    "input",
    toList([["target", $json.object(toList([["value", $json.string(value)]]))]]),
  );
}

/**
 * Simulate a submit event on the first element matching the given query. The
 * simulated event payload looks like this:
 *
 * ```json
 * {
 *   "detail": {
 *     "formData": [
 *       ...
 *     ]
 *   }
 * }
 * ```
 *
 * and is appropriate for event handlers that use Lustre's `on_submit` handler
 * or custom handlers that only decode the non-standard `detail.formData`
 * property.
 */
export function submit(simulation, query, form_data) {
  return event(
    simulation,
    query,
    "submit",
    toList([
      [
        "detail",
        $json.object(
          toList([
            [
              "formData",
              $json.array(
                form_data,
                (entry) => {
                  return $json.preprocessed_array(
                    toList([$json.string(entry[0]), $json.string(entry[1])]),
                  );
                },
              ),
            ],
          ]),
        ),
      ],
    ]),
  );
}
