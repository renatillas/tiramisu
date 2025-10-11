import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $decode from "../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $pair from "../../gleam_stdlib/gleam/pair.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { Ok, Error, toList } from "../gleam.mjs";
import * as $attribute from "../lustre/attribute.mjs";
import * as $effect from "../lustre/effect.mjs";
import * as $constants from "../lustre/internals/constants.mjs";
import * as $vattr from "../lustre/vdom/vattr.mjs";
import { Event, Handler } from "../lustre/vdom/vattr.mjs";

/**
 * Dispatches a custom message from a Lustre component. This lets components
 * communicate with their parents the same way native DOM elements do.
 *
 * Any JSON-serialisable payload can be attached as additional data for any
 * event listeners to decode. This data will be on the event's `detail` property.
 */
export function emit(event, data) {
  return $effect.event(event, data);
}

/**
 * Construct a [`Handler`](#Handler) that can be used with [`advanced`](#advanced)
 * to conditionally stop propagation or prevent the default behaviour of an event.
 */
export function handler(message, prevent_default, stop_propagation) {
  return new Handler(prevent_default, stop_propagation, message);
}

function is_immediate_event(name) {
  if (name === "input") {
    return true;
  } else if (name === "change") {
    return true;
  } else if (name === "focus") {
    return true;
  } else if (name === "focusin") {
    return true;
  } else if (name === "focusout") {
    return true;
  } else if (name === "blur") {
    return true;
  } else if (name === "select") {
    return true;
  } else {
    return false;
  }
}

/**
 * Listens for the given event and then runs the given decoder on the event
 * object. If the decoder succeeds, the decoded event is dispatched to your
 * application's `update` function. If it fails, the event is silently ignored.
 *
 * The event name is typically an all-lowercase string such as "click" or "mousemove".
 * If you're listening for non-standard events (like those emitted by a custom
 * element) their event names might be slightly different.
 *
 * > **Note**: if you are developing a server component, it is important to also
 * > use [`server_component.include`](./server_component.html#include) to state
 * > which properties of the event you need to be sent to the server.
 */
export function on(name, handler) {
  return $vattr.event(
    name,
    $decode.map(handler, (msg) => { return new Handler(false, false, msg); }),
    $constants.empty_list,
    $vattr.never,
    $vattr.never,
    is_immediate_event(name),
    0,
    0,
  );
}

/**
 * Listens for the given event and then runs the given decoder on the event
 * object. This decoder is capable of _conditionally_ stopping propagation or
 * preventing the default behaviour of the event by returning a `Handler` record
 * with the appropriate flags set. This makes it possible to write event handlers
 * for more-advanced scenarios such as handling specific key presses.
 *
 * > **Note**: it is not possible to conditionally stop propagation or prevent
 * > the default behaviour of an event when using _server components_. Your event
 * > handler runs on the server, far away from the browser!
 *
 * > **Note**: if you are developing a server component, it is important to also
 * > use [`server_component.include`](./server_component.html#include) to state
 * > which properties of the event you need to be sent to the server.
 */
export function advanced(name, handler) {
  return $vattr.event(
    name,
    handler,
    $constants.empty_list,
    $vattr.possible,
    $vattr.possible,
    is_immediate_event(name),
    0,
    0,
  );
}

/**
 * Indicate that the event should have its default behaviour cancelled. This is
 * equivalent to calling `event.preventDefault()` in JavaScript.
 *
 * > **Note**: this will override the conditional behaviour of an event handler
 * > created with [`advanced`](#advanced).
 */
export function prevent_default(event) {
  if (event instanceof Event) {
    return new Event(
      event.kind,
      event.name,
      event.handler,
      event.include,
      $vattr.always,
      event.stop_propagation,
      event.immediate,
      event.debounce,
      event.throttle,
    );
  } else {
    return event;
  }
}

/**
 * Indicate that the event should not propagate to parent elements. This is
 * equivalent to calling `event.stopPropagation()` in JavaScript.
 *
 * > **Note**: this will override the conditional behaviour of an event handler
 * > created with [`advanced`](#advanced).
 */
export function stop_propagation(event) {
  if (event instanceof Event) {
    return new Event(
      event.kind,
      event.name,
      event.handler,
      event.include,
      event.prevent_default,
      $vattr.always,
      event.immediate,
      event.debounce,
      event.throttle,
    );
  } else {
    return event;
  }
}

/**
 * Use Lustre's built-in event debouncing to wait a delay after a burst of
 * events before dispatching the most recent one. You can visualise debounced
 * events like so:
 *
 * ```
 *  original : --a-b-cd--e----------f--------
 * debounced : ---------------e----------f---
 * ```
 *
 * This is particularly useful for server components where many events in quick
 * succession can introduce problems because of network latency.
 *
 * The unit of `delay` is millisecond, same as JavaScript's `setTimeout`.
 *
 * ### Example:
 *
 * ```gleam
 * type Msg {
 *     UserInputText(String)
 * }
 * 
 * html.input([event.debounce(event.on_input(fn(v) { UserInputText(v) }), 200)])
 * ```
 *
 * > **Note**: debounced events inherently introduce latency. Try to consider
 * > typical interaction patterns and experiment with different delays to balance
 * > responsiveness and update frequency.
 */
export function debounce(event, delay) {
  if (event instanceof Event) {
    return new Event(
      event.kind,
      event.name,
      event.handler,
      event.include,
      event.prevent_default,
      event.stop_propagation,
      event.immediate,
      $int.max(0, delay),
      event.throttle,
    );
  } else {
    return event;
  }
}

/**
 * Use Lustre's built-in event throttling to restrict the number of events
 * that can be dispatched in a given time period. You can visualise throttled
 * events like so:
 *
 * ```
 * original : --a-b-cd--e----------f--------
 * throttled : -a------ e----------e--------
 * ```
 *
 * This is particularly useful for server components where many events in quick
 * succession can introduce problems because of network latency.
 *
 * The unit of `delay` is millisecond, same as JavaScript's `setTimeout`.
 *
 * > **Note**: throttled events inherently reduce precision. Try to consider
 * > typical interaction patterns and experiment with different delays to balance
 * > responsiveness and update frequency.
 */
export function throttle(event, delay) {
  if (event instanceof Event) {
    return new Event(
      event.kind,
      event.name,
      event.handler,
      event.include,
      event.prevent_default,
      event.stop_propagation,
      event.immediate,
      event.debounce,
      $int.max(0, delay),
    );
  } else {
    return event;
  }
}

/**
 *
 */
export function on_click(msg) {
  return on("click", $decode.success(msg));
}

/**
 *
 */
export function on_mouse_down(msg) {
  return on("mousedown", $decode.success(msg));
}

/**
 *
 */
export function on_mouse_up(msg) {
  return on("mouseup", $decode.success(msg));
}

/**
 *
 */
export function on_mouse_enter(msg) {
  return on("mouseenter", $decode.success(msg));
}

/**
 *
 */
export function on_mouse_leave(msg) {
  return on("mouseleave", $decode.success(msg));
}

/**
 *
 */
export function on_mouse_over(msg) {
  return on("mouseover", $decode.success(msg));
}

/**
 *
 */
export function on_mouse_out(msg) {
  return on("mouseout", $decode.success(msg));
}

/**
 * Listens for key presses on an element, and dispatches a message with the
 * current key being pressed.
 */
export function on_keypress(msg) {
  return on(
    "keypress",
    $decode.field(
      "key",
      $decode.string,
      (key) => {
        let _pipe = key;
        let _pipe$1 = msg(_pipe);
        return $decode.success(_pipe$1);
      },
    ),
  );
}

/**
 * Listens for key down events on an element, and dispatches a message with the
 * current key being pressed.
 */
export function on_keydown(msg) {
  return on(
    "keydown",
    $decode.field(
      "key",
      $decode.string,
      (key) => {
        let _pipe = key;
        let _pipe$1 = msg(_pipe);
        return $decode.success(_pipe$1);
      },
    ),
  );
}

/**
 * Listens for key up events on an element, and dispatches a message with the
 * current key being released.
 */
export function on_keyup(msg) {
  return on(
    "keyup",
    $decode.field(
      "key",
      $decode.string,
      (key) => {
        let _pipe = key;
        let _pipe$1 = msg(_pipe);
        return $decode.success(_pipe$1);
      },
    ),
  );
}

/**
 * Listens for input events on elements such as `<input>`, `<textarea>` and
 * `<select>`. This handler automatically decodes the string value of the input
 * and passes it to the given message function. This is commonly used to
 * implement [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
 */
export function on_input(msg) {
  return on(
    "input",
    $decode.subfield(
      toList(["target", "value"]),
      $decode.string,
      (value) => { return $decode.success(msg(value)); },
    ),
  );
}

/**
 * Listens for change events on elements such as `<input>`, `<textarea>` and
 * `<select>`. This handler automatically decodes the string value of the input
 * and passes it to the given message function. This is commonly used to
 * implement [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
 */
export function on_change(msg) {
  return on(
    "change",
    $decode.subfield(
      toList(["target", "value"]),
      $decode.string,
      (value) => { return $decode.success(msg(value)); },
    ),
  );
}

/**
 * Listens for change events on `<input type="checkbox">` elements. This handler
 * automatically decodes the boolean value of the checkbox and passes it to
 * the given message function. This is commonly used to implement
 * [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
 */
export function on_check(msg) {
  return on(
    "change",
    $decode.subfield(
      toList(["target", "checked"]),
      $decode.bool,
      (checked) => { return $decode.success(msg(checked)); },
    ),
  );
}

function formdata_decoder() {
  let string_value_decoder = $decode.field(
    0,
    $decode.string,
    (key) => {
      return $decode.field(
        1,
        $decode.one_of(
          $decode.map($decode.string, (var0) => { return new Ok(var0); }),
          toList([$decode.success(new Error(undefined))]),
        ),
        (value) => {
          let _pipe = value;
          let _pipe$1 = $result.map(
            _pipe,
            (_capture) => { return $pair.new$(key, _capture); },
          );
          return $decode.success(_pipe$1);
        },
      );
    },
  );
  let _pipe = string_value_decoder;
  let _pipe$1 = $decode.list(_pipe);
  return $decode.map(_pipe$1, $result.values);
}

/**
 * Listens for submit events on a `<form>` element and receives a list of
 * name/value pairs for each field in the form. Files are not included in this
 * list: if you need them, you can write your own handler for the `"submit"`
 * event and decode the non-standard `detail.formData` property manually.
 *
 * This handler is best paired with the [`formal`](https://hexdocs.pm/formal/)
 * package which lets you process form submissions in a type-safe way.
 *
 * This will automatically call [`prevent_default`](#prevent_default) to stop
 * the browser's native form submission. In a Lustre app you'll want to handle
 * that yourself as an [`Effect`](./effect.html#Effect).
 */
export function on_submit(msg) {
  let _pipe = on(
    "submit",
    $decode.subfield(
      toList(["detail", "formData"]),
      formdata_decoder(),
      (formdata) => {
        let _pipe = formdata;
        let _pipe$1 = msg(_pipe);
        return $decode.success(_pipe$1);
      },
    ),
  );
  return prevent_default(_pipe);
}

export function on_focus(msg) {
  return on("focus", $decode.success(msg));
}

export function on_blur(msg) {
  return on("blur", $decode.success(msg));
}
