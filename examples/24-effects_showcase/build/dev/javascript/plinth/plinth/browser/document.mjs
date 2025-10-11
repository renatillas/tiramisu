import * as $array from "../../../gleam_javascript/gleam/javascript/array.mjs";
import * as $promise from "../../../gleam_javascript/gleam/javascript/promise.mjs";
import {
  querySelector as query_selector,
  querySelectorAll as query_selector_all,
  addEventListener as add_event_listener,
  createElement as create_element,
  createTextNode as create_text_node,
  body,
  getElementById as get_element_by_id,
  getElementsByTagName as get_elements_by_tag_name,
  readyState as ready_state,
  hidden,
  visibilityState as visibility_state,
  title,
  setTitle as set_title,
  fullscreenElement as fullscreen_element,
  exitFullscreen as exit_fullscreen,
} from "../../document_ffi.mjs";
import * as $element from "../../plinth/browser/element.mjs";
import * as $event from "../../plinth/browser/event.mjs";

export {
  add_event_listener,
  body,
  create_element,
  create_text_node,
  exit_fullscreen,
  fullscreen_element,
  get_element_by_id,
  get_elements_by_tag_name,
  hidden,
  query_selector,
  query_selector_all,
  ready_state,
  set_title,
  title,
  visibility_state,
};
