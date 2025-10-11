import * as $promise from "../../../gleam_javascript/gleam/javascript/promise.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import { DecodeError } from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import {
  cast as do_cast,
  addEventListener as add_event_listener,
  getAttribute as get_attribute,
  setAttribute as set_attribute,
  setInnerHTML as set_inner_html,
  setInnerText as set_inner_text,
  innerText as inner_text,
  insertAdjacentElement as do_insert_adjacent_element,
  insertAdjacentHTML as do_insert_adjacent_html,
  insertAdjacentText as do_insert_adjacent_text,
  nextElementSibling as next_element_sibling,
  closest,
  requestFullscreen as request_fullscreen,
  scrollIntoView as scroll_into_view,
  scrollHeight as scroll_height,
  scrollLeft as scroll_left,
  scrollTop as scroll_top,
  scrollWidth as scroll_width,
  setScrollHeight as set_scroll_height,
  setScrollLeft as set_scroll_left,
  setScrollTop as set_scroll_top,
  setScrollWidth as set_scroll_width,
  appendChild as append_child,
  remove,
  datasetGet as dataset_get,
  value,
  setValue as set_value,
  focus,
  blur,
  selectionStart as selection_start,
  setSelectionRange as set_selection_range,
  setTextContent as set_text_content,
  getChecked as get_checked,
  contains,
  classList as class_list,
} from "../../element_ffi.mjs";
import { Ok, Error, toList, CustomType as $CustomType } from "../../gleam.mjs";
import * as $dom_token_list from "../../plinth/browser/dom_token_list.mjs";
import * as $event from "../../plinth/browser/event.mjs";

export {
  add_event_listener,
  append_child,
  blur,
  class_list,
  closest,
  contains,
  dataset_get,
  focus,
  get_attribute,
  get_checked,
  inner_text,
  next_element_sibling,
  remove,
  request_fullscreen,
  scroll_height,
  scroll_into_view,
  scroll_left,
  scroll_top,
  scroll_width,
  selection_start,
  set_attribute,
  set_inner_html,
  set_inner_text,
  set_scroll_height,
  set_scroll_left,
  set_scroll_top,
  set_scroll_width,
  set_selection_range,
  set_text_content,
  set_value,
  value,
};

export class BeforeBegin extends $CustomType {}

export class AfterBegin extends $CustomType {}

export class BeforeEnd extends $CustomType {}

export class AfterEnd extends $CustomType {}

export function cast(raw) {
  let $ = do_cast(raw);
  if ($ instanceof Ok) {
    return $;
  } else {
    return new Error(
      new DecodeError("Element", $dynamic.classify(raw), toList([])),
    );
  }
}

function position_to_string(position) {
  if (position instanceof BeforeBegin) {
    return "beforebegin";
  } else if (position instanceof AfterBegin) {
    return "afterbegin";
  } else if (position instanceof BeforeEnd) {
    return "beforeend";
  } else {
    return "afterend";
  }
}

export function insert_adjacent_element(target, position, element) {
  let position$1 = position_to_string(position);
  return do_insert_adjacent_element(target, position$1, element);
}

export function insert_adjacent_html(target, position, html) {
  let position$1 = position_to_string(position);
  return do_insert_adjacent_html(target, position$1, html);
}

export function insert_adjacent_text(target, position, text) {
  let position$1 = position_to_string(position);
  return do_insert_adjacent_text(target, position$1, text);
}
