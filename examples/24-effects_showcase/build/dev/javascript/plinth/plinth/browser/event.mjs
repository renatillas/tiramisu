import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import { DecodeError } from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import {
  castEvent as do_cast_event,
  target,
  currentTarget as current_target,
  preventDefault as prevent_default,
  stopPropagation as stop_propagation,
  castKeyboardEvent as do_cast_keyboard_event,
  altKey as alt_key,
  code,
  ctrlKey as ctrl_key,
  isComposing as is_composing,
  key,
  metaKey as meta_key,
  shiftKey as shift_key,
} from "../../event_ffi.mjs";
import { Ok, Error, toList } from "../../gleam.mjs";

export {
  alt_key,
  code,
  ctrl_key,
  current_target,
  is_composing,
  key,
  meta_key,
  prevent_default,
  shift_key,
  stop_propagation,
  target,
};

function wrap_cast(raw, f) {
  let $ = f(raw);
  if ($ instanceof Ok) {
    return $;
  } else {
    return new Error(
      new DecodeError("Event", $dynamic.classify(raw), toList([])),
    );
  }
}

export function cast_event(raw) {
  return wrap_cast(raw, do_cast_event);
}

export function cast_keyboard_event(raw) {
  return wrap_cast(raw, do_cast_keyboard_event);
}
