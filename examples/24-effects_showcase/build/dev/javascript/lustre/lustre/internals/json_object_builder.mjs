import * as $json from "../../../gleam_json/gleam/json.mjs";
import { toList, Empty as $Empty, prepend as listPrepend } from "../../gleam.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";

export function new$() {
  return $constants.empty_list;
}

export function json(entries, key, value) {
  return listPrepend([key, value], entries);
}

export function tagged(kind) {
  return toList([["kind", $json.int(kind)]]);
}

export function build(entries) {
  return $json.object(entries);
}

export function string(entries, key, value) {
  let $ = value !== "";
  if ($) {
    return listPrepend([key, $json.string(value)], entries);
  } else {
    return entries;
  }
}

export function int(entries, key, value) {
  let $ = value !== 0;
  if ($) {
    return listPrepend([key, $json.int(value)], entries);
  } else {
    return entries;
  }
}

export function bool(entries, key, value) {
  if (value) {
    return listPrepend([key, $json.int(1)], entries);
  } else {
    return entries;
  }
}

export function list(entries, key, values, to_json) {
  if (values instanceof $Empty) {
    return entries;
  } else {
    return listPrepend([key, $json.array(values, to_json)], entries);
  }
}

export function object(entries, key, nested) {
  if (nested instanceof $Empty) {
    return entries;
  } else {
    return listPrepend([key, $json.object(nested)], entries);
  }
}
