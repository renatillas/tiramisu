import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import { CustomType as $CustomType, toBitArray, bitArraySlice } from "../../gleam.mjs";

export class Buffer extends $CustomType {
  constructor(remaining, data) {
    super();
    this.remaining = remaining;
    this.data = data;
  }
}

export function empty() {
  return new Buffer(0, toBitArray([]));
}

export function new$(data) {
  return new Buffer(0, data);
}

export function append(buffer, data) {
  let data_size = $bit_array.byte_size(data);
  let remaining = $int.max(buffer.remaining - data_size, 0);
  return new Buffer(remaining, toBitArray([buffer.data, data]));
}

export function slice(buffer, bits) {
  let bytes = bits * 8;
  let $ = buffer.data;
  if (bytes >= 0 && $.bitSize >= bytes) {
    let value = bitArraySlice($, 0, bytes);
    let rest = bitArraySlice($, bytes);
    return [value, rest];
  } else {
    return [buffer.data, toBitArray([])];
  }
}

export function with_capacity(buffer, size) {
  return new Buffer(size, buffer.data);
}

export function size(remaining) {
  return new Buffer(remaining, toBitArray([]));
}
