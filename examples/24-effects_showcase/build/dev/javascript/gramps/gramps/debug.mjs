import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { prepend as listPrepend, makeError, bitArraySlice, bitArraySliceToInt } from "../gleam.mjs";

const FILEPATH = "src/gramps/debug.gleam";

export function literal_bits(loop$source, loop$values) {
  while (true) {
    let source = loop$source;
    let values = loop$values;
    if (source.bitSize === 0) {
      return $list.reverse(values);
    } else if (source.bitSize >= 1) {
      let bit = bitArraySliceToInt(source, 0, 1, true, false);
      let rest = bitArraySlice(source, 1);
      loop$source = rest;
      loop$values = listPrepend(bit, values);
    } else {
      throw makeError(
        "panic",
        FILEPATH,
        "gramps/debug",
        7,
        "literal_bits",
        "where'd that bit go",
        {}
      )
    }
  }
}
