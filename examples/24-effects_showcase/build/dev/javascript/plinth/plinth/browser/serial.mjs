import * as $array from "../../../gleam_javascript/gleam/javascript/array.mjs";
import * as $promise from "../../../gleam_javascript/gleam/javascript/promise.mjs";
import {
  requestPort as request_port,
  getPorts as get_ports,
  getInfo as get_info,
  open,
  read,
} from "../../serial_ffi.mjs";

export { get_info, get_ports, open, read, request_port };
