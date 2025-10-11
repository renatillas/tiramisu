import * as $json from "../../../gleam_json/gleam/json.mjs";
import {
  newWorker as new$,
  postMessage as post_message,
  onMessage as on_message,
} from "../../worker_ffi.mjs";

export { new$, on_message, post_message };
