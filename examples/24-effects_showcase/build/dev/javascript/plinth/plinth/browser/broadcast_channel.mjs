import * as $json from "../../../gleam_json/gleam/json.mjs";
import { new_ as new$, postMessage as post_message, onMessage as on_message } from "../../broadcast_channel_ffi.mjs";
import * as $message from "../../plinth/browser/message.mjs";

export { new$, on_message, post_message };
