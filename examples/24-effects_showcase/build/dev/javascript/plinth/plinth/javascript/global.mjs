import {
  encodeURI as encode_uri,
  decodeURI as decode_uri,
  decodeURIComponent as decode_uri_component,
  setTimeout as set_timeout,
  clearTimeout as clear_timeout,
  setInterval as set_interval,
  clearInterval as clear_interval,
} from "../../global_ffi.mjs";

export {
  clear_interval,
  clear_timeout,
  decode_uri,
  decode_uri_component,
  encode_uri,
  set_interval,
  set_timeout,
};
