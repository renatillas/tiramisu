import {
  localStorage as local,
  sessionStorage as session,
  length,
  key,
  getItem as get_item,
  setItem as set_item,
  removeItem as remove_item,
  clear,
} from "../../storage_ffi.mjs";

export { clear, get_item, key, length, local, remove_item, session, set_item };
