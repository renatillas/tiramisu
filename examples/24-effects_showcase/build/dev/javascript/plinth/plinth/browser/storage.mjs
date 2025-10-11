import * as $promise from "../../../gleam_javascript/gleam/javascript/promise.mjs";
import { Ok, CustomType as $CustomType } from "../../gleam.mjs";
import * as $file_system from "../../plinth/browser/file_system.mjs";
import {
  getStorage as get,
  estimate as do_estimate,
  getDirectory as get_directory,
  persist,
  persisted,
} from "../../plinth_browser_storage_ffi.mjs";

export { get, get_directory, persist, persisted };

export class Estimate extends $CustomType {
  constructor(quota, usage) {
    super();
    this.quota = quota;
    this.usage = usage;
  }
}

export function estimate(storage) {
  return $promise.map_try(
    do_estimate(storage),
    (_use0) => {
      let quota;
      let usage;
      quota = _use0[0];
      usage = _use0[1];
      return new Ok(new Estimate(quota, usage));
    },
  );
}
