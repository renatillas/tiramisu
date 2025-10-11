import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import { Ok, Error } from "../../gleam.mjs";
import * as $array from "../../gleam/javascript/array.mjs";
import {
  newPromise as new$,
  start_promise as start,
  resolve,
  rescue,
  then_await as await$,
  map_promise as map,
  all_promises as await_array,
  all_promises as do_await_list,
  race_promises as race_list,
  race_promises as race_array,
  wait,
} from "../../gleam_javascript_ffi.mjs";

export {
  await$,
  await_array,
  map,
  new$,
  race_array,
  race_list,
  rescue,
  resolve,
  start,
  wait,
};

/**
 * Run a function on the value a promise resolves to, after it has resolved.
 * The value returned is discarded.
 */
export function tap(promise, callback) {
  let _pipe = promise;
  return map(
    _pipe,
    (a) => {
      callback(a);
      return a;
    },
  );
}

/**
 * Run a function on the value a promise resolves to, after it has resolved.
 *
 * The function is only called if the value is `Ok`, and the returned becomes
 * the new value contained by the promise.
 *
 * This is a convenience functin that combines the `map` function with `result.try`.
 */
export function map_try(promise, callback) {
  let _pipe = promise;
  return map(
    _pipe,
    (result) => {
      if (result instanceof Ok) {
        let a = result[0];
        return callback(a);
      } else {
        return result;
      }
    },
  );
}

/**
 * Run a promise returning function on the value a promise resolves to, after
 * it has resolved.
 *
 * The function is only called if the value is `Ok`, and the returned becomes
 * the new value contained by the promise.
 *
 * This is a convenience functin that combines the `await` function with
 * `result.try`.
 */
export function try_await(promise, callback) {
  let _pipe = promise;
  return await$(
    _pipe,
    (result) => {
      if (result instanceof Ok) {
        let a = result[0];
        return callback(a);
      } else {
        let e = result[0];
        return resolve(new Error(e));
      }
    },
  );
}

/**
 * Chain an asynchronous operation onto an list of promises, so it runs after the
 * promises have resolved.
 *
 * This is the equivilent of the `Promise.all` JavaScript static method.
 */
export function await_list(xs) {
  let _pipe = xs;
  let _pipe$1 = do_await_list(_pipe);
  return map(_pipe$1, $array.to_list);
}
