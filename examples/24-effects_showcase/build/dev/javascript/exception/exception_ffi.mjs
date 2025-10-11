import { Ok, Error as GError } from "./gleam.mjs";
import { Errored, Thrown } from "./exception.mjs";

export function on_crash(cleanup, body) {
  try {
    return body();
  } catch (e) {
    cleanup();
    throw e;
  }
}

export function rescue(f) {
  try {
    return new Ok(f());
  } catch (e) {
    if (e instanceof Error) {
      return new GError(new Errored(e));
    } else {
      return new GError(new Thrown(e));
    }
  }
}

export function defer(cleanup, body) {
  try {
    return body();
  } finally {
    cleanup();
  }
}
