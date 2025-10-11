import { Ok, Error as GError } from "./gleam.mjs";
import Dict from "../gleam_stdlib/dict.mjs";

export function get(key) {
  let value;

  if (globalThis.Deno) {
    value = Deno.env.get(key);
  } else if (globalThis.process) {
    value = process.env[key];
  }

  if (value === undefined) {
    return new GError(undefined);
  } else {
    return new Ok(value);
  }
}

export function set(key, value) {
  if (globalThis.Deno) {
    Deno.env.set(key, value);
  } else if (globalThis.process) {
    process.env[key] = value;
  }
}

export function unset(key) {
  if (globalThis.Deno) {
    Deno.env.delete(key);
  } else if (globalThis.process) {
    delete process.env[key];
  }
}

export function all() {
  if (globalThis.Deno) {
    return Dict.fromObject(Deno.env.toObject());
  } else if (globalThis.process) {
    return Dict.fromObject(process.env);
  } else {
    return new Dict();
  }
}
