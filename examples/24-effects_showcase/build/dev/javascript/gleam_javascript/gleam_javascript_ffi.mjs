import { Ok, Error } from "./gleam.mjs";
export function toArray(list) {
  return list.toArray();
}

export function map(thing, fn) {
  return thing.map(fn);
}

export function length(thing) {
  return thing.length;
}

export function reduce(thing, acc, fn) {
  return thing.reduce(fn, acc);
}

export function reduceRight(thing, acc, fn) {
  return thing.reduceRight(fn, acc);
}

export function index(thing, index) {
  return index in thing ? new Ok(thing[index]) : new Error(undefined);
}

export function object_from_entries(entries) {
  return Object.fromEntries(entries);
}

export function new_symbol(name) {
  return Symbol(name);
}
export function get_symbol(name) {
  return Symbol.for(name);
}
export function symbol_description(symbol) {
  const description = symbol.description;
  if (symbol.description === undefined) return new Error(undefined);
  return new Ok(description);
}

// A wrapper around a promise to prevent `Promise<Promise<T>>` collapsing into
// `Promise<T>`.
class PromiseLayer {
  constructor(promise) {
    this.promise = promise;
  }

  static wrap(value) {
    return value instanceof Promise ? new PromiseLayer(value) : value;
  }

  static unwrap(value) {
    return value instanceof PromiseLayer ? value.promise : value;
  }
}

export function newPromise(executor) {
  return new Promise((resolve) =>
    executor((value) => {
      resolve(PromiseLayer.wrap(value));
    }),
  );
}

export function start_promise() {
  let resolve;
  const promise = new Promise((r) => {
    resolve = (value) => {
      r(PromiseLayer.wrap(value));
    };
  });
  return [promise, resolve];
}

export function resolve(value) {
  return Promise.resolve(PromiseLayer.wrap(value));
}

export function then_await(promise, fn) {
  return promise.then((value) => fn(PromiseLayer.unwrap(value)));
}

export function map_promise(promise, fn) {
  return promise.then((value) =>
    PromiseLayer.wrap(fn(PromiseLayer.unwrap(value))),
  );
}

export function rescue(promise, fn) {
  return promise.catch((error) => fn(error));
}

export function wait(delay) {
  return new Promise((resolve) => {
    globalThis.setTimeout(resolve, delay);
  });
}

export function all_promises(...promises) {
  if (promises.length === 1) {
    return Promise.all(promises[0]);
  } else {
    return Promise.all(promises);
  }
}

export function race_promises(...promises) {
  if (promises.length === 1) {
    return Promise.race(promises[0]);
  } else {
    return Promise.race(promises);
  }
}

export function map_new() {
  return new Map();
}

export function map_set(map, key, value) {
  return map.set(key, value);
}

export function map_get(map, key) {
  if (map.has(key)) {
    return new Ok(map.get(key));
  }
  return new Error(undefined);
}

export function map_size(map) {
  return map.size;
}
