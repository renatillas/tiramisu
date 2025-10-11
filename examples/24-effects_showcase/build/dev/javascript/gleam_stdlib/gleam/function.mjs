/**
 * Takes a single argument and always returns its input value.
 */
export function identity(x) {
  return x;
}

/**
 * Takes an argument and a single function, calls that function with that
 * argument and returns that argument instead of the function return value.
 *
 * Useful for running synchronous side effects in a pipeline.
 */
export function tap(arg, effect) {
  effect(arg);
  return arg;
}
