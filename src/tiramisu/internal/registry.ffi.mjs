// Minimal FFI for the instance-scoped registry.
// Only browser APIs that can't be done in pure Gleam live here.

export function getDevicePixelRatio() {
  return Math.min(window.devicePixelRatio, 2);
}
