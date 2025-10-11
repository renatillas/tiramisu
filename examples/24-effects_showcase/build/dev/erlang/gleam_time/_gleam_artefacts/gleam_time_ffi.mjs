export function system_time() {
  const now = Date.now();
  const milliseconds = now % 1_000;
  const nanoseconds = milliseconds * 1000_000;
  const seconds = (now - milliseconds) / 1_000;
  return [seconds, nanoseconds];
}

export function local_time_offset_seconds() {
  return new Date().getTimezoneOffset() * -60;
}
