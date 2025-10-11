export function is_windows() {
  return (
    globalThis?.process?.platform === "win32" ||
    globalThis?.Deno?.build?.os === "windows"
  );
}
