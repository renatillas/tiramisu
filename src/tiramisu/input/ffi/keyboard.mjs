// Keyboard state management
let keysPressed = new Set();
let keysJustPressed = new Set();
let keysJustReleased = new Set();

export function initKeyboard() {
  window.addEventListener('keydown', (e) => {
    if (!keysPressed.has(e.code)) {
      keysJustPressed.add(e.code);
    }
    keysPressed.add(e.code);
  });

  window.addEventListener('keyup', (e) => {
    keysPressed.delete(e.code);
    keysJustReleased.add(e.code);
  });
}

export function isKeyPressed(keyCode) {
  return keysPressed.has(keyCode);
}

export function isKeyJustPressed(keyCode) {
  return keysJustPressed.has(keyCode);
}

export function isKeyJustReleased(keyCode) {
  return keysJustReleased.has(keyCode);
}

export function clearFrameState() {
  keysJustPressed.clear();
  keysJustReleased.clear();
}

export function getAllPressedKeys() {
  return Array.from(keysPressed);
}
