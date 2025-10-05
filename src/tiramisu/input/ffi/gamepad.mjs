// Gamepad API wrapper

// Get a gamepad by index
function getGamepad(index) {
  const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
  return gamepads[index] || null;
}

// Check if a gamepad is connected
export function isConnected(index) {
  const gamepad = getGamepad(index);
  return gamepad !== null && gamepad.connected;
}

// Check if a button is pressed
export function isButtonPressed(gamepadIndex, buttonIndex) {
  const gamepad = getGamepad(gamepadIndex);
  if (!gamepad || !gamepad.buttons[buttonIndex]) {
    return false;
  }
  return gamepad.buttons[buttonIndex].pressed;
}

// Get button pressure value (0.0 to 1.0)
export function getButtonValue(gamepadIndex, buttonIndex) {
  const gamepad = getGamepad(gamepadIndex);
  if (!gamepad || !gamepad.buttons[buttonIndex]) {
    return 0.0;
  }
  return gamepad.buttons[buttonIndex].value;
}

// Get axis value (-1.0 to 1.0)
export function getAxisValue(gamepadIndex, axisIndex) {
  const gamepad = getGamepad(gamepadIndex);
  if (!gamepad || gamepad.axes[axisIndex] === undefined) {
    return 0.0;
  }
  return gamepad.axes[axisIndex];
}

// Get number of connected gamepads
export function getGamepadCount() {
  const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
  let count = 0;
  for (let i = 0; i < gamepads.length; i++) {
    if (gamepads[i] && gamepads[i].connected) {
      count++;
    }
  }
  return count;
}
