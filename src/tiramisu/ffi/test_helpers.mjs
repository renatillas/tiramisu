// Test helper functions that create mock objects for opaque FFI types
// These are minimal objects that satisfy type requirements for testing

export function mockTexture() {
  // Mock Three.js Texture object
  return {
    isTexture: true,
    uuid: 'test-texture-' + Math.random(),
    name: 'MockTexture',
  };
}

export function mockAudio() {
  // Mock Web Audio API AudioBuffer
  return {
    duration: 1.0,
    length: 44100,
    numberOfChannels: 2,
    sampleRate: 44100,
  };
}

export function mockGeometry() {
  // Mock Three.js BufferGeometry
  return {
    isBufferGeometry: true,
    uuid: 'test-geometry-' + Math.random(),
    name: 'MockGeometry',
    attributes: {},
  };
}
