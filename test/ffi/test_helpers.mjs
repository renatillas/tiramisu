// Test helper functions for mocking browser APIs

/**
 * Mock audio context for testing
 */
export function mockAudio() {
  // Return a minimal mock that satisfies the test expectations
  return {
    createBuffer: () => ({}),
    createBufferSource: () => ({
      connect: () => {},
      start: () => {},
      stop: () => {},
    }),
    destination: {},
  };
}
