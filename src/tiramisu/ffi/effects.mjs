// Effects for game loop integration

/**
 * Request animation frame effect
 * Schedules a message to be dispatched on the next animation frame
 */
export function requestAnimationFrame(msg) {
  return {
    perform: (dispatch) => {
      window.requestAnimationFrame(() => {
        dispatch(msg);
      });
    }
  };
}
