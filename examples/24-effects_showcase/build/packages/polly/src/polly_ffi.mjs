export function repeatedly(timeout, state, callback) {
  let timerId = setTimeout(step, timeout)
  return stop
  
  function step() {
    state = callback(state)
    timerId = setTimeout(step, timeout)
  }

  function stop() {
    clearTimeout(timerId)
  }
}
