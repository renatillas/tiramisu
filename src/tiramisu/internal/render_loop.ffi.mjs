export function start(onTick) {
  let previousTimestamp = null;
  let animationId = null;
  let running = true;

  function tick(timestamp) {
    if (!running) return;

    animationId = requestAnimationFrame(tick);

    const deltaMs =
      previousTimestamp === null ? 16.67 : timestamp - previousTimestamp;

    previousTimestamp = timestamp;
    onTick(deltaMs, Math.round(timestamp));
  }

  animationId = requestAnimationFrame(tick);

  return {
    stop() {
      running = false;
      if (animationId !== null) {
        cancelAnimationFrame(animationId);
        animationId = null;
      }
    },
  };
}

export function stop(loop) {
  loop.stop();
}
