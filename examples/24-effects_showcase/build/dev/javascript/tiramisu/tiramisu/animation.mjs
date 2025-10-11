import * as $maths from "../../gleam_community_maths/gleam_community/maths.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $vec3 from "../../vec/vec/vec3.mjs";
import { CustomType as $CustomType, divideFloat } from "../gleam.mjs";
import * as $transform from "../tiramisu/transform.mjs";

export class Linear extends $CustomType {}

export class EaseInQuad extends $CustomType {}

export class EaseOutQuad extends $CustomType {}

export class EaseInOutQuad extends $CustomType {}

export class EaseInCubic extends $CustomType {}

export class EaseOutCubic extends $CustomType {}

export class EaseInOutCubic extends $CustomType {}

export class EaseInSine extends $CustomType {}

export class EaseOutSine extends $CustomType {}

export class EaseInOutSine extends $CustomType {}

export class Tween extends $CustomType {
  constructor(start_value, end_value, duration, elapsed, easing, lerp_fn) {
    super();
    this.start_value = start_value;
    this.end_value = end_value;
    this.duration = duration;
    this.elapsed = elapsed;
    this.easing = easing;
    this.lerp_fn = lerp_fn;
  }
}

/**
 * Apply easing function to a value t in [0, 1]
 */
export function ease(easing, t) {
  let t$1 = $float.clamp(t, 0.0, 1.0);
  if (easing instanceof Linear) {
    return t$1;
  } else if (easing instanceof EaseInQuad) {
    return t$1 * t$1;
  } else if (easing instanceof EaseOutQuad) {
    return t$1 * (2.0 - t$1);
  } else if (easing instanceof EaseInOutQuad) {
    let $ = t$1 < 0.5;
    if ($) {
      return (2.0 * t$1) * t$1;
    } else {
      let t_adj = t$1 - 1.0;
      return -1.0 * (((2.0 * t_adj) * t_adj) - 1.0);
    }
  } else if (easing instanceof EaseInCubic) {
    return (t$1 * t$1) * t$1;
  } else if (easing instanceof EaseOutCubic) {
    let t_adj = t$1 - 1.0;
    return ((t_adj * t_adj) * t_adj) + 1.0;
  } else if (easing instanceof EaseInOutCubic) {
    let $ = t$1 < 0.5;
    if ($) {
      return ((4.0 * t$1) * t$1) * t$1;
    } else {
      let t_adj = (2.0 * t$1) - 2.0;
      return (((t_adj * t_adj) * t_adj) + 2.0) / 2.0;
    }
  } else if (easing instanceof EaseInSine) {
    let angle = t$1 * 1.5707963267948966;
    return 1.0 - $maths.cos(angle);
  } else if (easing instanceof EaseOutSine) {
    let angle = t$1 * 1.5707963267948966;
    return $maths.sin(angle);
  } else {
    let angle = 3.141592653589793 * t$1;
    return (1.0 - $maths.cos(angle)) / 2.0;
  }
}

/**
 * Create a new tween
 */
export function tween(start, end, duration, easing, lerp_fn) {
  return new Tween(start, end, duration, 0.0, easing, lerp_fn);
}

/**
 * Update a tween with delta time
 */
export function update_tween(tween, delta) {
  return new Tween(
    tween.start_value,
    tween.end_value,
    tween.duration,
    tween.elapsed + delta,
    tween.easing,
    tween.lerp_fn,
  );
}

/**
 * Get the current value of a tween
 */
export function get_tween_value(tween) {
  let _block;
  let $ = tween.elapsed >= tween.duration;
  if ($) {
    _block = 1.0;
  } else {
    _block = divideFloat(tween.elapsed, tween.duration);
  }
  let t = _block;
  let eased_t = ease(tween.easing, t);
  return tween.lerp_fn(tween.start_value, tween.end_value, eased_t);
}

/**
 * Check if a tween is complete
 */
export function is_tween_complete(tween) {
  return tween.elapsed >= tween.duration;
}

/**
 * Tween a Float value
 */
export function tween_float(start, end, duration, easing) {
  return tween(
    start,
    end,
    duration,
    easing,
    (a, b, t) => { return a + ((b - a) * t); },
  );
}

export function tween_vec3(start, end, duration, easing) {
  return tween(
    start,
    end,
    duration,
    easing,
    (a, b, t) => {
      return new $vec3.Vec3(
        a.x + ((b.x - a.x) * t),
        a.y + ((b.y - a.y) * t),
        a.z + ((b.z - a.z) * t),
      );
    },
  );
}

export function tween_transform(start, end, duration, easing) {
  return tween(start, end, duration, easing, $transform.lerp);
}

/**
 * Reset a tween to start
 */
export function reset_tween(tween) {
  return new Tween(
    tween.start_value,
    tween.end_value,
    tween.duration,
    0.0,
    tween.easing,
    tween.lerp_fn,
  );
}

/**
 * Reverse a tween (swap start and end)
 */
export function reverse_tween(tween) {
  return new Tween(
    tween.end_value,
    tween.start_value,
    tween.duration,
    tween.elapsed,
    tween.easing,
    tween.lerp_fn,
  );
}
