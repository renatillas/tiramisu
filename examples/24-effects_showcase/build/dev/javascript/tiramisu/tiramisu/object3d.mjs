import { CustomType as $CustomType } from "../gleam.mjs";
import { getClipName as clip_name, getClipDuration as clip_duration } from "../threejs.ffi.mjs";

export { clip_duration, clip_name };

export class LoopOnce extends $CustomType {}

export class LoopRepeat extends $CustomType {}

export class Animation extends $CustomType {
  constructor(clip, loop, speed, weight) {
    super();
    this.clip = clip;
    this.loop = loop;
    this.speed = speed;
    this.weight = weight;
  }
}

/**
 * Play a single animation
 */
export class SingleAnimation extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Blend between two animations
 */
export class BlendedAnimations extends $CustomType {
  constructor(from, to, blend_factor) {
    super();
    this.from = from;
    this.to = to;
    this.blend_factor = blend_factor;
  }
}

/**
 * Create an animation from a clip with default settings (loop repeat, normal speed, full weight)
 */
export function new_animation(clip) {
  return new Animation(clip, new LoopRepeat(), 1.0, 1.0);
}

/**
 * Set the loop mode
 */
export function set_loop(anim, mode) {
  return new Animation(anim.clip, mode, anim.speed, anim.weight);
}

/**
 * Set the animation speed (1.0 = normal, 2.0 = double speed, 0.5 = half speed)
 */
export function set_speed(anim, speed) {
  return new Animation(anim.clip, anim.loop, speed, anim.weight);
}

/**
 * Set the animation weight (0.0 to 1.0, for blending animations)
 */
export function set_weight(anim, weight) {
  return new Animation(anim.clip, anim.loop, anim.speed, weight);
}
