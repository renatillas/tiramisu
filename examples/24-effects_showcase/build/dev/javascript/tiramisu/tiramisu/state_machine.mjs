import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import {
  Ok,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  divideFloat,
  isEqual,
} from "../gleam.mjs";
import * as $object3d from "../tiramisu/object3d.mjs";

export class Always extends $CustomType {}

/**
 * Transition after a duration (seconds)
 */
export class AfterDuration extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Custom condition function that receives context
 */
export class Custom extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Transition extends $CustomType {
  constructor(from, to, condition, blend_duration) {
    super();
    this.from = from;
    this.to = to;
    this.condition = condition;
    this.blend_duration = blend_duration;
  }
}

export class State extends $CustomType {
  constructor(id, animation, is_looping) {
    super();
    this.id = id;
    this.animation = animation;
    this.is_looping = is_looping;
  }
}

/**
 * Playing a single state
 */
export class Playing extends $CustomType {
  constructor(state, elapsed) {
    super();
    this.state = state;
    this.elapsed = elapsed;
  }
}

/**
 * Blending between two states
 */
export class Blending extends $CustomType {
  constructor(from, to, blend_progress, blend_duration) {
    super();
    this.from = from;
    this.to = to;
    this.blend_progress = blend_progress;
    this.blend_duration = blend_duration;
  }
}

class StateMachine extends $CustomType {
  constructor(states, transitions, current, default_blend) {
    super();
    this.states = states;
    this.transitions = transitions;
    this.current = current;
    this.default_blend = default_blend;
  }
}

export class None extends $CustomType {}

/**
 * Play a single animation
 */
export class Single extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Blend between two animations
 */
export class Blend extends $CustomType {
  constructor(from, to, factor) {
    super();
    this.from = from;
    this.to = to;
    this.factor = factor;
  }
}

/**
 * Create a new state machine with a starting state
 */
export function new$(initial_state) {
  return new StateMachine(
    $dict.new$(),
    toList([]),
    new Playing(initial_state, 0.0),
    0.2,
  );
}

/**
 * Add a state to the state machine
 */
export function add_state(machine, id, animation, looping) {
  let state = new State(id, animation, looping);
  return new StateMachine(
    $dict.insert(machine.states, id, state),
    machine.transitions,
    machine.current,
    machine.default_blend,
  );
}

/**
 * Add a transition between two states
 */
export function add_transition(machine, from, to, condition, blend_duration) {
  let transition = new Transition(from, to, condition, blend_duration);
  return new StateMachine(
    machine.states,
    listPrepend(transition, machine.transitions),
    machine.current,
    machine.default_blend,
  );
}

/**
 * Set the default blend duration for transitions
 */
export function set_default_blend(machine, duration) {
  return new StateMachine(
    machine.states,
    machine.transitions,
    machine.current,
    duration,
  );
}

/**
 * Get the current animation(s) to play
 * Returns a single animation or blend information
 */
export function get_current_animation(machine) {
  let $ = machine.current;
  if ($ instanceof Playing) {
    let state_id = $.state;
    let $1 = $dict.get(machine.states, state_id);
    if ($1 instanceof Ok) {
      let state = $1[0];
      return new Single(state.animation);
    } else {
      return new None();
    }
  } else {
    let from = $.from;
    let to = $.to;
    let progress = $.blend_progress;
    let duration = $.blend_duration;
    let $1 = $dict.get(machine.states, from);
    let $2 = $dict.get(machine.states, to);
    if ($2 instanceof Ok && $1 instanceof Ok) {
      let to_state = $2[0];
      let from_state = $1[0];
      let blend_factor = divideFloat(progress, duration);
      return new Blend(from_state.animation, to_state.animation, blend_factor);
    } else {
      return new None();
    }
  }
}

/**
 * Manually trigger a transition to a specific state
 */
export function transition_to(machine, target, blend_duration) {
  let blend = $option.unwrap(blend_duration, machine.default_blend);
  let $ = machine.current;
  if ($ instanceof Playing) {
    let from = $.state;
    return new StateMachine(
      machine.states,
      machine.transitions,
      new Blending(from, target, 0.0, blend),
      machine.default_blend,
    );
  } else {
    let current_to = $.to;
    return new StateMachine(
      machine.states,
      machine.transitions,
      new Blending(current_to, target, 0.0, blend),
      machine.default_blend,
    );
  }
}

/**
 * Get the current state ID
 */
export function current_state(machine) {
  let $ = machine.current;
  if ($ instanceof Playing) {
    let state_id = $.state;
    return state_id;
  } else {
    let to = $.to;
    return to;
  }
}

/**
 * Check if currently blending
 */
export function is_blending(machine) {
  let $ = machine.current;
  if ($ instanceof Playing) {
    return false;
  } else {
    return true;
  }
}

/**
 * Get blend progress (0.0 to 1.0) if blending
 */
export function blend_progress(machine) {
  let $ = machine.current;
  if ($ instanceof Playing) {
    return new $option.None();
  } else {
    let progress = $.blend_progress;
    let duration = $.blend_duration;
    return new $option.Some(divideFloat(progress, duration));
  }
}

/**
 * Check if a condition is met
 * 
 * @ignore
 */
function check_condition(condition, context) {
  if (condition instanceof Always) {
    return true;
  } else if (condition instanceof AfterDuration) {
    return false;
  } else {
    let check = condition[0];
    return check(context);
  }
}

/**
 * Find a valid transition from the current state
 * 
 * @ignore
 */
function find_valid_transition(machine, from_state, context) {
  let _pipe = machine.transitions;
  return $list.find(
    _pipe,
    (transition) => {
      return (isEqual(transition.from, from_state)) && check_condition(
        transition.condition,
        context,
      );
    },
  );
}

/**
 * Update the state machine (should be called every frame)
 * Returns updated state machine and whether a transition occurred
 */
export function update(machine, context, delta_time) {
  let $ = machine.current;
  if ($ instanceof Playing) {
    let state_id = $.state;
    let elapsed = $.elapsed;
    let new_elapsed = elapsed + delta_time;
    let $1 = find_valid_transition(machine, state_id, context);
    if ($1 instanceof Ok) {
      let transition = $1[0];
      let new_current = new Blending(
        state_id,
        transition.to,
        0.0,
        transition.blend_duration,
      );
      return [
        new StateMachine(
          machine.states,
          machine.transitions,
          new_current,
          machine.default_blend,
        ),
        true,
      ];
    } else {
      return [
        new StateMachine(
          machine.states,
          machine.transitions,
          new Playing(state_id, new_elapsed),
          machine.default_blend,
        ),
        false,
      ];
    }
  } else {
    let from = $.from;
    let to = $.to;
    let progress = $.blend_progress;
    let duration = $.blend_duration;
    let new_progress = progress + delta_time;
    let $1 = new_progress >= duration;
    if ($1) {
      return [
        new StateMachine(
          machine.states,
          machine.transitions,
          new Playing(to, 0.0),
          machine.default_blend,
        ),
        true,
      ];
    } else {
      return [
        new StateMachine(
          machine.states,
          machine.transitions,
          new Blending(from, to, new_progress, duration),
          machine.default_blend,
        ),
        false,
      ];
    }
  }
}

/**
 * Get a state by ID
 */
export function get_state(machine, id) {
  return $dict.get(machine.states, id);
}

/**
 * Get all state IDs
 */
export function state_ids(machine) {
  return $dict.keys(machine.states);
}

/**
 * Get the number of states
 */
export function state_count(machine) {
  return $dict.size(machine.states);
}

/**
 * Get the number of transitions
 */
export function transition_count(machine) {
  return $list.length(machine.transitions);
}

/**
 * Convert AnimationOutput to AnimationPlayback for use with Model3D
 */
export function to_playback(output) {
  if (output instanceof None) {
    return new $option.None();
  } else if (output instanceof Single) {
    let anim = output[0];
    return new $option.Some(new $object3d.SingleAnimation(anim));
  } else {
    let from = output.from;
    let to = output.to;
    let factor = output.factor;
    return new $option.Some(new $object3d.BlendedAnimations(from, to, factor));
  }
}
