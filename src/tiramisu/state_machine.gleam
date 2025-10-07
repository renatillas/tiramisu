/// Animation State Machine
///
/// A declarative system for managing complex animation transitions.
/// Uses an immutable state machine that integrates with the Model3D scene node.
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import tiramisu/object3d.{type Animation}

/// Condition for transitioning between states
/// The generic parameter `ctx` allows you to pass context (like GameContext) to custom conditions
pub type Condition(ctx) {
  /// Always transition (immediate)
  Always
  /// Transition after a duration (seconds)
  AfterDuration(Float)
  /// Custom condition function that receives context
  Custom(fn(ctx) -> Bool)
}

/// Transition between two states
pub type Transition(state, ctx) {
  Transition(
    from: state,
    to: state,
    condition: Condition(ctx),
    blend_duration: Float,
  )
}

/// An animation state with its configuration
pub type State(state) {
  State(id: state, animation: Animation, is_looping: Bool)
}

/// The current state of a running state machine
pub type StateMachineState(state) {
  /// Playing a single state
  Playing(state: state, elapsed: Float)
  /// Blending between two states
  Blending(from: state, to: state, blend_progress: Float, blend_duration: Float)
}

/// An animation state machine
pub opaque type StateMachine(state, ctx) {
  StateMachine(
    states: Dict(state, State(state)),
    transitions: List(Transition(state, ctx)),
    current: StateMachineState(state),
    default_blend: Float,
  )
}

/// Create a new state machine with a starting state
pub fn new(initial_state: state) -> StateMachine(state, ctx) {
  StateMachine(
    states: dict.new(),
    transitions: [],
    current: Playing(initial_state, 0.0),
    default_blend: 0.2,
  )
}

/// Add a state to the state machine
pub fn add_state(
  machine: StateMachine(state, ctx),
  id: state,
  animation: Animation,
  looping looping: Bool,
) -> StateMachine(state, ctx) {
  let state = State(id: id, animation: animation, is_looping: looping)
  StateMachine(..machine, states: dict.insert(machine.states, id, state))
}

/// Add a transition between two states
pub fn add_transition(
  machine: StateMachine(state, ctx),
  from from: state,
  to to: state,
  condition condition: Condition(ctx),
  blend_duration blend_duration: Float,
) -> StateMachine(state, ctx) {
  let transition = Transition(from, to, condition, blend_duration)
  StateMachine(..machine, transitions: [transition, ..machine.transitions])
}

/// Set the default blend duration for transitions
pub fn set_default_blend(
  machine: StateMachine(state, ctx),
  duration: Float,
) -> StateMachine(state, ctx) {
  StateMachine(..machine, default_blend: duration)
}

/// Update the state machine (should be called every frame)
/// Returns updated state machine and whether a transition occurred
pub fn update(
  machine: StateMachine(state, ctx),
  context: ctx,
  delta_time: Float,
) -> #(StateMachine(state, ctx), Bool) {
  case machine.current {
    Playing(state_id, elapsed) -> {
      let new_elapsed = elapsed +. delta_time

      // Check for transitions from current state
      case find_valid_transition(machine, state_id, context) {
        Ok(transition) -> {
          // Start blending to new state
          let new_current =
            Blending(
              from: state_id,
              to: transition.to,
              blend_progress: 0.0,
              blend_duration: transition.blend_duration,
            )
          #(StateMachine(..machine, current: new_current), True)
        }
        Error(_) -> {
          // No transition, continue playing
          #(
            StateMachine(..machine, current: Playing(state_id, new_elapsed)),
            False,
          )
        }
      }
    }

    Blending(from, to, progress, duration) -> {
      let new_progress = progress +. delta_time

      case new_progress >=. duration {
        True -> {
          // Blend complete, switch to new state
          #(StateMachine(..machine, current: Playing(to, 0.0)), True)
        }
        False -> {
          // Continue blending
          #(
            StateMachine(
              ..machine,
              current: Blending(from, to, new_progress, duration),
            ),
            False,
          )
        }
      }
    }
  }
}

/// Get the current animation(s) to play
/// Returns a single animation or blend information
pub fn get_current_animation(
  machine: StateMachine(state, ctx),
) -> AnimationOutput {
  case machine.current {
    Playing(state_id, _) -> {
      case dict.get(machine.states, state_id) {
        Ok(state) -> Single(state.animation)
        Error(_) -> None
      }
    }

    Blending(from, to, progress, duration) -> {
      case dict.get(machine.states, from), dict.get(machine.states, to) {
        Ok(from_state), Ok(to_state) -> {
          let blend_factor = progress /. duration
          Blend(
            from: from_state.animation,
            to: to_state.animation,
            factor: blend_factor,
          )
        }
        _, _ -> None
      }
    }
  }
}

/// Output from the state machine
pub type AnimationOutput {
  /// No animation
  None
  /// Play a single animation
  Single(Animation)
  /// Blend between two animations
  Blend(from: Animation, to: Animation, factor: Float)
}

/// Manually trigger a transition to a specific state
pub fn transition_to(
  machine: StateMachine(state, ctx),
  target: state,
  blend_duration: Option(Float),
) -> StateMachine(state, ctx) {
  let blend = option.unwrap(blend_duration, machine.default_blend)

  case machine.current {
    Playing(from, _) -> {
      StateMachine(..machine, current: Blending(from, target, 0.0, blend))
    }
    Blending(_, current_to, _, _) -> {
      // Already blending, start blend from current target
      StateMachine(..machine, current: Blending(current_to, target, 0.0, blend))
    }
  }
}

/// Get the current state ID
pub fn current_state(machine: StateMachine(state, ctx)) -> state {
  case machine.current {
    Playing(state_id, _) -> state_id
    Blending(_, to, _, _) -> to
  }
}

/// Check if currently blending
pub fn is_blending(machine: StateMachine(state, ctx)) -> Bool {
  case machine.current {
    Playing(..) -> False
    Blending(..) -> True
  }
}

/// Get blend progress (0.0 to 1.0) if blending
pub fn blend_progress(machine: StateMachine(state, ctx)) -> Option(Float) {
  case machine.current {
    Playing(..) -> option.None
    Blending(_, _, progress, duration) -> option.Some(progress /. duration)
  }
}

// --- Internal Functions ---

/// Find a valid transition from the current state
fn find_valid_transition(
  machine: StateMachine(state, ctx),
  from_state: state,
  context: ctx,
) -> Result(Transition(state, ctx), Nil) {
  machine.transitions
  |> list.find(fn(transition) {
    transition.from == from_state
    && check_condition(transition.condition, context)
  })
}

/// Check if a condition is met
fn check_condition(condition: Condition(ctx), context: ctx) -> Bool {
  case condition {
    Always -> True
    AfterDuration(_) -> False
    // Handled separately by elapsed time
    Custom(check) -> check(context)
  }
}

/// Get a state by ID
pub fn get_state(
  machine: StateMachine(state, ctx),
  id: state,
) -> Result(State(state), Nil) {
  dict.get(machine.states, id)
}

/// Get all state IDs
pub fn state_ids(machine: StateMachine(state, ctx)) -> List(state) {
  dict.keys(machine.states)
}

/// Get the number of states
pub fn state_count(machine: StateMachine(state, ctx)) -> Int {
  dict.size(machine.states)
}

/// Get the number of transitions
pub fn transition_count(machine: StateMachine(state, ctx)) -> Int {
  list.length(machine.transitions)
}

/// Convert AnimationOutput to AnimationPlayback for use with Model3D
pub fn to_playback(
  output: AnimationOutput,
) -> Option(object3d.AnimationPlayback) {
  case output {
    None -> option.None
    Single(anim) -> option.Some(object3d.SingleAnimation(anim))
    Blend(from:, to:, factor:) ->
      option.Some(object3d.BlendedAnimations(from, to, factor))
  }
}
