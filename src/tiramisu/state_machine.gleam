//// <script>
//// const docs = [
////   {
////     header: "Creating state machines",
////     functions: [
////       "new",
////       "add_state",
////       "add_transition",
////       "set_default_blend"
////     ]
////   },
////   {
////     header: "State machine updates",
////     functions: [
////       "update",
////       "transition_to"
////     ]
////   },
////   {
////     header: "Querying state",
////     functions: [
////       "current_state",
////       "get_current_animation",
////       "is_blending",
////       "blend_progress",
////       "get_state"
////     ]
////   },
////   {
////     header: "Inspection",
////     functions: [
////       "state_ids",
////       "state_count",
////       "transition_count"
////     ]
////   },
////   {
////     header: "Animation playback",
////     functions: [
////       "to_playback"
////     ]
////   }
//// ]
////
//// const callback = () => {
////   const list = document.querySelector(".sidebar > ul:last-of-type")
////   const sortedLists = document.createDocumentFragment()
////   const sortedMembers = document.createDocumentFragment()
////
////   for (const section of docs) {
////     sortedLists.append((() => {
////       const node = document.createElement("h3")
////       node.append(section.header)
////       return node
////     })())
////     sortedMembers.append((() => {
////       const node = document.createElement("h2")
////       node.append(section.header)
////       return node
////     })())
////
////     const sortedList = document.createElement("ul")
////     sortedLists.append(sortedList)
////
////
////     for (const funcName of section.functions) {
////       const href = `#${funcName}`
////       const member = document.querySelector(
////         `.member:has(h2 > a[href="${href}"])`
////       )
////       const sidebar = list.querySelector(`li:has(a[href="${href}"])`)
////       sortedList.append(sidebar)
////       sortedMembers.append(member)
////     }
////   }
////
////   document.querySelector(".sidebar").insertBefore(sortedLists, list)
////   document
////     .querySelector(".module-members:has(#module-values)")
////     .insertBefore(
////       sortedMembers,
////       document.querySelector("#module-values").nextSibling
////     )
//// }
////
//// document.readyState !== "loading"
////   ? callback()
////   : document.addEventListener(
////     "DOMContentLoaded",
////     callback,
////     { once: true }
////   )
//// </script>
//// State Machine module - declarative animation state management.
////
//// Provides a type-safe, immutable state machine for managing complex animation transitions
//// in 3D models. Supports condition-based transitions, automatic blending, and custom state types.
////
//// ## Core Concepts
////
//// - **Generic State Types**: Use any type for states (enums, strings, custom types)
//// - **Context Parameter**: Pass game context to custom transition conditions
//// - **Declarative Transitions**: Define state transitions with conditions and blend durations
//// - **Automatic Blending**: Smooth animation blending during state transitions
//// - **Immutable Updates**: Update returns new state machine (functional pattern)
//// - **Model3D Integration**: Seamless integration with 3D model animation playback
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/state_machine
//// import tiramisu/object3d
//// import tiramisu/scene
////
//// // Define your state type
//// pub type CharacterState {
////   Idle
////   Walking
////   Running
////   Jumping
//// }
////
//// // Define your context type (for custom conditions)
//// pub type GameContext {
////   GameContext(velocity: Float, is_grounded: Bool)
//// }
////
//// // Create state machine
//// let machine =
////   state_machine.new(Idle)
////   |> state_machine.add_state(Idle, idle_anim, looping: True)
////   |> state_machine.add_state(Walking, walk_anim, looping: True)
////   |> state_machine.add_state(Running, run_anim, looping: True)
////   |> state_machine.add_state(Jumping, jump_anim, looping: False)
////   |> state_machine.add_transition(
////     from: Idle,
////     to: Walking,
////     condition: state_machine.Custom(fn(ctx) { ctx.velocity >. 0.1 }),
////     blend_duration: 0.2,
////   )
////   |> state_machine.add_transition(
////     from: Walking,
////     to: Running,
////     condition: state_machine.Custom(fn(ctx) { ctx.velocity >. 5.0 }),
////     blend_duration: 0.3,
////   )
////   |> state_machine.add_transition(
////     from: Running,
////     to: Walking,
////     condition: state_machine.Custom(fn(ctx) { ctx.velocity <=. 5.0 }),
////     blend_duration: 0.3,
////   )
////   |> state_machine.add_transition(
////     from: Walking,
////     to: Idle,
////     condition: state_machine.Custom(fn(ctx) { ctx.velocity <=. 0.1 }),
////     blend_duration: 0.2,
////   )
////
//// // Update state machine in your game loop
//// fn update(model: Model, ctx: tiramisu.Context) -> Model {
////   let game_ctx = GameContext(velocity: model.velocity, is_grounded: model.grounded)
////   let #(new_machine, _transitioned) =
////     state_machine.update(model.anim_machine, game_ctx, ctx.delta_time)
////
////   let animation =
////     state_machine.get_current_animation(new_machine)
////     |> state_machine.to_playback()
////
////   Model(..model, anim_machine: new_machine)
//// }
////
//// // Use in scene
//// fn view(model: Model) -> scene.Node {
////   let animation =
////     state_machine.get_current_animation(model.anim_machine)
////     |> state_machine.to_playback()
////
////   scene.Model(
////     id: "character",
////     model: model.character_model,
////     animation: animation,
////     transform: model.transform,
////   )
//// }
//// ```
////
//// ## Transition Conditions
////
//// Three types of conditions control when transitions occur:
////
//// - **Always**: Transition immediately (useful for forced state changes)
//// - **AfterDuration(seconds)**: Transition after elapsed time in current state
//// - **Custom(fn(ctx) -> Bool)**: Custom condition based on game context
////
//// ### Custom Condition Examples
////
//// ```gleam
//// // Velocity-based transition
//// state_machine.Custom(fn(ctx) { ctx.player_speed >. 5.0 })
////
//// // Input-based transition
//// state_machine.Custom(fn(ctx) { ctx.jump_pressed && ctx.is_grounded })
////
//// // Distance-based transition
//// state_machine.Custom(fn(ctx) {
////   vec3.distance(ctx.player_pos, ctx.enemy_pos) <. 10.0
//// })
//// ```
////
//// ## Animation Blending
////
//// State machines automatically blend animations during transitions:
////
//// - **blend_duration**: How long the blend takes (in seconds)
//// - **blend_progress**: Current blend progress (0.0 to 1.0)
//// - **Smooth transitions**: Linear interpolation between animation poses
////
//// Use `is_blending()` and `blend_progress()` to query blend state.
////
//// ## Manual Transitions
////
//// You can manually trigger transitions (bypassing conditions):
////
//// ```gleam
//// // Force transition to specific state with custom blend duration
//// let machine =
////   state_machine.transition_to(machine, Running, option.Some(0.5))
////
//// // Force transition with default blend duration
//// let machine =
////   state_machine.transition_to(machine, Idle, option.None)
//// ```

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import tiramisu/animation.{type Animation}

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

/// Create a new state machine with a starting state.
///
/// The machine starts with no states or transitions. Use `add_state()` and
/// `add_transition()` to build up the state graph.
///
/// **Default blend duration**: 0.2 seconds (can be changed with `set_default_blend()`).
///
/// ## Example
///
/// ```gleam
/// // Define state type
/// type PlayerState {
///   Idle
///   Walking
///   Running
/// }
///
/// // Create machine starting in Idle state
/// let machine = state_machine.new(Idle)
/// ```
pub fn new(initial_state: state) -> StateMachine(state, ctx) {
  StateMachine(
    states: dict.new(),
    transitions: [],
    current: Playing(initial_state, 0.0),
    default_blend: 0.2,
  )
}

/// Add a state to the state machine.
///
/// Each state links a unique ID to an animation clip and playback settings.
///
/// **Looping**: Set `True` for repeating animations (idle, walk), `False` for one-shots (jump, attack).
///
/// ## Example
///
/// ```gleam
/// let machine = state_machine.new(Idle)
///   |> state_machine.add_state(Idle, idle_anim, looping: True)
///   |> state_machine.add_state(Walking, walk_anim, looping: True)
///   |> state_machine.add_state(Attacking, attack_anim, looping: False)
/// ```
pub fn add_state(
  machine: StateMachine(state, ctx),
  id: state,
  animation: Animation,
  looping looping: Bool,
) -> StateMachine(state, ctx) {
  let state = State(id: id, animation: animation, is_looping: looping)
  StateMachine(..machine, states: dict.insert(machine.states, id, state))
}

/// Add a transition between two states.
///
/// Transitions define when and how to switch from one animation to another.
///
/// **Blend duration**: Time in seconds for smooth animation blending (typical: 0.1-0.5s).
/// **Condition**: When to trigger the transition (`Always`, `AfterDuration`, or `Custom`).
///
/// ## Example
///
/// ```gleam
/// state_machine.new(Idle)
///   |> state_machine.add_state(Idle, idle_anim, looping: True)
///   |> state_machine.add_state(Walking, walk_anim, looping: True)
///   // Transition when velocity exceeds threshold
///   |> state_machine.add_transition(
///     from: Idle,
///     to: Walking,
///     condition: state_machine.Custom(fn(ctx) { ctx.velocity >. 0.1 }),
///     blend_duration: 0.2,
///   )
///   // Transition back when velocity drops
///   |> state_machine.add_transition(
///     from: Walking,
///     to: Idle,
///     condition: state_machine.Custom(fn(ctx) { ctx.velocity <=. 0.1 }),
///     blend_duration: 0.3,
///   )
/// ```
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

/// Set the default blend duration for manual transitions.
///
/// This duration is used when calling `transition_to()` without specifying a blend duration.
/// Does not affect transitions added with `add_transition()` (those use their own blend_duration).
///
/// **Duration**: Time in seconds (default: 0.2s).
///
/// ## Example
///
/// ```gleam
/// let machine = state_machine.new(Idle)
///   |> state_machine.set_default_blend(0.5)  // 500ms default blend
///
/// // Later, manual transition uses default 0.5s blend
/// let machine = state_machine.transition_to(machine, Running, option.None)
/// ```
pub fn set_default_blend(
  machine: StateMachine(state, ctx),
  duration: Float,
) -> StateMachine(state, ctx) {
  StateMachine(..machine, default_blend: duration)
}

/// Update the state machine (call every frame in your game loop).
///
/// Evaluates transition conditions and advances blend progress. Returns the updated
/// machine and a boolean indicating if a transition occurred this frame.
///
/// **delta_time**: Time in milliseconds since last frame (e.g., 16.67ms for 60 FPS).
/// **context**: Your custom context type passed to `Custom` condition functions.
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: tiramisu.Context) -> Model {
///   // Create context for state machine conditions
///   let game_ctx = GameContext(
///     velocity: model.velocity,
///     is_grounded: model.grounded,
///   )
///
///   // Update state machine
///   let #(new_machine, transitioned) =
///     state_machine.update(model.anim_machine, game_ctx, ctx.delta_time)
///
///   // Log transitions
///   case transitioned {
///     True -> io.println("State changed!")
///     False -> Nil
///   }
///
///   Model(..model, anim_machine: new_machine)
/// }
/// ```
pub fn update(
  machine: StateMachine(state, ctx),
  context: ctx,
  delta_time: Float,
) -> #(StateMachine(state, ctx), Bool) {
  // Convert delta_time from milliseconds to seconds for blend calculations
  // (blend_duration is specified in seconds in the API)
  let delta_seconds = delta_time /. 1000.0

  case machine.current {
    Playing(state_id, elapsed) -> {
      let new_elapsed = elapsed +. delta_seconds

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
      let new_progress = progress +. delta_seconds

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

/// Get the current animation output from the state machine.
///
/// Returns either a single animation (when playing), blended animations (when transitioning),
/// or none (if state not found).
///
/// Use `to_playback()` to convert the result to `Option(AnimationPlayback)` for use with `scene.Model3D`.
///
/// ## Example
///
/// ```gleam
/// fn view(model: Model) -> scene.Node {
///   let animation =
///     state_machine.get_current_animation(model.anim_machine)
///     |> state_machine.to_playback()
///
///   scene.Model3D(
///     id: "character",
///     model: model.character_model,
///     animation: animation,
///     transform: model.transform,
///     physics: option.None,
///   )
/// }
/// ```
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

/// Manually trigger a transition to a specific state.
///
/// Bypasses all transition conditions and forces an immediate state change.
/// Useful for external events like taking damage, dying, or cutscene triggers.
///
/// **blend_duration**: Optional blend time in seconds. If `None`, uses default blend duration.
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: tiramisu.Context) -> Model {
///   case msg {
///     TakeDamage(_) -> {
///       // Force transition to hit state with fast blend
///       let machine =
///         state_machine.transition_to(
///           model.anim_machine,
///           HitReaction,
///           option.Some(0.1),
///         )
///       Model(..model, anim_machine: machine)
///     }
///
///     Die -> {
///       // Force transition using default blend
///       let machine =
///         state_machine.transition_to(model.anim_machine, Dead, option.None)
///       Model(..model, anim_machine: machine)
///     }
///
///     _ -> model
///   }
/// }
/// ```
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

/// Get the current state ID.
///
/// When blending, returns the target state (where we're transitioning to).
///
/// ## Example
///
/// ```gleam
/// let state = state_machine.current_state(machine)
///
/// case state {
///   Idle -> io.println("Character is idle")
///   Running -> io.println("Character is running")
///   _ -> Nil
/// }
/// ```
pub fn current_state(machine: StateMachine(state, ctx)) -> state {
  case machine.current {
    Playing(state_id, _) -> state_id
    Blending(_, to, _, _) -> to
  }
}

/// Check if currently blending between states.
///
/// Returns `True` during animation transitions, `False` when playing a single state.
///
/// ## Example
///
/// ```gleam
/// // Disable certain actions during transitions
/// case state_machine.is_blending(model.anim_machine) {
///   True -> {
///     // Can't attack while transitioning
///     model
///   }
///   False -> {
///     // Allow attack
///     handle_attack(model)
///   }
/// }
/// ```
pub fn is_blending(machine: StateMachine(state, ctx)) -> Bool {
  case machine.current {
    Playing(..) -> False
    Blending(..) -> True
  }
}

/// Get blend progress as a normalized value (0.0 to 1.0).
///
/// Returns `None` if not currently blending, `Some(progress)` during transitions.
///
/// **Progress**: 0.0 = start of blend, 1.0 = end of blend.
///
/// ## Example
///
/// ```gleam
/// // Visualize blend progress
/// case state_machine.blend_progress(model.anim_machine) {
///   option.Some(progress) -> {
///     io.println("Blend: " <> float.to_string(progress *. 100.0) <> "%")
///   }
///   option.None -> Nil
/// }
/// ```
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

/// Get a state by ID.
///
/// Returns the state configuration including animation and looping setting.
/// Useful for inspecting state machine configuration.
///
/// ## Example
///
/// ```gleam
/// case state_machine.get_state(machine, Running) {
///   Ok(state) -> {
///     io.println("Running animation duration: " <>
///       float.to_string(animation.clip_duration(state.animation)))
///   }
///   Error(_) -> io.println("State not found")
/// }
/// ```
pub fn get_state(
  machine: StateMachine(state, ctx),
  id: state,
) -> Result(State(state), Nil) {
  dict.get(machine.states, id)
}

/// Get all state IDs in the state machine.
///
/// Useful for debugging or building UI to visualize state machines.
///
/// ## Example
///
/// ```gleam
/// // List all states
/// state_machine.state_ids(machine)
/// |> list.each(fn(state) {
///   io.println("State: " <> string.inspect(state))
/// })
/// ```
pub fn state_ids(machine: StateMachine(state, ctx)) -> List(state) {
  dict.keys(machine.states)
}

/// Get the number of states in the state machine.
///
/// ## Example
///
/// ```gleam
/// io.println("State machine has " <>
///   int.to_string(state_machine.state_count(machine)) <>
///   " states")
/// ```
pub fn state_count(machine: StateMachine(state, ctx)) -> Int {
  dict.size(machine.states)
}

/// Get the number of transitions in the state machine.
///
/// ## Example
///
/// ```gleam
/// io.println("State machine has " <>
///   int.to_string(state_machine.transition_count(machine)) <>
///   " transitions")
/// ```
pub fn transition_count(machine: StateMachine(state, ctx)) -> Int {
  list.length(machine.transitions)
}

/// Convert AnimationOutput to AnimationPlayback for use with scene.Model3D.
///
/// This helper converts the state machine's output format to the format expected
/// by 3D model nodes. Use it after `get_current_animation()`.
///
/// ## Example
///
/// ```gleam
/// fn view(model: Model) -> scene.Node {
///   let animation =
///     state_machine.get_current_animation(model.anim_machine)
///     |> state_machine.to_playback()
///
///   scene.Model3D(
///     id: "character",
///     model: model.character_model,
///     animation: animation,  // Option(AnimationPlayback)
///     transform: model.transform,
///     physics: option.None,
///   )
/// }
/// ```
pub fn to_playback(
  output: AnimationOutput,
) -> Option(animation.AnimationPlayback) {
  case output {
    None -> option.None
    Single(anim) -> option.Some(animation.SingleAnimation(anim))
    Blend(from:, to:, factor:) ->
      option.Some(animation.BlendedAnimations(from, to, factor))
  }
}
