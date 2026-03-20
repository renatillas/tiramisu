import gleam/bool
import gleam/dict
import gleam/list
import tiramisu/dev/extension

pub type ResolveDecision {
  Drop
  Queue
  ApplyNow(List(extension.RuntimeAction))
}

pub type Pending {
  Pending(
    owner: extension.RequestOwner,
    key: extension.RequestKey,
    version: Int,
    actions: List(extension.RuntimeAction),
  )
}

pub opaque type State {
  State(
    versions: dict.Dict(String, Int),
    next_version: Int,
    pending: List(Pending),
  )
}

pub fn new() -> State {
  State(versions: dict.new(), next_version: 0, pending: [])
}

pub fn register(
  state: State,
  owner: extension.RequestOwner,
  key: extension.RequestKey,
) -> #(State, Int) {
  let next_version = state.next_version + 1
  let next_state =
    State(
      ..state,
      versions: dict.insert(
        state.versions,
        registry_key(owner, key),
        next_version,
      ),
      next_version: next_version,
    )
  #(next_state, next_version)
}

pub fn is_current(
  state: State,
  owner: extension.RequestOwner,
  key: extension.RequestKey,
  version: Int,
) -> Bool {
  case dict.get(state.versions, registry_key(owner, key)) {
    Ok(current_version) -> current_version == version
    Error(Nil) -> False
  }
}

pub fn enqueue(
  state: State,
  owner: extension.RequestOwner,
  key: extension.RequestKey,
  version: Int,
  actions: List(extension.RuntimeAction),
) -> State {
  State(..state, pending: [
    Pending(owner:, key:, version:, actions:),
    ..state.pending
  ])
}

pub fn drain_pending(state: State) -> #(State, List(Pending)) {
  #(State(..state, pending: []), list.reverse(state.pending))
}

pub fn resolve(
  state: State,
  owner: extension.RequestOwner,
  key: extension.RequestKey,
  version: Int,
  actions: List(extension.RuntimeAction),
  reconciling: Bool,
  runtime_available: Bool,
  owner_exists: Bool,
) -> #(State, ResolveDecision) {
  use <- bool.guard(
    when: !is_current(state, owner, key, version) || !owner_exists,
    return: #(state, Drop),
  )
  use <- bool.guard(when: reconciling || !runtime_available, return: #(
    enqueue(state, owner, key, version, actions),
    Queue,
  ))
  #(state, ApplyNow(actions))
}

pub fn drain_ready(
  state: State,
  owner_exists: fn(extension.RequestOwner) -> Bool,
) -> #(State, List(List(extension.RuntimeAction))) {
  let #(next_state, pending_requests) = drain_pending(state)

  let ready_actions =
    list.fold(pending_requests, [], fn(acc, pending_request) {
      let Pending(owner:, key:, version:, actions:) = pending_request
      case is_current(next_state, owner, key, version) && owner_exists(owner) {
        True -> [actions, ..acc]
        False -> acc
      }
    })
    |> list.reverse

  #(next_state, ready_actions)
}

fn registry_key(
  owner: extension.RequestOwner,
  key: extension.RequestKey,
) -> String {
  extension.request_owner_to_string(owner)
  <> "::"
  <> extension.request_key_to_string(key)
}
