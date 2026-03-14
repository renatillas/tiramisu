import gleam/dict
import gleam/list
import tiramisu/dev/extension

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
  let version = state.next_version + 1
  let state =
    State(
      ..state,
      versions: dict.insert(state.versions, registry_key(owner, key), version),
      next_version: version,
    )
  #(state, version)
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

fn registry_key(
  owner: extension.RequestOwner,
  key: extension.RequestKey,
) -> String {
  extension.request_owner_to_string(owner)
  <> "::"
  <> extension.request_key_to_string(key)
}
