import gleam/list
import gleam/option
import lustre/effect
import tiramisu/dev/extension
import tiramisu/internal/async_policy
import tiramisu/internal/async_state

pub fn resolve_decision_test() {
  let owner = extension.NodeOwner("mesh")
  let key = extension.request_key("src")
  let #(state, version) = async_state.new() |> async_state.register(owner, key)

  let #(state, decision) =
    async_state.resolve(
      state,
      owner,
      key,
      version,
      [],
      False,
      True,
      True,
    )
  let assert async_state.ApplyNow([]) = decision

  let #(_, stale_decision) =
    async_state.resolve(
      state,
      owner,
      key,
      version - 1,
      [],
      False,
      True,
      True,
    )
  let assert async_state.Drop = stale_decision

  let #(_, missing_owner_decision) =
    async_state.resolve(
      state,
      owner,
      key,
      version,
      [],
      False,
      True,
      False,
    )
  let assert async_state.Drop = missing_owner_decision
}

pub fn resolve_queueing_test() {
  let owner = extension.NodeOwner("mesh")
  let key = extension.request_key("src")
  let actions = [extension.action(fn(runtime) { #(runtime, effect.none()) })]
  let #(state, version) = async_state.new() |> async_state.register(owner, key)

  let #(state, decision) =
    async_state.resolve(
      state,
      owner,
      key,
      version,
      actions,
      True,
      True,
      True,
    )
  let assert async_state.Queue = decision

  let #(state, queued) =
    async_state.drain_ready(state, fn(request_owner) { request_owner == owner })
  let assert [resolved_actions] = queued
  assert list.length(resolved_actions) == 1

  let #(_, queued_without_runtime_decision) =
    async_state.resolve(
      state,
      owner,
      key,
      version,
      actions,
      False,
      False,
      True,
    )
  let assert async_state.Queue = queued_without_runtime_decision
}

pub fn drain_ready_drops_stale_and_missing_owners_test() {
  let owner = extension.NodeOwner("mesh")
  let other = extension.NodeOwner("other")
  let key = extension.request_key("src")

  let #(state, stale_version) = async_state.new() |> async_state.register(owner, key)
  let #(state, _) = async_state.register(state, owner, key)
  let #(state, other_version) = async_state.register(state, other, key)

  let state =
    state
    |> async_state.enqueue(owner, key, stale_version, [extension.action(fn(runtime) { #(runtime, effect.none()) })])
    |> async_state.enqueue(other, key, other_version, [extension.action(fn(runtime) { #(runtime, effect.none()) })])

  let #(state, ready) =
    async_state.drain_ready(state, fn(request_owner) { request_owner == owner })

  assert list.is_empty(ready)
  let #(_, pending) = async_state.drain_pending(state)
  assert list.is_empty(pending)
}

pub fn background_reload_policy_test() {
  assert async_policy.should_reload_background(
    previous_signature: option.None,
    next_signature: "texture:background.jpg::srgb",
  )

  assert !async_policy.should_reload_background(
    previous_signature: option.Some("texture:background.jpg::srgb"),
    next_signature: "texture:background.jpg::srgb",
  )
}

pub fn model_event_policy_test() {
  let assert async_policy.EmitModelEvent =
    async_policy.model_event_outcome(is_current: True, owner_exists: True)

  let assert async_policy.DropModelEvent =
    async_policy.model_event_outcome(is_current: False, owner_exists: True)
}
