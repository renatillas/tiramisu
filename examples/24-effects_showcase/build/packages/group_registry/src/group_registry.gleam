//// Groups that can be joined and left by processes, and the members of which
//// can be listed on request. This may be useful for publish-subscribe
//// patterns, where many processes want to receive messages another.
////
//// This module is implemented using Erlang's [`pg`][pg] module and uses ETS
//// for storage, so listing the members of a group is optimised for speed.
////
//// Groups are tracked by group registry processes, which you should add to
//// your supervision tree. Each registry is independant from each other.
////
//// If a member terminates, it is automatically removed from the group.
////
//// If a group registry terminates the groups are lost and will need to be
//// recreated. Restarting the group registry will not recover the groups.
////
//// There is no memory cost to the registry of a group without any members.
////
//// ## Publish-subscribe
////
//// This module is useful for pubsub, but it does not offer any functions for
//// sending messages itself. To perform pubsub add the subscriber messages to
//// a group, then the publishers can use the `members` function to get a
//// list of subjects for the subscribers and send messages to them.
////
//// ## Distributed groups
////
//// If two nodes in an Erlang cluster have process registries or `pg`
//// instances created with the same name (called a "scope" in the `pg`
//// documentation) they will share group membership in an eventually
//// consistent way. See the `pg` documentation for more information. Note that
//// names created with the `process.new_name` are unique, so calling that
//// function with the same prefix string on each node in an Erlang cluster
//// will result in distinct names.
////
//// [pg]: https://www.erlang.org/doc/apps/kernel/pg.html
////
//// ## Scalability
////
//// Inserting members or getting all the members of a group `pg` is fast, but
//// removing members from large groups with thousands of members in them is
//// much slower. This module is best suited to numerous small groups.
////
//// If you need larger groups and members to be removed or to terminate
//// frequently you may want to experiment with other registries. Always
//// benchmark and profile your code when performance matters.

import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision

//
// Group registry
//

/// A reference to the group registry process.
///
/// The type parameter is a message type that processes registered with the
/// registry can be sent.
///
pub type GroupRegistry(message)

/// The message type that the registry accepts. This type is opaque and cannot
/// be constructed directly. Instead use the functions in this module to
/// interact with the registry.
///
/// The type parameter is a message type that processes registered with the
/// registry can be sent.
///
pub type Message(message)

/// Start the registry with the given name. You likely want to use the
/// `supervised` function instead, to add the registry to your supervision
/// tree, but this may still be useful in your tests.
///
/// Remember that names must be created at the start of your program, and must
/// not be created dynamically such as within your supervision tree (it may
/// restart, creating new names) or in a loop.
///
pub fn start(
  name: Name(Message(message)),
) -> actor.StartResult(GroupRegistry(message)) {
  case erlang_start(name) {
    Ok(pid) -> Ok(actor.Started(pid:, data: get_registry(name)))
    Error(reason) -> Error(actor.InitExited(process.Abnormal(reason)))
  }
}

/// A specification for starting the registry under a supervisor, using the
/// given name. You should likely use this function in applications.
///
/// Remember that names must be created at the start of your program, and must
/// not be created dynamically such as within your supervision tree (it may
/// restart, creating new names) or in a loop.
///
pub fn supervised(
  name: Name(Message(message)),
) -> supervision.ChildSpecification(GroupRegistry(message)) {
  supervision.worker(fn() { start(name) })
}

@external(erlang, "gleam@function", "identity")
pub fn get_registry(name: Name(Message(message))) -> GroupRegistry(message)

/// Add a process to a group.
///
/// A process can join a group many times and must then leave the group the
/// same number of times.
///
/// A subject is returned which can be used to send to messages to the member,
/// or for the member to receive messages.
///
pub fn join(
  registry: GroupRegistry(message),
  group: String,
  new_member: Pid,
) -> Subject(message) {
  erlang_join(registry, group, new_member)
  make_subject(registry, group, new_member)
}

/// Remove the given processes from the group, if they are members.
///
pub fn leave(
  registry: GroupRegistry(message),
  group: String,
  members: List(Pid),
) -> Nil {
  erlang_leave(registry, group, members)
  Nil
}

/// Returns subjects for all processes in the group. They are returned in
/// no specific order.
///
/// If a process joined the group multiple times it will be present in the list
/// that number of times.
///
pub fn members(
  registry: GroupRegistry(message),
  group: String,
) -> List(Subject(message)) {
  erlang_members(registry, group)
  |> list.map(make_subject(registry, group, _))
}

//
// Helpers
//

fn make_subject(
  registry: GroupRegistry(message),
  group: String,
  pid: Pid,
) -> Subject(message) {
  let tag = make_tag(#(registry, group))
  process.unsafely_create_subject(pid, tag)
}

//
// Erlang FFI
//

type DoNotLeak

@external(erlang, "pg", "start_link")
fn erlang_start(name: Name(Message(m))) -> Result(Pid, Dynamic)

@external(erlang, "pg", "join")
fn erlang_join(
  registry: GroupRegistry(m),
  group: String,
  new_members: Pid,
) -> DoNotLeak

@external(erlang, "pg", "leave")
fn erlang_leave(
  registry: GroupRegistry(m),
  group: String,
  members: List(Pid),
) -> DoNotLeak

@external(erlang, "pg", "get_members")
fn erlang_members(registry: GroupRegistry(m), group: String) -> List(Pid)

@external(erlang, "gleam@function", "identity")
fn make_tag(a: #(GroupRegistry(m), String)) -> Dynamic
