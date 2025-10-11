# group_registry

Organise processes into groups! Useful for pubsub.

[![Package Version](https://img.shields.io/hexpm/v/group_registry)](https://hex.pm/packages/group_registry)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/group_registry/)

```sh
gleam add group_registry@1
```

Add a group registry to your supervision tree with
`group_registry.supervised` and a name that you created at the start of your
program.

```gleam
import gleam/otp/actor
import gleam/otp/static_supervisor as supervisor
import group_registry

pub fn my_supervisor(name: Name(_)) -> actor.StartResult(_) {
  supervisor.new(supervisor.RestForOne)
  |> supervisor.add(group_registry.supervised(name))
  |> supervisor.start
}
```

Elsewhere in your code use `group_registry.get_registry` to get a reference to
the registry using the name you passed to `group_registry.supervised`.

Processes can then use the `group_registry.join` function to join groups.

```gleam
import gleam/otp/actor
import group_registry.{type GroupRegistry}

pub fn start_actor(registry: GroupRegistry(String)) -> actor.StartResult(_) {
  actor.new_with_initialiser(100, fn(_) { 
    // Join a group named "updates"
    let subject = group_registry.join(registry, "extra-cool-processes")
    // Add the group subject to the selector so messages will be received
    actor.initialised(Nil)
    |> actor.selecting(process.new_selector() |> process.select(subject))
    |> Ok
  })
  |> actor.on_message(fn(state, message) {
    io.println("Got message: " <> message)
    actor.continue(state)
  })
  |> actor.start
}
```

Other processes can then publish messages to the members of the group. 

```gleam
import gleam/erlang/process
import group_registry.{type GroupRegistry}

pub fn publish(registry: GroupRegistry(String)) -> Nil {
  // This returns a list of subjects for the processes in the group
  let members = group_registry.members(registry, "extra-cool-processes")

  // Send a message to each one
  list.each(members, fn(member) {
    process.send(member, "Hello!")
  })
}
```

When a process terminates it is automatically cleaned up and removed from the
groups in the registry.

Further documentation can be found at <https://hexdocs.pm/group_registry>.
