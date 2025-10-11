-module(group_registry).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/group_registry.gleam").
-export([get_registry/1, start/1, supervised/1, leave/3, join/3, members/2]).
-export_type([group_registry/1, message/1, do_not_leak/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Groups that can be joined and left by processes, and the members of which\n"
    " can be listed on request. This may be useful for publish-subscribe\n"
    " patterns, where many processes want to receive messages another.\n"
    "\n"
    " This module is implemented using Erlang's [`pg`][pg] module and uses ETS\n"
    " for storage, so listing the members of a group is optimised for speed.\n"
    "\n"
    " Groups are tracked by group registry processes, which you should add to\n"
    " your supervision tree. Each registry is independant from each other.\n"
    "\n"
    " If a member terminates, it is automatically removed from the group.\n"
    "\n"
    " If a group registry terminates the groups are lost and will need to be\n"
    " recreated. Restarting the group registry will not recover the groups.\n"
    "\n"
    " There is no memory cost to the registry of a group without any members.\n"
    "\n"
    " ## Publish-subscribe\n"
    "\n"
    " This module is useful for pubsub, but it does not offer any functions for\n"
    " sending messages itself. To perform pubsub add the subscriber messages to\n"
    " a group, then the publishers can use the `members` function to get a\n"
    " list of subjects for the subscribers and send messages to them.\n"
    "\n"
    " ## Distributed groups\n"
    "\n"
    " If two nodes in an Erlang cluster have process registries or `pg`\n"
    " instances created with the same name (called a \"scope\" in the `pg`\n"
    " documentation) they will share group membership in an eventually\n"
    " consistent way. See the `pg` documentation for more information. Note that\n"
    " names created with the `process.new_name` are unique, so calling that\n"
    " function with the same prefix string on each node in an Erlang cluster\n"
    " will result in distinct names.\n"
    "\n"
    " [pg]: https://www.erlang.org/doc/apps/kernel/pg.html\n"
    "\n"
    " ## Scalability\n"
    "\n"
    " Inserting members or getting all the members of a group `pg` is fast, but\n"
    " removing members from large groups with thousands of members in them is\n"
    " much slower. This module is best suited to numerous small groups.\n"
    "\n"
    " If you need larger groups and members to be removed or to terminate\n"
    " frequently you may want to experiment with other registries. Always\n"
    " benchmark and profile your code when performance matters.\n"
).

-type group_registry(QMQ) :: any() | {gleam_phantom, QMQ}.

-type message(QMR) :: any() | {gleam_phantom, QMR}.

-type do_not_leak() :: any().

-file("src/group_registry.gleam", 104).
-spec get_registry(gleam@erlang@process:name(message(QNC))) -> group_registry(QNC).
get_registry(Name) ->
    gleam@function:identity(Name).

-file("src/group_registry.gleam", 81).
?DOC(
    " Start the registry with the given name. You likely want to use the\n"
    " `supervised` function instead, to add the registry to your supervision\n"
    " tree, but this may still be useful in your tests.\n"
    "\n"
    " Remember that names must be created at the start of your program, and must\n"
    " not be created dynamically such as within your supervision tree (it may\n"
    " restart, creating new names) or in a loop.\n"
).
-spec start(gleam@erlang@process:name(message(QMS))) -> {ok,
        gleam@otp@actor:started(group_registry(QMS))} |
    {error, gleam@otp@actor:start_error()}.
start(Name) ->
    case pg:start_link(Name) of
        {ok, Pid} ->
            {ok, {started, Pid, gleam@function:identity(Name)}};

        {error, Reason} ->
            {error, {init_exited, {abnormal, Reason}}}
    end.

-file("src/group_registry.gleam", 97).
?DOC(
    " A specification for starting the registry under a supervisor, using the\n"
    " given name. You should likely use this function in applications.\n"
    "\n"
    " Remember that names must be created at the start of your program, and must\n"
    " not be created dynamically such as within your supervision tree (it may\n"
    " restart, creating new names) or in a loop.\n"
).
-spec supervised(gleam@erlang@process:name(message(QMX))) -> gleam@otp@supervision:child_specification(group_registry(QMX)).
supervised(Name) ->
    gleam@otp@supervision:worker(fun() -> start(Name) end).

-file("src/group_registry.gleam", 125).
?DOC(" Remove the given processes from the group, if they are members.\n").
-spec leave(group_registry(any()), binary(), list(gleam@erlang@process:pid_())) -> nil.
leave(Registry, Group, Members) ->
    pg:leave(Registry, Group, Members),
    nil.

-file("src/group_registry.gleam", 152).
-spec make_subject(group_registry(QNQ), binary(), gleam@erlang@process:pid_()) -> gleam@erlang@process:subject(QNQ).
make_subject(Registry, Group, Pid) ->
    Tag = gleam@function:identity({Registry, Group}),
    gleam@erlang@process:unsafely_create_subject(Pid, Tag).

-file("src/group_registry.gleam", 114).
?DOC(
    " Add a process to a group.\n"
    "\n"
    " A process can join a group many times and must then leave the group the\n"
    " same number of times.\n"
    "\n"
    " A subject is returned which can be used to send to messages to the member,\n"
    " or for the member to receive messages.\n"
).
-spec join(group_registry(QNG), binary(), gleam@erlang@process:pid_()) -> gleam@erlang@process:subject(QNG).
join(Registry, Group, New_member) ->
    pg:join(Registry, Group, New_member),
    make_subject(Registry, Group, New_member).

-file("src/group_registry.gleam", 140).
?DOC(
    " Returns subjects for all processes in the group. They are returned in\n"
    " no specific order.\n"
    "\n"
    " If a process joined the group multiple times it will be present in the list\n"
    " that number of times.\n"
).
-spec members(group_registry(QNM), binary()) -> list(gleam@erlang@process:subject(QNM)).
members(Registry, Group) ->
    _pipe = pg:get_members(Registry, Group),
    gleam@list:map(
        _pipe,
        fun(_capture) -> make_subject(Registry, Group, _capture) end
    ).
