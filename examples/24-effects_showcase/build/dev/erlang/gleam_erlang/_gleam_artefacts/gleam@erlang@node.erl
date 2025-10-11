-module(gleam@erlang@node).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/erlang/node.gleam").
-export([self/0, visible/0, connect/1, name/1]).
-export_type([node_/0, connect_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Multiple Erlang VM instances can form a cluster to make a distributed\n"
    " Erlang system, talking directly to each other using messages rather than\n"
    " other communication protocols like HTTP. In a distributed Erlang system\n"
    " each virtual machine is called a _node_. This module provides Node related\n"
    " types and functions to be used as a foundation by other packages providing\n"
    " more specialised functionality.\n"
    "\n"
    " For more information on distributed Erlang systems see the Erlang\n"
    " documentation: <https://www.erlang.org/doc/system/distributed.html>.\n"
).

-type node_() :: any().

-type connect_error() :: failed_to_connect | local_node_is_not_alive.

-file("src/gleam/erlang/node.gleam", 18).
?DOC(" Return the current node.\n").
-spec self() -> node_().
self() ->
    erlang:node().

-file("src/gleam/erlang/node.gleam", 31).
?DOC(
    " Return a list of all visible nodes in the cluster, not including the current\n"
    " node.\n"
    "\n"
    " The current node can be included by calling `self()` and prepending the\n"
    " result.\n"
    "\n"
    " ```gleam\n"
    " let all_nodes = [node.self(), ..node.visible()]\n"
    " ```\n"
).
-spec visible() -> list(node_()).
visible() ->
    erlang:nodes().

-file("src/gleam/erlang/node.gleam", 52).
?DOC(
    " Establish a connection to a node, so the nodes can send messages to each\n"
    " other and any other connected nodes.\n"
    "\n"
    " Returns `Error(FailedToConnect)` if the node is not reachable.\n"
    "\n"
    " Returns `Error(LocalNodeIsNotAlive)` if the local node is not alive, meaning\n"
    " it is not running in distributed mode.\n"
).
-spec connect(gleam@erlang@atom:atom_()) -> {ok, node_()} |
    {error, connect_error()}.
connect(Node) ->
    gleam_erlang_ffi:connect_node(Node).

-file("src/gleam/erlang/node.gleam", 63).
?DOC(
    " Get the atom name of a node.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " assert name(my_node) == atom.create(\"app1@localhost\")\n"
    " ```\n"
).
-spec name(node_()) -> gleam@erlang@atom:atom_().
name(Node) ->
    gleam_erlang_ffi:identity(Node).
