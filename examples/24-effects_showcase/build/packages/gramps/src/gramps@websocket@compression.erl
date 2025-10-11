-module(gramps@websocket@compression).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gramps/websocket/compression.gleam").
-export([init/1, set_controlling_process/2, close/1, inflate/2, deflate/2]).
-export_type([compression_context/0, context/0, flush/0, deflated/0, default/0, context_takeover/0, compression/0]).

-type compression_context() :: any().

-type context() :: {context, compression_context(), boolean()}.

-type flush() :: sync.

-type deflated() :: deflated.

-type default() :: default.

-type context_takeover() :: {context_takeover, boolean(), boolean()}.

-type compression() :: {compression, context(), context()}.

-file("src/gramps/websocket/compression.gleam", 32).
-spec init(context_takeover()) -> compression().
init(Takeover) ->
    Inflate = zlib:open(),
    Inflate_context = {context, Inflate, erlang:element(2, Takeover)},
    zlib:'inflateInit'(Inflate, -15),
    Deflate = zlib:open(),
    Deflate_context = {context, Deflate, erlang:element(3, Takeover)},
    zlib:'deflateInit'(Deflate, default, deflated, -15, 8, default),
    {compression, Inflate_context, Deflate_context}.

-file("src/gramps/websocket/compression.gleam", 108).
-spec set_controlling_process(context(), gleam@erlang@process:pid_()) -> gleam@erlang@atom:atom_().
set_controlling_process(Context, Pid) ->
    zlib:set_controlling_process(Context, Pid).

-file("src/gramps/websocket/compression.gleam", 110).
-spec close(context()) -> nil.
close(Context) ->
    zlib:close(erlang:element(2, Context)).

-file("src/gramps/websocket/compression.gleam", 65).
-spec inflate(context(), bitstring()) -> bitstring().
inflate(Context, Data) ->
    Output = begin
        _pipe = erlang:element(2, Context),
        _pipe@1 = zlib:inflate(
            _pipe,
            <<Data/bitstring, 16#00, 16#00, 16#FF, 16#FF>>
        ),
        erlang:list_to_bitstring(_pipe@1)
    end,
    _ = case erlang:element(3, Context) of
        true ->
            zlib:'inflateReset'(erlang:element(2, Context));

        false ->
            nil
    end,
    Output.

-file("src/gramps/websocket/compression.gleam", 86).
-spec deflate(context(), bitstring()) -> bitstring().
deflate(Context, Data) ->
    Data@1 = begin
        _pipe = erlang:element(2, Context),
        _pipe@1 = zlib:deflate(_pipe, Data, sync),
        erlang:list_to_bitstring(_pipe@1)
    end,
    Size = erlang:byte_size(Data@1) - 4,
    Return = case Data@1 of
        <<Value:Size/binary, 16#00, 16#00, 16#FF, 16#FF>> ->
            Value;

        _ ->
            Data@1
    end,
    _ = case erlang:element(3, Context) of
        true ->
            zlib:'deflateReset'(erlang:element(2, Context));

        false ->
            nil
    end,
    Return.
