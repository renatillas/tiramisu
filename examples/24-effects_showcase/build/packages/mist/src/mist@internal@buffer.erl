-module(mist@internal@buffer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/buffer.gleam").
-export([empty/0, new/1, append/2, slice/2, with_capacity/2, size/1]).
-export_type([buffer/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type buffer() :: {buffer, integer(), bitstring()}.

-file("src/mist/internal/buffer.gleam", 8).
?DOC(false).
-spec empty() -> buffer().
empty() ->
    {buffer, 0, <<>>}.

-file("src/mist/internal/buffer.gleam", 12).
?DOC(false).
-spec new(bitstring()) -> buffer().
new(Data) ->
    {buffer, 0, Data}.

-file("src/mist/internal/buffer.gleam", 16).
?DOC(false).
-spec append(buffer(), bitstring()) -> buffer().
append(Buffer, Data) ->
    Data_size = erlang:byte_size(Data),
    Remaining = gleam@int:max(erlang:element(2, Buffer) - Data_size, 0),
    {buffer,
        Remaining,
        <<(erlang:element(3, Buffer))/bitstring, Data/bitstring>>}.

-file("src/mist/internal/buffer.gleam", 22).
?DOC(false).
-spec slice(buffer(), integer()) -> {bitstring(), bitstring()}.
slice(Buffer, Bits) ->
    Bytes = Bits * 8,
    case erlang:element(3, Buffer) of
        <<Value:Bytes/bitstring, Rest/bitstring>> ->
            {Value, Rest};

        _ ->
            {erlang:element(3, Buffer), <<>>}
    end.

-file("src/mist/internal/buffer.gleam", 30).
?DOC(false).
-spec with_capacity(buffer(), integer()) -> buffer().
with_capacity(Buffer, Size) ->
    {buffer, Size, erlang:element(3, Buffer)}.

-file("src/mist/internal/buffer.gleam", 34).
?DOC(false).
-spec size(integer()) -> buffer().
size(Remaining) ->
    {buffer, Remaining, <<>>}.
