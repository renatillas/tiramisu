-module(glint@internal@utils).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([max_string_length/1, wordwrap/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/glint/internal/utils.gleam", 8).
?DOC(false).
-spec max_string_length(list(binary())) -> integer().
max_string_length(Strings) ->
    gleam@list:fold(Strings, 0, fun(Max, F) -> _pipe = F,
            _pipe@1 = string:length(_pipe),
            gleam@int:max(_pipe@1, Max) end).

-file("src/glint/internal/utils.gleam", 27).
?DOC(false).
-spec do_wordwrap(list(binary()), integer(), binary(), list(binary())) -> list(binary()).
do_wordwrap(Tokens, Max_width, Line, Lines) ->
    case Tokens of
        [Token | Tokens@1] ->
            Token_length = string:length(Token),
            Line_length = string:length(Line),
            case {Line, ((Line_length + 1) + Token_length) =< Max_width} of
                {<<""/utf8>>, _} ->
                    do_wordwrap(Tokens@1, Max_width, Token, Lines);

                {_, true} ->
                    do_wordwrap(
                        Tokens@1,
                        Max_width,
                        <<<<Line/binary, " "/utf8>>/binary, Token/binary>>,
                        Lines
                    );

                {_, false} ->
                    do_wordwrap(Tokens@1, Max_width, Token, [Line | Lines])
            end;

        [] when Line =:= <<""/utf8>> ->
            lists:reverse(Lines);

        [] ->
            lists:reverse([Line | Lines])
    end.

-file("src/glint/internal/utils.gleam", 63).
?DOC(false).
-spec space_split_lines(binary()) -> list(binary()).
space_split_lines(S) ->
    Chunks = begin
        _pipe = S,
        _pipe@1 = gleam@string:trim(_pipe),
        _pipe@2 = gleam@string:to_graphemes(_pipe@1),
        gleam@list:chunk(_pipe@2, fun(S@1) -> S@1 =:= <<"\n"/utf8>> end)
    end,
    Lines = begin
        gleam@list:fold(
            Chunks,
            {[], false},
            fun(Acc, Chunk) -> case {Chunk, erlang:element(1, Acc)} of
                    {[<<"\n"/utf8>>, <<"\n"/utf8>> | Rest], [S@2 | Accs]} ->
                        {[<<S@2/binary, (gleam@string:concat(Rest))/binary>> |
                                Accs],
                            true};

                    {[<<"\n"/utf8>>], [S@3 | Accs@1]} ->
                        {[<<S@3/binary, " "/utf8>> | Accs@1], false};

                    {_, [S@4 | Accs@2]} when not erlang:element(2, Acc) ->
                        {[<<S@4/binary, (gleam@string:concat(Chunk))/binary>> |
                                Accs@2],
                            false};

                    {_, _} ->
                        {[gleam@string:concat(Chunk) | erlang:element(1, Acc)],
                            false}
                end end
        )
    end,
    lists:reverse(erlang:element(1, Lines)).

-file("src/glint/internal/utils.gleam", 19).
?DOC(false).
-spec wordwrap(binary(), integer()) -> list(binary()).
wordwrap(S, Max_width) ->
    gleam@bool:guard(
        S =:= <<""/utf8>>,
        [],
        fun() ->
            gleam@list:flat_map(space_split_lines(S), fun(Line) -> _pipe = Line,
                    _pipe@1 = gleam@string:split(_pipe, <<" "/utf8>>),
                    do_wordwrap(_pipe@1, Max_width, <<""/utf8>>, []) end)
        end
    ).
