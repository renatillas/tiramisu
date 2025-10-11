-module(gleam_regexp_ffi).

-export([compile/2, check/2, split/2, replace/3, scan/2, match_map/3]).

compile(String, Options) ->
    {options, Caseless, Multiline} = Options,
    OptionsList = [
        unicode,
        ucp,
        Caseless andalso caseless,
        Multiline andalso multiline
    ],
    FilteredOptions = [Option || Option <- OptionsList, Option /= false],
    case re:compile(String, FilteredOptions) of
        {ok, MP} -> {ok, MP};
        {error, {Str, Pos}} ->
            {error, {compile_error, unicode:characters_to_binary(Str), Pos}}
    end.

check(Regexp, String) ->
    re:run(String, Regexp) /= nomatch.

split(Regexp, String) ->
    re:split(String, Regexp).

submatches(_, {-1, 0}) -> none;
submatches(String, {Start, Length}) ->
    BinarySlice = binary:part(String, {Start, Length}),
    case string:is_empty(binary_to_list(BinarySlice)) of
        true -> none;
        false -> {some, BinarySlice}
    end.

matches(String, [{Start, Length} | Submatches]) ->
    Submatches1 = lists:map(fun(X) -> submatches(String, X) end, Submatches),
    {match, binary:part(String, Start, Length), Submatches1}.

scan(Regexp, String) ->
    case re:run(String, Regexp, [global]) of
        {match, Captured} -> lists:map(fun(X) -> matches(String, X) end, Captured);
        nomatch -> []
    end.

replace(Regexp, Subject, Replacement) ->
    re:replace(Subject, Regexp, Replacement, [global, {return, binary}]).

match_map(Regexp, Subject, Replacement) ->
    Replacement1 = fun(Content, Submatches) ->
        Submatches1 = lists:map(fun gleam@string:to_option/1, Submatches),
        Replacement({match, Content, Submatches1})
    end,
    re:replace(Subject, Regexp, Replacement1, [global, {return, binary}]).
