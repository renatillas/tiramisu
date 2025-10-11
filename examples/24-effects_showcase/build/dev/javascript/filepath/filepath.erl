-module(filepath).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([join/2, split_unix/1, directory_name/1, is_absolute/1, split_windows/1, split/1, base_name/1, extension/1, strip_extension/1, expand/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Work with file paths in Gleam!\n"
    "\n"
    " This library expects paths to be valid unicode. If you need to work with\n"
    " non-unicode paths you will need to convert them to unicode before using\n"
    " this library.\n"
).

-file("src/filepath.gleam", 49).
-spec relative(binary()) -> binary().
relative(Path) ->
    case Path of
        <<"/"/utf8, Path@1/binary>> ->
            relative(Path@1);

        _ ->
            Path
    end.

-file("src/filepath.gleam", 56).
-spec remove_trailing_slash(binary()) -> binary().
remove_trailing_slash(Path) ->
    case gleam@string:ends_with(Path, <<"/"/utf8>>) of
        true ->
            gleam@string:drop_end(Path, 1);

        false ->
            Path
    end.

-file("src/filepath.gleam", 35).
?DOC(
    " Join two paths together.\n"
    "\n"
    " This function does not expand `..` or `.` segments, use the `expand`\n"
    " function to do this.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " join(\"/usr/local\", \"bin\")\n"
    " // -> \"/usr/local/bin\"\n"
    " ```\n"
).
-spec join(binary(), binary()) -> binary().
join(Left, Right) ->
    _pipe@2 = case {Left, Right} of
        {_, <<"/"/utf8>>} ->
            Left;

        {<<""/utf8>>, _} ->
            relative(Right);

        {<<"/"/utf8>>, <<"/"/utf8, _/binary>>} ->
            Right;

        {<<"/"/utf8>>, _} ->
            <<Left/binary, Right/binary>>;

        {_, _} ->
            _pipe = remove_trailing_slash(Left),
            _pipe@1 = gleam@string:append(_pipe, <<"/"/utf8>>),
            gleam@string:append(_pipe@1, relative(Right))
    end,
    remove_trailing_slash(_pipe@2).

-file("src/filepath.gleam", 97).
?DOC(
    " Split a path into its segments, using `/` as the path separator.\n"
    "\n"
    " Typically you would want to use `split` instead of this function, but if you\n"
    " want non-Windows path behaviour on a Windows system then you can use this\n"
    " function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " split(\"/usr/local/bin\", \"bin\")\n"
    " // -> [\"/\", \"usr\", \"local\", \"bin\"]\n"
    " ```\n"
).
-spec split_unix(binary()) -> list(binary()).
split_unix(Path) ->
    _pipe = case gleam@string:split(Path, <<"/"/utf8>>) of
        [<<""/utf8>>] ->
            [];

        [<<""/utf8>> | Rest] ->
            [<<"/"/utf8>> | Rest];

        Rest@1 ->
            Rest@1
    end,
    gleam@list:filter(_pipe, fun(X) -> X /= <<""/utf8>> end).

-file("src/filepath.gleam", 267).
-spec get_directory_name(list(binary()), binary(), binary()) -> binary().
get_directory_name(Path, Acc, Segment) ->
    case Path of
        [<<"/"/utf8>> | Rest] ->
            get_directory_name(
                Rest,
                <<Acc/binary, Segment/binary>>,
                <<"/"/utf8>>
            );

        [First | Rest@1] ->
            get_directory_name(Rest@1, Acc, <<Segment/binary, First/binary>>);

        [] ->
            Acc
    end.

-file("src/filepath.gleam", 259).
?DOC(
    " Get the directory name of a path, that is the path without the file name.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " directory_name(\"/usr/local/bin\")\n"
    " // -> \"/usr/local\"\n"
    " ```\n"
).
-spec directory_name(binary()) -> binary().
directory_name(Path) ->
    Path@1 = remove_trailing_slash(Path),
    case Path@1 of
        <<"/"/utf8, Rest/binary>> ->
            get_directory_name(
                gleam@string:to_graphemes(Rest),
                <<"/"/utf8>>,
                <<""/utf8>>
            );

        _ ->
            get_directory_name(
                gleam@string:to_graphemes(Path@1),
                <<""/utf8>>,
                <<""/utf8>>
            )
    end.

-file("src/filepath.gleam", 294).
?DOC(
    " Check if a path is absolute.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " is_absolute(\"/usr/local/bin\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_absolute(\"usr/local/bin\")\n"
    " // -> False\n"
    " ```\n"
).
-spec is_absolute(binary()) -> boolean().
is_absolute(Path) ->
    gleam@string:starts_with(Path, <<"/"/utf8>>).

-file("src/filepath.gleam", 339).
-spec expand_segments(list(binary()), list(binary())) -> {ok, binary()} |
    {error, nil}.
expand_segments(Path, Base) ->
    case {Base, Path} of
        {[<<""/utf8>>], [<<".."/utf8>> | _]} ->
            {error, nil};

        {[], [<<".."/utf8>> | _]} ->
            {error, nil};

        {[_ | Base@1], [<<".."/utf8>> | Path@1]} ->
            expand_segments(Path@1, Base@1);

        {_, [<<"."/utf8>> | Path@2]} ->
            expand_segments(Path@2, Base);

        {_, [S | Path@3]} ->
            expand_segments(Path@3, [S | Base]);

        {_, []} ->
            {ok, gleam@string:join(lists:reverse(Base), <<"/"/utf8>>)}
    end.

-file("src/filepath.gleam", 364).
-spec root_slash_to_empty(list(binary())) -> list(binary()).
root_slash_to_empty(Segments) ->
    case Segments of
        [<<"/"/utf8>> | Rest] ->
            [<<""/utf8>> | Rest];

        _ ->
            Segments
    end.

-file("src/filepath.gleam", 153).
-spec pop_windows_drive_specifier(binary()) -> {gleam@option:option(binary()),
    binary()}.
pop_windows_drive_specifier(Path) ->
    Start = gleam@string:slice(Path, 0, 3),
    Codepoints = gleam@string:to_utf_codepoints(Start),
    case gleam@list:map(Codepoints, fun gleam@string:utf_codepoint_to_int/1) of
        [Drive, Colon, Slash] when (((Slash =:= 47) orelse (Slash =:= 92)) andalso (Colon =:= 58)) andalso (((Drive >= 65) andalso (Drive =< 90)) orelse ((Drive >= 97) andalso (Drive =< 122))) ->
            Drive_letter = gleam@string:slice(Path, 0, 1),
            Drive@1 = <<(gleam@string:lowercase(Drive_letter))/binary,
                ":/"/utf8>>,
            Path@1 = gleam@string:drop_start(Path, 3),
            {{some, Drive@1}, Path@1};

        _ ->
            {none, Path}
    end.

-file("src/filepath.gleam", 120).
?DOC(
    " Split a path into its segments, using `/` and `\\` as the path separators. If\n"
    " there is a drive letter at the start of the path then it is lowercased.\n"
    "\n"
    " Typically you would want to use `split` instead of this function, but if you\n"
    " want Windows path behaviour on a non-Windows system then you can use this\n"
    " function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " split(\"/usr/local/bin\", \"bin\")\n"
    " // -> [\"/\", \"usr\", \"local\", \"bin\"]\n"
    " ```\n"
).
-spec split_windows(binary()) -> list(binary()).
split_windows(Path) ->
    {Drive, Path@1} = pop_windows_drive_specifier(Path),
    Segments = begin
        _pipe = gleam@string:split(Path@1, <<"/"/utf8>>),
        gleam@list:flat_map(
            _pipe,
            fun(_capture) -> gleam@string:split(_capture, <<"\\"/utf8>>) end
        )
    end,
    Segments@1 = case Drive of
        {some, Drive@1} ->
            [Drive@1 | Segments];

        none ->
            Segments
    end,
    case Segments@1 of
        [<<""/utf8>>] ->
            [];

        [<<""/utf8>> | Rest] ->
            [<<"/"/utf8>> | Rest];

        Rest@1 ->
            Rest@1
    end.

-file("src/filepath.gleam", 77).
?DOC(
    " Split a path into its segments.\n"
    "\n"
    " When running on Windows both `/` and `\\` are treated as path separators, and \n"
    " if the path starts with a drive letter then the drive letter then it is\n"
    " lowercased.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " split(\"/usr/local/bin\", \"bin\")\n"
    " // -> [\"/\", \"usr\", \"local\", \"bin\"]\n"
    " ```\n"
).
-spec split(binary()) -> list(binary()).
split(Path) ->
    case filepath_ffi:is_windows() of
        true ->
            split_windows(Path);

        false ->
            split_unix(Path)
    end.

-file("src/filepath.gleam", 240).
?DOC(
    " Get the base name of a path, that is the name of the file without the\n"
    " containing directory.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " base_name(\"/usr/local/bin\")\n"
    " // -> \"bin\"\n"
    " ```\n"
).
-spec base_name(binary()) -> binary().
base_name(Path) ->
    gleam@bool:guard(Path =:= <<"/"/utf8>>, <<""/utf8>>, fun() -> _pipe = Path,
            _pipe@1 = split(_pipe),
            _pipe@2 = gleam@list:last(_pipe@1),
            gleam@result:unwrap(_pipe@2, <<""/utf8>>) end).

-file("src/filepath.gleam", 190).
?DOC(
    " Get the file extension of a path.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " extension(\"src/main.gleam\")\n"
    " // -> Ok(\"gleam\")\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " extension(\"package.tar.gz\")\n"
    " // -> Ok(\"gz\")\n"
    " ```\n"
).
-spec extension(binary()) -> {ok, binary()} | {error, nil}.
extension(Path) ->
    File = base_name(Path),
    case gleam@string:split(File, <<"."/utf8>>) of
        [<<""/utf8>>, _] ->
            {error, nil};

        [_, Extension] ->
            {ok, Extension};

        [_ | Rest] ->
            gleam@list:last(Rest);

        _ ->
            {error, nil}
    end.

-file("src/filepath.gleam", 219).
?DOC(
    " Remove the extension from a file, if it has any.\n"
    " \n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " strip_extension(\"src/main.gleam\")\n"
    " // -> \"src/main\"\n"
    " ```\n"
    " \n"
    " ```gleam\n"
    " strip_extension(\"package.tar.gz\")\n"
    " // -> \"package.tar\"\n"
    " ```\n"
    " \n"
    " ```gleam\n"
    " strip_extension(\"src/gleam\")\n"
    " // -> \"src/gleam\"\n"
    " ```\n"
).
-spec strip_extension(binary()) -> binary().
strip_extension(Path) ->
    case extension(Path) of
        {ok, Extension} ->
            gleam@string:drop_end(Path, gleam@string:length(Extension) + 1);

        {error, nil} ->
            Path
    end.

-file("src/filepath.gleam", 324).
?DOC(
    " Expand `..` and `.` segments in a path.\n"
    "\n"
    " If the path has a `..` segment that would go up past the root of the path\n"
    " then an error is returned. This may be useful to example to ensure that a\n"
    " path specified by a user does not go outside of a directory.\n"
    "\n"
    " If the path is absolute then the result will always be absolute.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " expand(\"/usr/local/../bin\")\n"
    " // -> Ok(\"/usr/bin\")\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " expand(\"/tmp/../..\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " expand(\"src/../..\")\n"
    " // -> Error(\"..\")\n"
    " ```\n"
).
-spec expand(binary()) -> {ok, binary()} | {error, nil}.
expand(Path) ->
    Is_absolute = is_absolute(Path),
    Result = begin
        _pipe = Path,
        _pipe@1 = split(_pipe),
        _pipe@2 = root_slash_to_empty(_pipe@1),
        _pipe@3 = expand_segments(_pipe@2, []),
        gleam@result:map(_pipe@3, fun remove_trailing_slash/1)
    end,
    case Is_absolute andalso (Result =:= {ok, <<""/utf8>>}) of
        true ->
            {ok, <<"/"/utf8>>};

        false ->
            Result
    end.
