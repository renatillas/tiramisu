-module(glint@internal@help).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([command_help_to_string/2]).
-export_type([args_count/0, config/0, metadata/0, flag/0, command/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type args_count() :: {min_args, integer()} | {eq_args, integer()}.

-type config() :: {config,
        gleam@option:option(binary()),
        gleam@option:option(gleam_community@colour:colour()),
        gleam@option:option(gleam_community@colour:colour()),
        gleam@option:option(gleam_community@colour:colour()),
        boolean(),
        gleam@option:option(binary()),
        integer(),
        integer(),
        integer(),
        integer(),
        binary(),
        binary()}.

-type metadata() :: {metadata, binary(), binary()}.

-type flag() :: {flag, metadata(), binary()}.

-type command() :: {command,
        metadata(),
        list(flag()),
        list(metadata()),
        gleam@option:option(args_count()),
        list(binary())}.

-file("src/glint/internal/help.gleam", 13).
?DOC(false).
-spec heading_style(binary(), gleam_community@colour:colour()) -> binary().
heading_style(Heading, Colour) ->
    _pipe = Heading,
    _pipe@1 = gleam_community@ansi:bold(_pipe),
    _pipe@2 = gleam_community@ansi:underline(_pipe@1),
    _pipe@3 = gleam_community@ansi:italic(_pipe@2),
    gleam_community@ansi:hex(_pipe@3, gleam_community@colour:to_rgb_hex(Colour)).

-file("src/glint/internal/help.gleam", 131).
?DOC(false).
-spec args_count_to_usage_string(args_count()) -> binary().
args_count_to_usage_string(Count) ->
    case Count of
        {eq_args, 0} ->
            <<""/utf8>>;

        {eq_args, 1} ->
            <<"[ 1 argument ]"/utf8>>;

        {eq_args, N} ->
            <<<<"[ "/utf8, (erlang:integer_to_binary(N))/binary>>/binary,
                " arguments ]"/utf8>>;

        {min_args, N@1} ->
            <<<<"[ "/utf8, (erlang:integer_to_binary(N@1))/binary>>/binary,
                " or more arguments ]"/utf8>>
    end.

-file("src/glint/internal/help.gleam", 218).
?DOC(false).
-spec flag_help_to_string(flag(), config()) -> binary().
flag_help_to_string(Help, Config) ->
    <<<<(erlang:element(12, Config))/binary,
            (erlang:element(2, erlang:element(2, Help)))/binary>>/binary,
        (case erlang:element(3, Help) of
            <<""/utf8>> ->
                <<""/utf8>>;

            _ ->
                <<<<<<(erlang:element(13, Config))/binary, "<"/utf8>>/binary,
                        (erlang:element(3, Help))/binary>>/binary,
                    ">"/utf8>>
        end)/binary>>.

-file("src/glint/internal/help.gleam", 111).
?DOC(false).
-spec flags_help_to_usage_strings(list(flag()), config()) -> list(binary()).
flags_help_to_usage_strings(Help, Config) ->
    _pipe = Help,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(_capture) -> flag_help_to_string(_capture, Config) end
    ),
    gleam@list:sort(_pipe@1, fun gleam@string:compare/2).

-file("src/glint/internal/help.gleam", 119).
?DOC(false).
-spec flags_help_to_usage_string(config(), list(flag())) -> binary().
flags_help_to_usage_string(Config, Help) ->
    gleam@bool:guard(
        Help =:= [],
        <<""/utf8>>,
        fun() ->
            Content = begin
                _pipe = Help,
                _pipe@1 = flags_help_to_usage_strings(_pipe, Config),
                gleam@string:join(_pipe@1, <<" "/utf8>>)
            end,
            <<<<"[ "/utf8, Content/binary>>/binary, " ]"/utf8>>
        end
    ).

-file("src/glint/internal/help.gleam", 285).
?DOC(false).
-spec format_content(binary(), binary(), integer(), config()) -> {binary(),
    boolean()}.
format_content(Left, Right, Left_length, Config) ->
    Left_formatted = gleam@string:pad_end(Left, Left_length, <<" "/utf8>>),
    Lines = begin
        _pipe = erlang:element(9, Config),
        _pipe@1 = gleam@int:subtract(
            _pipe,
            Left_length + erlang:element(8, Config)
        ),
        _pipe@2 = gleam@int:max(_pipe@1, erlang:element(10, Config)),
        glint@internal@utils:wordwrap(Right, _pipe@2)
    end,
    Right_formatted = gleam@string:join(
        Lines,
        <<"\n"/utf8,
            (gleam@string:repeat(
                <<" "/utf8>>,
                erlang:element(8, Config) + Left_length
            ))/binary>>
    ),
    Wrapped = case Lines of
        [] ->
            false;

        [_] ->
            false;

        _ ->
            true
    end,
    {<<<<<<"\n"/utf8,
                    (gleam@string:repeat(
                        <<" "/utf8>>,
                        erlang:element(8, Config)
                    ))/binary>>/binary,
                Left_formatted/binary>>/binary,
            Right_formatted/binary>>,
        Wrapped}.

-file("src/glint/internal/help.gleam", 258).
?DOC(false).
-spec to_spaced_indented_string(
    list(MBW),
    fun((MBW) -> {binary(), binary()}),
    integer(),
    config()
) -> binary().
to_spaced_indented_string(Data, F, Left_length, Config) ->
    Left_length@1 = Left_length + erlang:element(11, Config),
    {Content, Wrapped@1} = gleam@list:fold(
        Data,
        {[], false},
        fun(Acc, Data@1) ->
            {Left, Right} = F(Data@1),
            {Line, Wrapped} = format_content(Left, Right, Left_length@1, Config),
            {[Line | erlang:element(1, Acc)],
                Wrapped orelse erlang:element(2, Acc)}
        end
    ),
    Joiner = case Wrapped@1 of
        true ->
            <<"\n"/utf8>>;

        false ->
            <<""/utf8>>
    end,
    _pipe = Content,
    _pipe@1 = gleam@list:sort(_pipe, fun gleam@string:compare/2),
    gleam@string:join(_pipe@1, Joiner).

-file("src/glint/internal/help.gleam", 191).
?DOC(false).
-spec flags_help_to_string(list(flag()), config()) -> binary().
flags_help_to_string(Help, Config) ->
    gleam@bool:guard(
        Help =:= [],
        <<""/utf8>>,
        fun() ->
            Longest_flag_length = begin
                _pipe = Help,
                _pipe@1 = gleam@list:map(
                    _pipe,
                    fun(_capture) -> flag_help_to_string(_capture, Config) end
                ),
                _pipe@2 = glint@internal@utils:max_string_length(_pipe@1),
                gleam@int:max(_pipe@2, erlang:element(10, Config))
            end,
            Heading = case erlang:element(4, Config) of
                none ->
                    <<"FLAGS:"/utf8>>;

                {some, Pretty} ->
                    heading_style(<<"FLAGS:"/utf8>>, Pretty)
            end,
            Content = to_spaced_indented_string(
                [{flag,
                        {metadata,
                            <<"help"/utf8>>,
                            <<"Print help information"/utf8>>},
                        <<""/utf8>>} |
                    Help],
                fun(Help@1) ->
                    {flag_help_to_string(Help@1, Config),
                        erlang:element(3, erlang:element(2, Help@1))}
                end,
                Longest_flag_length,
                Config
            ),
            <<Heading/binary, Content/binary>>
        end
    ).

-file("src/glint/internal/help.gleam", 231).
?DOC(false).
-spec subcommands_help_to_string(list(metadata()), config()) -> binary().
subcommands_help_to_string(Help, Config) ->
    gleam@bool:guard(
        Help =:= [],
        <<""/utf8>>,
        fun() ->
            Longest_subcommand_length = begin
                _pipe = Help,
                _pipe@1 = gleam@list:map(
                    _pipe,
                    fun(H) -> erlang:element(2, H) end
                ),
                _pipe@2 = glint@internal@utils:max_string_length(_pipe@1),
                gleam@int:max(_pipe@2, erlang:element(10, Config))
            end,
            Heading = case erlang:element(5, Config) of
                none ->
                    <<"SUBCOMMANDS:"/utf8>>;

                {some, Pretty} ->
                    heading_style(<<"SUBCOMMANDS:"/utf8>>, Pretty)
            end,
            Content = to_spaced_indented_string(
                Help,
                fun(Help@1) ->
                    {erlang:element(2, Help@1), erlang:element(3, Help@1)}
                end,
                Longest_subcommand_length,
                Config
            ),
            <<Heading/binary, Content/binary>>
        end
    ).

-file("src/glint/internal/help.gleam", 142).
?DOC(false).
-spec command_help_to_usage_string(command(), config()) -> binary().
command_help_to_usage_string(Help, Config) ->
    App_name = case erlang:element(2, Config) of
        {some, Name} when erlang:element(6, Config) ->
            <<"gleam run -m "/utf8, Name/binary>>;

        {some, Name@1} ->
            Name@1;

        none ->
            <<"gleam run"/utf8>>
    end,
    Flags = flags_help_to_usage_string(Config, erlang:element(3, Help)),
    Subcommands@1 = case begin
        _pipe = gleam@list:map(
            erlang:element(4, Help),
            fun(Sc) -> erlang:element(2, Sc) end
        ),
        _pipe@1 = gleam@list:sort(_pipe, fun gleam@string:compare/2),
        gleam@string:join(_pipe@1, <<" | "/utf8>>)
    end of
        <<""/utf8>> ->
            <<""/utf8>>;

        Subcommands ->
            <<<<"( "/utf8, Subcommands/binary>>/binary, " )"/utf8>>
    end,
    Named_args = begin
        _pipe@2 = erlang:element(6, Help),
        _pipe@3 = gleam@list:map(
            _pipe@2,
            fun(S) -> <<<<"<"/utf8, S/binary>>/binary, ">"/utf8>> end
        ),
        gleam@string:join(_pipe@3, <<" "/utf8>>)
    end,
    Unnamed_args = begin
        _pipe@4 = gleam@option:map(
            erlang:element(5, Help),
            fun args_count_to_usage_string/1
        ),
        gleam@option:unwrap(_pipe@4, <<"[ ARGS ]"/utf8>>)
    end,
    Max_usage_width = erlang:element(9, Config) - erlang:element(8, Config),
    Content = begin
        _pipe@5 = [App_name,
            erlang:element(2, erlang:element(2, Help)),
            Subcommands@1,
            Named_args,
            Unnamed_args,
            Flags],
        _pipe@6 = gleam@list:filter(_pipe@5, fun(S@1) -> S@1 /= <<""/utf8>> end),
        _pipe@7 = gleam@string:join(_pipe@6, <<" "/utf8>>),
        _pipe@8 = glint@internal@utils:wordwrap(_pipe@7, Max_usage_width),
        gleam@string:join(
            _pipe@8,
            <<"\n"/utf8,
                (gleam@string:repeat(
                    <<" "/utf8>>,
                    erlang:element(8, Config) * 2
                ))/binary>>
        )
    end,
    <<<<<<(case erlang:element(3, Config) of
                    none ->
                        <<"USAGE:"/utf8>>;

                    {some, Pretty} ->
                        heading_style(<<"USAGE:"/utf8>>, Pretty)
                end)/binary, "\n"/utf8>>/binary, (gleam@string:repeat(
                <<" "/utf8>>,
                erlang:element(8, Config)
            ))/binary>>/binary, Content/binary>>.

-file("src/glint/internal/help.gleam", 84).
?DOC(false).
-spec command_help_to_string(command(), config()) -> binary().
command_help_to_string(Help, Config) ->
    Command = case erlang:element(2, erlang:element(2, Help)) of
        <<""/utf8>> ->
            <<""/utf8>>;

        S ->
            <<"Command: "/utf8, S/binary>>
    end,
    Command_description = begin
        _pipe = erlang:element(3, erlang:element(2, Help)),
        _pipe@1 = glint@internal@utils:wordwrap(
            _pipe,
            erlang:element(9, Config)
        ),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    _pipe@3 = [begin
            _pipe@2 = erlang:element(7, Config),
            gleam@option:unwrap(_pipe@2, <<""/utf8>>)
        end,
        Command,
        Command_description,
        command_help_to_usage_string(Help, Config),
        flags_help_to_string(erlang:element(3, Help), Config),
        subcommands_help_to_string(erlang:element(4, Help), Config)],
    _pipe@4 = gleam@list:filter(_pipe@3, fun(S@1) -> S@1 /= <<""/utf8>> end),
    gleam@string:join(_pipe@4, <<"\n\n"/utf8>>).
