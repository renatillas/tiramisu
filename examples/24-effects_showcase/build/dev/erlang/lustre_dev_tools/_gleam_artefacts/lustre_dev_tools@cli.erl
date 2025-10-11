-module(lustre_dev_tools@cli).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/cli.gleam").
-export([log/2, success/2, string/5, bool/5, string_list/5, int/5]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre_dev_tools/cli.gleam", 16).
?DOC(false).
-spec log(binary(), boolean()) -> nil.
log(Message, Quiet) ->
    case Quiet of
        true ->
            nil;

        false ->
            gleam_stdlib:println(
                gleam_community@ansi:grey(
                    <<<<"   "/utf8, Message/binary>>/binary, "..."/utf8>>
                )
            )
    end.

-file("src/lustre_dev_tools/cli.gleam", 23).
?DOC(false).
-spec success(binary(), boolean()) -> nil.
success(Message, Quiet) ->
    case Quiet of
        true ->
            nil;

        false ->
            gleam_stdlib:println(
                gleam_community@ansi:green(<<"✅ "/utf8, Message/binary>>)
            )
    end.

-file("src/lustre_dev_tools/cli.gleam", 52).
?DOC(false).
-spec string(
    binary(),
    list(binary()),
    lustre_dev_tools@project:project(),
    binary(),
    fun((fun((glint:flags()) -> {ok, binary()} |
        {error, lustre_dev_tools@error:error()})) -> glint:command(AIKS))
) -> glint:command(AIKS).
string(Name, Path, Project, Description, Continue) ->
    flag(
        Name,
        Path,
        Project,
        fun glint:string_flag/1,
        fun tom:get_string/2,
        Description,
        Continue
    ).

-file("src/lustre_dev_tools/cli.gleam", 108).
?DOC(false).
-spec flag(
    binary(),
    list(binary()),
    lustre_dev_tools@project:project(),
    fun((binary()) -> glint:flag(AILJ)),
    fun((gleam@dict:dict(binary(), tom:toml()), list(binary())) -> {ok, AILJ} |
        {error, tom:get_error()}),
    binary(),
    fun((fun((glint:flags()) -> {ok, AILJ} |
        {error, lustre_dev_tools@error:error()})) -> glint:command(AILS))
) -> glint:command(AILS).
flag(Name, Path, Project, Read_flag, Read_toml, Description, Continue) ->
    case {Name, Path} of
        {<<""/utf8>>, []} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Invalid flag config. Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lustre_dev_tools/cli"/utf8>>,
                    function => <<"flag"/utf8>>,
                    line => 119});

        {<<""/utf8>>, _} ->
            Continue(
                fun(_) -> case Read_toml(erlang:element(3, Project), Path) of
                        {ok, Value} ->
                            {ok, Value};

                        {error, _} ->
                            {error, {missing_required_flag, Path}}
                    end end
            );

        {_, []} ->
            glint:flag(
                begin
                    _pipe = Read_flag(Name),
                    glint:flag_help(
                        _pipe,
                        begin
                            _pipe@1 = Description,
                            gleam@string:trim(_pipe@1)
                        end
                    )
                end,
                fun(From_flags) ->
                    Continue(fun(Flags) -> case From_flags(Flags) of
                                {ok, Value@1} ->
                                    {ok, Value@1};

                                {error, _} ->
                                    {error,
                                        {missing_required_flag,
                                            [<<"--"/utf8, Name/binary>>]}}
                            end end)
                end
            );

        {_, _} ->
            glint:flag(
                begin
                    _pipe@2 = Read_flag(Name),
                    glint:flag_help(
                        _pipe@2,
                        begin
                            _pipe@3 = Description,
                            gleam@string:trim(_pipe@3)
                        end
                    )
                end,
                fun(From_flags@1) ->
                    Continue(fun(Flags@1) -> case From_flags@1(Flags@1) of
                                {ok, Value@2} ->
                                    {ok, Value@2};

                                {error, _} ->
                                    case Read_toml(
                                        erlang:element(3, Project),
                                        Path
                                    ) of
                                        {ok, Value@3} ->
                                            {ok, Value@3};

                                        {error, _} ->
                                            {error,
                                                {missing_required_flag, Path}}
                                    end
                            end end)
                end
            )
    end.

-file("src/lustre_dev_tools/cli.gleam", 32).
?DOC(false).
-spec bool(
    binary(),
    list(binary()),
    lustre_dev_tools@project:project(),
    binary(),
    fun((fun((glint:flags()) -> {ok, boolean()} |
        {error, lustre_dev_tools@error:error()})) -> glint:command(AIKM))
) -> glint:command(AIKM).
bool(Name, Path, Project, Description, Continue) ->
    flag(
        Name,
        Path,
        Project,
        fun glint:bool_flag/1,
        fun tom:get_bool/2,
        Description,
        Continue
    ).

-file("src/lustre_dev_tools/cli.gleam", 70).
?DOC(false).
-spec string_list(
    binary(),
    list(binary()),
    lustre_dev_tools@project:project(),
    binary(),
    fun((fun((glint:flags()) -> {ok, list(binary())} |
        {error, lustre_dev_tools@error:error()})) -> glint:command(AIKZ))
) -> glint:command(AIKZ).
string_list(Name, Path, Project, Description, Continue) ->
    Get_strings = fun(Toml, Path@1) -> _pipe = tom:get_array(Toml, Path@1),
        gleam@result:'try'(
            _pipe,
            fun(_capture) ->
                gleam@list:try_map(_capture, fun tom:as_string/1)
            end
        ) end,
    flag(
        Name,
        Path,
        Project,
        fun glint:strings_flag/1,
        Get_strings,
        Description,
        Continue
    ).

-file("src/lustre_dev_tools/cli.gleam", 96).
?DOC(false).
-spec int(
    binary(),
    list(binary()),
    lustre_dev_tools@project:project(),
    binary(),
    fun((fun((glint:flags()) -> {ok, integer()} |
        {error, lustre_dev_tools@error:error()})) -> glint:command(AILF))
) -> glint:command(AILF).
int(Name, Path, Project, Description, Continue) ->
    flag(
        Name,
        Path,
        Project,
        fun glint:int_flag/1,
        fun tom:get_int/2,
        Description,
        Continue
    ).
