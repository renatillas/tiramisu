-module(glint).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([pretty_help/2, with_name/2, without_exit/1, as_module/1, with_indent_width/2, with_max_output_width/2, with_min_first_column_width/2, with_column_gap/2, global_help/2, map_command/2, command_help/2, unnamed_args/2, named_arg/2, default_pretty_help/0, flag_constraint/2, flag_help/2, flag_default/2, flag/2, command/1, get_flag/2, int_flag/1, ints_flag/1, bool_flag/1, string_flag/1, strings_flag/1, float_flag/1, floats_flag/1, path_help/3, add/3, group_flag/3, new/0, execute/2, run_and_handle/3, run/2]).
-export_type([config/0, pretty_help/0, glint/1, args_count/0, command/1, internal_command/1, named_args/0, command_node/1, out/1, value/0, flag/1, flag_internals/1, flag_entry/0, flags/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type config() :: {config,
        gleam@option:option(pretty_help()),
        gleam@option:option(binary()),
        boolean(),
        gleam@option:option(binary()),
        boolean(),
        integer(),
        integer(),
        integer(),
        integer()}.

-type pretty_help() :: {pretty_help,
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour()}.

-opaque glint(MFS) :: {glint, config(), command_node(MFS)}.

-type args_count() :: {eq_args, integer()} | {min_args, integer()}.

-opaque command(MFT) :: {command,
        fun((named_args(), list(binary()), flags()) -> MFT),
        flags(),
        binary(),
        gleam@option:option(args_count()),
        list(binary())}.

-type internal_command(MFU) :: {internal_command,
        fun((named_args(), list(binary()), flags()) -> MFU),
        flags(),
        gleam@option:option(args_count()),
        list(binary())}.

-opaque named_args() :: {named_args, gleam@dict:dict(binary(), binary())}.

-type command_node(MFV) :: {command_node,
        gleam@option:option(internal_command(MFV)),
        gleam@dict:dict(binary(), command_node(MFV)),
        flags(),
        binary()}.

-type out(MFW) :: {out, MFW} | {help, binary()}.

-type value() :: {b, flag_internals(boolean())} |
    {i, flag_internals(integer())} |
    {li, flag_internals(list(integer()))} |
    {f, flag_internals(float())} |
    {lf, flag_internals(list(float()))} |
    {s, flag_internals(binary())} |
    {ls, flag_internals(list(binary()))}.

-opaque flag(MFX) :: {flag,
        binary(),
        binary(),
        fun((binary()) -> {ok, MFX} | {error, snag:snag()}),
        fun((flag_internals(MFX)) -> value()),
        fun((flags(), binary()) -> {ok, MFX} | {error, snag:snag()}),
        gleam@option:option(MFX)}.

-type flag_internals(MFY) :: {flag_internals,
        gleam@option:option(MFY),
        fun((binary()) -> {ok, MFY} | {error, snag:snag()})}.

-type flag_entry() :: {flag_entry, value(), binary()}.

-opaque flags() :: {flags, gleam@dict:dict(binary(), flag_entry())}.

-file("src/glint.gleam", 63).
?DOC(
    " Enable custom colours for help text headers.\n"
    "\n"
    " For a pre-made style, pass in [`glint.default_pretty_help`](#default_pretty_help)\n"
).
-spec pretty_help(glint(MGF), pretty_help()) -> glint(MGF).
pretty_help(Glint, Pretty) ->
    _record = Glint,
    {glint,
        begin
            _record@1 = erlang:element(2, Glint),
            {config,
                {some, Pretty},
                erlang:element(3, _record@1),
                erlang:element(4, _record@1),
                erlang:element(5, _record@1),
                erlang:element(6, _record@1),
                erlang:element(7, _record@1),
                erlang:element(8, _record@1),
                erlang:element(9, _record@1),
                erlang:element(10, _record@1)}
        end,
        erlang:element(3, _record)}.

-file("src/glint.gleam", 71).
?DOC(
    " Give the current glint application a name.\n"
    "\n"
    " The name specified here is used when generating help text for the current glint instance.\n"
).
-spec with_name(glint(MGI), binary()) -> glint(MGI).
with_name(Glint, Name) ->
    _record = Glint,
    {glint,
        begin
            _record@1 = erlang:element(2, Glint),
            {config,
                erlang:element(2, _record@1),
                {some, Name},
                erlang:element(4, _record@1),
                erlang:element(5, _record@1),
                erlang:element(6, _record@1),
                erlang:element(7, _record@1),
                erlang:element(8, _record@1),
                erlang:element(9, _record@1),
                erlang:element(10, _record@1)}
        end,
        erlang:element(3, _record)}.

-file("src/glint.gleam", 79).
?DOC(
    " By default, Glint exits with error status 1 when an error is encountered (eg. invalid flag or command not found)\n"
    "\n"
    " Calling this function disables that feature.\n"
).
-spec without_exit(glint(MGL)) -> glint(MGL).
without_exit(Glint) ->
    _record = Glint,
    {glint,
        begin
            _record@1 = erlang:element(2, Glint),
            {config,
                erlang:element(2, _record@1),
                erlang:element(3, _record@1),
                erlang:element(4, _record@1),
                erlang:element(5, _record@1),
                false,
                erlang:element(7, _record@1),
                erlang:element(8, _record@1),
                erlang:element(9, _record@1),
                erlang:element(10, _record@1)}
        end,
        erlang:element(3, _record)}.

-file("src/glint.gleam", 87).
?DOC(
    " Adjust the generated help text to reflect that the current glint app should be run as a gleam module.\n"
    "\n"
    " Use in conjunction with [`glint.with_name`](#with_name) to get usage text output like `gleam run -m <name>`\n"
).
-spec as_module(glint(MGO)) -> glint(MGO).
as_module(Glint) ->
    _record = Glint,
    {glint,
        begin
            _record@1 = erlang:element(2, Glint),
            {config,
                erlang:element(2, _record@1),
                erlang:element(3, _record@1),
                true,
                erlang:element(5, _record@1),
                erlang:element(6, _record@1),
                erlang:element(7, _record@1),
                erlang:element(8, _record@1),
                erlang:element(9, _record@1),
                erlang:element(10, _record@1)}
        end,
        erlang:element(3, _record)}.

-file("src/glint.gleam", 96).
?DOC(
    " Adjusts the indent width used to indent content under the usage, flags,\n"
    " and subcommands headings in the help output.\n"
    "\n"
    " Default: 4.\n"
).
-spec with_indent_width(glint(MGR), integer()) -> glint(MGR).
with_indent_width(Glint, Indent_width) ->
    _record = Glint,
    {glint,
        begin
            _record@1 = erlang:element(2, Glint),
            {config,
                erlang:element(2, _record@1),
                erlang:element(3, _record@1),
                erlang:element(4, _record@1),
                erlang:element(5, _record@1),
                erlang:element(6, _record@1),
                Indent_width,
                erlang:element(8, _record@1),
                erlang:element(9, _record@1),
                erlang:element(10, _record@1)}
        end,
        erlang:element(3, _record)}.

-file("src/glint.gleam", 104).
?DOC(
    " Adjusts the output width at which help text will wrap onto a new line.\n"
    "\n"
    " Default: 80.\n"
).
-spec with_max_output_width(glint(MGU), integer()) -> glint(MGU).
with_max_output_width(Glint, Max_output_width) ->
    _record = Glint,
    {glint,
        begin
            _record@1 = erlang:element(2, Glint),
            {config,
                erlang:element(2, _record@1),
                erlang:element(3, _record@1),
                erlang:element(4, _record@1),
                erlang:element(5, _record@1),
                erlang:element(6, _record@1),
                erlang:element(7, _record@1),
                Max_output_width,
                erlang:element(9, _record@1),
                erlang:element(10, _record@1)}
        end,
        erlang:element(3, _record)}.

-file("src/glint.gleam", 112).
?DOC(
    " Adjusts the minimum width of the column containing flag and command names in the help output.\n"
    "\n"
    " Default: 20.\n"
).
-spec with_min_first_column_width(glint(MGX), integer()) -> glint(MGX).
with_min_first_column_width(Glint, Min_first_column_width) ->
    _record = Glint,
    {glint,
        begin
            _record@1 = erlang:element(2, Glint),
            {config,
                erlang:element(2, _record@1),
                erlang:element(3, _record@1),
                erlang:element(4, _record@1),
                erlang:element(5, _record@1),
                erlang:element(6, _record@1),
                erlang:element(7, _record@1),
                erlang:element(8, _record@1),
                Min_first_column_width,
                erlang:element(10, _record@1)}
        end,
        erlang:element(3, _record)}.

-file("src/glint.gleam", 123).
?DOC(
    " Adjusts the size of the gap between columns in the help output.\n"
    "\n"
    " Default: 2.\n"
).
-spec with_column_gap(glint(MHA), integer()) -> glint(MHA).
with_column_gap(Glint, Column_gap) ->
    _record = Glint,
    {glint,
        begin
            _record@1 = erlang:element(2, Glint),
            {config,
                erlang:element(2, _record@1),
                erlang:element(3, _record@1),
                erlang:element(4, _record@1),
                erlang:element(5, _record@1),
                erlang:element(6, _record@1),
                erlang:element(7, _record@1),
                erlang:element(8, _record@1),
                erlang:element(9, _record@1),
                Column_gap}
        end,
        erlang:element(3, _record)}.

-file("src/glint.gleam", 240).
?DOC(
    " Set help text for the application as a whole.\n"
    "\n"
    " Help text set with this function wil be printed at the top of the help text for every command.\n"
    " To set help text specifically for the root command please use [`glint.command_help`](#command_help) or [`glint.path_help([],...)`](#path_help)\n"
    "\n"
    " This function allows for user-supplied newlines in long text strings. Individual newline characters are instead converted to spaces.\n"
    " This is useful for developers to format their help text in a more readable way in the source code.\n"
    "\n"
    " For formatted text to appear on a new line, use 2 newline characters.\n"
    " For formatted text to appear in a new paragraph, use 3 newline characters.\n"
).
-spec global_help(glint(MHJ), binary()) -> glint(MHJ).
global_help(Glint, Description) ->
    _record = Glint,
    {glint,
        begin
            _record@1 = erlang:element(2, Glint),
            {config,
                erlang:element(2, _record@1),
                erlang:element(3, _record@1),
                erlang:element(4, _record@1),
                {some, Description},
                erlang:element(6, _record@1),
                erlang:element(7, _record@1),
                erlang:element(8, _record@1),
                erlang:element(9, _record@1),
                erlang:element(10, _record@1)}
        end,
        erlang:element(3, _record)}.

-file("src/glint.gleam", 289).
?DOC(" Trim each path element and remove any resulting empty strings.\n").
-spec sanitize_path(list(binary())) -> list(binary()).
sanitize_path(Path) ->
    _pipe = Path,
    _pipe@1 = gleam@list:map(_pipe, fun gleam@string:trim/1),
    gleam@list:filter(_pipe@1, fun(S) -> S /= <<""/utf8>> end).

-file("src/glint.gleam", 322).
?DOC(
    " Map the output of a [`Command`](#Command)\n"
    "\n"
    " This function can be useful when you are handling user-defined commands or commands from other packages and need to make sure the return type matches your own commands.\n"
).
-spec map_command(command(MHY), fun((MHY) -> MIA)) -> command(MIA).
map_command(Command, Fun) ->
    {command,
        fun(Named_args, Args, Flags) ->
            Fun((erlang:element(2, Command))(Named_args, Args, Flags))
        end,
        erlang:element(3, Command),
        erlang:element(4, Command),
        erlang:element(5, Command),
        erlang:element(6, Command)}.

-file("src/glint.gleam", 340).
?DOC(
    " Attach a helptext description to a [`Command(a)`](#Command)\n"
    "\n"
    " This function allows for user-supplied newlines in long text strings. Individual newline characters are instead converted to spaces.\n"
    " This is useful for developers to format their help text in a more readable way in the source code.\n"
    "\n"
    " For formatted text to appear on a new line, use 2 newline characters.\n"
    " For formatted text to appear in a new paragraph, use 3 newline characters.\n"
).
-spec command_help(binary(), fun(() -> command(MIC))) -> command(MIC).
command_help(Desc, F) ->
    _record = F(),
    {command,
        erlang:element(2, _record),
        erlang:element(3, _record),
        Desc,
        erlang:element(5, _record),
        erlang:element(6, _record)}.

-file("src/glint.gleam", 359).
?DOC(
    " Specify a specific number of unnamed args that a given command expects.\n"
    "\n"
    " Use in conjunction with [`glint.ArgsCount`](#ArgsCount) to specify either a minimum or a specific number of args.\n"
    "\n"
    " ### Example:\n"
    "\n"
    " ```gleam\n"
    " ...\n"
    " // for a command that accets only 1 unnamed argument:\n"
    " use <- glint.unnamed_args(glint.EqArgs(1))\n"
    " ...\n"
    " named, unnamed, flags <- glint.command()\n"
    " let assert Ok([arg]) = unnamed\n"
    " ```\n"
).
-spec unnamed_args(args_count(), fun(() -> command(MIF))) -> command(MIF).
unnamed_args(Count, F) ->
    _record = F(),
    {command,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        {some, Count},
        erlang:element(6, _record)}.

-file("src/glint.gleam", 387).
?DOC(
    " Add a list of named arguments to a [`Command(a)`](#Command). The value can be retrieved from the command's [`NamedArgs`](#NamedArgs)\n"
    "\n"
    " These named arguments will be matched with the first N arguments passed to the command.\n"
    "\n"
    "\n"
    " **IMPORTANT**:\n"
    "\n"
    " - Matched named arguments will **not** be present in the commmand's unnamed args list\n"
    "\n"
    " - All named arguments must match for a command to succeed.\n"
    "\n"
    " ### Example:\n"
    "\n"
    " ```gleam\n"
    " ...\n"
    " use first_name <- glint.named_arg(\"first name\")\n"
    " ...\n"
    " use named, unnamed, flags <- glint.command()\n"
    " let first = first_name(named)\n"
    " ```\n"
).
-spec named_arg(
    binary(),
    fun((fun((named_args()) -> binary())) -> command(MII))
) -> command(MII).
named_arg(Name, F) ->
    Cmd = begin
        F(
            fun(Named_args) ->
                _assert_subject = gleam_stdlib:map_get(
                    erlang:element(2, Named_args),
                    Name
                ),
                {ok, Arg} = case _assert_subject of
                    {ok, _} -> _assert_subject;
                    _assert_fail ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                    value => _assert_fail,
                                    module => <<"glint"/utf8>>,
                                    function => <<"named_arg"/utf8>>,
                                    line => 394})
                end,
                Arg
            end
        )
    end,
    _record = Cmd,
    {command,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        [Name | erlang:element(6, Cmd)]}.

-file("src/glint.gleam", 519).
-spec args_compare(args_count(), integer()) -> {ok, nil} | {error, snag:snag()}.
args_compare(Expected, Actual) ->
    gleam@result:map_error(case Expected of
            {eq_args, Expected@1} when Actual =:= Expected@1 ->
                {ok, nil};

            {min_args, Expected@2} when Actual >= Expected@2 ->
                {ok, nil};

            {eq_args, Expected@3} ->
                {error, erlang:integer_to_binary(Expected@3)};

            {min_args, Expected@4} ->
                {error,
                    <<"at least "/utf8,
                        (erlang:integer_to_binary(Expected@4))/binary>>}
        end, fun(Err) ->
            snag:new(
                <<<<<<"expected: "/utf8, Err/binary>>/binary,
                        " argument(s), provided: "/utf8>>/binary,
                    (erlang:integer_to_binary(Actual))/binary>>
            )
        end).

-file("src/glint.gleam", 642).
?DOC(
    " Default colouring for help text.\n"
    "\n"
    " mint (r: 182, g: 255, b: 234) colour for usage\n"
    "\n"
    " pink (r: 255, g: 175, b: 243) colour for flags\n"
    "\n"
    " buttercup (r: 252, g: 226, b: 174) colour for subcommands\n"
).
-spec default_pretty_help() -> pretty_help().
default_pretty_help() ->
    _assert_subject = gleam_community@colour:from_rgb255(182, 255, 234),
    {ok, Usage_colour} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 643})
    end,
    _assert_subject@1 = gleam_community@colour:from_rgb255(255, 175, 243),
    {ok, Flags_colour} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@1,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 644})
    end,
    _assert_subject@2 = gleam_community@colour:from_rgb255(252, 226, 174),
    {ok, Subcommands_colour} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@2,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 645})
    end,
    {pretty_help, Usage_colour, Flags_colour, Subcommands_colour}.

-file("src/glint.gleam", 717).
?DOC(" generate the string representation for the type of a flag\n").
-spec flag_type_info(flag_entry()) -> binary().
flag_type_info(Flag) ->
    case erlang:element(2, Flag) of
        {i, _} ->
            <<"INT"/utf8>>;

        {b, _} ->
            <<"BOOL"/utf8>>;

        {f, _} ->
            <<"FLOAT"/utf8>>;

        {lf, _} ->
            <<"FLOAT_LIST"/utf8>>;

        {li, _} ->
            <<"INT_LIST"/utf8>>;

        {ls, _} ->
            <<"STRING_LIST"/utf8>>;

        {s, _} ->
            <<"STRING"/utf8>>
    end.

-file("src/glint.gleam", 744).
?DOC(" build the help representation for a list of subcommands\n").
-spec build_subcommands_help(gleam@dict:dict(binary(), command_node(any()))) -> list(glint@internal@help:metadata()).
build_subcommands_help(Subcommands) ->
    gleam@dict:fold(
        Subcommands,
        [],
        fun(Acc, Name, Node) ->
            [{metadata, Name, erlang:element(5, Node)} | Acc]
        end
    ).

-file("src/glint.gleam", 893).
?DOC(" initialize custom builders using a Value constructor and a parsing function\n").
-spec new_builder(
    binary(),
    fun((flag_internals(MKW)) -> value()),
    fun((flags(), binary()) -> {ok, MKW} | {error, snag:snag()}),
    fun((binary()) -> {ok, MKW} | {error, snag:snag()})
) -> flag(MKW).
new_builder(Name, Valuer, Getter, P) ->
    {flag, Name, <<""/utf8>>, P, Valuer, Getter, none}.

-file("src/glint.gleam", 911).
?DOC(" convert a (Flag(a) into its corresponding FlagEntry representation\n").
-spec build_flag(flag(any())) -> flag_entry().
build_flag(Fb) ->
    {flag_entry,
        (erlang:element(5, Fb))(
            {flag_internals, erlang:element(7, Fb), erlang:element(4, Fb)}
        ),
        erlang:element(3, Fb)}.

-file("src/glint.gleam", 964).
-spec attempt(
    {ok, MLO} | {error, MLP},
    fun((MLO) -> {ok, any()} | {error, MLP})
) -> {ok, MLO} | {error, MLP}.
attempt(Val, F) ->
    gleam@result:'try'(Val, fun(A) -> gleam@result:replace(F(A), A) end).

-file("src/glint.gleam", 957).
?DOC(
    " attach a Constraint(a) to a Parser(a,Snag)\n"
    " this function should not be used directly unless\n"
).
-spec wrap_with_constraint(
    fun((binary()) -> {ok, MLI} | {error, snag:snag()}),
    fun((MLI) -> {ok, MLI} | {error, snag:snag()})
) -> fun((binary()) -> {ok, MLI} | {error, snag:snag()}).
wrap_with_constraint(P, Constraint) ->
    fun(Input) -> attempt(P(Input), Constraint) end.

-file("src/glint.gleam", 948).
?DOC(
    " Attach a constraint to a flag.\n"
    "\n"
    " As constraints are just functions, this works well as both part of a pipeline or with `use`.\n"
    "\n"
    "\n"
    " ### Pipe:\n"
    "\n"
    " ```gleam\n"
    " glint.int_flag(\"my_flag\")\n"
    " |> glint.flag_help(\"An awesome flag\")\n"
    " |> glint.flag_constraint(fn(i) {\n"
    "   case i < 0 {\n"
    "     True -> snag.error(\"must be greater than 0\")\n"
    "     False -> Ok(i)\n"
    "   }})\n"
    " ```\n"
    "\n"
    " ### Use:\n"
    "\n"
    " ```gleam\n"
    " use i <- glint.flag_constraint(\n"
    "   glint.int_flag(\"my_flag\")\n"
    "   |> glint.flag_help(\"An awesome flag\")\n"
    " )\n"
    " case i < 0 {\n"
    "   True -> snag.error(\"must be greater than 0\")\n"
    "   False -> Ok(i)\n"
    " }\n"
    " ```\n"
).
-spec flag_constraint(flag(MLE), fun((MLE) -> {ok, MLE} | {error, snag:snag()})) -> flag(MLE).
flag_constraint(Builder, Constraint) ->
    _record = Builder,
    {flag,
        erlang:element(2, _record),
        erlang:element(3, _record),
        wrap_with_constraint(erlang:element(4, Builder), Constraint),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record)}.

-file("src/glint.gleam", 993).
?DOC(
    " Attach a help text description to a flag.\n"
    "\n"
    " This function allows for user-supplied newlines in long text strings. Individual newline characters are instead converted to spaces.\n"
    " This is useful for developers to format their help text in a more readable way in the source code.\n"
    "\n"
    " For formatted text to appear on a new line, use 2 newline characters.\n"
    " For formatted text to appear in a new paragraph, use 3 newline characters.\n"
    "\n"
    " ### Example:\n"
    "\n"
    " ```gleam\n"
    " glint.int_flag(\"awesome_flag\")\n"
    " |> glint.flag_help(\"Some great text!\")\n"
    " ```\n"
).
-spec flag_help(flag(MLX), binary()) -> flag(MLX).
flag_help(Flag, Description) ->
    _record = Flag,
    {flag,
        erlang:element(2, _record),
        Description,
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record)}.

-file("src/glint.gleam", 1006).
?DOC(
    " Set the default value for a flag.\n"
    "\n"
    " ### Example:\n"
    "\n"
    " ```gleam\n"
    " glint.int_flag(\"awesome_flag\")\n"
    " |> glint.flag_default(1)\n"
    " ```\n"
).
-spec flag_default(flag(MMA), MMA) -> flag(MMA).
flag_default(Flag, Default) ->
    _record = Flag,
    {flag,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        {some, Default}}.

-file("src/glint.gleam", 1016).
-spec insert(flags(), binary(), flag_entry()) -> flags().
insert(Flags, Name, Flag) ->
    {flags, gleam@dict:insert(erlang:element(2, Flags), Name, Flag)}.

-file("src/glint.gleam", 421).
?DOC(
    " Add a [`Flag(a)`](#Flag) to a [`Command(a)`](#Command)\n"
    "\n"
    " The provided callback is provided a function to fetch the current flag fvalue from the command input [`Flags`](#Flags).\n"
    "\n"
    " This function is most ergonomic as part of a `use` chain when building commands.\n"
    "\n"
    " ### Example:\n"
    "\n"
    " ```gleam\n"
    " ...\n"
    " use repeat <- glint.flag(\n"
    "   glint.int_flag(\"repeat\")\n"
    "   |> glint.flag_default(1)\n"
    "   |> glint.flag_help(\"Repeat the message n-times\")\n"
    " )\n"
    " ...\n"
    " use named, unnamed, flags <- glint.command()\n"
    " let repeat_value = repeat(flags)\n"
    " ```\n"
).
-spec flag(
    flag(MIL),
    fun((fun((flags()) -> {ok, MIL} | {error, snag:snag()})) -> command(MIO))
) -> command(MIO).
flag(Flag, F) ->
    Cmd = F(
        fun(_capture) ->
            (erlang:element(6, Flag))(_capture, erlang:element(2, Flag))
        end
    ),
    _record = Cmd,
    {command,
        erlang:element(2, _record),
        insert(
            erlang:element(3, Cmd),
            erlang:element(2, Flag),
            build_flag(Flag)
        ),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record)}.

-file("src/glint.gleam", 1020).
-spec merge(flags(), flags()) -> flags().
merge(A, B) ->
    {flags, maps:merge(erlang:element(2, A), erlang:element(2, B))}.

-file("src/glint.gleam", 1024).
-spec fold(flags(), MMD, fun((MMD, binary(), flag_entry()) -> MMD)) -> MMD.
fold(Flags, Acc, F) ->
    gleam@dict:fold(erlang:element(2, Flags), Acc, F).

-file("src/glint.gleam", 731).
?DOC(" build the help representation for a list of flags\n").
-spec build_flags_help(flags()) -> list(glint@internal@help:flag()).
build_flags_help(Flags) ->
    fold(
        Flags,
        [],
        fun(Acc, Name, Flag) ->
            [{flag,
                    {metadata, Name, erlang:element(3, Flag)},
                    flag_type_info(Flag)} |
                Acc]
        end
    ).

-file("src/glint.gleam", 687).
?DOC(" build the help representation for a subtree of commands\n").
-spec build_command_help(binary(), command_node(any())) -> glint@internal@help:command().
build_command_help(Name, Node) ->
    {Description, Flags, Unnamed_args, Named_args} = begin
        _pipe = erlang:element(2, Node),
        _pipe@1 = gleam@option:map(
            _pipe,
            fun(Cmd) ->
                {erlang:element(5, Node),
                    build_flags_help(
                        merge(erlang:element(4, Node), erlang:element(3, Cmd))
                    ),
                    erlang:element(4, Cmd),
                    erlang:element(5, Cmd)}
            end
        ),
        gleam@option:unwrap(_pipe@1, {erlang:element(5, Node), [], none, []})
    end,
    {command,
        {metadata, Name, Description},
        Flags,
        build_subcommands_help(erlang:element(3, Node)),
        begin
            gleam@option:map(Unnamed_args, fun(Args) -> case Args of
                        {eq_args, N} ->
                            {eq_args, N};

                        {min_args, N@1} ->
                            {min_args, N@1}
                    end end)
        end,
        Named_args}.

-file("src/glint.gleam", 1028).
-spec new_flags() -> flags().
new_flags() ->
    {flags, maps:new()}.

-file("src/glint.gleam", 278).
?DOC(" Helper for initializing empty commands\n").
-spec empty_command() -> command_node(any()).
empty_command() ->
    {command_node, none, maps:new(), new_flags(), <<""/utf8>>}.

-file("src/glint.gleam", 308).
?DOC(
    " Create a [Command(a)](#Command) from a [Runner(a)](#Runner).\n"
    "\n"
    " ### Example:\n"
    "\n"
    " ```gleam\n"
    " use <- glint.command_help(\"Some awesome help text\")\n"
    " use named_arg <- glint.named_arg(\"some_arg\")\n"
    " use <- glint.unnamed_args(glint.EqArgs(0))\n"
    " ...\n"
    " use named, unnamed, flags <- glint.command()\n"
    " let my_arg = named_arg(named)\n"
    " ...\n"
    " ```\n"
).
-spec command(fun((named_args(), list(binary()), flags()) -> MHV)) -> command(MHV).
command(Runner) ->
    {command, Runner, new_flags(), <<""/utf8>>, none, []}.

-file("src/glint.gleam", 1079).
-spec access_type_error(binary()) -> {ok, any()} | {error, snag:snag()}.
access_type_error(Flag_type) ->
    snag:error(<<"cannot access flag as "/utf8, Flag_type/binary>>).

-file("src/glint.gleam", 1083).
-spec flag_not_provided_error() -> {ok, any()} | {error, snag:snag()}.
flag_not_provided_error() ->
    snag:error(<<"no value provided"/utf8>>).

-file("src/glint.gleam", 1087).
-spec construct_value(
    binary(),
    flag_internals(MMK),
    fun((flag_internals(MMK)) -> value())
) -> {ok, value()} | {error, snag:snag()}.
construct_value(Input, Internal, Constructor) ->
    gleam@result:map(
        (erlang:element(3, Internal))(Input),
        fun(Val) ->
            Constructor(
                begin
                    _record = Internal,
                    {flag_internals, {some, Val}, erlang:element(3, _record)}
                end
            )
        end
    ).

-file("src/glint.gleam", 1098).
?DOC(" Computes the new flag value given the input and the expected flag type\n").
-spec compute_flag(binary(), value()) -> {ok, value()} | {error, snag:snag()}.
compute_flag(Input, Current) ->
    _pipe = Input,
    _pipe@1 = case Current of
        {i, Internal} ->
            fun(_capture) ->
                construct_value(
                    _capture,
                    Internal,
                    fun(Field@0) -> {i, Field@0} end
                )
            end;

        {li, Internal@1} ->
            fun(_capture@1) ->
                construct_value(
                    _capture@1,
                    Internal@1,
                    fun(Field@0) -> {li, Field@0} end
                )
            end;

        {f, Internal@2} ->
            fun(_capture@2) ->
                construct_value(
                    _capture@2,
                    Internal@2,
                    fun(Field@0) -> {f, Field@0} end
                )
            end;

        {lf, Internal@3} ->
            fun(_capture@3) ->
                construct_value(
                    _capture@3,
                    Internal@3,
                    fun(Field@0) -> {lf, Field@0} end
                )
            end;

        {s, Internal@4} ->
            fun(_capture@4) ->
                construct_value(
                    _capture@4,
                    Internal@4,
                    fun(Field@0) -> {s, Field@0} end
                )
            end;

        {ls, Internal@5} ->
            fun(_capture@5) ->
                construct_value(
                    _capture@5,
                    Internal@5,
                    fun(Field@0) -> {ls, Field@0} end
                )
            end;

        {b, Internal@6} ->
            fun(_capture@6) ->
                construct_value(
                    _capture@6,
                    Internal@6,
                    fun(Field@0) -> {b, Field@0} end
                )
            end
    end(_pipe),
    snag:context(_pipe@1, <<"failed to compute value for flag"/utf8>>).

-file("src/glint.gleam", 1113).
-spec layer_invalid_flag(snag:snag(), binary()) -> snag:snag().
layer_invalid_flag(Err, Flag) ->
    snag:layer(Err, <<<<"invalid flag '"/utf8, Flag/binary>>/binary, "'"/utf8>>).

-file("src/glint.gleam", 1117).
-spec no_value_flag_err(binary()) -> snag:snag().
no_value_flag_err(Flag_input) ->
    _pipe = (<<<<"flag '"/utf8, Flag_input/binary>>/binary,
        "' has no assigned value"/utf8>>),
    _pipe@1 = snag:new(_pipe),
    layer_invalid_flag(_pipe@1, Flag_input).

-file("src/glint.gleam", 1123).
-spec undefined_flag_err(binary()) -> snag:snag().
undefined_flag_err(Key) ->
    _pipe = <<"flag provided but not defined"/utf8>>,
    _pipe@1 = snag:new(_pipe),
    layer_invalid_flag(_pipe@1, Key).

-file("src/glint.gleam", 1129).
-spec cannot_parse(binary(), binary()) -> snag:snag().
cannot_parse(Value, Kind) ->
    _pipe = (<<<<<<"cannot parse value '"/utf8, Value/binary>>/binary,
            "' as "/utf8>>/binary,
        Kind/binary>>),
    snag:new(_pipe).

-file("src/glint.gleam", 1138).
?DOC(" Access the contents for the associated flag\n").
-spec get(flags(), binary()) -> {ok, flag_entry()} | {error, snag:snag()}.
get(Flags, Name) ->
    _pipe = gleam_stdlib:map_get(erlang:element(2, Flags), Name),
    gleam@result:replace_error(_pipe, undefined_flag_err(Name)).

-file("src/glint.gleam", 1045).
-spec update_flag_value(flags(), {binary(), binary()}) -> {ok, flags()} |
    {error, snag:snag()}.
update_flag_value(Flags, Data) ->
    {Key, Input} = Data,
    gleam@result:'try'(
        get(Flags, Key),
        fun(Contents) ->
            gleam@result:map(
                begin
                    _pipe = compute_flag(Input, erlang:element(2, Contents)),
                    gleam@result:map_error(
                        _pipe,
                        fun(_capture) -> layer_invalid_flag(_capture, Key) end
                    )
                end,
                fun(Value) ->
                    insert(
                        Flags,
                        Key,
                        begin
                            _record = Contents,
                            {flag_entry, Value, erlang:element(3, _record)}
                        end
                    )
                end
            )
        end
    ).

-file("src/glint.gleam", 1058).
-spec attempt_toggle_flag(flags(), binary()) -> {ok, flags()} |
    {error, snag:snag()}.
attempt_toggle_flag(Flags, Key) ->
    gleam@result:'try'(
        get(Flags, Key),
        fun(Contents) -> case erlang:element(2, Contents) of
                {b, {flag_internals, none, _} = Internal} ->
                    _pipe = begin
                        _record = Internal,
                        {flag_internals,
                            {some, true},
                            erlang:element(3, _record)}
                    end,
                    _pipe@1 = {b, _pipe},
                    _pipe@2 = (fun(Val) -> _record@1 = Contents,
                        {flag_entry, Val, erlang:element(3, _record@1)} end)(
                        _pipe@1
                    ),
                    _pipe@3 = gleam@dict:insert(
                        erlang:element(2, Flags),
                        Key,
                        _pipe@2
                    ),
                    _pipe@4 = {flags, _pipe@3},
                    {ok, _pipe@4};

                {b, {flag_internals, {some, Val@1}, _} = Internal@1} ->
                    _pipe@5 = begin
                        _record@2 = Internal@1,
                        {flag_internals,
                            {some, not Val@1},
                            erlang:element(3, _record@2)}
                    end,
                    _pipe@6 = {b, _pipe@5},
                    _pipe@7 = (fun(Val@2) -> _record@3 = Contents,
                        {flag_entry, Val@2, erlang:element(3, _record@3)} end)(
                        _pipe@6
                    ),
                    _pipe@8 = gleam@dict:insert(
                        erlang:element(2, Flags),
                        Key,
                        _pipe@7
                    ),
                    _pipe@9 = {flags, _pipe@8},
                    {ok, _pipe@9};

                _ ->
                    {error, no_value_flag_err(Key)}
            end end
    ).

-file("src/glint.gleam", 1143).
-spec get_value(
    flags(),
    binary(),
    fun((flag_entry()) -> {ok, MMQ} | {error, snag:snag()})
) -> {ok, MMQ} | {error, snag:snag()}.
get_value(Flags, Key, Kind) ->
    _pipe = get(Flags, Key),
    _pipe@1 = gleam@result:'try'(_pipe, Kind),
    snag:context(
        _pipe@1,
        <<<<"failed to retrieve value for flag '"/utf8, Key/binary>>/binary,
            "'"/utf8>>
    ).

-file("src/glint.gleam", 1158).
?DOC(
    " Gets the value for the associated flag.\n"
    "\n"
    " This function should only ever be used when fetching flags set at the group level.\n"
    " For local flags please use the getter functions provided when calling [`glint.flag`](#flag).\n"
).
-spec get_flag(flags(), flag(MMT)) -> {ok, MMT} | {error, snag:snag()}.
get_flag(Flags, Flag) ->
    (erlang:element(6, Flag))(Flags, erlang:element(2, Flag)).

-file("src/glint.gleam", 1164).
?DOC(" Gets the current value for the associated int flag\n").
-spec get_int_flag(flags(), binary()) -> {ok, integer()} | {error, snag:snag()}.
get_int_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {i, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {i, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"int"/utf8>>)
            end end).

-file("src/glint.gleam", 829).
?DOC(" Initialise an int flag.\n").
-spec int_flag(binary()) -> flag(integer()).
int_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {i, Field@0} end,
        fun get_int_flag/2,
        fun(Input) -> _pipe = Input,
            _pipe@1 = gleam_stdlib:parse_int(_pipe),
            gleam@result:replace_error(
                _pipe@1,
                cannot_parse(Input, <<"int"/utf8>>)
            ) end
    ).

-file("src/glint.gleam", 1175).
?DOC(" Gets the current value for the associated ints flag\n").
-spec get_ints_flag(flags(), binary()) -> {ok, list(integer())} |
    {error, snag:snag()}.
get_ints_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {li, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {li, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"int list"/utf8>>)
            end end).

-file("src/glint.gleam", 838).
?DOC(" Initialise an int list flag.\n").
-spec ints_flag(binary()) -> flag(list(integer())).
ints_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {li, Field@0} end,
        fun get_ints_flag/2,
        fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@string:split(_pipe, <<","/utf8>>),
            _pipe@2 = gleam@list:try_map(_pipe@1, fun gleam_stdlib:parse_int/1),
            gleam@result:replace_error(
                _pipe@2,
                cannot_parse(Input, <<"int list"/utf8>>)
            ) end
    ).

-file("src/glint.gleam", 1186).
?DOC(" Gets the current value for the associated bool flag\n").
-spec get_bool_flag(flags(), binary()) -> {ok, boolean()} | {error, snag:snag()}.
get_bool_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {b, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {b, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"bool"/utf8>>)
            end end).

-file("src/glint.gleam", 882).
?DOC(" Initialise a boolean flag.\n").
-spec bool_flag(binary()) -> flag(boolean()).
bool_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {b, Field@0} end,
        fun get_bool_flag/2,
        fun(Input) -> case string:lowercase(Input) of
                <<"true"/utf8>> ->
                    {ok, true};

                <<"t"/utf8>> ->
                    {ok, true};

                <<"false"/utf8>> ->
                    {ok, false};

                <<"f"/utf8>> ->
                    {ok, false};

                _ ->
                    {error, cannot_parse(Input, <<"bool"/utf8>>)}
            end end
    ).

-file("src/glint.gleam", 1197).
?DOC(" Gets the current value for the associated string flag\n").
-spec get_string_flag(flags(), binary()) -> {ok, binary()} |
    {error, snag:snag()}.
get_string_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {s, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {s, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"string"/utf8>>)
            end end).

-file("src/glint.gleam", 867).
?DOC(" Initialise a string flag.\n").
-spec string_flag(binary()) -> flag(binary()).
string_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {s, Field@0} end,
        fun get_string_flag/2,
        fun(S) -> {ok, S} end
    ).

-file("src/glint.gleam", 1208).
?DOC(" Gets the current value for the associated strings flag\n").
-spec get_strings_flag(flags(), binary()) -> {ok, list(binary())} |
    {error, snag:snag()}.
get_strings_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {ls, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {ls, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"string list"/utf8>>)
            end end).

-file("src/glint.gleam", 873).
?DOC(" Intitialise a string list flag.\n").
-spec strings_flag(binary()) -> flag(list(binary())).
strings_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {ls, Field@0} end,
        fun get_strings_flag/2,
        fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@string:split(_pipe, <<","/utf8>>),
            {ok, _pipe@1} end
    ).

-file("src/glint.gleam", 1222).
?DOC(" Gets the current value for the associated float flag\n").
-spec get_floats(flags(), binary()) -> {ok, float()} | {error, snag:snag()}.
get_floats(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {f, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {f, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"float"/utf8>>)
            end end).

-file("src/glint.gleam", 848).
?DOC("Initialise a float flag.\n").
-spec float_flag(binary()) -> flag(float()).
float_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {f, Field@0} end,
        fun get_floats/2,
        fun(Input) -> _pipe = Input,
            _pipe@1 = gleam_stdlib:parse_float(_pipe),
            gleam@result:replace_error(
                _pipe@1,
                cannot_parse(Input, <<"float"/utf8>>)
            ) end
    ).

-file("src/glint.gleam", 1233).
?DOC(" Gets the current value for the associated float flag\n").
-spec get_floats_flag(flags(), binary()) -> {ok, list(float())} |
    {error, snag:snag()}.
get_floats_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {lf, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {lf, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"float list"/utf8>>)
            end end).

-file("src/glint.gleam", 857).
?DOC(" Initialise a float list flag.\n").
-spec floats_flag(binary()) -> flag(list(float())).
floats_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {lf, Field@0} end,
        fun get_floats_flag/2,
        fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@string:split(_pipe, <<","/utf8>>),
            _pipe@2 = gleam@list:try_map(
                _pipe@1,
                fun gleam_stdlib:parse_float/1
            ),
            gleam@result:replace_error(
                _pipe@2,
                cannot_parse(Input, <<"float list"/utf8>>)
            ) end
    ).

-file("src/glint.gleam", 1259).
-spec do_update_at(
    command_node(MNM),
    list(binary()),
    fun((command_node(MNM)) -> command_node(MNM))
) -> command_node(MNM).
do_update_at(Node, Path, F) ->
    case Path of
        [] ->
            F(Node);

        [Next | Rest] ->
            _record = Node,
            {command_node,
                erlang:element(2, _record),
                begin
                    gleam@dict:upsert(
                        erlang:element(3, Node),
                        Next,
                        fun(Found) -> _pipe = Found,
                            _pipe@1 = gleam@option:lazy_unwrap(
                                _pipe,
                                fun empty_command/0
                            ),
                            do_update_at(_pipe@1, Rest, F) end
                    )
                end,
                erlang:element(4, _record),
                erlang:element(5, _record)}
    end.

-file("src/glint.gleam", 1248).
-spec update_at(
    glint(MNG),
    list(binary()),
    fun((command_node(MNG)) -> command_node(MNG))
) -> glint(MNG).
update_at(Glint, Path, F) ->
    _record = Glint,
    {glint,
        erlang:element(2, _record),
        do_update_at(erlang:element(3, Glint), sanitize_path(Path), F)}.

-file("src/glint.gleam", 220).
?DOC(
    " Set the help text for a specific command path.\n"
    "\n"
    " This function is intended to allow users to set the help text of commands that might not be directly instantiated,\n"
    " such as commands with no business logic associated to them but that have subcommands.\n"
    "\n"
    " Using this function should almost never be necessary, in most cases you should use [`glint.command_help`](#command_help) insstead.\n"
).
-spec path_help(glint(MHF), list(binary()), binary()) -> glint(MHF).
path_help(Glint, Path, Description) ->
    update_at(Glint, Path, fun(Node) -> _record = Node,
            {command_node,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                Description} end).

-file("src/glint.gleam", 258).
?DOC(
    " Adds a new command to be run at the specified path.\n"
    "\n"
    " If the path is `[]`, the root command is set with the provided function and\n"
    " flags.\n"
    "\n"
    " Note: all command paths are sanitized by stripping whitespace and removing any empty string elements.\n"
    "\n"
    " ```gleam\n"
    " glint.new()\n"
    " |> glint.add(at: [], do: root_command())\n"
    " |> glint.add(at: [\"subcommand\"], do: subcommand())\n"
    " ...\n"
    " ```\n"
).
-spec add(glint(MHM), list(binary()), command(MHM)) -> glint(MHM).
add(Glint, Path, Command) ->
    update_at(Glint, Path, fun(Node) -> _record = Node,
            {command_node,
                {some,
                    {internal_command,
                        erlang:element(2, Command),
                        erlang:element(3, Command),
                        erlang:element(5, Command),
                        erlang:element(6, Command)}},
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(4, Command)} end).

-file("src/glint.gleam", 432).
?DOC(
    " Add a flag for a group of commands.\n"
    " The provided flags will be available to all commands at or beyond the provided path\n"
).
-spec group_flag(glint(MIR), list(binary()), flag(any())) -> glint(MIR).
group_flag(Glint, Path, Flag) ->
    update_at(Glint, Path, fun(Node) -> _record = Node,
            {command_node,
                erlang:element(2, _record),
                erlang:element(3, _record),
                insert(
                    erlang:element(4, Node),
                    erlang:element(2, Flag),
                    build_flag(Flag)
                ),
                erlang:element(5, _record)} end).

-file("src/glint.gleam", 210).
?DOC(" Create a new glint instance.\n").
-spec new() -> glint(any()).
new() ->
    {glint,
        {config, none, none, false, none, true, 4, 80, 20, 2},
        empty_command()}.

-file("src/glint.gleam", 668).
-spec build_help_config(config()) -> glint@internal@help:config().
build_help_config(Config) ->
    {config,
        erlang:element(3, Config),
        gleam@option:map(
            erlang:element(2, Config),
            fun(P) -> erlang:element(2, P) end
        ),
        gleam@option:map(
            erlang:element(2, Config),
            fun(P@1) -> erlang:element(3, P@1) end
        ),
        gleam@option:map(
            erlang:element(2, Config),
            fun(P@2) -> erlang:element(4, P@2) end
        ),
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(7, Config),
        erlang:element(8, Config),
        erlang:element(9, Config),
        erlang:element(10, Config),
        <<"--"/utf8>>,
        <<"="/utf8>>}.

-file("src/glint.gleam", 657).
?DOC(" generate the help text for a command\n").
-spec cmd_help(list(binary()), command_node(any()), config()) -> binary().
cmd_help(Path, Cmd, Config) ->
    _pipe = Path,
    _pipe@1 = lists:reverse(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<" "/utf8>>),
    _pipe@3 = build_command_help(_pipe@2, Cmd),
    glint@internal@help:command_help_to_string(
        _pipe@3,
        build_help_config(Config)
    ).

-file("src/glint.gleam", 1036).
?DOC(
    " Updates a flag value, ensuring that the new value can satisfy the required type.\n"
    " Assumes that all flag inputs passed in start with --\n"
    " This function is only intended to be used from glint.execute_root\n"
).
-spec update_flags(flags(), binary()) -> {ok, flags()} | {error, snag:snag()}.
update_flags(Flags, Flag_input) ->
    Flag_input@1 = gleam@string:drop_start(
        Flag_input,
        string:length(<<"--"/utf8>>)
    ),
    case gleam@string:split_once(Flag_input@1, <<"="/utf8>>) of
        {ok, Data} ->
            update_flag_value(Flags, Data);

        {error, _} ->
            attempt_toggle_flag(Flags, Flag_input@1)
    end.

-file("src/glint.gleam", 533).
?DOC(" Executes the current root command.\n").
-spec execute_root(
    list(binary()),
    config(),
    command_node(MJN),
    list(binary()),
    list(binary())
) -> {ok, MJN} | {error, binary()}.
execute_root(Path, Config, Cmd, Args, Flag_inputs) ->
    _pipe@5 = begin
        gleam@result:'try'(
            gleam@option:to_result(
                erlang:element(2, Cmd),
                snag:new(<<"command not found"/utf8>>)
            ),
            fun(Contents) ->
                gleam@result:'try'(
                    gleam@list:try_fold(
                        Flag_inputs,
                        merge(
                            erlang:element(4, Cmd),
                            erlang:element(3, Contents)
                        ),
                        fun update_flags/2
                    ),
                    fun(New_flags) ->
                        gleam@result:'try'(
                            begin
                                Named = gleam@list:zip(
                                    erlang:element(5, Contents),
                                    Args
                                ),
                                case erlang:length(Named) =:= erlang:length(
                                    erlang:element(5, Contents)
                                ) of
                                    true ->
                                        {ok, maps:from_list(Named)};

                                    false ->
                                        snag:error(
                                            <<"unmatched named arguments: "/utf8,
                                                (begin
                                                    _pipe = erlang:element(
                                                        5,
                                                        Contents
                                                    ),
                                                    _pipe@1 = gleam@list:drop(
                                                        _pipe,
                                                        erlang:length(Named)
                                                    ),
                                                    _pipe@2 = gleam@list:map(
                                                        _pipe@1,
                                                        fun(S) ->
                                                            <<<<"'"/utf8,
                                                                    S/binary>>/binary,
                                                                "'"/utf8>>
                                                        end
                                                    ),
                                                    gleam@string:join(
                                                        _pipe@2,
                                                        <<", "/utf8>>
                                                    )
                                                end)/binary>>
                                        )
                                end
                            end,
                            fun(Named_args) ->
                                Args@1 = gleam@list:drop(
                                    Args,
                                    maps:size(Named_args)
                                ),
                                gleam@result:map(
                                    case erlang:element(4, Contents) of
                                        {some, Count} ->
                                            _pipe@3 = Count,
                                            _pipe@4 = args_compare(
                                                _pipe@3,
                                                erlang:length(Args@1)
                                            ),
                                            snag:context(
                                                _pipe@4,
                                                <<"invalid number of arguments provided"/utf8>>
                                            );

                                        none ->
                                            {ok, nil}
                                    end,
                                    fun(_) ->
                                        (erlang:element(2, Contents))(
                                            {named_args, Named_args},
                                            Args@1,
                                            New_flags
                                        )
                                    end
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    gleam@result:map_error(
        _pipe@5,
        fun(Err) ->
            <<<<(begin
                        _pipe@6 = Err,
                        _pipe@7 = snag:layer(
                            _pipe@6,
                            <<"failed to run command"/utf8>>
                        ),
                        snag:pretty_print(_pipe@7)
                    end)/binary,
                    "\nSee the following help text, available via the '--help' flag.\n\n"/utf8>>/binary,
                (cmd_help(Path, Cmd, Config))/binary>>
        end
    ).

-file("src/glint.gleam", 477).
?DOC(" Find which command to execute and run it with computed flags and args\n").
-spec do_execute(
    command_node(MJD),
    config(),
    list(binary()),
    list(binary()),
    boolean(),
    list(binary())
) -> {ok, out(MJD)} | {error, binary()}.
do_execute(Cmd, Config, Args, Flags, Help, Command_path) ->
    case Args of
        [] when Help ->
            {ok, {help, cmd_help(Command_path, Cmd, Config)}};

        [] ->
            _pipe = execute_root(Command_path, Config, Cmd, [], Flags),
            gleam@result:map(_pipe, fun(Field@0) -> {out, Field@0} end);

        [Arg | Rest] ->
            case gleam_stdlib:map_get(erlang:element(3, Cmd), Arg) of
                {ok, Sub_command} ->
                    _pipe@1 = begin
                        _record = Sub_command,
                        {command_node,
                            erlang:element(2, _record),
                            erlang:element(3, _record),
                            merge(
                                erlang:element(4, Cmd),
                                erlang:element(4, Sub_command)
                            ),
                            erlang:element(5, _record)}
                    end,
                    do_execute(
                        _pipe@1,
                        Config,
                        Rest,
                        Flags,
                        Help,
                        [Arg | Command_path]
                    );

                _ when Help ->
                    {ok, {help, cmd_help(Command_path, Cmd, Config)}};

                _ ->
                    _pipe@2 = execute_root(
                        Command_path,
                        Config,
                        Cmd,
                        Args,
                        Flags
                    ),
                    gleam@result:map(
                        _pipe@2,
                        fun(Field@0) -> {out, Field@0} end
                    )
            end
    end.

-file("src/glint.gleam", 456).
?DOC(false).
-spec execute(glint(MIX), list(binary())) -> {ok, out(MIX)} | {error, binary()}.
execute(Glint, Args) ->
    Help_flag = <<"--"/utf8,
        (erlang:element(
            2,
            erlang:element(
                2,
                {flag,
                    {metadata,
                        <<"help"/utf8>>,
                        <<"Print help information"/utf8>>},
                    <<""/utf8>>}
            )
        ))/binary>>,
    {Help, Args@3} = case gleam@list:partition(
        Args,
        fun(S) -> S =:= Help_flag end
    ) of
        {[], Args@1} ->
            {false, Args@1};

        {_, Args@2} ->
            {true, Args@2}
    end,
    {Flags, Args@4} = gleam@list:partition(
        Args@3,
        fun(_capture) ->
            gleam_stdlib:string_starts_with(_capture, <<"--"/utf8>>)
        end
    ),
    do_execute(
        erlang:element(3, Glint),
        erlang:element(2, Glint),
        Args@4,
        Flags,
        Help,
        []
    ).

-file("src/glint.gleam", 613).
?DOC(
    " Run a glint app with a custom handler for command output.\n"
    " This function prints any errors enountered or the help text if requested.\n"
    "\n"
    " IMPORTANT: This function exits with code 1 if an error was encountered.\n"
    " If this behaviour is not desired please disable it with [`glint.without_exit`](#without_exit)\n"
).
-spec run_and_handle(glint(MJW), list(binary()), fun((MJW) -> any())) -> nil.
run_and_handle(Glint, Args, Handle) ->
    case execute(Glint, Args) of
        {error, S} ->
            gleam_stdlib:println(S),
            case erlang:element(6, erlang:element(2, Glint)) of
                true ->
                    erlang:halt(1);

                false ->
                    nil
            end;

        {ok, {help, S@1}} ->
            gleam_stdlib:println(S@1);

        {ok, {out, Out}} ->
            Handle(Out),
            nil
    end.

-file("src/glint.gleam", 603).
?DOC(
    " Run a glint app and print any errors enountered, or the help text if requested.\n"
    " This function ignores any value returned by the command that was run.\n"
    " If you would like to do handle the command output please see the [`glint.run_and_handle`](#run_and_handle) function.\n"
    "\n"
    " IMPORTANT: This function exits with code 1 if an error was encountered.\n"
    " If this behaviour is not desired please disable it with [`glint.without_exit`](#without_exit)\n"
).
-spec run(glint(any()), list(binary())) -> nil.
run(Glint, Args) ->
    run_and_handle(Glint, Args, fun(_) -> nil end).
