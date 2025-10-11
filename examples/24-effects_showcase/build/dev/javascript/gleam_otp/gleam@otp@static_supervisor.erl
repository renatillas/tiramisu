-module(gleam@otp@static_supervisor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/otp/static_supervisor.gleam").
-export([new/1, restart_tolerance/3, auto_shutdown/2, add/2, start/1, supervised/1, init/1, start_child_callback/1]).
-export_type([supervisor/0, strategy/0, auto_shutdown/0, builder/0, erlang_start_flags/0, erlang_start_flag/1, erlang_child_spec/0, erlang_child_spec_property/1, timeout_/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " A supervisor where the number and types of the children are specified\n"
    " once, and the supervisor manages them using a configured restart strategy.\n"
    "\n"
    " For further detail see the Erlang documentation:\n"
    " <https://www.erlang.org/doc/apps/stdlib/supervisor.html>.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " import gleam/otp/actor\n"
    " import gleam/otp/static_supervisor.{type Supervisor} as supervisor\n"
    " import app/database_pool\n"
    " import app/http_server\n"
    " \n"
    " pub fn start_supervisor() -> actor.StartResult(Supervisor) {\n"
    "   supervisor.new(supervisor.OneForOne)\n"
    "   |> supervisor.add(database_pool.supervised())\n"
    "   |> supervisor.add(http_server.supervised())\n"
    "   |> supervisor.start\n"
    " }\n"
    " ```\n"
).

-opaque supervisor() :: {supervisor, gleam@erlang@process:pid_()}.

-type strategy() :: one_for_one | one_for_all | rest_for_one.

-type auto_shutdown() :: never | any_significant | all_significant.

-opaque builder() :: {builder,
        strategy(),
        integer(),
        integer(),
        auto_shutdown(),
        list(gleam@otp@supervision:child_specification(nil))}.

-type erlang_start_flags() :: any().

-type erlang_start_flag(FHY) :: {strategy, strategy()} |
    {intensity, integer()} |
    {period, integer()} |
    {auto_shutdown, auto_shutdown()} |
    {gleam_phantom, FHY}.

-type erlang_child_spec() :: any().

-type erlang_child_spec_property(FHZ) :: {id, integer()} |
    {start,
        {gleam@erlang@atom:atom_(),
            gleam@erlang@atom:atom_(),
            list(fun(() -> {ok, gleam@otp@actor:started(FHZ)} |
                {error, gleam@otp@actor:start_error()}))}} |
    {restart, gleam@otp@supervision:restart()} |
    {significant, boolean()} |
    {type, gleam@erlang@atom:atom_()} |
    {shutdown, timeout_()}.

-type timeout_() :: any().

-file("src/gleam/otp/static_supervisor.gleam", 110).
?DOC(" Create a new supervisor builder, ready for further configuration.\n").
-spec new(strategy()) -> builder().
new(Strategy) ->
    {builder, Strategy, 2, 5, never, []}.

-file("src/gleam/otp/static_supervisor.gleam", 131).
?DOC(
    " To prevent a supervisor from getting into an infinite loop of child\n"
    " process terminations and restarts, a maximum restart tolerance is\n"
    " defined using two integer values specified with keys intensity and\n"
    " period in the above map. Assuming the values MaxR for intensity and MaxT\n"
    " for period, then, if more than MaxR restarts occur within MaxT seconds,\n"
    " the supervisor terminates all child processes and then itself. The\n"
    " termination reason for the supervisor itself in that case will be\n"
    " shutdown. \n"
    "\n"
    " Intensity defaults to 2 and period defaults to 5.\n"
).
-spec restart_tolerance(builder(), integer(), integer()) -> builder().
restart_tolerance(Builder, Intensity, Period) ->
    {builder,
        erlang:element(2, Builder),
        Intensity,
        Period,
        erlang:element(5, Builder),
        erlang:element(6, Builder)}.

-file("src/gleam/otp/static_supervisor.gleam", 142).
?DOC(
    " A supervisor can be configured to automatically shut itself down with\n"
    " exit reason shutdown when significant children terminate.\n"
).
-spec auto_shutdown(builder(), auto_shutdown()) -> builder().
auto_shutdown(Builder, Value) ->
    {builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        Value,
        erlang:element(6, Builder)}.

-file("src/gleam/otp/static_supervisor.gleam", 202).
?DOC(" Add a child to the supervisor.\n").
-spec add(builder(), gleam@otp@supervision:child_specification(any())) -> builder().
add(Builder, Child) ->
    {builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        [gleam@otp@supervision:map_data(Child, fun(_) -> nil end) |
            erlang:element(6, Builder)]}.

-file("src/gleam/otp/static_supervisor.gleam", 209).
-spec convert_child(gleam@otp@supervision:child_specification(any()), integer()) -> erlang_child_spec().
convert_child(Child, Id) ->
    Mfa = {erlang:binary_to_atom(<<"gleam@otp@static_supervisor"/utf8>>),
        erlang:binary_to_atom(<<"start_child_callback"/utf8>>),
        [erlang:element(2, Child)]},
    {Type_, Shutdown} = case erlang:element(5, Child) of
        supervisor ->
            {erlang:binary_to_atom(<<"supervisor"/utf8>>),
                gleam_otp_external:make_timeout(-1)};

        {worker, Ms} ->
            {erlang:binary_to_atom(<<"worker"/utf8>>),
                gleam_otp_external:make_timeout(Ms)}
    end,
    maps:from_list(
        [{id, Id},
            {start, Mfa},
            {restart, erlang:element(3, Child)},
            {significant, erlang:element(4, Child)},
            {type, Type_},
            {shutdown, Shutdown}]
    ).

-file("src/gleam/otp/static_supervisor.gleam", 159).
?DOC(
    " Start a new supervisor process with the configuration and children\n"
    " specified within the builder.\n"
    "\n"
    " Typically you would use the `supervised` function to add your supervisor to\n"
    " a supervision tree instead of using this function directly.\n"
    "\n"
    " The supervisor will be linked to the parent process that calls this\n"
    " function.\n"
    "\n"
    " If any child fails to start the supevisor first terminates all already\n"
    " started child processes with reason shutdown and then terminate itself and\n"
    " returns an error.\n"
).
-spec start(builder()) -> {ok, gleam@otp@actor:started(supervisor())} |
    {error, gleam@otp@actor:start_error()}.
start(Builder) ->
    Flags = maps:from_list(
        [{strategy, erlang:element(2, Builder)},
            {intensity, erlang:element(3, Builder)},
            {period, erlang:element(4, Builder)},
            {auto_shutdown, erlang:element(5, Builder)}]
    ),
    Module = erlang:binary_to_atom(<<"gleam@otp@static_supervisor"/utf8>>),
    Children = begin
        _pipe = erlang:element(6, Builder),
        _pipe@1 = lists:reverse(_pipe),
        gleam@list:index_map(_pipe@1, fun convert_child/2)
    end,
    case supervisor:start_link(Module, {Flags, Children}) of
        {ok, Pid} ->
            {ok, {started, Pid, {supervisor, Pid}}};

        {error, Error} ->
            {error, gleam_otp_external:convert_erlang_start_error(Error)}
    end.

-file("src/gleam/otp/static_supervisor.gleam", 188).
?DOC(
    " Create a `ChildSpecification` that adds this supervisor as the child of\n"
    " another, making it fault tolerant and part of the application's supervision\n"
    " tree. You should prefer to starting unsupervised supervisors with the\n"
    " `start` function.\n"
    "\n"
    " If any child fails to start the supevisor first terminates all already\n"
    " started child processes with reason shutdown and then terminate itself and\n"
    " returns an error.\n"
).
-spec supervised(builder()) -> gleam@otp@supervision:child_specification(supervisor()).
supervised(Builder) ->
    gleam@otp@supervision:supervisor(fun() -> start(Builder) end).

-file("src/gleam/otp/static_supervisor.gleam", 271).
?DOC(false).
-spec init(gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
    {error, any()}.
init(Start_data) ->
    {ok, Start_data}.

-file("src/gleam/otp/static_supervisor.gleam", 277).
?DOC(false).
-spec start_child_callback(
    fun(() -> {ok, gleam@otp@actor:started(any())} |
        {error, gleam@otp@actor:start_error()})
) -> {ok, gleam@erlang@process:pid_()} | {error, gleam@otp@actor:start_error()}.
start_child_callback(Start) ->
    case Start() of
        {ok, Started} ->
            {ok, erlang:element(2, Started)};

        {error, Error} ->
            {error, Error}
    end.
