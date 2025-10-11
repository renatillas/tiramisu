-module(gleam@otp@supervision).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/otp/supervision.gleam").
-export([worker/1, supervisor/1, significant/2, timeout/2, restart/2, map_data/2]).
-export_type([restart/0, child_type/0, child_specification/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type restart() :: permanent | transient | temporary.

-type child_type() :: {worker, integer()} | supervisor.

-type child_specification(ETW) :: {child_specification,
        fun(() -> {ok, gleam@otp@actor:started(ETW)} |
            {error, gleam@otp@actor:start_error()}),
        restart(),
        boolean(),
        child_type()}.

-file("src/gleam/otp/supervision.gleam", 61).
?DOC(
    " A regular child process.\n"
    "\n"
    " You should use this unless your process is also a supervisor.\n"
    "\n"
    " The default shutdown timeout is 5000ms. This can be changed with the\n"
    " `timeout` function.\n"
).
-spec worker(
    fun(() -> {ok, gleam@otp@actor:started(ETX)} |
        {error, gleam@otp@actor:start_error()})
) -> child_specification(ETX).
worker(Start) ->
    {child_specification, Start, permanent, false, {worker, 5000}}.

-file("src/gleam/otp/supervision.gleam", 76).
?DOC(
    " A special child that is a supervisor itself.\n"
    "\n"
    " Supervisor children have an unlimited shutdown time, there is no timeout.\n"
).
-spec supervisor(
    fun(() -> {ok, gleam@otp@actor:started(EUC)} |
        {error, gleam@otp@actor:start_error()})
) -> child_specification(EUC).
supervisor(Start) ->
    {child_specification, Start, permanent, false, supervisor}.

-file("src/gleam/otp/supervision.gleam", 96).
?DOC(
    " This defines if a child is considered significant for automatic\n"
    " self-shutdown of the supervisor.\n"
    "\n"
    " You most likely do not want to consider any children significant.\n"
    "\n"
    " This will be ignored if the supervisor auto shutdown is set to `Never`,\n"
    " which is the default.\n"
    "\n"
    " The default value for significance is `False`.\n"
).
-spec significant(child_specification(EUH), boolean()) -> child_specification(EUH).
significant(Child, Significant) ->
    {child_specification,
        erlang:element(2, Child),
        erlang:element(3, Child),
        Significant,
        erlang:element(5, Child)}.

-file("src/gleam/otp/supervision.gleam", 110).
?DOC(
    " This defines the amount of milliseconds a child has to shut down before\n"
    " being brutal killed by the supervisor.\n"
    "\n"
    " If not set the default for a child is 5000ms.\n"
    "\n"
    " This will be ignored if the child is a supervisor itself.\n"
).
-spec timeout(child_specification(EUK), integer()) -> child_specification(EUK).
timeout(Child, Ms) ->
    case erlang:element(5, Child) of
        {worker, _} ->
            {child_specification,
                erlang:element(2, Child),
                erlang:element(3, Child),
                erlang:element(4, Child),
                {worker, Ms}};

        _ ->
            Child
    end.

-file("src/gleam/otp/supervision.gleam", 124).
?DOC(
    " When the child is to be restarted. See the `Restart` documentation for\n"
    " more.\n"
    "\n"
    " The default value for restart is `Permanent`.\n"
).
-spec restart(child_specification(EUN), restart()) -> child_specification(EUN).
restart(Child, Restart) ->
    {child_specification,
        erlang:element(2, Child),
        Restart,
        erlang:element(4, Child),
        erlang:element(5, Child)}.

-file("src/gleam/otp/supervision.gleam", 133).
?DOC(" Transform the data of the started child process.\n").
-spec map_data(child_specification(EUQ), fun((EUQ) -> EUS)) -> child_specification(EUS).
map_data(Child, Transform) ->
    {child_specification, fun() -> case (erlang:element(2, Child))() of
                {ok, Started} ->
                    {ok,
                        {started,
                            erlang:element(2, Started),
                            Transform(erlang:element(3, Started))}};

                {error, E} ->
                    {error, E}
            end end, erlang:element(3, Child), erlang:element(4, Child), erlang:element(
            5,
            Child
        )}.
