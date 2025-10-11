-module(gleam@otp@factory_supervisor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/otp/factory_supervisor.gleam").
-export([get_by_name/1, named/2, restart_tolerance/3, timeout/2, restart_strategy/2, start/1, supervised/1, start_child/2, init/1, start_child_callback/2, worker_child/1, supervisor_child/1]).
-export_type([supervisor/2, message/2, builder/2, erlang_start_flags/0, erlang_supervisor_name/2, strategy/0, erlang_start_flag/1, erlang_child_spec/0, erlang_child_spec_property/2, timeout_/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " A supervisor where child processes are started dynamically from a\n"
    " pre-specified template, so new processes can be created as needed\n"
    " while the program is running.\n"
    "\n"
    " When the supervisor is shut down it shuts down all its children\n"
    " concurrently and in no specified order.\n"
    "\n"
    " For further detail see the Erlang documentation, particularly the parts\n"
    " about the `simple_one_for_one` restart strategy, which is the Erlang\n"
    " equivilent of the factory supervisor.\n"
    " <https://www.erlang.org/doc/apps/stdlib/supervisor.html>.\n"
    "\n"
    " ## Usage\n"
    "\n"
    " Add the factory supervisor to your supervision tree using the `supervised`\n"
    " function and a name created at the start of the program. The `new`\n"
    " function takes a \"template function\", which is a function that takes one\n"
    " argument and starts a linked child process.\n"
    "\n"
    " You most likely want to give the factory supervisor a name, and to pass\n"
    " that name to any other processes that will want to cause new child\n"
    " processes to be started under the factory supervisor. In this example a\n"
    " web server is used.\n"
    "\n"
    " ```gleam\n"
    " import gleam/erlang/process.{type Name}\n"
    " import gleam/otp/actor.{type StartResult}\n"
    " import gleam/otp/factory_supervisor as factory\n"
    " import gleam/otp/static_supervisor as supervisor\n"
    " import my_app\n"
    " \n"
    " /// This function starts the application's supervision tree.\n"
    " ///\n"
    " /// It takes a record as an argument that \n"
    " ///\n"
    " pub fn start_supervision_tree(reporters_name: Name(_)) -> StartResult(_) {\n"
    "   // Define a named factory supervisor that can create new child processes\n"
    "   // using the `my_app.start_reporter_actor` function, which is defined\n"
    "   // elsewhere in the program.\n"
    "   let reporter_factory_supervisor =\n"
    "     factory.worker_child(my_app.start_reporter_actor)\n"
    "     |> factory.named(reporters_name)\n"
    "     |> factory.supervised\n"
    " \n"
    "   // This web server process takes the name, so it can contact the factory\n"
    "   // supervisor to command it to start new processes as needed.\n"
    "   let web_server = my_app.supervised_web_server(reporters_name)\n"
    " \n"
    "   // Create the top-level static supervisor with the supervisor and web\n"
    "   // server as its children\n"
    "   supervisor.new(supervisor.RestForOne)\n"
    "   |> supervisor.add(reporter_factory_supervisor)\n"
    "   |> supervisor.add(web_server)\n"
    "   |> supervisor.start\n"
    " }\n"
    " ```\n"
    "\n"
    " Any process with the name of the factory supervisor can use the\n"
    " `get_by_name` function to get a reference to the supervisor, and then use\n"
    " the `start_child` function to have it start new child processes.\n"
    "\n"
    " Remember! Each process name created with `process.new_name` is unique.\n"
    " Two names created by calling the function twice are different names, even\n"
    " if the same string is given as an argument. You must create the name value\n"
    " at the start of your program and then pass it down into application code\n"
    " and library code that uses names.\n"
    "\n"
    " ```gleam\n"
    " import gleam/http/request.{type Request}\n"
    " import gleam/http/response.{type Response}\n"
    " import gleam/otp/factory_supervisor\n"
    " import my_app\n"
    " \n"
    " /// In our example this function is called each time a HTTP request is \n"
    " /// received by the web server.\n"
    " pub fn handle_request(req: Request(_), reporters: Name(_)) -> Response(_) {\n"
    "   // Get a reference to the supervisor using the name\n"
    "   let supervisor = factory_supervisor.get_by_name(reporters)\n"
    " \n"
    "   // Start a new child process under the supervisor, passing the request path \n"
    "   // to use as the argument for the child-starting template function.\n"
    "   let start_result = factory_supervisor.start_child(supervisor, request.path)\n"
    " \n"
    "   // A response is sent to the HTTP client.\n"
    "   // The child starting template function returns a result, with the error case\n"
    "   // being used when children fail to start. Because of this the `start_child`\n"
    "   // function also returns a result, so it must be handled too.\n"
    "   case start_result {\n"
    "     Ok(_) -> response.new(200)\n"
    "     Error(_) -> response.new(500)\n"
    "   }\n"
    " }\n"
    " ```\n"
).

-opaque supervisor(EWK, EWL) :: {supervisor, gleam@erlang@process:pid_()} |
    {named_supervisor, gleam@erlang@process:name(message(EWK, EWL))}.

-type message(EWM, EWN) :: any() | {gleam_phantom, EWM, EWN}.

-opaque builder(EWO, EWP) :: {builder,
        gleam@otp@supervision:child_type(),
        fun((EWO) -> {ok, gleam@otp@actor:started(EWP)} |
            {error, gleam@otp@actor:start_error()}),
        gleam@otp@supervision:restart(),
        integer(),
        integer(),
        gleam@option:option(gleam@erlang@process:name(message(EWO, EWP)))}.

-type erlang_start_flags() :: any().

-type erlang_supervisor_name(EWQ, EWR) :: {local,
        gleam@erlang@process:name(message(EWQ, EWR))}.

-type strategy() :: simple_one_for_one.

-type erlang_start_flag(EWS) :: {strategy, strategy()} |
    {intensity, integer()} |
    {period, integer()} |
    {gleam_phantom, EWS}.

-type erlang_child_spec() :: any().

-type erlang_child_spec_property(EWT, EWU) :: {id, integer()} |
    {start,
        {gleam@erlang@atom:atom_(),
            gleam@erlang@atom:atom_(),
            list(fun((EWT) -> {ok, gleam@otp@actor:started(EWU)} |
                {error, gleam@otp@actor:start_error()}))}} |
    {restart, gleam@otp@supervision:restart()} |
    {type, gleam@erlang@atom:atom_()} |
    {shutdown, timeout_()}.

-type timeout_() :: any().

-file("src/gleam/otp/factory_supervisor.gleam", 137).
?DOC(
    " Get a reference to a supervisor using its registered name.\n"
    "\n"
    " If no supervisor has been started using this name then functions\n"
    " using this reference will fail.\n"
    "\n"
    " # Panics\n"
    "\n"
    " Functions using the `Supervisor` reference returned by this function\n"
    " will panic if there is no factory supervisor registered with the name\n"
    " when they are called. Always make sure your supervisors are themselves\n"
    " supervised.\n"
).
-spec get_by_name(gleam@erlang@process:name(message(EWV, EWW))) -> supervisor(EWV, EWW).
get_by_name(Name) ->
    {named_supervisor, Name}.

-file("src/gleam/otp/factory_supervisor.gleam", 206).
?DOC(
    " Provide a name for the supervisor to be registered with when started,\n"
    " enabling it be more easily contacted by other processes. This is useful for\n"
    " enabling processes that can take over from an older one that has exited due\n"
    " to a failure.\n"
    "\n"
    " If the name is already registered to another process then the factory\n"
    " supervisor will fail to start.\n"
).
-spec named(builder(EXM, EXN), gleam@erlang@process:name(message(EXM, EXN))) -> builder(EXM, EXN).
named(Builder, Name) ->
    {builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        {some, Name}}.

-file("src/gleam/otp/factory_supervisor.gleam", 224).
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
-spec restart_tolerance(builder(EXV, EXW), integer(), integer()) -> builder(EXV, EXW).
restart_tolerance(Builder, Intensity, Period) ->
    {builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        Intensity,
        Period,
        erlang:element(7, Builder)}.

-file("src/gleam/otp/factory_supervisor.gleam", 239).
?DOC(
    " Configure the amount of milliseconds a child has to shut down before\n"
    " being brutal killed by the supervisor.\n"
    "\n"
    " If not set the default for a child is 5000ms.\n"
    "\n"
    " This will be ignored if the child is a supervisor itself.\n"
).
-spec timeout(builder(EYB, EYC), integer()) -> builder(EYB, EYC).
timeout(Builder, Ms) ->
    case erlang:element(2, Builder) of
        {worker, _} ->
            {builder,
                {worker, Ms},
                erlang:element(3, Builder),
                erlang:element(4, Builder),
                erlang:element(5, Builder),
                erlang:element(6, Builder),
                erlang:element(7, Builder)};

        _ ->
            Builder
    end.

-file("src/gleam/otp/factory_supervisor.gleam", 256).
?DOC(
    " Configure the strategy for restarting children when they exit. See the\n"
    " documentation for the `supervision.Restart` for details.\n"
    "\n"
    " If not set the default strategy is `supervision.Transient`, so children\n"
    " will be restarted if they terminate abnormally.\n"
).
-spec restart_strategy(builder(EYH, EYI), gleam@otp@supervision:restart()) -> builder(EYH, EYI).
restart_strategy(Builder, Restart_strategy) ->
    case erlang:element(2, Builder) of
        {worker, _} ->
            {builder,
                erlang:element(2, Builder),
                erlang:element(3, Builder),
                Restart_strategy,
                erlang:element(5, Builder),
                erlang:element(6, Builder),
                erlang:element(7, Builder)};

        _ ->
            Builder
    end.

-file("src/gleam/otp/factory_supervisor.gleam", 275).
?DOC(
    " Start a new supervisor process with the configuration and child template\n"
    " specified within the builder.\n"
    "\n"
    " Typically you would use the `supervised` function to add your supervisor to\n"
    " a supervision tree instead of using this function directly.\n"
    "\n"
    " The supervisor will be linked to the parent process that calls this\n"
    " function.\n"
).
-spec start(builder(EYN, EYO)) -> {ok,
        gleam@otp@actor:started(supervisor(EYN, EYO))} |
    {error, gleam@otp@actor:start_error()}.
start(Builder) ->
    Flags = maps:from_list(
        [{strategy, simple_one_for_one},
            {intensity, erlang:element(5, Builder)},
            {period, erlang:element(6, Builder)}]
    ),
    Module_atom = erlang:binary_to_atom(<<"gleam@otp@factory_supervisor"/utf8>>),
    Function_atom = erlang:binary_to_atom(<<"start_child_callback"/utf8>>),
    Mfa = {Module_atom, Function_atom, [erlang:element(3, Builder)]},
    {Type_, Shutdown} = case erlang:element(2, Builder) of
        supervisor ->
            {erlang:binary_to_atom(<<"supervisor"/utf8>>),
                gleam_otp_external:make_timeout(-1)};

        {worker, Ms} ->
            {erlang:binary_to_atom(<<"worker"/utf8>>),
                gleam_otp_external:make_timeout(Ms)}
    end,
    Child = maps:from_list(
        [{id, 0},
            {start, Mfa},
            {restart, erlang:element(4, Builder)},
            {type, Type_},
            {shutdown, Shutdown}]
    ),
    Configuration = {Flags, [Child]},
    Start_result = case erlang:element(7, Builder) of
        none ->
            supervisor:start_link(Module_atom, Configuration);

        {some, Name} ->
            supervisor:start_link({local, Name}, Module_atom, Configuration)
    end,
    case Start_result of
        {ok, Pid} ->
            {ok, {started, Pid, {supervisor, Pid}}};

        {error, Error} ->
            {error, gleam_otp_external:convert_erlang_start_error(Error)}
    end.

-file("src/gleam/otp/factory_supervisor.gleam", 388).
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
-spec supervised(builder(EZM, EZN)) -> gleam@otp@supervision:child_specification(supervisor(EZM, EZN)).
supervised(Builder) ->
    gleam@otp@supervision:supervisor(fun() -> start(Builder) end).

-file("src/gleam/otp/factory_supervisor.gleam", 397).
?DOC(
    " Start a new child using the supervisor's child template and the given\n"
    " argument. The start result of the child is returned.\n"
).
-spec start_child(supervisor(EZT, EZU), EZT) -> {ok,
        gleam@otp@actor:started(EZU)} |
    {error, gleam@otp@actor:start_error()}.
start_child(Supervisor, Argument) ->
    Start = case Supervisor of
        {named_supervisor, Name} ->
            fun(_capture) -> supervisor:start_child(Name, _capture) end;

        {supervisor, Pid} ->
            fun(_capture@1) -> supervisor:start_child(Pid, _capture@1) end
    end,
    case Start([Argument]) of
        {ok, Pid@1, Data} ->
            {ok, {started, Pid@1, Data}};

        {error, Reason} ->
            {error, Reason}
    end.

-file("src/gleam/otp/factory_supervisor.gleam", 425).
?DOC(false).
-spec init(gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
    {error, any()}.
init(Start_data) ->
    {ok, Start_data}.

-file("src/gleam/otp/factory_supervisor.gleam", 431).
?DOC(false).
-spec start_child_callback(
    fun((FAR) -> {ok, gleam@otp@actor:started(FAS)} |
        {error, gleam@otp@actor:start_error()}),
    FAR
) -> gleam@otp@internal@result2:result2(gleam@erlang@process:pid_(), FAS, gleam@otp@actor:start_error()).
start_child_callback(Start, Argument) ->
    case Start(Argument) of
        {ok, Started} ->
            {ok, erlang:element(2, Started), erlang:element(3, Started)};

        {error, Error} ->
            {error, Error}
    end.

-file("src/gleam/otp/factory_supervisor.gleam", 164).
?DOC(
    " Configure a supervisor with a child-starting template function.\n"
    "\n"
    " You should use this unless the child processes are also supervisors.\n"
    "\n"
    " The default shutdown timeout is 5000ms. This can be changed with the\n"
    " `timeout` function.\n"
).
-spec worker_child(
    fun((EXC) -> {ok, gleam@otp@actor:started(EXD)} |
        {error, gleam@otp@actor:start_error()})
) -> builder(EXC, EXD).
worker_child(Template) ->
    {builder, {worker, 5000}, Template, transient, 2, 5, none}.

-file("src/gleam/otp/factory_supervisor.gleam", 185).
?DOC(
    " Configure a supervisor with a template that will start children that are\n"
    " also supervisors.\n"
    "\n"
    " You should only use this if the child processes are also supervisors.\n"
    "\n"
    " Supervisor children have an unlimited amount of time to shutdown, there is\n"
    " no timeout.\n"
).
-spec supervisor_child(
    fun((EXH) -> {ok, gleam@otp@actor:started(EXI)} |
        {error, gleam@otp@actor:start_error()})
) -> builder(EXH, EXI).
supervisor_child(Template) ->
    {builder, supervisor, Template, transient, 2, 5, none}.
