-module(gleam@erlang@process).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/erlang/process.gleam").
-export([self/0, spawn/1, spawn_unlinked/1, unsafely_create_subject/2, new_name/1, named_subject/1, subject_name/1, 'receive'/2, receive_forever/1, new_selector/0, selector_receive/2, selector_receive_forever/1, map_selector/2, merge_selector/2, flush_messages/0, select_map/3, select/2, select_record/4, select_other/2, deselect/2, sleep/1, sleep_forever/0, is_alive/1, monitor/1, select_specific_monitor/3, select_monitors/2, select_trapped_exits/2, demonitor_process/1, deselect_specific_monitor/2, link/1, unlink/1, send_after/3, new_subject/0, cancel_timer/1, kill/1, send_exit/1, send_abnormal_exit/2, trap_exits/1, register/2, unregister/1, named/1, subject_owner/1, send/2, call/3, call_forever/2]).
-export_type([pid_/0, subject/1, name/1, do_not_leak/0, selector/1, exit_message/0, exit_reason/0, anything_selector_tag/0, process_monitor_flag/0, monitor/0, down/0, timer/0, cancelled/0, kill_flag/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type pid_() :: any().

-opaque subject(DVB) :: {subject, pid_(), gleam@dynamic:dynamic_()} |
    {named_subject, name(DVB)}.

-type name(DVC) :: any() | {gleam_phantom, DVC}.

-type do_not_leak() :: any().

-type selector(DVD) :: any() | {gleam_phantom, DVD}.

-type exit_message() :: {exit_message, pid_(), exit_reason()}.

-type exit_reason() :: normal | killed | {abnormal, gleam@dynamic:dynamic_()}.

-type anything_selector_tag() :: anything.

-type process_monitor_flag() :: process.

-type monitor() :: any().

-type down() :: {process_down, monitor(), pid_(), exit_reason()} |
    {port_down, monitor(), gleam@erlang@port:port_(), exit_reason()}.

-type timer() :: any().

-type cancelled() :: timer_not_found | {cancelled, integer()}.

-type kill_flag() :: kill.

-file("src/gleam/erlang/process.gleam", 17).
?DOC(" Get the `Pid` for the current process.\n").
-spec self() -> pid_().
self() ->
    erlang:self().

-file("src/gleam/erlang/process.gleam", 36).
?DOC(
    " Create a new Erlang process that runs concurrently to the creator. In other\n"
    " languages this might be called a fibre, a green thread, or a coroutine.\n"
    "\n"
    " The child process is linked to the creator process. When a process\n"
    " terminates an exit signal is sent to all other processes that are linked to\n"
    " it, causing the process to either terminate or have to handle the signal.\n"
    " If you want an unlinked process use the `spawn_unlinked` function.\n"
    "\n"
    " More can be read about processes and links in the [Erlang documentation][1].\n"
    "\n"
    " [1]: https://www.erlang.org/doc/reference_manual/processes.html\n"
    "\n"
    " This function starts processes via the Erlang `proc_lib` module, and as\n"
    " such they benefit from the functionality described in the\n"
    " [`proc_lib` documentation](https://www.erlang.org/doc/apps/stdlib/proc_lib.html).\n"
).
-spec spawn(fun(() -> any())) -> pid_().
spawn(Running) ->
    proc_lib:spawn_link(Running).

-file("src/gleam/erlang/process.gleam", 53).
?DOC(
    " Create a new Erlang process that runs concurrently to the creator. In other\n"
    " languages this might be called a fibre, a green thread, or a coroutine.\n"
    "\n"
    " Typically you want to create a linked process using the `spawn` function,\n"
    " but creating an unlinked process may be occasionally useful.\n"
    "\n"
    " More can be read about processes and links in the [Erlang documentation][1].\n"
    "\n"
    " [1]: https://www.erlang.org/doc/reference_manual/processes.html\n"
    "\n"
    " This function starts processes via the Erlang `proc_lib` module, and as\n"
    " such they benefit from the functionality described in the\n"
    " [`proc_lib` documentation](https://www.erlang.org/doc/apps/stdlib/proc_lib.html).\n"
).
-spec spawn_unlinked(fun(() -> any())) -> pid_().
spawn_unlinked(A) ->
    proc_lib:spawn(A).

-file("src/gleam/erlang/process.gleam", 90).
?DOC(false).
-spec unsafely_create_subject(pid_(), gleam@dynamic:dynamic_()) -> subject(any()).
unsafely_create_subject(Owner, Tag) ->
    {subject, Owner, Tag}.

-file("src/gleam/erlang/process.gleam", 136).
?DOC(
    " Generate a new name that a process can register itself with using the\n"
    " `register` function, and other processes can send messages to using\n"
    " `named_subject`.\n"
    "\n"
    " The string argument is a prefix for the Erlang name. A unique suffix is\n"
    " added to the prefix to make the name, removing the possibility of name\n"
    " collisions.\n"
    "\n"
    " ## Safe use\n"
    "\n"
    " Use this function to create all the names your program needs when it\n"
    " starts. **Never call this function dynamically** such as within a loop or\n"
    " within a process within a supervision tree.\n"
    "\n"
    " Each time this function is called a new atom will be generated. Generating\n"
    " too many atoms will result in the atom table getting filled and causing the\n"
    " entire virtual machine to crash.\n"
).
-spec new_name(binary()) -> name(any()).
new_name(Prefix) ->
    gleam_erlang_ffi:new_name(Prefix).

-file("src/gleam/erlang/process.gleam", 143).
?DOC(
    " Create a subject for a name, which can be used to send and receive messages.\n"
    "\n"
    " All subjects created for the same name behave identically and can be used\n"
    " interchangably.\n"
).
-spec named_subject(name(DVK)) -> subject(DVK).
named_subject(Name) ->
    {named_subject, Name}.

-file("src/gleam/erlang/process.gleam", 149).
?DOC(" Get the name of a subject, returning an error if it doesn't have one.\n").
-spec subject_name(subject(DVN)) -> {ok, name(DVN)} | {error, nil}.
subject_name(Subject) ->
    case Subject of
        {named_subject, Name} ->
            {ok, Name};

        {subject, _, _} ->
            {error, nil}
    end.

-file("src/gleam/erlang/process.gleam", 247).
?DOC(
    " Receive a message that has been sent to current process using the `Subject`.\n"
    "\n"
    " If there is not an existing message for the `Subject` in the process'\n"
    " mailbox or one does not arrive `within` the permitted timeout then the\n"
    " `Error(Nil)` is returned.\n"
    "\n"
    " Only the process that is owner of the `Subject` can receive a message using\n"
    " it. If a process that does not own the `Subject` attempts to receive with it\n"
    " then it will not receive a message.\n"
    "\n"
    " To wait for messages from multiple `Subject`s at the same time see the\n"
    " `Selector` type.\n"
    "\n"
    " The `within` parameter specifies the timeout duration in milliseconds.\n"
    "\n"
    " ## Panics\n"
    "\n"
    " This function will panic if a process tries to receive with a non-named\n"
    " subject that it does not own.\n"
).
-spec 'receive'(subject(DWB), integer()) -> {ok, DWB} | {error, nil}.
'receive'(Subject, Timeout) ->
    case Subject of
        {named_subject, _} ->
            gleam_erlang_ffi:'receive'(Subject, Timeout);

        {subject, Owner, _} ->
            case Owner =:= erlang:self() of
                true ->
                    gleam_erlang_ffi:'receive'(Subject, Timeout);

                false ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Cannot receive with a subject owned by another process"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"gleam/erlang/process"/utf8>>,
                            function => <<"receive"/utf8>>,
                            line => 257})
            end
    end.

-file("src/gleam/erlang/process.gleam", 272).
?DOC(
    " Receive a message that has been sent to current process using the `Subject`.\n"
    "\n"
    " Same as `receive` but waits forever and returns the message as is.\n"
).
-spec receive_forever(subject(DWJ)) -> DWJ.
receive_forever(Subject) ->
    gleam_erlang_ffi:'receive'(Subject).

-file("src/gleam/erlang/process.gleam", 301).
?DOC(
    " Create a new `Selector` which can be used to receive messages on multiple\n"
    " `Subject`s at once.\n"
).
-spec new_selector() -> selector(any()).
new_selector() ->
    gleam_erlang_ffi:new_selector().

-file("src/gleam/erlang/process.gleam", 321).
?DOC(
    " Receive a message that has been sent to current process using any of the\n"
    " `Subject`s that have been added to the `Selector` with the `select*`\n"
    " functions.\n"
    "\n"
    " If there is not an existing message for the `Selector` in the process'\n"
    " mailbox or one does not arrive `within` the permitted timeout then the\n"
    " `Error(Nil)` is returned.\n"
    "\n"
    " Only the process that is owner of the `Subject`s can receive a message using\n"
    " them. If a process that does not own the a `Subject` attempts to receive\n"
    " with it then it will not receive a message.\n"
    "\n"
    " To wait forever for the next message rather than for a limited amount of\n"
    " time see the `selector_receive_forever` function.\n"
    "\n"
    " The `within` parameter specifies the timeout duration in milliseconds.\n"
).
-spec selector_receive(selector(DWN), integer()) -> {ok, DWN} | {error, nil}.
selector_receive(From, Within) ->
    gleam_erlang_ffi:select(From, Within).

-file("src/gleam/erlang/process.gleam", 330).
?DOC(
    " Similar to the `select` function but will wait forever for a message to\n"
    " arrive rather than timing out after a specified amount of time.\n"
).
-spec selector_receive_forever(selector(DWR)) -> DWR.
selector_receive_forever(From) ->
    gleam_erlang_ffi:select(From).

-file("src/gleam/erlang/process.gleam", 339).
?DOC(
    " Add a transformation function to a selector. When a message is received\n"
    " using this selector the transformation function is applied to the message.\n"
    "\n"
    " This function can be used to change the type of messages received and may\n"
    " be useful when combined with the `merge_selector` function.\n"
).
-spec map_selector(selector(DWT), fun((DWT) -> DWV)) -> selector(DWV).
map_selector(A, B) ->
    gleam_erlang_ffi:map_selector(A, B).

-file("src/gleam/erlang/process.gleam", 348).
?DOC(
    " Merge one selector into another, producing a selector that contains the\n"
    " message handlers of both.\n"
    "\n"
    " If a subject is handled by both selectors the handler function of the\n"
    " second selector is used.\n"
).
-spec merge_selector(selector(DWX), selector(DWX)) -> selector(DWX).
merge_selector(A, B) ->
    gleam_erlang_ffi:merge_selector(A, B).

-file("src/gleam/erlang/process.gleam", 384).
?DOC(
    " Discard all messages in the current process' mailbox.\n"
    "\n"
    " Warning: This function may cause other processes to crash if they sent a\n"
    " message to the current process and are waiting for a response, so use with\n"
    " caution.\n"
    "\n"
    " This function may be useful in tests.\n"
).
-spec flush_messages() -> nil.
flush_messages() ->
    gleam_erlang_ffi:flush_messages().

-file("src/gleam/erlang/process.gleam", 411).
?DOC(
    " Add a new `Subject` to the `Selector` so that its messages can be selected\n"
    " from the receiver process inbox.\n"
    "\n"
    " The `mapping` function provided with the `Subject` can be used to convert\n"
    " the type of messages received using this `Subject`. This is useful for when\n"
    " you wish to add multiple `Subject`s to a `Selector` when they have differing\n"
    " message types. If you do not wish to transform the incoming messages in any\n"
    " way then the `identity` function can be given.\n"
    "\n"
    " See `deselect` to remove a subject from a selector.\n"
).
-spec select_map(selector(DXI), subject(DXK), fun((DXK) -> DXI)) -> selector(DXI).
select_map(Selector, Subject, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    case Subject of
        {named_subject, Name} ->
            gleam_erlang_ffi:insert_selector_handler(
                Selector,
                {Name, 2},
                Handler
            );

        {subject, _, Tag} ->
            gleam_erlang_ffi:insert_selector_handler(
                Selector,
                {Tag, 2},
                Handler
            )
    end.

-file("src/gleam/erlang/process.gleam", 393).
?DOC(
    " Add a new `Subject` to the `Selector` so that its messages can be selected\n"
    " from the receiver process inbox.\n"
    "\n"
    " See `select_map` to add subjects of a different message type.\n"
    "\n"
    " See `deselect` to remove a subject from a selector.\n"
).
-spec select(selector(DXE), subject(DXE)) -> selector(DXE).
select(Selector, Subject) ->
    select_map(Selector, Subject, fun(X) -> X end).

-file("src/gleam/erlang/process.gleam", 447).
?DOC(
    " Add a handler to a selector for tuple messages with a given tag in the\n"
    " first position followed by a given number of fields.\n"
    "\n"
    " Typically you want to use the `select` function with a `Subject` instead,\n"
    " but this function may be useful if you need to receive messages sent from\n"
    " other BEAM languages that do not use the `Subject` type.\n"
    "\n"
    " This will not select messages sent via a subject even if the message has\n"
    " the same tag in the first position. This is because when a message is sent\n"
    " via a subject a new tag is used that is unique and specific to that subject.\n"
).
-spec select_record(
    selector(DXS),
    any(),
    integer(),
    fun((gleam@dynamic:dynamic_()) -> DXS)
) -> selector(DXS).
select_record(Selector, Tag, Arity, Transform) ->
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        {Tag, Arity + 1},
        Transform
    ).

-file("src/gleam/erlang/process.gleam", 467).
?DOC(
    " Add a catch-all handler to a selector that will be used when no other\n"
    " handler in a selector is suitable for a given message.\n"
    "\n"
    " This may be useful for when you want to ensure that any message in the inbox\n"
    " is handled, or when you need to handle messages from other BEAM languages\n"
    " which do not use subjects or record format messages.\n"
).
-spec select_other(selector(DXW), fun((gleam@dynamic:dynamic_()) -> DXW)) -> selector(DXW).
select_other(Selector, Handler) ->
    gleam_erlang_ffi:insert_selector_handler(Selector, anything, Handler).

-file("src/gleam/erlang/process.gleam", 426).
?DOC(
    " Remove a new `Subject` from the `Selector` so that its messages will not be\n"
    " selected from the receiver process inbox.\n"
).
-spec deselect(selector(DXN), subject(any())) -> selector(DXN).
deselect(Selector, Subject) ->
    case Subject of
        {named_subject, Name} ->
            gleam_erlang_ffi:remove_selector_handler(Selector, {Name, 2});

        {subject, _, Tag} ->
            gleam_erlang_ffi:remove_selector_handler(Selector, {Tag, 2})
    end.

-file("src/gleam/erlang/process.gleam", 491).
?DOC(
    " Suspends the process calling this function for the specified number of\n"
    " milliseconds.\n"
).
-spec sleep(integer()) -> nil.
sleep(A) ->
    gleam_erlang_ffi:sleep(A).

-file("src/gleam/erlang/process.gleam", 498).
?DOC(
    " Suspends the process forever! This may be useful for suspending the main\n"
    " process in a Gleam program when it has no more work to do but we want other\n"
    " processes to continue to work.\n"
).
-spec sleep_forever() -> nil.
sleep_forever() ->
    gleam_erlang_ffi:sleep_forever().

-file("src/gleam/erlang/process.gleam", 507).
?DOC(
    " Check to see whether the process for a given `Pid` is alive.\n"
    "\n"
    " See the [Erlang documentation][1] for more information.\n"
    "\n"
    " [1]: http://erlang.org/doc/man/erlang.html#is_process_alive-1\n"
).
-spec is_alive(pid_()) -> boolean().
is_alive(A) ->
    erlang:is_process_alive(A).

-file("src/gleam/erlang/process.gleam", 537).
?DOC(
    " Start monitoring a process so that when the monitored process exits a\n"
    " message is sent to the monitoring process.\n"
    "\n"
    " The message is only sent once, when the target process exits. If the\n"
    " process was not alive when this function is called the message will never\n"
    " be received.\n"
    "\n"
    " The down message can be received with a selector and the\n"
    " `select_monitors` function.\n"
    "\n"
    " The process can be demonitored with the `demonitor_process` function.\n"
).
-spec monitor(pid_()) -> monitor().
monitor(Pid) ->
    erlang:monitor(process, Pid).

-file("src/gleam/erlang/process.gleam", 550).
?DOC(
    " Select for a message sent for a given monitor.\n"
    "\n"
    " Each monitor handler added to a selector has a select performance cost,\n"
    " so prefer [`select_monitors`](#select_monitors) if you are select\n"
    " for multiple monitors.\n"
    "\n"
    " The handler can be removed from the selector later using\n"
    " [`deselect_specific_monitor`](#deselect_specific_monitor).\n"
).
-spec select_specific_monitor(selector(DYI), monitor(), fun((down()) -> DYI)) -> selector(DYI).
select_specific_monitor(Selector, Monitor, Mapping) ->
    gleam_erlang_ffi:insert_selector_handler(Selector, Monitor, Mapping).

-file("src/gleam/erlang/process.gleam", 564).
?DOC(
    " Select for any messages sent for any monitors set up by the select process.\n"
    "\n"
    " If you want to select for a specific message then use \n"
    " [`select_specific_monitor`](#select_specific_monitor), but this\n"
    " function is preferred if you need to select for multiple monitors.\n"
).
-spec select_monitors(selector(DYL), fun((down()) -> DYL)) -> selector(DYL).
select_monitors(Selector, Mapping) ->
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        {erlang:binary_to_atom(<<"DOWN"/utf8>>), 5},
        fun(Message) -> Mapping(gleam_erlang_ffi:cast_down_message(Message)) end
    ).

-file("src/gleam/erlang/process.gleam", 364).
?DOC(
    " Add a handler for trapped exit messages. In order for these messages to be\n"
    " sent to the process when a linked process exits the process must call the\n"
    " `trap_exit` beforehand.\n"
).
-spec select_trapped_exits(selector(DXB), fun((exit_message()) -> DXB)) -> selector(DXB).
select_trapped_exits(Selector, Handler) ->
    Tag = erlang:binary_to_atom(<<"EXIT"/utf8>>),
    Handler@1 = fun(Message) ->
        Handler(
            {exit_message,
                erlang:element(2, Message),
                gleam_erlang_ffi:cast_exit_reason(erlang:element(3, Message))}
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler@1).

-file("src/gleam/erlang/process.gleam", 585).
?DOC(
    " Remove the monitor for a process so that when the monitor process exits a\n"
    " `Down` message is not sent to the monitoring process.\n"
    "\n"
    " If the message has already been sent it is removed from the monitoring\n"
    " process' mailbox.\n"
).
-spec demonitor_process(monitor()) -> nil.
demonitor_process(Monitor) ->
    gleam_erlang_ffi:demonitor(Monitor),
    nil.

-file("src/gleam/erlang/process.gleam", 598).
?DOC(
    " Remove a `Monitor` from a `Selector` prevoiusly added by\n"
    " [`select_specific_monitor`](#select_specific_monitor). If\n"
    " the `Monitor` is not in the `Selector` it will be returned\n"
    " unchanged.\n"
).
-spec deselect_specific_monitor(selector(DYO), monitor()) -> selector(DYO).
deselect_specific_monitor(Selector, Monitor) ->
    gleam_erlang_ffi:remove_selector_handler(Selector, Monitor).

-file("src/gleam/erlang/process.gleam", 729).
?DOC(
    " Creates a link between the calling process and another process.\n"
    "\n"
    " When a process crashes any linked processes will also crash. This is useful\n"
    " to ensure that groups of processes that depend on each other all either\n"
    " succeed or fail together.\n"
    "\n"
    " Returns `True` if the link was created successfully, returns `False` if the\n"
    " process was not alive and as such could not be linked.\n"
).
-spec link(pid_()) -> boolean().
link(Pid) ->
    gleam_erlang_ffi:link(Pid).

-file("src/gleam/erlang/process.gleam", 736).
?DOC(" Removes any existing link between the caller process and the target process.\n").
-spec unlink(pid_()) -> nil.
unlink(Pid) ->
    erlang:unlink(Pid),
    nil.

-file("src/gleam/erlang/process.gleam", 751).
?DOC(" Send a message over a channel after a specified number of milliseconds.\n").
-spec send_after(subject(DZK), integer(), DZK) -> timer().
send_after(Subject, Delay, Message) ->
    case Subject of
        {named_subject, Name} ->
            erlang:send_after(Delay, Name, {Name, Message});

        {subject, Owner, Tag} ->
            erlang:send_after(Delay, Owner, {Tag, Message})
    end.

-file("src/gleam/erlang/process.gleam", 158).
?DOC(" Create a new `Subject` owned by the current process.\n").
-spec new_subject() -> subject(any()).
new_subject() ->
    {subject, erlang:self(), gleam_erlang_ffi:identity(erlang:make_ref())}.

-file("src/gleam/erlang/process.gleam", 781).
?DOC(" Cancel a given timer, causing it not to trigger if it has not done already.\n").
-spec cancel_timer(timer()) -> cancelled().
cancel_timer(Timer) ->
    case gleam@dynamic@decode:run(
        erlang:cancel_timer(Timer),
        {decoder, fun gleam@dynamic@decode:decode_int/1}
    ) of
        {ok, I} ->
            {cancelled, I};

        {error, _} ->
            timer_not_found
    end.

-file("src/gleam/erlang/process.gleam", 805).
?DOC(
    " Send an untrappable `kill` exit signal to the target process.\n"
    "\n"
    " See the documentation for the Erlang [`erlang:exit`][1] function for more\n"
    " information.\n"
    "\n"
    " [1]: https://erlang.org/doc/man/erlang.html#exit-1\n"
).
-spec kill(pid_()) -> nil.
kill(Pid) ->
    erlang:exit(Pid, kill),
    nil.

-file("src/gleam/erlang/process.gleam", 821).
?DOC(
    " Sends an exit signal to a process, indicating that the process is to shut\n"
    " down.\n"
    "\n"
    " See the [Erlang documentation][1] for more information.\n"
    "\n"
    " [1]: http://erlang.org/doc/man/erlang.html#exit-2\n"
).
-spec send_exit(pid_()) -> nil.
send_exit(Pid) ->
    erlang:exit(Pid, normal),
    nil.

-file("src/gleam/erlang/process.gleam", 833).
?DOC(
    " Sends an exit signal to a process, indicating that the process is to shut\n"
    " down due to an abnormal reason such as a failure.\n"
    "\n"
    " See the [Erlang documentation][1] for more information.\n"
    "\n"
    " [1]: http://erlang.org/doc/man/erlang.html#exit-2\n"
).
-spec send_abnormal_exit(pid_(), any()) -> nil.
send_abnormal_exit(Pid, Reason) ->
    erlang:exit(Pid, Reason),
    nil.

-file("src/gleam/erlang/process.gleam", 849).
?DOC(
    " Set whether the current process is to trap exit signals or not.\n"
    "\n"
    " When not trapping exits if a linked process crashes the exit signal\n"
    " propagates to the process which will also crash.\n"
    " This is the normal behaviour before this function is called.\n"
    "\n"
    " When trapping exits (after this function is called) if a linked process\n"
    " crashes an exit message is sent to the process instead. These messages can\n"
    " be handled with the `select_trapped_exits` function.\n"
).
-spec trap_exits(boolean()) -> nil.
trap_exits(A) ->
    gleam_erlang_ffi:trap_exits(A).

-file("src/gleam/erlang/process.gleam", 860).
?DOC(
    " Register a process under a given name, allowing it to be looked up using\n"
    " the `named` function.\n"
    "\n"
    " This function will return an error under the following conditions:\n"
    " - The process for the pid no longer exists.\n"
    " - The name has already been registered.\n"
    " - The process already has a name.\n"
).
-spec register(pid_(), name(any())) -> {ok, nil} | {error, nil}.
register(Pid, Name) ->
    gleam_erlang_ffi:register_process(Pid, Name).

-file("src/gleam/erlang/process.gleam", 871).
?DOC(
    " Un-register a process name, after which the process can no longer be looked\n"
    " up by that name, and both the name and the process can be re-used in other\n"
    " registrations.\n"
    "\n"
    " It is possible to un-register process that are not from your application,\n"
    " including those from Erlang/OTP itself. This is not recommended and will\n"
    " likely result in undesirable behaviour and crashes.\n"
).
-spec unregister(name(any())) -> {ok, nil} | {error, nil}.
unregister(Name) ->
    gleam_erlang_ffi:unregister_process(Name).

-file("src/gleam/erlang/process.gleam", 876).
?DOC(" Look up a process by registered name, returning the pid if it exists.\n").
-spec named(name(any())) -> {ok, pid_()} | {error, nil}.
named(Name) ->
    gleam_erlang_ffi:process_named(Name).

-file("src/gleam/erlang/process.gleam", 168).
?DOC(
    " Get the owner process for a subject, which is the process that will\n"
    " receive any messages sent using the subject.\n"
    "\n"
    " If the subject was created from a name and no process is currently\n"
    " registered with that name then this function will return an error.\n"
).
-spec subject_owner(subject(any())) -> {ok, pid_()} | {error, nil}.
subject_owner(Subject) ->
    case Subject of
        {named_subject, Name} ->
            gleam_erlang_ffi:process_named(Name);

        {subject, Pid, _} ->
            {ok, Pid}
    end.

-file("src/gleam/erlang/process.gleam", 214).
?DOC(
    " Send a message to a process using a `Subject`. The message must be of the\n"
    " type that the `Subject` accepts.\n"
    "\n"
    " This function does not wait for the `Subject` owner process to call the\n"
    " `receive` function, instead it returns once the message has been placed in\n"
    " the process' mailbox.\n"
    " \n"
    " # Named Subjects\n"
    " \n"
    " If this function is called on a named subject for which a process has not been \n"
    " registered, it will simply drop the message as there's no mailbox to send it to.\n"
    "\n"
    " # Panics\n"
    "\n"
    " This function will panic when sending to a named subject if no process is\n"
    " currently registed under that name.\n"
    "\n"
    " # Ordering\n"
    "\n"
    " If process P1 sends two messages to process P2 it is guaranteed that process\n"
    " P1 will receive the messages in the order they were sent.\n"
    "\n"
    " If you wish to receive the messages in a different order you can send them\n"
    " on two different subjects and the receiver function can call the `receive`\n"
    " function for each subject in the desired order, or you can write some Erlang\n"
    " code to perform a selective receive.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " let subject = new_subject()\n"
    " send(subject, \"Hello, Joe!\")\n"
    " ```\n"
).
-spec send(subject(DVZ), DVZ) -> nil.
send(Subject, Message) ->
    case Subject of
        {subject, Pid, Tag} ->
            erlang:send(Pid, {Tag, Message});

        {named_subject, Name} ->
            Pid@2 = case gleam_erlang_ffi:process_named(Name) of
                {ok, Pid@1} -> Pid@1;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Sending to unregistered name"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"gleam/erlang/process"/utf8>>,
                                function => <<"send"/utf8>>,
                                line => 220,
                                value => _assert_fail,
                                start => 8194,
                                'end' => 8226,
                                pattern_start => 8205,
                                pattern_end => 8212})
            end,
            erlang:send(Pid@2, {Name, Message})
    end,
    nil.

-file("src/gleam/erlang/process.gleam", 605).
-spec perform_call(
    subject(DYR),
    fun((subject(DYT)) -> DYR),
    fun((selector(DYT)) -> {ok, DYT} | {error, nil})
) -> DYT.
perform_call(Subject, Make_request, Run_selector) ->
    Reply_subject = new_subject(),
    Callee@1 = case subject_owner(Subject) of
        {ok, Callee} -> Callee;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Callee subject had no owner"/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"perform_call"/utf8>>,
                        line => 611,
                        value => _assert_fail,
                        start => 21173,
                        'end' => 21219,
                        pattern_start => 21184,
                        pattern_end => 21194})
    end,
    Monitor = monitor(Callee@1),
    send(Subject, Make_request(Reply_subject)),
    Reply = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = select(_pipe, Reply_subject),
        _pipe@2 = select_specific_monitor(
            _pipe@1,
            Monitor,
            fun(Down) -> erlang:error(#{gleam_error => panic,
                        message => (<<"callee exited: "/utf8,
                            (gleam@string:inspect(Down))/binary>>),
                        file => <<?FILEPATH/utf8>>,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"perform_call"/utf8>>,
                        line => 626}) end
        ),
        Run_selector(_pipe@2)
    end,
    Reply@2 = case Reply of
        {ok, Reply@1} -> Reply@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"callee did not send reply before timeout"/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"perform_call"/utf8>>,
                        line => 630,
                        value => _assert_fail@1,
                        start => 21766,
                        'end' => 21794,
                        pattern_start => 21777,
                        pattern_end => 21786})
    end,
    demonitor_process(Monitor),
    Reply@2.

-file("src/gleam/erlang/process.gleam", 695).
?DOC(
    " Send a message to a process and wait a given number of milliseconds for a\n"
    " reply.\n"
    "\n"
    " ## Panics\n"
    "\n"
    " This function will panic under the following circumstances:\n"
    " - The callee process exited prior to sending a reply.\n"
    " - The callee process did not send a reply within the permitted amount of\n"
    "   time.\n"
    " - The subject is a named subject but no process is registered with that\n"
    "   name.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " pub type Message {\n"
    "   // This message variant is to be used with `call`.\n"
    "   // The `reply` field contains a subject that the reply message will be\n"
    "   // sent over.\n"
    "   SayHello(reply_to: Subject(String), name: String)\n"
    " }\n"
    " \n"
    " // Typically we make public functions that hide the details of a process'\n"
    " // message-based API.\n"
    " pub fn say_hello(subject: Subject(Message), name: String) -> String {\n"
    "   // The `SayHello` message constructor is given _partially applied_ with\n"
    "   // all the arguments except the reply subject, which will be supplied by\n"
    "   // the `call` function itself before sending the message.\n"
    "   process.call(subject, 100, SayHello(_, name))\n"
    " }\n"
    "\n"
    " // This is the message handling logic used by the process that owns the\n"
    " // subject, and so receives the messages. In a real project it would be\n"
    " // within a process or some higher level abstraction like an actor, but for\n"
    " // this demonstration that has been omitted.\n"
    " pub fn handle_message(message: Message) -> Nil {\n"
    "   case message {\n"
    "     SayHello(reply:, name:) -> {\n"
    "       let data = \"Hello, \" <> name <> \"!\"\n"
    "       // The reply subject is used to send the response back.\n"
    "       // If the receiver process does not sent a reply in time then the\n"
    "       // caller will crash.\n"
    "       process.send(reply, data)\n"
    "     }\n"
    "   }\n"
    " }\n"
    "\n"
    " // Here is what it looks like using the functional API to call the process.\n"
    " pub fn run(subject: Subject(Message)) {\n"
    "   say_hello(subject, \"Lucy\")\n"
    "   // -> \"Hello, Lucy!\"\n"
    "   say_hello(subject, \"Nubi\")\n"
    "   // -> \"Hello, Nubi!\"\n"
    " }\n"
    " ```\n"
).
-spec call(subject(DYY), integer(), fun((subject(DZA)) -> DYY)) -> DZA.
call(Subject, Timeout, Make_request) ->
    perform_call(
        Subject,
        Make_request,
        fun(_capture) -> gleam_erlang_ffi:select(_capture, Timeout) end
    ).

-file("src/gleam/erlang/process.gleam", 712).
?DOC(
    " Send a message to a process and wait for a reply.\n"
    "\n"
    " # Panics\n"
    "\n"
    " This function will panic under the following circumstances:\n"
    " - The callee process exited prior to sending a reply.\n"
    " - The subject is a named subject but no process is registered with that\n"
    "   name.\n"
).
-spec call_forever(subject(DZC), fun((subject(DZE)) -> DZC)) -> DZE.
call_forever(Subject, Make_request) ->
    perform_call(
        Subject,
        Make_request,
        fun(S) -> {ok, gleam_erlang_ffi:select(S)} end
    ).
