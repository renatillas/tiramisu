-module(gleam@otp@actor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/otp/actor.gleam").
-export([continue/1, stop/0, stop_abnormal/1, with_selector/2, initialised/1, selecting/2, returning/2, new/1, new_with_initialiser/2, on_message/2, named/2, start/1, send/2, call/3]).
-export_type([message/1, next/2, self/2, started/1, initialised/3, builder/3, start_error/0, start_init_message/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " This module provides the _Actor_ abstraction, one of the most common\n"
    " building blocks of Gleam OTP programs.\n"
    "\n"
    " An Actor is a process like any other BEAM process and can be used to hold\n"
    " state, execute code, and communicate with other processes by sending and\n"
    " receiving messages. The advantage of using the actor abstraction over a bare\n"
    " process is that it provides a single interface for commonly needed\n"
    " functionality, including support for the [tracing and debugging\n"
    " features in OTP][erlang-sys].\n"
    "\n"
    " Gleam's Actor is similar to Erlang's `gen_server` and Elixir's `GenServer`\n"
    " but differs in that it offers a fully typed interface. This different API is\n"
    " why Gleam uses the name \"Actor\" rather than some variation of\n"
    " \"generic-server\".\n"
    "\n"
    " [erlang-sys]: https://www.erlang.org/doc/man/sys.html\n"
    "\n"
    " ## Example\n"
    "\n"
    " An Actor can be used to create a client-server interaction between an Actor\n"
    " (the server) and other processes (the clients). In this example we have an\n"
    " Actor that works as a stack, allowing clients to push and pop elements.\n"
    "\n"
    " ```gleam\n"
    " pub fn main() {\n"
    "   // Start the actor with initial state of an empty list, and the\n"
    "   // `handle_message` callback function (defined below).\n"
    "   // We assert that it starts successfully.\n"
    "   // \n"
    "   // In real-world Gleam OTP programs we would likely write a wrapper functions\n"
    "   // called `start`, `push` `pop`, `shutdown` to start and interact with the\n"
    "   // Actor. We are not doing that here for the sake of showing how the Actor \n"
    "   // API works.\n"
    "   let assert Ok(actor) =\n"
    "     actor.new([]) |> actor.on_message(handle_message) |> actor.start\n"
    "   let subject = actor.data\n"
    "\n"
    "   // We can send a message to the actor to push elements onto the stack.\n"
    "   process.send(subject, Push(\"Joe\"))\n"
    "   process.send(subject, Push(\"Mike\"))\n"
    "   process.send(subject, Push(\"Robert\"))\n"
    "\n"
    "   // The `Push` message expects no response, these messages are sent purely for\n"
    "   // the side effect of mutating the state held by the actor.\n"
    "   //\n"
    "   // We can also send the `Pop` message to take a value off of the actor's\n"
    "   // stack. This message expects a response, so we use `process.call` to send a\n"
    "   // message and wait until a reply is received.\n"
    "   //\n"
    "   // In this instance we are giving the actor 10 milliseconds to reply, if the\n"
    "   // `call` function doesn't get a reply within this time it will panic and\n"
    "   // crash the client process.\n"
    "   let assert Ok(\"Robert\") = process.call(subject, 10, Pop)\n"
    "   let assert Ok(\"Mike\") = process.call(subject, 10, Pop)\n"
    "   let assert Ok(\"Joe\") = process.call(subject, 10, Pop)\n"
    "\n"
    "   // The stack is now empty, so if we pop again the actor replies with an error.\n"
    "   let assert Error(Nil) = process.call(subject, 10, Pop)\n"
    "\n"
    "   // Lastly, we can send a message to the actor asking it to shut down.\n"
    "   process.send(subject, Shutdown)\n"
    " }\n"
    " ```\n"
    "\n"
    " Here is the code that is used to implement this actor:\n"
    "\n"
    " ```gleam\n"
    " // First step of implementing the stack Actor is to define the message type that\n"
    " // it can receive.\n"
    " //\n"
    " // The type of the elements in the stack is not fixed so a type parameter\n"
    " // is used for it instead of a concrete type such as `String` or `Int`.\n"
    " pub type Message(element) {\n"
    "   // The `Shutdown` message is used to tell the actor to stop.\n"
    "   // It is the simplest message type, it contains no data.\n"
    "   //\n"
    "   // Most the time we don't define an API to shut down an actor, but in this\n"
    "   // example we do to show how it can be done.\n"
    "   Shutdown\n"
    " \n"
    "   // The `Push` message is used to add a new element to the stack.\n"
    "   // It contains the item to add, the type of which is the `element`\n"
    "   // parameterised type.\n"
    "   Push(push: element)\n"
    " \n"
    "   // The `Pop` message is used to remove an element from the stack.\n"
    "   // It contains a `Subject`, which is used to send the response back to the\n"
    "   // message sender. In this case the reply is of type `Result(element, Nil)`.\n"
    "   Pop(reply_with: Subject(Result(element, Nil)))\n"
    " }\n"
    " \n"
    " // The last part is to implement the `handle_message` callback function.\n"
    " //\n"
    " // This function is called by the Actor for each message it receives.\n"
    " // Actors are single threaded only doing one thing at a time, so they handle\n"
    " // messages sequentially one at a time, in the order they are received.\n"
    " //\n"
    " // The function takes the current state and a message, and returns a data\n"
    " // structure that indicates what to do next, along with the new state.\n"
    " fn handle_message(\n"
    "   stack: List(e),\n"
    "   message: Message(e),\n"
    " ) -> actor.Next(List(e), Message(e)) {\n"
    "   case message {\n"
    "     // For the `Shutdown` message we return the `actor.stop` value, which causes\n"
    "     // the actor to discard any remaining messages and stop.\n"
    "     // We may chose to do some clean-up work here, but this actor doesn't need\n"
    "     // to do this.\n"
    "     Shutdown -> actor.stop()\n"
    " \n"
    "     // For the `Push` message we add the new element to the stack and return\n"
    "     // `actor.continue` with this new stack, causing the actor to process any\n"
    "     // queued messages or wait for more.\n"
    "     Push(value) -> {\n"
    "       let new_state = [value, ..stack]\n"
    "       actor.continue(new_state)\n"
    "     }\n"
    " \n"
    "     // For the `Pop` message we attempt to remove an element from the stack,\n"
    "     // sending it or an error back to the caller, before continuing.\n"
    "     Pop(client) -> {\n"
    "       case stack {\n"
    "         [] -> {\n"
    "           // When the stack is empty we can't pop an element, so we send an\n"
    "           // error back.\n"
    "           process.send(client, Error(Nil))\n"
    "           actor.continue([])\n"
    "         }\n"
    " \n"
    "         [first, ..rest] -> {\n"
    "           // Otherwise we send the first element back and use the remaining\n"
    "           // elements as the new state.\n"
    "           process.send(client, Ok(first))\n"
    "           actor.continue(rest)\n"
    "         }\n"
    "       }\n"
    "     }\n"
    "   }\n"
    " }\n"
    " ```\n"
).

-type message(EAR) :: {message, EAR} |
    {system, gleam@otp@system:system_message()} |
    {unexpected, gleam@dynamic:dynamic_()}.

-opaque next(EAS, EAT) :: {continue,
        EAS,
        gleam@option:option(gleam@erlang@process:selector(EAT))} |
    {stop, gleam@erlang@process:exit_reason()}.

-type self(EAU, EAV) :: {self,
        gleam@otp@system:mode(),
        gleam@erlang@process:pid_(),
        EAU,
        gleam@erlang@process:selector(message(EAV)),
        gleam@otp@system:debug_state(),
        fun((EAU, EAV) -> next(EAU, EAV))}.

-type started(EAW) :: {started, gleam@erlang@process:pid_(), EAW}.

-opaque initialised(EAX, EAY, EAZ) :: {initialised,
        EAX,
        gleam@option:option(gleam@erlang@process:selector(EAY)),
        EAZ}.

-opaque builder(EBA, EBB, EBC) :: {builder,
        fun((gleam@erlang@process:subject(EBB)) -> {ok,
                initialised(EBA, EBB, EBC)} |
            {error, binary()}),
        integer(),
        fun((EBA, EBB) -> next(EBA, EBB)),
        gleam@option:option(gleam@erlang@process:name(EBB))}.

-type start_error() :: init_timeout |
    {init_failed, binary()} |
    {init_exited, gleam@erlang@process:exit_reason()}.

-type start_init_message(EBD) :: {ack, {ok, EBD} | {error, binary()}} |
    {mon, gleam@erlang@process:down()}.

-file("src/gleam/otp/actor.gleam", 185).
?DOC(" Indicate the actor should continue, processing any waiting or future messages.\n").
-spec continue(EBI) -> next(EBI, any()).
continue(State) ->
    {continue, State, none}.

-file("src/gleam/otp/actor.gleam", 193).
?DOC(
    " Indicate the actor should stop and shut-down, handling no futher messages.\n"
    "\n"
    " The reason for exiting is `Normal`.\n"
).
-spec stop() -> next(any(), any()).
stop() ->
    {stop, normal}.

-file("src/gleam/otp/actor.gleam", 202).
?DOC(
    " Indicate the actor is in a bad state and should shut down. It will not\n"
    " handle any new messages, and any linked processes will also exit abnormally.\n"
    "\n"
    " The provided reason will be given and propagated.\n"
).
-spec stop_abnormal(binary()) -> next(any(), any()).
stop_abnormal(Reason) ->
    {stop, {abnormal, gleam_stdlib:identity(Reason)}}.

-file("src/gleam/otp/actor.gleam", 210).
?DOC(
    " Provide a selector to change the messages that the actor is handling\n"
    " going forward. This replaces any selector that was previously given\n"
    " in the actor's `init` callback, or in any previous `Next` value.\n"
).
-spec with_selector(next(EBU, EBV), gleam@erlang@process:selector(EBV)) -> next(EBU, EBV).
with_selector(Value, Selector) ->
    case Value of
        {continue, State, _} ->
            {continue, State, {some, Selector}};

        {stop, _} ->
            Value
    end.

-file("src/gleam/otp/actor.gleam", 268).
?DOC(
    " Takes the post-initialisation state of the actor. This state will be passed\n"
    " to the `on_message` callback each time a message is received.\n"
).
-spec initialised(ECB) -> initialised(ECB, any(), nil).
initialised(State) ->
    {initialised, State, none, nil}.

-file("src/gleam/otp/actor.gleam", 277).
?DOC(
    " Add a selector for the actor to receive messages with.\n"
    "\n"
    " If a message is received by the actor but not selected for with the\n"
    " selector then the actor will discard it and log a warning.\n"
).
-spec selecting(
    initialised(ECG, any(), ECI),
    gleam@erlang@process:selector(ECM)
) -> initialised(ECG, ECM, ECI).
selecting(Initialised, Selector) ->
    {initialised,
        erlang:element(2, Initialised),
        {some, Selector},
        erlang:element(4, Initialised)}.

-file("src/gleam/otp/actor.gleam", 287).
?DOC(
    " Add the data to return to the parent process. This might be a subject that\n"
    " the actor will receive messages over.\n"
).
-spec returning(initialised(ECR, ECS, any()), ECX) -> initialised(ECR, ECS, ECX).
returning(Initialised, Return) ->
    {initialised,
        erlang:element(2, Initialised),
        erlang:element(3, Initialised),
        Return}.

-file("src/gleam/otp/actor.gleam", 329).
?DOC(
    " Create a builder for an actor without a custom initialiser. The actor\n"
    " returns a subject to the parent that can be used to send messages to the\n"
    " actor.\n"
    "\n"
    " If the actor has been given a name with the `named` function then the\n"
    " subject is a named subject.\n"
    "\n"
    " If you wish to create an actor with some other initialisation logic that\n"
    " runs before it starts handling messages, see `new_with_initialiser`.\n"
).
-spec new(EDB) -> builder(EDB, EDC, gleam@erlang@process:subject(EDC)).
new(State) ->
    Initialise = fun(Subject) -> _pipe = initialised(State),
        _pipe@1 = returning(_pipe, Subject),
        {ok, _pipe@1} end,
    {builder, Initialise, 1000, fun(State@1, _) -> continue(State@1) end, none}.

-file("src/gleam/otp/actor.gleam", 358).
?DOC(
    " Create a builder for an actor with a custom initialiser that runs before\n"
    " the start function returns to the parent, and before the actor starts\n"
    " handling messages.\n"
    "\n"
    " The first argument is a number of milliseconds that the initialiser\n"
    " function is expected to return within. If it takes longer the initialiser\n"
    " is considered to have failed and the actor will be killed, and an error\n"
    " will be returned to the parent.\n"
    "\n"
    " The actor's default subject is passed to the initialiser function. You can\n"
    " chose to return it to the parent with `returning`, use it in some other\n"
    " way, or ignore it completely.\n"
    "\n"
    " If a custom selector is given using the `selecting` function then this\n"
    " overwrites the default selector, which selects for the default subject, so\n"
    " you will need to add the subject to the custom selector yourself.\n"
).
-spec new_with_initialiser(
    integer(),
    fun((gleam@erlang@process:subject(EDH)) -> {ok, initialised(EDJ, EDH, EDK)} |
        {error, binary()})
) -> builder(EDJ, EDH, EDK).
new_with_initialiser(Timeout, Initialise) ->
    {builder, Initialise, Timeout, fun(State, _) -> continue(State) end, none}.

-file("src/gleam/otp/actor.gleam", 376).
?DOC(
    " Set the message handler for the actor. This callback function will be\n"
    " called each time the actor receives a message.\n"
    "\n"
    " Actors handle messages sequentially, later messages being handled after the\n"
    " previous one has been handled.\n"
).
-spec on_message(builder(EDT, EDU, EDV), fun((EDT, EDU) -> next(EDT, EDU))) -> builder(EDT, EDU, EDV).
on_message(Builder, Handler) ->
    {builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        Handler,
        erlang:element(5, Builder)}.

-file("src/gleam/otp/actor.gleam", 395).
?DOC(
    " Provide a name for the actor to be registered with when started, enabling\n"
    " it to receive messages via a named subject. This is useful for making\n"
    " processes that can take over from an older one that has exited due to a\n"
    " failure, or to avoid passing subjects from receiver processes to sender\n"
    " processes.\n"
    "\n"
    " If the name is already registered to another process then the actor will\n"
    " fail to start.\n"
    "\n"
    " When this function is used the actor's default subject will be a named\n"
    " subject using this name.\n"
).
-spec named(builder(EEE, EEF, EEG), gleam@erlang@process:name(EEF)) -> builder(EEE, EEF, EEG).
named(Builder, Name) ->
    {builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        {some, Name}}.

-file("src/gleam/otp/actor.gleam", 402).
-spec exit_process(gleam@erlang@process:exit_reason()) -> gleam@erlang@process:exit_reason().
exit_process(Reason) ->
    case Reason of
        {abnormal, Reason@1} ->
            gleam@erlang@process:send_abnormal_exit(erlang:self(), Reason@1);

        killed ->
            gleam@erlang@process:kill(erlang:self());

        _ ->
            nil
    end,
    Reason.

-file("src/gleam/otp/actor.gleam", 443).
-spec select_system_messages(gleam@erlang@process:selector(message(EET))) -> gleam@erlang@process:selector(message(EET)).
select_system_messages(Selector) ->
    _pipe = Selector,
    gleam@erlang@process:select_record(
        _pipe,
        erlang:binary_to_atom(<<"system"/utf8>>),
        2,
        fun gleam_otp_external:convert_system_message/1
    ).

-file("src/gleam/otp/actor.gleam", 412).
-spec receive_message(self(any(), EEP)) -> message(EEP).
receive_message(Self) ->
    Selector = case erlang:element(2, Self) of
        suspended ->
            _pipe = gleam_erlang_ffi:new_selector(),
            select_system_messages(_pipe);

        running ->
            _pipe@1 = gleam_erlang_ffi:new_selector(),
            _pipe@2 = gleam@erlang@process:select_other(
                _pipe@1,
                fun(Field@0) -> {unexpected, Field@0} end
            ),
            _pipe@3 = gleam_erlang_ffi:merge_selector(
                _pipe@2,
                erlang:element(5, Self)
            ),
            select_system_messages(_pipe@3)
    end,
    gleam_erlang_ffi:select(Selector).

-file("src/gleam/otp/actor.gleam", 453).
-spec process_status_info(self(any(), any())) -> gleam@otp@system:status_info().
process_status_info(Self) ->
    {status_info,
        erlang:binary_to_atom(<<"gleam@otp@actor"/utf8>>),
        erlang:element(3, Self),
        erlang:element(2, Self),
        erlang:element(6, Self),
        gleam_otp_external:identity(erlang:element(4, Self))}.

-file("src/gleam/otp/actor.gleam", 463).
-spec loop(self(any(), any())) -> gleam@erlang@process:exit_reason().
loop(Self) ->
    case receive_message(Self) of
        {system, System} ->
            case System of
                {get_state, Callback} ->
                    Callback(
                        gleam_otp_external:identity(erlang:element(4, Self))
                    ),
                    loop(Self);

                {resume, Callback@1} ->
                    Callback@1(),
                    loop(
                        {self,
                            running,
                            erlang:element(3, Self),
                            erlang:element(4, Self),
                            erlang:element(5, Self),
                            erlang:element(6, Self),
                            erlang:element(7, Self)}
                    );

                {suspend, Callback@2} ->
                    Callback@2(),
                    loop(
                        {self,
                            suspended,
                            erlang:element(3, Self),
                            erlang:element(4, Self),
                            erlang:element(5, Self),
                            erlang:element(6, Self),
                            erlang:element(7, Self)}
                    );

                {get_status, Callback@3} ->
                    Callback@3(process_status_info(Self)),
                    loop(Self)
            end;

        {unexpected, Message} ->
            logger:warning(
                unicode:characters_to_list(
                    <<"Actor discarding unexpected message: ~s"/utf8>>
                ),
                [unicode:characters_to_list(gleam@string:inspect(Message))]
            ),
            loop(Self);

        {message, Msg} ->
            case (erlang:element(7, Self))(erlang:element(4, Self), Msg) of
                {stop, Reason} ->
                    exit_process(Reason);

                {continue, State, New_selector} ->
                    Selector = case New_selector of
                        none ->
                            erlang:element(5, Self);

                        {some, S} ->
                            gleam_erlang_ffi:map_selector(
                                S,
                                fun(Field@0) -> {message, Field@0} end
                            )
                    end,
                    loop(
                        {self,
                            erlang:element(2, Self),
                            erlang:element(3, Self),
                            State,
                            Selector,
                            erlang:element(6, Self),
                            erlang:element(7, Self)}
                    )
            end
    end.

-file("src/gleam/otp/actor.gleam", 573).
-spec try_register_self(gleam@erlang@process:name(any())) -> {ok, nil} |
    {error, binary()}.
try_register_self(Name) ->
    case gleam_erlang_ffi:register_process(erlang:self(), Name) of
        {ok, nil} ->
            {ok, nil};

        {error, _} ->
            {error, <<"name already registered"/utf8>>}
    end.

-file("src/gleam/otp/actor.gleam", 522).
-spec initialise_actor(
    builder(any(), any(), EFM),
    gleam@erlang@process:pid_(),
    gleam@erlang@process:subject({ok, EFM} | {error, binary()})
) -> gleam@erlang@process:exit_reason().
initialise_actor(Builder, Parent, Ack) ->
    Result@1 = begin
        gleam@result:'try'(case erlang:element(5, Builder) of
                none ->
                    {ok, gleam@erlang@process:new_subject()};

                {some, Name} ->
                    gleam@result:'try'(
                        try_register_self(Name),
                        fun(_) ->
                            {ok, gleam@erlang@process:named_subject(Name)}
                        end
                    )
            end, fun(Subject) ->
                gleam@result:'try'(
                    (erlang:element(2, Builder))(Subject),
                    fun(Result) -> {ok, {Subject, Result}} end
                )
            end)
    end,
    case Result@1 of
        {ok, {Subject@1, {initialised, State, Selector, Return}}} ->
            Selector@2 = case Selector of
                {some, Selector@1} ->
                    Selector@1;

                none ->
                    _pipe = gleam_erlang_ffi:new_selector(),
                    gleam@erlang@process:select(_pipe, Subject@1)
            end,
            Selector@3 = gleam_erlang_ffi:map_selector(
                Selector@2,
                fun(Field@0) -> {message, Field@0} end
            ),
            gleam@erlang@process:send(Ack, {ok, Return}),
            Self = {self,
                running,
                Parent,
                State,
                Selector@3,
                sys:debug_options([]),
                erlang:element(4, Builder)},
            loop(Self);

        {error, Reason} ->
            gleam@erlang@process:send(Ack, {error, Reason}),
            exit_process(normal)
    end.

-file("src/gleam/otp/actor.gleam", 592).
?DOC(" Starts an actor from a given `Builder`. On failure, `start` returns a `StartError`\n").
-spec start(builder(any(), any(), EFZ)) -> {ok, started(EFZ)} |
    {error, start_error()}.
start(Builder) ->
    Timeout = erlang:element(3, Builder),
    Ack_subject = gleam@erlang@process:new_subject(),
    Self = erlang:self(),
    Child = proc_lib:spawn_link(
        fun() -> initialise_actor(Builder, Self, Ack_subject) end
    ),
    Monitor = gleam@erlang@process:monitor(Child),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = gleam@erlang@process:select_map(
            _pipe,
            Ack_subject,
            fun(Field@0) -> {ack, Field@0} end
        ),
        gleam@erlang@process:select_specific_monitor(
            _pipe@1,
            Monitor,
            fun(Field@0) -> {mon, Field@0} end
        )
    end,
    Result = case gleam_erlang_ffi:select(Selector, Timeout) of
        {ok, {ack, {ok, Subject}}} ->
            {ok, Subject};

        {ok, {ack, {error, Reason}}} ->
            {error, {init_failed, Reason}};

        {ok, {mon, Down}} ->
            {error, {init_exited, erlang:element(4, Down)}};

        {error, nil} ->
            gleam@erlang@process:unlink(Child),
            gleam@erlang@process:kill(Child),
            {error, init_timeout}
    end,
    gleam@erlang@process:demonitor_process(Monitor),
    case Result of
        {ok, Data} ->
            {ok, {started, Child, Data}};

        {error, Error} ->
            {error, Error}
    end.

-file("src/gleam/otp/actor.gleam", 642).
?DOC(
    " Send a message over a given channel.\n"
    "\n"
    " This is a re-export of `process.send`, for the sake of convenience.\n"
).
-spec send(gleam@erlang@process:subject(EGG), EGG) -> nil.
send(Subject, Msg) ->
    gleam@erlang@process:send(Subject, Msg).

-file("src/gleam/otp/actor.gleam", 654).
?DOC(
    " Send a synchronous message and wait for a response from the receiving\n"
    " process.\n"
    "\n"
    " If a reply is not received within the given timeout then the sender process\n"
    " crashes rather than leaving the processes in an invalid state. \n"
    "\n"
    " This is a re-export of `process.call`, for the sake of convenience.\n"
).
-spec call(
    gleam@erlang@process:subject(EGI),
    integer(),
    fun((gleam@erlang@process:subject(EGK)) -> EGI)
) -> EGK.
call(Subject, Timeout, Make_message) ->
    gleam@erlang@process:call(Subject, Timeout, Make_message).
