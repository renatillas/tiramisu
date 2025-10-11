-module(booklet_ffi).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1, start_worker/0]).

-export([make/1, get/1, update/2]).


% TODO: Query the worker again for the table if the table ref is invalid
% TODO: store default_value inside the tuple so we can always recover

% -- Public API ----------------------------------------------------------------


make(DefaultValue) ->
    Table = get_table(),
    Key = make_ref(),
    true = ets:insert_new(Table, {Key, 0, DefaultValue}),
    {Table, Key}.


get({Table, Key}) ->
    ets:lookup_element(Table, Key, 3).


update({Table, Key}, Updater) ->
    [{Key, Rev, Value}] = ets:lookup(Table, Key),
    NewValue = Updater(Value),
    Pattern = {Key, Rev, '_'},
    Replacement = {const, {Key, Rev + 1, NewValue}},
    case ets:select_replace(Table, [{Pattern, [], [Replacement]}]) of
        0 -> update({Table, Key}, Updater);
        _ -> NewValue
    end.


% -- APPLICATION ---------------------------------------------------------------

start(_StartType, _StartArgs) ->
    supervisor:start_link(?MODULE, supervisor).

stop(_State) ->
    ok.

% -- SUPERVISOR ----------------------------------------------------------------

init(supervisor) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 6,
        period => 3600
    },
    ChildSpec = #{
        id => booklet,
        start => {?MODULE, start_worker, []},
        shutdown => brutal_kill
    },
    {ok, {SupFlags, [ChildSpec]}}.

% -- CHILD ---------------------------------------------------------------------

start_worker() ->
    Pid = spawn_link(fun worker/0),
    true = register(booklet, Pid),
    {ok, Pid}.

worker() -> 
    Table = ets:new(booklet, [set, public, {keypos, 1}, {write_concurrency, auto}, {read_concurrency, true}]),
    worker_loop(Table).

worker_loop(Table) ->
    receive
        {get, From, Ref} -> From ! {Ref, Table}, worker_loop(Table);
        _ -> worker_loop(Table)
    end.

get_table() ->
    Ref = make_ref(),
    booklet ! {get, self(), Ref},
    receive {Ref, Table} -> Table end.
