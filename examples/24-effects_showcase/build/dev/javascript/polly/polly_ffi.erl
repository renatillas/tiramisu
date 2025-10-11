-module(polly_ffi).
-export([
    repeatedly/3
]).

repeatedly(Timeout, State, Callback) ->
    Pid = spawn(fun() -> do_repeatedly(Timeout, State, Callback) end),
    Stop = fun() -> Pid ! stop end,
    Stop.
    

do_repeatedly(Timeout, State, Callback) -> 
    receive
        stop -> ok
        after Timeout ->
            NewState = Callback(State),
            do_repeatedly(Timeout, NewState, Callback)
    end.
