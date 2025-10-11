-module(exception_ffi).

-export([rescue/1, defer/2, on_crash/2]).

rescue(F) ->
    try {ok, F()}
    catch
      error:Term -> {error, {errored, Term}};
      throw:Term -> {error, {thrown, Term}};
      exit:Term -> {error, {exited, Term}}
    end.

defer(Cleanup, Body) ->
    try Body()
    after Cleanup()
    end.

on_crash(Cleanup, Body) ->
    try Body()
    catch A:B:C ->
        Cleanup(), 
        erlang:raise(A,B,C)
    end.
