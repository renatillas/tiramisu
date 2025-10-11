-module(filepath_ffi).

-export([is_windows/0]).

is_windows() ->
    case os:type() of
        {win32, _} -> true;
        _ -> false
    end.
