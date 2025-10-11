-module(gleam_time_ffi).
-export([system_time/0, local_time_offset_seconds/0]).

system_time() ->
    {0, erlang:system_time(nanosecond)}.

local_time_offset_seconds() ->
    Utc = calendar:universal_time(),
    Local = calendar:local_time(),
    UtcSeconds = calendar:datetime_to_gregorian_seconds(Utc),
    LocalSeconds = calendar:datetime_to_gregorian_seconds(Local),
    LocalSeconds - UtcSeconds.
