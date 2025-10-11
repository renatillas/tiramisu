-module(platform_ffi).
-export([runtime/0, os/0, arch/0]).

runtime() -> unicode:characters_to_binary("erlang").

os() ->
    Platform =
        case os:type() of
            {win32, nt} ->
                "win32";
            {unix, Os} ->
                erlang:atom_to_list(Os);
            {_, _} ->
                "unknown"
        end,
    list_to_binary(Platform).

arch() ->
    case erlang:system_info(os_type) of
        {unix, _} ->
            [Arch, _] = string:split(erlang:system_info(system_architecture), "-"),
            unicode:characters_to_binary(Arch);
        {win32, _} ->
            case erlang:system_info(wordsize) of
                4 -> <<"ia32">>;
                8 -> <<"x64">>
            end
    end.
