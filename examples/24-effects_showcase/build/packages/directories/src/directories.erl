-module(directories).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([tmp_dir/0, home_dir/0, cache_dir/0, config_dir/0, config_local_dir/0, data_dir/0, data_local_dir/0, executable_dir/0, preference_dir/0, runtime_dir/0, state_dir/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/directories.gleam", 16).
?DOC(" Return the first directory from the list that exists, or Nil\n").
-spec check_dirs(list(binary())) -> {ok, binary()} | {error, nil}.
check_dirs(Paths) ->
    _pipe = Paths,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(A) ->
            not gleam@string:is_empty(A) andalso gleam@result:unwrap(
                simplifile_erl:is_directory(A),
                false
            )
        end
    ),
    gleam@list:first(_pipe@1).

-file("src/directories.gleam", 11).
?DOC(
    " Return the first environment variable from the list\n"
    " that is set and is a valid directory\n"
).
-spec check_dir_from_env(list(binary())) -> {ok, binary()} | {error, nil}.
check_dir_from_env(Vars) ->
    _pipe = Vars,
    _pipe@1 = gleam@list:filter_map(_pipe, fun envoy_ffi:get/1),
    check_dirs(_pipe@1).

-file("src/directories.gleam", 24).
-spec get_env(binary()) -> binary().
get_env(Var) ->
    gleam@result:unwrap(envoy_ffi:get(Var), <<""/utf8>>).

-file("src/directories.gleam", 28).
-spec home_dir_path(binary()) -> binary().
home_dir_path(Path) ->
    <<(get_env(<<"HOME"/utf8>>))/binary, Path/binary>>.

-file("src/directories.gleam", 32).
-spec other_os_message(binary()) -> {ok, binary()} | {error, nil}.
other_os_message(Other_os) ->
    gleam@io:print_error(
        <<<<"[WARN][directories] Operating system '"/utf8, Other_os/binary>>/binary,
            "' is not supported by this library"/utf8>>
    ),
    {error, nil}.

-file("src/directories.gleam", 48).
?DOC(
    " Returns the path to a temporary directory\n"
    " \n"
    " It'll first check `%TMPDIR%`, `%TEMP%`, `%TMP%`, and return the first one that is a valid directory\n"
    " \n"
    " If that fails, It'll check `C:\\TEMP`, `C:\\TMP`, `\\TEMP`, `\\TMP` on windows.\n"
    " \n"
    " On MacOS, Linux, and FreeBSD, it'll check `/tmp`, `/var/tmp`, `/usr/tmp`,\n"
).
-spec tmp_dir() -> {ok, binary()} | {error, nil}.
tmp_dir() ->
    case check_dir_from_env(
        [<<"TMPDIR"/utf8>>, <<"TEMP"/utf8>>, <<"TMP"/utf8>>]
    ) of
        {ok, Path} ->
            {ok, Path};

        {error, nil} ->
            case platform:os() of
                win32 ->
                    check_dirs(
                        [<<"C:\\TEMP"/utf8>>,
                            <<"C:\\TMP"/utf8>>,
                            <<"\\TEMP"/utf8>>,
                            <<"\\TMP"/utf8>>]
                    );

                darwin ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                linux ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                free_bsd ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                open_bsd ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                sun_os ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                aix ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                {other_os, Os} ->
                    other_os_message(Os)
            end
    end.

-file("src/directories.gleam", 74).
?DOC(
    " Returns the path to the user's home directory\n"
    " \n"
    " It'll check `%UserProfile%` and `%Profile%` on windows, returning first one that is a valid directory\n"
    " \n"
    " On MacOS, Linux, and FreeBSD, it'll return the value of `$HOME` if it exists\n"
).
-spec home_dir() -> {ok, binary()} | {error, nil}.
home_dir() ->
    case platform:os() of
        win32 ->
            check_dir_from_env([<<"UserProfile"/utf8>>, <<"Profile"/utf8>>]);

        darwin ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        linux ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        free_bsd ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        open_bsd ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        sun_os ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        aix ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        {other_os, Os} ->
            other_os_message(Os)
    end.

-file("src/directories.gleam", 94).
?DOC(
    " Returns the path to the user-specific cache directory\n"
    " \n"
    " On Windows, it'll return the value of `%APPDATA%` if it exists\n"
    " \n"
    " On MacOS, it'll return value of `$HOME/Library/Caches` if it exists\n"
    " \n"
    " On Linux and FreeBSD, it'll check `$XDG_CACHE_HOME` and `$HOME/.cache`, returning the first one that is a valid directory\n"
).
-spec cache_dir() -> {ok, binary()} | {error, nil}.
cache_dir() ->
    case platform:os() of
        win32 ->
            check_dir_from_env([<<"APPDATA"/utf8>>]);

        darwin ->
            check_dirs(
                [<<(get_env(<<"HOME"/utf8>>))/binary, "/Library/Caches"/utf8>>]
            );

        linux ->
            check_dirs(
                [get_env(<<"XDG_CACHE_HOME"/utf8>>),
                    home_dir_path(<<"/.cache"/utf8>>)]
            );

        free_bsd ->
            check_dirs(
                [get_env(<<"XDG_CACHE_HOME"/utf8>>),
                    home_dir_path(<<"/.cache"/utf8>>)]
            );

        open_bsd ->
            check_dirs(
                [get_env(<<"XDG_CACHE_HOME"/utf8>>),
                    home_dir_path(<<"/.cache"/utf8>>)]
            );

        sun_os ->
            check_dirs(
                [get_env(<<"XDG_CACHE_HOME"/utf8>>),
                    home_dir_path(<<"/.cache"/utf8>>)]
            );

        aix ->
            check_dirs(
                [get_env(<<"XDG_CACHE_HOME"/utf8>>),
                    home_dir_path(<<"/.cache"/utf8>>)]
            );

        {other_os, Os} ->
            other_os_message(Os)
    end.

-file("src/directories.gleam", 115).
?DOC(
    " Returns the path to the user-specific config directory. This directory may be synced across computers\n"
    " \n"
    " On Windows, it'll return the value of `%APPDATA%` if it exists\n"
    " \n"
    " On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists\n"
    " \n"
    " On Linux and FreeBSD, it'll check `$XDG_CONFIG_HOME` and `$HOME/.config`, returning the first one that is a valid directory\n"
).
-spec config_dir() -> {ok, binary()} | {error, nil}.
config_dir() ->
    case platform:os() of
        win32 ->
            check_dir_from_env([<<"APPDATA"/utf8>>]);

        darwin ->
            check_dirs(
                [<<(get_env(<<"HOME"/utf8>>))/binary,
                        "/Library/Application Support"/utf8>>]
            );

        linux ->
            check_dirs(
                [get_env(<<"XDG_CONFIG_HOME"/utf8>>),
                    home_dir_path(<<"/.config"/utf8>>)]
            );

        free_bsd ->
            check_dirs(
                [get_env(<<"XDG_CONFIG_HOME"/utf8>>),
                    home_dir_path(<<"/.config"/utf8>>)]
            );

        open_bsd ->
            check_dirs(
                [get_env(<<"XDG_CONFIG_HOME"/utf8>>),
                    home_dir_path(<<"/.config"/utf8>>)]
            );

        sun_os ->
            check_dirs(
                [get_env(<<"XDG_CONFIG_HOME"/utf8>>),
                    home_dir_path(<<"/.config"/utf8>>)]
            );

        aix ->
            check_dirs(
                [get_env(<<"XDG_CONFIG_HOME"/utf8>>),
                    home_dir_path(<<"/.config"/utf8>>)]
            );

        {other_os, Os} ->
            other_os_message(Os)
    end.

-file("src/directories.gleam", 137).
?DOC(
    " Returns the path to the user-specific local config directory. Similar to `config_dir`, except Windows won't sync it when connected to a domain with a roaming profile\n"
    " \n"
    " On Windows, it'll return the value of `%LOCALAPPDATA%` if it exists\n"
    " \n"
    " On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists\n"
    " \n"
    " On Linux and FreeBSD, it'll check `$XDG_CONFIG_HOME` and `$HOME/.config`, returning the first one that is a valid directory\n"
).
-spec config_local_dir() -> {ok, binary()} | {error, nil}.
config_local_dir() ->
    case platform:os() of
        win32 ->
            check_dir_from_env([<<"LOCALAPPDATA"/utf8>>]);

        _ ->
            config_dir()
    end.

-file("src/directories.gleam", 151).
?DOC(
    " Returns the path to the user-specific data directory. This directory may be synced across computers\n"
    " \n"
    " On Windows, it'll return the value of `%APPDATA%` if it exists\n"
    " \n"
    " On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists\n"
    " \n"
    " On Linux and FreeBSD, it'll check `$XDG_DATA_HOME``and $HOME/.local/share, returning the first one that is a valid directory\n"
).
-spec data_dir() -> {ok, binary()} | {error, nil}.
data_dir() ->
    case platform:os() of
        linux ->
            check_dirs(
                [get_env(<<"XDG_DATA_HOME"/utf8>>),
                    home_dir_path(<<"/.local/share"/utf8>>)]
            );

        free_bsd ->
            check_dirs(
                [get_env(<<"XDG_DATA_HOME"/utf8>>),
                    home_dir_path(<<"/.local/share"/utf8>>)]
            );

        _ ->
            config_dir()
    end.

-file("src/directories.gleam", 166).
?DOC(
    " Returns the path to the user-specific data directory. Similar to `data_dir`, except Windows won't sync it when connected to a domain with a roaming profile\n"
    " \n"
    " On Windows, it'll return the value of `%LOCALAPPDATA%` if it exists\n"
    " \n"
    " On MacOS, it'll return the value of `$HOME/Library/Application Support` if it exists\n"
    " \n"
    " On Linux and FreeBSD, it'll check DG_DATA_HOME ```$H`````ocal/share, r```g``` the first one that is a valid directory\n"
).
-spec data_local_dir() -> {ok, binary()} | {error, nil}.
data_local_dir() ->
    case platform:os() of
        win32 ->
            check_dir_from_env([<<"LOCALAPPDATA"/utf8>>]);

        _ ->
            data_dir()
    end.

-file("src/directories.gleam", 178).
?DOC(
    " Returns the path to which user-specific executable files may be written. \n"
    " \n"
    " On Linux and FreeBSD, it'll check $XDG_BIN_HOME, $HOME/.local/bin, $XDG_DATA_HOME/../bin and return the first one that is a valid directory\n"
    " \n"
    " On all other platforms, it'll always return `Error(Nil)`\n"
).
-spec executable_dir() -> {ok, binary()} | {error, nil}.
executable_dir() ->
    case platform:os() of
        win32 ->
            {error, nil};

        darwin ->
            {error, nil};

        linux ->
            check_dirs(
                [get_env(<<"XDG_BIN_HOME"/utf8>>),
                    home_dir_path(<<"/.local/bin"/utf8>>),
                    <<(get_env(<<"XDG_DATA_HOME"/utf8>>))/binary,
                        "../bin"/utf8>>]
            );

        free_bsd ->
            check_dirs(
                [get_env(<<"XDG_BIN_HOME"/utf8>>),
                    home_dir_path(<<"/.local/bin"/utf8>>),
                    <<(get_env(<<"XDG_DATA_HOME"/utf8>>))/binary,
                        "../bin"/utf8>>]
            );

        open_bsd ->
            check_dirs(
                [get_env(<<"XDG_BIN_HOME"/utf8>>),
                    home_dir_path(<<"/.local/bin"/utf8>>),
                    <<(get_env(<<"XDG_DATA_HOME"/utf8>>))/binary,
                        "../bin"/utf8>>]
            );

        sun_os ->
            check_dirs(
                [get_env(<<"XDG_BIN_HOME"/utf8>>),
                    home_dir_path(<<"/.local/bin"/utf8>>),
                    <<(get_env(<<"XDG_DATA_HOME"/utf8>>))/binary,
                        "../bin"/utf8>>]
            );

        aix ->
            check_dirs(
                [get_env(<<"XDG_BIN_HOME"/utf8>>),
                    home_dir_path(<<"/.local/bin"/utf8>>),
                    <<(get_env(<<"XDG_DATA_HOME"/utf8>>))/binary,
                        "../bin"/utf8>>]
            );

        {other_os, Os} ->
            other_os_message(Os)
    end.

-file("src/directories.gleam", 202).
?DOC(
    " Returns the path to the user-specific preferences directory. This directory may be synced across computers\n"
    " \n"
    " On Windows, it'll return the value of `%APPDATA%` if it exists\n"
    " \n"
    " On MacOS, it'll return the value of `$HOME/Library/Preferences` if it exists\n"
    " \n"
    " On Linux and FreeBSD, it'll check $XDG_CONFIG_HOME and $HOME/.config, returning the first one that is a valid directory\n"
).
-spec preference_dir() -> {ok, binary()} | {error, nil}.
preference_dir() ->
    case platform:os() of
        darwin ->
            check_dirs([home_dir_path(<<"/Library/Preferences"/utf8>>)]);

        _ ->
            config_dir()
    end.

-file("src/directories.gleam", 214).
?DOC(
    " Returns the path to which user-specific runtime files and other file objects may be placed. \n"
    " \n"
    " On Linux and FreeBSD, it'll check $XDG_RUNTIME_DIR if it is a valid directory\n"
    " \n"
    " On all other platforms, it'll always return `Error(Nil)`\n"
).
-spec runtime_dir() -> {ok, binary()} | {error, nil}.
runtime_dir() ->
    case platform:os() of
        win32 ->
            {error, nil};

        darwin ->
            {error, nil};

        linux ->
            check_dir_from_env([<<"XDG_RUNTIME_DIR"/utf8>>]);

        free_bsd ->
            check_dir_from_env([<<"XDG_RUNTIME_DIR"/utf8>>]);

        open_bsd ->
            check_dir_from_env([<<"XDG_RUNTIME_DIR"/utf8>>]);

        sun_os ->
            check_dir_from_env([<<"XDG_RUNTIME_DIR"/utf8>>]);

        aix ->
            check_dir_from_env([<<"XDG_RUNTIME_DIR"/utf8>>]);

        {other_os, Os} ->
            other_os_message(Os)
    end.

-file("src/directories.gleam", 234).
?DOC(
    " Returns the path to which user-specific state may be stored. \n"
    " \n"
    " The state directory contains data that should be retained between sessions (unlike the runtime directory), \n"
    " but may not be important/portable enough to be synchronized across machines (unlike the config/preferences/data directories).\n"
    " \n"
    " On Linux and FreeBSD, it'll check $XDG_STATE_HOME and $HOME/.local/state, returning the first one that is a valid directory\n"
    " \n"
    " On all other platforms, it'll always return `Error(Nil)`\n"
).
-spec state_dir() -> {ok, binary()} | {error, nil}.
state_dir() ->
    case platform:os() of
        win32 ->
            {error, nil};

        darwin ->
            {error, nil};

        linux ->
            check_dirs(
                [get_env(<<"XDG_STATE_HOME"/utf8>>),
                    home_dir_path(<<"/.local/state"/utf8>>)]
            );

        free_bsd ->
            check_dirs(
                [get_env(<<"XDG_STATE_HOME"/utf8>>),
                    home_dir_path(<<"/.local/state"/utf8>>)]
            );

        open_bsd ->
            check_dirs(
                [get_env(<<"XDG_STATE_HOME"/utf8>>),
                    home_dir_path(<<"/.local/state"/utf8>>)]
            );

        sun_os ->
            check_dirs(
                [get_env(<<"XDG_STATE_HOME"/utf8>>),
                    home_dir_path(<<"/.local/state"/utf8>>)]
            );

        aix ->
            check_dirs(
                [get_env(<<"XDG_STATE_HOME"/utf8>>),
                    home_dir_path(<<"/.local/state"/utf8>>)]
            );

        {other_os, Os} ->
            other_os_message(Os)
    end.
