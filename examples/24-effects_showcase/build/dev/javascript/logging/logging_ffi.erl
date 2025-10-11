-module(logging_ffi).
-export([configure/0, format/2]).

configure() ->
    logger:update_primary_config(#{
        level => info,
        filter_default => log,
        filters => [
            {domain,{fun logger_filters:domain/2, {stop, sub, [otp,sasl]}}},
            {domain,{fun logger_filters:domain/2, {stop, sub, [supervisor_report]}}}
        ],
        metadata => #{}
    }),
    logger:update_handler_config(default, #{
        formatter => {logging_ffi, #{colored => is_colored_output()}}
    }),
    nil.

is_colored_output() ->
    not (
        envvar_enabled("NO_COLOUR", false)
        orelse envvar_enabled("NO_COLOR", false)
    ).

envvar_enabled(Name, Default) ->
    case os:getenv(Name) of
        % Not set, use default
        false -> Default;
        % Set to a false value
        "" -> false;
        "false" -> false;
        % Otherwise it is enabled
        _ -> true
    end.

format(#{level := Level, msg := Msg, meta := _Meta}, Config) ->
    [format_level(Level, Config), format_msg(Msg), $\n].

format_level(Level, #{colored := Colored}) ->
    case Level of
        emergency when Colored -> "\x1b[1;41mEMRG\x1b[0m";
        alert when Colored -> "\x1b[1;41mALRT\x1b[0m";
        critical when Colored -> "\x1b[1;41mCRIT\x1b[0m";
        error when Colored -> "\x1b[1;31mEROR\x1b[0m";
        warning when Colored -> "\x1b[1;33mWARN\x1b[0m";
        notice when Colored -> "\x1b[1;32mNTCE\x1b[0m";
        info when Colored -> "\x1b[1;34mINFO\x1b[0m";
        debug when Colored -> "\x1b[1;36mDEBG\x1b[0m";
        emergency -> "EMRG";
        alert -> "ALRT";
        critical -> "CRIT";
        error -> "EROR";
        warning -> "WARN";
        notice -> "NTCE";
        info -> "INFO";
        debug -> "DEBG"
    end.

format_msg(Report0) ->
    case Report0 of
        {string, Msg} ->
            [$\s, Msg];

        {report, Report1} when is_map(Report1) ->
            format_kv(maps:to_list(Report1));

        {report, Report1} when is_list(Report1) ->
            format_kv(Report1);

        _ ->
            [$\s, gleam@string:inspect(Report0)]
    end.

format_kv(Pairs) ->
    case Pairs of
        [] -> [];
        [{K, V} | Rest] when is_atom(K) -> [
            $\s, erlang:atom_to_binary(K), $=, gleam@string:inspect(V)
            | format_kv(Rest)
        ];
        [{K, V} | Rest]  -> [
            $\s, gleam@string:inspect(K), $=, gleam@string:inspect(V)
            | format_kv(Rest)
        ];
        Other -> gleam@string:inspect(Other)
    end.
