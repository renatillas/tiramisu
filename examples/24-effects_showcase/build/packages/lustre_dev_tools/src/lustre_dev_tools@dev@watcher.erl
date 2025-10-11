-module(lustre_dev_tools@dev@watcher).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/dev/watcher.gleam").
-export([start/4, subscribe/2, unsubscribe/2]).
-export_type([event/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type event() :: {change, binary(), binary()} |
    styles |
    {build_error, lustre_dev_tools@error:error()}.

-file("src/lustre_dev_tools/dev/watcher.gleam", 116).
?DOC(false).
-spec handle_change(
    lustre_dev_tools@project:project(),
    booklet:booklet(gleam@option:option(lustre_dev_tools@error:error())),
    binary(),
    binary()
) -> event().
handle_change(Project, Error, Dir, Path) ->
    Result = begin
        gleam@result:'try'(
            lustre_dev_tools@bin@gleam:build(Project),
            fun(_) -> gleam@result:'try'(case erlang:element(10, Project) of
                        true ->
                            Module = begin
                                _pipe = <<"import { main } from '../../build/dev/javascript/${name}/${entry}.mjs'; main();"/utf8>>,
                                _pipe@1 = gleam@string:replace(
                                    _pipe,
                                    <<"${name}"/utf8>>,
                                    erlang:element(2, Project)
                                ),
                                gleam@string:replace(
                                    _pipe@1,
                                    <<"${entry}"/utf8>>,
                                    erlang:element(2, Project)
                                )
                            end,
                            Name = <<(justin:snake_case(
                                    erlang:element(2, Project)
                                ))/binary,
                                ".dev.mjs"/utf8>>,
                            Path@1 = filepath:join(
                                erlang:element(9, Project),
                                Name
                            ),
                            gleam@result:'try'(
                                begin
                                    _pipe@2 = simplifile:write(Path@1, Module),
                                    gleam@result:map_error(
                                        _pipe@2,
                                        fun(_capture) ->
                                            {could_not_write_file,
                                                Path@1,
                                                _capture}
                                        end
                                    )
                                end,
                                fun(_) ->
                                    lustre_dev_tools@bin@bun:build(
                                        Project,
                                        [Path@1],
                                        filepath:join(
                                            erlang:element(4, Project),
                                            <<"build/dev/javascript"/utf8>>
                                        ),
                                        false,
                                        true
                                    )
                                end
                            );

                        false ->
                            {ok, nil}
                    end, fun(_) -> {ok, nil} end) end
        )
    end,
    case Result of
        {ok, _} ->
            case booklet_ffi:get(Error) of
                {some, _} ->
                    lustre_dev_tools@cli:success(
                        <<"Appliction successfully rebuilt."/utf8>>,
                        false
                    );

                none ->
                    nil
            end,
            booklet:set(Error, none),
            {change, Dir, Path};

        {error, Reason} ->
            booklet:set(Error, {some, Reason}),
            gleam_stdlib:println_error(
                gleam_community@ansi:grey(<<(case Reason of
                            {external_command_failed,
                                <<"gleam"/utf8>>,
                                Reason@1} ->
                                Reason@1;

                            _ ->
                                lustre_dev_tools@error:explain(Reason)
                        end)/binary, "\n"/utf8>>)
            ),
            {build_error, Reason}
    end.

-file("src/lustre_dev_tools/dev/watcher.gleam", 75).
?DOC(false).
-spec start_bun_watcher(
    lustre_dev_tools@project:project(),
    booklet:booklet(gleam@option:option(lustre_dev_tools@error:error())),
    list(binary()),
    group_registry:group_registry(event())
) -> {ok, gleam@erlang@process:subject(lustre_dev_tools@port:message())} |
    {error, lustre_dev_tools@error:error()}.
start_bun_watcher(Project, Error, Watch, Registry) ->
    lustre_dev_tools@bin@bun:watch(
        Project,
        Watch,
        fun(Dir, Path) ->
            Event = handle_change(Project, Error, Dir, Path),
            _pipe = group_registry:members(Registry, <<"watch"/utf8>>),
            gleam@list:each(
                _pipe,
                fun(_capture) -> gleam@erlang@process:send(_capture, Event) end
            )
        end
    ).

-file("src/lustre_dev_tools/dev/watcher.gleam", 88).
?DOC(false).
-spec start_polly_watcher(
    lustre_dev_tools@project:project(),
    booklet:booklet(gleam@option:option(lustre_dev_tools@error:error())),
    list(binary()),
    group_registry:group_registry(event())
) -> {ok, polly:watcher()} | {error, list({binary(), simplifile:file_error()})}.
start_polly_watcher(Project, Error, Watch, Registry) ->
    {First@1, Rest@1} = case Watch of
        [First | Rest] -> {First, Rest};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"lustre_dev_tools/dev/watcher"/utf8>>,
                        function => <<"start_polly_watcher"/utf8>>,
                        line => 94,
                        value => _assert_fail,
                        start => 2383,
                        'end' => 2417,
                        pattern_start => 2394,
                        pattern_end => 2409})
    end,
    Polly = begin
        _pipe = polly:new(),
        _pipe@1 = polly:add_dir(_pipe, First@1),
        _pipe@2 = polly:ignore_initial_missing(_pipe@1),
        gleam@list:fold(Rest@1, _pipe@2, fun polly:add_dir/2)
    end,
    polly:watch(Polly, fun(Change) -> case Change of
                {changed, Path} ->
                    Dir@1 = case gleam@list:find(
                        Watch,
                        fun(_capture) ->
                            gleam_stdlib:string_starts_with(
                                erlang:element(2, Change),
                                _capture
                            )
                        end
                    ) of
                        {ok, Dir} -> Dir;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"lustre_dev_tools/dev/watcher"/utf8>>,
                                        function => <<"start_polly_watcher"/utf8>>,
                                        line => 105,
                                        value => _assert_fail@1,
                                        start => 2688,
                                        'end' => 2761,
                                        pattern_start => 2699,
                                        pattern_end => 2706})
                    end,
                    Event = handle_change(Project, Error, Dir@1, Path),
                    _pipe@3 = group_registry:members(Registry, <<"watch"/utf8>>),
                    gleam@list:each(
                        _pipe@3,
                        fun(_capture@1) ->
                            gleam@erlang@process:send(_capture@1, Event)
                        end
                    );

                {created, Path} ->
                    Dir@1 = case gleam@list:find(
                        Watch,
                        fun(_capture) ->
                            gleam_stdlib:string_starts_with(
                                erlang:element(2, Change),
                                _capture
                            )
                        end
                    ) of
                        {ok, Dir} -> Dir;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"lustre_dev_tools/dev/watcher"/utf8>>,
                                        function => <<"start_polly_watcher"/utf8>>,
                                        line => 105,
                                        value => _assert_fail@1,
                                        start => 2688,
                                        'end' => 2761,
                                        pattern_start => 2699,
                                        pattern_end => 2706})
                    end,
                    Event = handle_change(Project, Error, Dir@1, Path),
                    _pipe@3 = group_registry:members(Registry, <<"watch"/utf8>>),
                    gleam@list:each(
                        _pipe@3,
                        fun(_capture@1) ->
                            gleam@erlang@process:send(_capture@1, Event)
                        end
                    );

                {deleted, Path} ->
                    Dir@1 = case gleam@list:find(
                        Watch,
                        fun(_capture) ->
                            gleam_stdlib:string_starts_with(
                                erlang:element(2, Change),
                                _capture
                            )
                        end
                    ) of
                        {ok, Dir} -> Dir;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"lustre_dev_tools/dev/watcher"/utf8>>,
                                        function => <<"start_polly_watcher"/utf8>>,
                                        line => 105,
                                        value => _assert_fail@1,
                                        start => 2688,
                                        'end' => 2761,
                                        pattern_start => 2699,
                                        pattern_end => 2706})
                    end,
                    Event = handle_change(Project, Error, Dir@1, Path),
                    _pipe@3 = group_registry:members(Registry, <<"watch"/utf8>>),
                    gleam@list:each(
                        _pipe@3,
                        fun(_capture@1) ->
                            gleam@erlang@process:send(_capture@1, Event)
                        end
                    );

                {error, _, _} ->
                    nil
            end end).

-file("src/lustre_dev_tools/dev/watcher.gleam", 38).
?DOC(false).
-spec start(
    lustre_dev_tools@project:project(),
    booklet:booklet(gleam@option:option(lustre_dev_tools@error:error())),
    list(binary()),
    gleam@option:option(binary())
) -> {ok, group_registry:group_registry(event())} |
    {error, lustre_dev_tools@error:error()}.
start(Project, Error, Watch, Tailwind_entry) ->
    Name = gleam_erlang_ffi:new_name(<<"registry"/utf8>>),
    Registry@1 = case group_registry:start(Name) of
        {ok, {started, _, Registry}} -> Registry;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"lustre_dev_tools/dev/watcher"/utf8>>,
                        function => <<"start"/utf8>>,
                        line => 45,
                        value => _assert_fail,
                        start => 1122,
                        'end' => 1193,
                        pattern_start => 1133,
                        pattern_end => 1164})
    end,
    gleam@result:'try'(case Tailwind_entry of
            {some, Entry} ->
                lustre_dev_tools@bin@tailwind:watch(
                    Project,
                    Entry,
                    filepath:join(
                        erlang:element(4, Project),
                        <<"build/dev/javascript"/utf8>>
                    ),
                    true,
                    fun() ->
                        _pipe = group_registry:members(
                            Registry@1,
                            <<"watch"/utf8>>
                        ),
                        gleam@list:each(
                            _pipe,
                            fun(_capture) ->
                                gleam@erlang@process:send(_capture, styles)
                            end
                        )
                    end
                );

            none ->
                {ok, nil}
        end, fun(_) ->
            case start_bun_watcher(Project, Error, Watch, Registry@1) of
                {ok, _} ->
                    {ok, Registry@1};

                {error, _} ->
                    _pipe@1 = start_polly_watcher(
                        Project,
                        Error,
                        Watch,
                        Registry@1
                    ),
                    _pipe@2 = gleam@result:replace(_pipe@1, Registry@1),
                    gleam@result:replace_error(
                        _pipe@2,
                        {could_not_start_file_watcher,
                            <<"polly"/utf8>>,
                            system_ffi:detect_os(),
                            system_ffi:detect_arch()}
                    )
            end
        end).

-file("src/lustre_dev_tools/dev/watcher.gleam", 186).
?DOC(false).
-spec subscribe(
    group_registry:group_registry(event()),
    gleam@erlang@process:pid_()
) -> gleam@erlang@process:subject(event()).
subscribe(Registry, Client) ->
    group_registry:join(Registry, <<"watch"/utf8>>, Client).

-file("src/lustre_dev_tools/dev/watcher.gleam", 190).
?DOC(false).
-spec unsubscribe(
    group_registry:group_registry(event()),
    gleam@erlang@process:pid_()
) -> nil.
unsubscribe(Registry, Client) ->
    group_registry:leave(Registry, <<"watch"/utf8>>, [Client]).
