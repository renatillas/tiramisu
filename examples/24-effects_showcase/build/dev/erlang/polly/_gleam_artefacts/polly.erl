-module(polly).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/polly.gleam").
-export([add_dir/2, add_file/2, max_depth/2, interval/2, filter/2, default_filter/2, new/0, ignore_initial_missing/1, stop/1, watch_with/3, watch/2]).
-export_type([event/0, options/1, no_watched_dirs/0, has_watched_dirs/0, watcher/0, vfs/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type event() :: {created, binary()} |
    {changed, binary()} |
    {deleted, binary()} |
    {error, binary(), simplifile:file_error()}.

-opaque options(AGCG) :: {options,
        integer(),
        list(binary()),
        integer(),
        fun((simplifile:file_type(), binary()) -> boolean()),
        boolean()} |
    {gleam_phantom, AGCG}.

-type no_watched_dirs() :: any().

-type has_watched_dirs() :: any().

-opaque watcher() :: {watcher, fun(() -> nil)}.

-type vfs() :: {file, binary(), integer()} |
    {folder, binary(), integer(), list(vfs())}.

-file("src/polly.gleam", 96).
?DOC(
    " Tell Polly which directory to watch. If it does not exist, `watch` will return an error.\n"
    " \n"
    " If the directory goes away after watching has started, Polly will continue to\n"
    " check on it to see if it came back.\n"
    " \n"
    " Paths are not expanded by default, so the paths reported by events and passed\n"
    " to the filter function will be prefixed with whatever you specified here.\n"
).
-spec add_dir(options(any()), binary()) -> options(has_watched_dirs()).
add_dir(Options, Path) ->
    {options, Interval, Paths, Max_depth, Filter, Ignore_initial_missing} = Options,
    {options,
        Interval,
        [Path | Paths],
        Max_depth,
        Filter,
        Ignore_initial_missing}.

-file("src/polly.gleam", 112).
?DOC(
    " Tell Polly to watch a single file.\n"
    " \n"
    " Polly doesn't care if you tell her to watch a file or directory, but\n"
    " using this function makes your intent clearer!\n"
).
-spec add_file(options(any()), binary()) -> options(has_watched_dirs()).
add_file(Options, Path) ->
    add_dir(Options, Path).

-file("src/polly.gleam", 127).
?DOC(
    " Limit the maximum depth that Polly will walk each directory.\n"
    "\n"
    " A limit of `0` would mean that Polly _only_ watches the specified list of\n"
    " files or directories. A limit of `1` means that she will also look at the\n"
    " files inside the given directories, but not at any nested directories.\n"
    " \n"
    " There is no limit by default, but setting a limit might be good to\n"
    " better control resource usage of the watcher.\n"
    " \n"
    " Calling this function multiple times will cause polly to only remember the\n"
    " lowest limit provided.\n"
).
-spec max_depth(options(AGCR), integer()) -> options(AGCR).
max_depth(Options, Max_depth) ->
    case (erlang:element(4, Options) < 0) orelse (Max_depth < erlang:element(
        4,
        Options
    )) of
        true ->
            {options,
                erlang:element(2, Options),
                erlang:element(3, Options),
                Max_depth,
                erlang:element(5, Options),
                erlang:element(6, Options)};

        false ->
            Options
    end.

-file("src/polly.gleam", 144).
?DOC(
    " Set the interval in-between file-system polls, in milliseconds.\n"
    " \n"
    " This is the time that Polly rests between calls, so if scanning your directory\n"
    " tree takes 100ms, and you configured 1000ms here, the total time between calls\n"
    " will roughly be 1100ms.\n"
    " \n"
    " Doing it this way makes sure that Polly doesn't stumble over herself.\n"
).
-spec interval(options(AGCU), integer()) -> options(AGCU).
interval(Options, Interval) ->
    case Interval > 0 of
        true ->
            {options,
                Interval,
                erlang:element(3, Options),
                erlang:element(4, Options),
                erlang:element(5, Options),
                erlang:element(6, Options)};

        false ->
            Options
    end.

-file("src/polly.gleam", 166).
?DOC(
    " Filter files using the given predicate.\n"
    " \n"
    " Polly will ignore files and directories for which the predicate returns `False`\n"
    " completely, and any event happening for them or for a contained file of them\n"
    " will not get reported.\n"
    " \n"
    " Keep in mind that the filter is checked for every part of a path, not just\n"
    " leaf nodes! So for example, if you have a path `./src/app.gleam`, your filter\n"
    " function will first be called on `.`, then on `./src`, and then finally on\n"
    " `./src/app.gleam`.\n"
    " \n"
    " By default, all hidden files are ignored by using the `default_filter`.\n"
).
-spec filter(
    options(AGCX),
    fun((simplifile:file_type(), binary()) -> boolean())
) -> options(AGCX).
filter(Options, Filter) ->
    {options,
        erlang:element(2, Options),
        erlang:element(3, Options),
        erlang:element(4, Options),
        Filter,
        erlang:element(6, Options)}.

-file("src/polly.gleam", 174).
?DOC(" The default filter function, ignoring hidden files starting with a colon `\".\"`\n").
-spec default_filter(simplifile:file_type(), binary()) -> boolean().
default_filter(_, Path) ->
    case filepath:base_name(Path) of
        <<"."/utf8>> ->
            true;

        <<".."/utf8>> ->
            true;

        Basename ->
            not gleam_stdlib:string_starts_with(Basename, <<"."/utf8>>)
    end.

-file("src/polly.gleam", 79).
?DOC(
    " Start creating a new configuration using the default options.\n"
    " \n"
    " By default, an interval of 1 second is set, and the `default_filter` is used.\n"
).
-spec new() -> options(no_watched_dirs()).
new() ->
    {options, 1000, [], -1, fun default_filter/2, false}.

-file("src/polly.gleam", 189).
?DOC(
    " Tell Polly that it is fine if a file or directory does not exist initially.\n"
    " \n"
    " By default, if a file or directory cannot be found when calling `watch`,\n"
    " Polly will immediately return to you with an `Enoent` error and refuse to run.\n"
    " \n"
    " When this option is active, Polly will instead note the missing directory,\n"
    " and continuously check if it appears, similarly to how she does after a\n"
    " file or directory goes away after she has first seen it.\n"
).
-spec ignore_initial_missing(options(AGDA)) -> options(AGDA).
ignore_initial_missing(Options) ->
    {options,
        erlang:element(2, Options),
        erlang:element(3, Options),
        erlang:element(4, Options),
        erlang:element(5, Options),
        true}.

-file("src/polly.gleam", 269).
-spec collect_errors(list({binary(), simplifile:file_error()}), event()) -> list({binary(),
    simplifile:file_error()}).
collect_errors(Errors, Event) ->
    case Event of
        {error, Path, Reason} ->
            [{Path, Reason} | Errors];

        _ ->
            Errors
    end.

-file("src/polly.gleam", 283).
?DOC(
    " Stop this watcher.\n"
    " \n"
    " If Polly currently scans your directories, she might not hear you right away\n"
    " and may still report events for one run, after which she will stop.\n"
).
-spec stop(watcher()) -> nil.
stop(Watcher) ->
    (erlang:element(2, Watcher))().

-file("src/polly.gleam", 575).
-spec delete(binary(), vfs(), AGEE, fun((AGEE, event()) -> AGEE)) -> AGEE.
delete(Path, Vfs, State, Emit) ->
    Full_path = filepath:join(Path, erlang:element(2, Vfs)),
    case Vfs of
        {file, _, _} ->
            Emit(State, {deleted, Full_path});

        {folder, _, _, Children} ->
            State@2 = gleam@list:fold(
                Children,
                State,
                fun(State@1, Child) ->
                    delete(Full_path, Child, State@1, Emit)
                end
            ),
            Emit(State@2, {deleted, Full_path})
    end.

-file("src/polly.gleam", 594).
-spec get_modkey(simplifile:file_info()) -> integer().
get_modkey(Stat) ->
    gleam@int:max(erlang:element(10, Stat), erlang:element(11, Stat)).

-file("src/polly.gleam", 598).
-spec readdir(binary()) -> {ok, list(binary())} |
    {error, simplifile:file_error()}.
readdir(Path) ->
    _pipe = simplifile_erl:read_directory(Path),
    gleam@result:map(
        _pipe,
        fun(_capture) ->
            gleam@list:sort(_capture, fun gleam@string:compare/2)
        end
    ).

-file("src/polly.gleam", 461).
-spec diff_children(
    fun((simplifile:file_type(), binary()) -> boolean()),
    integer(),
    binary(),
    list(vfs()),
    list(binary()),
    list(vfs()),
    AGEC,
    fun((AGEC, event()) -> AGEC)
) -> {list(vfs()), AGEC}.
diff_children(
    Filter,
    Depth,
    Path,
    Old_children,
    New_entries,
    New_children,
    State,
    Emit
) ->
    case {Old_children, New_entries} of
        {[], []} ->
            {lists:reverse(New_children), State};

        {[First_old | Rest_old], [First_new | Rest_new]} ->
            case gleam@string:compare(erlang:element(2, First_old), First_new) of
                eq ->
                    case diff(Filter, Depth, Path, First_old, State, Emit) of
                        {{some, New_vfs}, State@1} ->
                            diff_children(
                                Filter,
                                Depth,
                                Path,
                                Rest_old,
                                Rest_new,
                                [New_vfs | New_children],
                                State@1,
                                Emit
                            );

                        {none, State@2} ->
                            diff_children(
                                Filter,
                                Depth,
                                Path,
                                Rest_old,
                                Rest_new,
                                New_children,
                                State@2,
                                Emit
                            )
                    end;

                gt ->
                    {New_vfs@1, State@3} = create(
                        Filter,
                        Depth,
                        Path,
                        First_new,
                        State,
                        Emit
                    ),
                    New_children@1 = case New_vfs@1 of
                        {some, New_vfs@2} ->
                            [New_vfs@2 | New_children];

                        none ->
                            New_children
                    end,
                    diff_children(
                        Filter,
                        Depth,
                        Path,
                        Old_children,
                        Rest_new,
                        New_children@1,
                        State@3,
                        Emit
                    );

                lt ->
                    diff_children(
                        Filter,
                        Depth,
                        Path,
                        Rest_old,
                        New_entries,
                        New_children,
                        delete(Path, First_old, State, Emit),
                        Emit
                    )
            end;

        {[], [First_new@1 | Rest_new@1]} ->
            {New_vfs@3, State@4} = create(
                Filter,
                Depth,
                Path,
                First_new@1,
                State,
                Emit
            ),
            New_children@2 = case New_vfs@3 of
                {some, New_vfs@4} ->
                    [New_vfs@4 | New_children];

                none ->
                    New_children
            end,
            diff_children(
                Filter,
                Depth,
                Path,
                Old_children,
                Rest_new@1,
                New_children@2,
                State@4,
                Emit
            );

        {[First_old@1 | Rest_old@1], []} ->
            diff_children(
                Filter,
                Depth,
                Path,
                Rest_old@1,
                New_entries,
                New_children,
                delete(Path, First_old@1, State, Emit),
                Emit
            )
    end.

-file("src/polly.gleam", 375).
-spec diff(
    fun((simplifile:file_type(), binary()) -> boolean()),
    integer(),
    binary(),
    vfs(),
    AGDX,
    fun((AGDX, event()) -> AGDX)
) -> {gleam@option:option(vfs()), AGDX}.
diff(Filter, Depth, Path, Vfs, State, Emit) ->
    Full_path = filepath:join(Path, erlang:element(2, Vfs)),
    case simplifile_erl:link_info(Full_path) of
        {ok, Stat} ->
            Type_ = simplifile:file_info_type(Stat),
            case Filter(Type_, Full_path) of
                true ->
                    case {Type_, Vfs} of
                        {file, {file, Name, Old_key}} ->
                            New_key = get_modkey(Stat),
                            case New_key =:= Old_key of
                                true ->
                                    {{some, Vfs}, State};

                                false ->
                                    {{some, {file, Name, New_key}},
                                        Emit(State, {changed, Full_path})}
                            end;

                        {directory, {folder, _, _, _}} when Depth =:= 0 ->
                            {{some, Vfs}, State};

                        {directory, {folder, Name@1, _, Old_children}} when Depth =/= 0 ->
                            case readdir(Full_path) of
                                {ok, New_entries} ->
                                    {Children, State@1} = diff_children(
                                        Filter,
                                        Depth - 1,
                                        Full_path,
                                        Old_children,
                                        New_entries,
                                        [],
                                        State,
                                        Emit
                                    ),
                                    {{some,
                                            {folder,
                                                Name@1,
                                                get_modkey(Stat),
                                                Children}},
                                        State@1};

                                {error, enoent} ->
                                    {none, delete(Path, Vfs, State, Emit)};

                                {error, eacces} ->
                                    {none, delete(Path, Vfs, State, Emit)};

                                {error, Reason} ->
                                    {{some, Vfs},
                                        Emit(State, {error, Full_path, Reason})}
                            end;

                        {_, _} ->
                            create_stat(
                                Filter,
                                Depth,
                                erlang:element(2, Vfs),
                                Full_path,
                                Stat,
                                delete(Path, Vfs, State, Emit),
                                Emit
                            )
                    end;

                false ->
                    {none, delete(Path, Vfs, State, Emit)}
            end;

        {error, eacces} ->
            {none, delete(Path, Vfs, State, Emit)};

        {error, enoent} ->
            {none, delete(Path, Vfs, State, Emit)};

        {error, Reason@1} ->
            {{some, Vfs}, Emit(State, {error, Path, Reason@1})}
    end.

-file("src/polly.gleam", 353).
-spec create_children(
    fun((simplifile:file_type(), binary()) -> boolean()),
    integer(),
    binary(),
    list(binary()),
    list(vfs()),
    AGDV,
    fun((AGDV, event()) -> AGDV)
) -> {list(vfs()), AGDV}.
create_children(Filter, Depth, Path, Children, Oks, State, Emit) ->
    case Children of
        [] ->
            {lists:reverse(Oks), State};

        [First | Rest] ->
            case create(Filter, Depth, Path, First, State, Emit) of
                {{some, Vfs}, State@1} ->
                    create_children(
                        Filter,
                        Depth,
                        Path,
                        Rest,
                        [Vfs | Oks],
                        State@1,
                        Emit
                    );

                {none, State@2} ->
                    create_children(
                        Filter,
                        Depth,
                        Path,
                        Rest,
                        Oks,
                        State@2,
                        Emit
                    )
            end
    end.

-file("src/polly.gleam", 294).
-spec create(
    fun((simplifile:file_type(), binary()) -> boolean()),
    integer(),
    binary(),
    binary(),
    AGDP,
    fun((AGDP, event()) -> AGDP)
) -> {gleam@option:option(vfs()), AGDP}.
create(Filter, Depth, Path, Name, State, Emit) ->
    Full_path = filepath:join(Path, Name),
    case simplifile_erl:link_info(Full_path) of
        {ok, Stat} ->
            create_stat(Filter, Depth, Name, Full_path, Stat, State, Emit);

        {error, enoent} ->
            {none, State};

        {error, eacces} ->
            {none, State};

        {error, Reason} ->
            {none, Emit(State, {error, Full_path, Reason})}
    end.

-file("src/polly.gleam", 311).
-spec create_stat(
    fun((simplifile:file_type(), binary()) -> boolean()),
    integer(),
    binary(),
    binary(),
    simplifile:file_info(),
    AGDR,
    fun((AGDR, event()) -> AGDR)
) -> {gleam@option:option(vfs()), AGDR}.
create_stat(Filter, Depth, Name, Full_path, Stat, State, Emit) ->
    Type_ = simplifile:file_info_type(Stat),
    gleam@bool:guard(
        not Filter(Type_, Full_path),
        {none, State},
        fun() -> case Type_ of
                file ->
                    {{some, {file, Name, get_modkey(Stat)}},
                        Emit(State, {created, Full_path})};

                directory when Depth =:= 0 ->
                    {{some, {folder, Name, get_modkey(Stat), []}},
                        Emit(State, {created, Full_path})};

                directory when Depth =/= 0 ->
                    case readdir(Full_path) of
                        {ok, Entries} ->
                            Depth@1 = Depth - 1,
                            State@1 = Emit(State, {created, Full_path}),
                            {Children, State@2} = create_children(
                                Filter,
                                Depth@1,
                                Full_path,
                                Entries,
                                [],
                                State@1,
                                Emit
                            ),
                            {{some, {folder, Name, get_modkey(Stat), Children}},
                                State@2};

                        {error, enoent} ->
                            {none, State};

                        {error, eacces} ->
                            {none, State};

                        {error, Reason} ->
                            {none, Emit(State, {error, Full_path, Reason})}
                    end;

                _ ->
                    {none, State}
            end end
    ).

-file("src/polly.gleam", 219).
?DOC(
    " /// Like `watch`, but similar to `list.fold` Polly will also keep some state\n"
    " around for you and pass it back on each invocation.\n"
).
-spec watch_with(
    options(has_watched_dirs()),
    AGDJ,
    fun((AGDJ, event()) -> AGDJ)
) -> {ok, watcher()} | {error, list({binary(), simplifile:file_error()})}.
watch_with(Options, Initial, Emit) ->
    {options, Interval, Paths, Max_depth, Filter, Ignore_initial_missing} = Options,
    gleam@result:'try'(
        begin
            gleam@list:try_map(
                Paths,
                fun(Path) ->
                    case create(
                        Filter,
                        Max_depth,
                        Path,
                        <<""/utf8>>,
                        [],
                        fun collect_errors/2
                    ) of
                        {{some, Vfs}, []} ->
                            {ok, {Path, {some, Vfs}}};

                        {none, []} ->
                            case Ignore_initial_missing of
                                false ->
                                    {error, [{Path, enoent}]};

                                true ->
                                    {ok, {Path, none}}
                            end;

                        {_, Errors} ->
                            {error, Errors}
                    end
                end
            )
        end,
        fun(Roots) ->
            State = {Roots, Initial},
            Stop = polly_ffi:repeatedly(
                Interval,
                State,
                fun(State@1) ->
                    {Roots@1, State@2} = State@1,
                    gleam@list:fold(
                        Roots@1,
                        {[], State@2},
                        fun(_use0, _use1) ->
                            {Roots@2, State@3} = _use0,
                            {Path@1, Vfs@1} = _use1,
                            case Vfs@1 of
                                {some, Vfs@2} ->
                                    {New_vfs, State@4} = diff(
                                        Filter,
                                        Max_depth,
                                        Path@1,
                                        Vfs@2,
                                        State@3,
                                        Emit
                                    ),
                                    {[{Path@1, New_vfs} | Roots@2], State@4};

                                none ->
                                    {New_vfs@1, State@5} = create(
                                        Filter,
                                        Max_depth,
                                        Path@1,
                                        <<""/utf8>>,
                                        State@3,
                                        Emit
                                    ),
                                    {[{Path@1, New_vfs@1} | Roots@2], State@5}
                            end
                        end
                    )
                end
            ),
            {ok, {watcher, Stop}}
        end
    ).

-file("src/polly.gleam", 207).
?DOC(
    " Tell Polly to start watching all the specified directories for changes.\n"
    " \n"
    " The callback is called synchronously while collecting change events since\n"
    " the last run. It is adviseable to move heavier cpu-bound tasks from this\n"
    " callback into their own processes or threads.\n"
    " \n"
    " When running on the Erlang target, this spawns a new process.\n"
).
-spec watch(options(has_watched_dirs()), fun((event()) -> any())) -> {ok,
        watcher()} |
    {error, list({binary(), simplifile:file_error()})}.
watch(Options, Emit) ->
    watch_with(
        Options,
        nil,
        fun(_, Event) ->
            Emit(Event),
            nil
        end
    ).
