-module(lustre_dev_tools@project).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/project.gleam").
-export([config/0, initialise/0, exists/2]).
-export_type([project/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type project() :: {project,
        binary(),
        gleam@dict:dict(binary(), tom:toml()),
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        boolean()}.

-file("src/lustre_dev_tools/project.gleam", 116).
?DOC(false).
-spec find_root(binary()) -> binary().
find_root(Path) ->
    case simplifile_erl:is_file(filepath:join(Path, <<"gleam.toml"/utf8>>)) of
        {ok, true} ->
            Path;

        {ok, false} ->
            find_root(filepath:join(Path, <<"../"/utf8>>));

        {error, _} ->
            find_root(filepath:join(Path, <<"../"/utf8>>))
    end.

-file("src/lustre_dev_tools/project.gleam", 76).
?DOC(false).
-spec config() -> project().
config() ->
    Root = find_root(<<"./"/utf8>>),
    Bin = filepath:join(Root, <<".lustre/bin"/utf8>>),
    Src = filepath:join(Root, <<"src"/utf8>>),
    Dev = filepath:join(Root, <<"dev"/utf8>>),
    Assets = filepath:join(Root, <<"assets"/utf8>>),
    Build = filepath:join(Root, <<".lustre/build"/utf8>>),
    Has_node_modules = begin
        _pipe = simplifile_erl:is_directory(
            filepath:join(Root, <<"node_modules"/utf8>>)
        ),
        gleam@result:unwrap(_pipe, false)
    end,
    Toml@1 = case simplifile:read(filepath:join(Root, <<"gleam.toml"/utf8>>)) of
        {ok, Toml} -> Toml;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"lustre_dev_tools/project"/utf8>>,
                        function => <<"config"/utf8>>,
                        line => 93,
                        value => _assert_fail,
                        start => 2377,
                        'end' => 2449,
                        pattern_start => 2388,
                        pattern_end => 2396})
    end,
    Config@1 = case tom:parse(Toml@1) of
        {ok, Config} -> Config;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"lustre_dev_tools/project"/utf8>>,
                        function => <<"config"/utf8>>,
                        line => 94,
                        value => _assert_fail@1,
                        start => 2452,
                        'end' => 2491,
                        pattern_start => 2463,
                        pattern_end => 2473})
    end,
    Name@1 = case tom:get_string(Config@1, [<<"name"/utf8>>]) of
        {ok, Name} -> Name;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"lustre_dev_tools/project"/utf8>>,
                        function => <<"config"/utf8>>,
                        line => 95,
                        value => _assert_fail@2,
                        start => 2494,
                        'end' => 2548,
                        pattern_start => 2505,
                        pattern_end => 2513})
    end,
    Options = begin
        _pipe@1 = tom:get_table(Config@1, [<<"tools"/utf8>>, <<"lustre"/utf8>>]),
        gleam@result:unwrap(_pipe@1, maps:new())
    end,
    {project,
        Name@1,
        Options,
        Root,
        Src,
        Dev,
        Assets,
        Bin,
        Build,
        Has_node_modules}.

-file("src/lustre_dev_tools/project.gleam", 36).
?DOC(false).
-spec initialise() -> {ok, project()} | {error, lustre_dev_tools@error:error()}.
initialise() ->
    Project = config(),
    gleam@result:'try'(case erlang:element(4, Project) of
            <<"./"/utf8>> ->
                {ok, nil};

            <<"./"/utf8, Path/binary>> ->
                {error, {must_be_project_root, Path}};

            Path ->
                {error, {must_be_project_root, Path}}
        end, fun(_) ->
            gleam@result:'try'(
                begin
                    _pipe = simplifile:create_directory_all(
                        erlang:element(8, Project)
                    ),
                    gleam@result:map_error(
                        _pipe,
                        fun(Field@0) -> {could_not_initialise_dev_tools, Field@0} end
                    )
                end,
                fun(_) ->
                    gleam@result:'try'(
                        begin
                            _pipe@1 = simplifile:create_directory_all(
                                erlang:element(9, Project)
                            ),
                            gleam@result:map_error(
                                _pipe@1,
                                fun(Field@0) -> {could_not_initialise_dev_tools, Field@0} end
                            )
                        end,
                        fun(_) ->
                            Gitignore_path = filepath:join(
                                erlang:element(4, Project),
                                <<".gitignore"/utf8>>
                            ),
                            gleam@result:'try'(
                                case simplifile:read(Gitignore_path) of
                                    {ok, Gitignore} ->
                                        case gleam_stdlib:contains_string(
                                            Gitignore,
                                            <<".lustre"/utf8>>
                                        ) of
                                            true ->
                                                {ok, nil};

                                            false ->
                                                _pipe@2 = simplifile:append(
                                                    Gitignore_path,
                                                    (<<"\n#Added automatically by Lustre Dev Tools\n/.lustre\n/dist\n"/utf8>>)
                                                ),
                                                _pipe@3 = gleam@result:map_error(
                                                    _pipe@2,
                                                    fun(Field@0) -> {could_not_initialise_dev_tools, Field@0} end
                                                ),
                                                gleam@result:replace(
                                                    _pipe@3,
                                                    nil
                                                )
                                        end;

                                    {error, _} ->
                                        {ok, nil}
                                end,
                                fun(_) -> {ok, Project} end
                            )
                        end
                    )
                end
            )
        end).

-file("src/lustre_dev_tools/project.gleam", 123).
?DOC(false).
-spec exists(project(), binary()) -> boolean().
exists(Project, Module) ->
    case simplifile_erl:is_file(
        filepath:join(
            erlang:element(5, Project),
            <<Module/binary, ".gleam"/utf8>>
        )
    ) of
        {ok, true} ->
            true;

        {ok, false} ->
            false;

        {error, _} ->
            false
    end.
