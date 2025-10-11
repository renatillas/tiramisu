-module(lustre@dev).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/dev.gleam").
-export([main/0]).
-export_type([add_options/0, build_options/0, start_options/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " # Available commands\n"
    "\n"
    " Below is a list of the available commands when running `gleam run -m lustre/dev`.\n"
    " Each command has its own CLI help text to document which flags are supported\n"
    " and a separate [TOML reference](https://hexdocs.pm/lustre_dev_tools/toml-reference.html)\n"
    " documents additional configuration options that can be set in your `gleam.toml`.\n"
    "\n"
    " ## `add <..integrations>`\n"
    "\n"
    " Add various binary dependencies to your project. Lustre uses various external\n"
    " tools to provide core functionality such as bundling JavaScript with Bun or\n"
    " building styles with Tailwind. This command can be used to download these\n"
    " integrations from GitHub for dev tools to use.\n"
    "\n"
    " Supported arguments are:\n"
    "\n"
    " - `bun`: Bun is a fast JavaScript runtime and bundler. It is used to bundle\n"
    " your Gleam code into a single JavaScript file that can be run in the browser.\n"
    "\n"
    " - `tailwind`, `tailwindcss`, or `tw`: Tailwind is a utility-first CSS framework\n"
    " supported automatically by these dev tools. This command will download the\n"
    " Tailwind CLI tool.\n"
    "\n"
    " Lustre will detect which integrations your project needs based on your code\n"
    " and configuration, and will automatically download necessary tools when you\n"
    " run any of the other commands. However ou may still want to run this command\n"
    " manually to ensure that your project has all the necessary tools installed\n"
    " before you go offline, for example.\n"
    "\n"
    " ## `build <..entries>`\n"
    "\n"
    " Build your Gleam project and produce a JavaScript bundle ready to be served\n"
    " and run in a Web browser. This command accepts zero or more entry modules as\n"
    " arguments.\n"
    "\n"
    " - If no entry modules are provided, the module matching the name of your app\n"
    " as defined in your `gleam.toml` will be used as the entry and the `main`\n"
    " function in that module will be called when the JavaScript bundle is run.\n"
    " An `index.html` file will also be generated and contain a script tag to load\n"
    " the produced bundle.\n"
    "\n"
    " - If one argument is provided, it should be the name of a module in your\n"
    " project like `your_app` or `your_app/some_module`. The `main` function in\n"
    " that module will be called when the JavaScript bundle is run. An `index.html`\n"
    " file will also be generated and contain a script tag to load the produced\n"
    " bundle.\n"
    "\n"
    " - If multiple arguments are provided, each should be the name of a module in\n"
    " your project. Multiple JavaScript bundles will be produced, one for each entry\n"
    " module, and an additional bundle containing all code shared between every\n"
    " entry module. In this case no `index.html` file will be generated automatically,\n"
    " and must be provided manually if needed.\n"
    "\n"
    " The produced JavaScript bundle(s) will be minified and written to your project's\n"
    " `dist` directory by default. Some optimisations such as dead-code elimination\n"
    " may also be performed.\n"
    "\n"
    " ## `start`\n"
    "\n"
    " Start a development server to run your Lustre app locally. This will watch\n"
    " your source files for changes and automatically rebuild and reload the app\n"
    " in your browser.\n"
).

-type add_options() :: {add_options, list(binary())}.

-type build_options() :: {build_options,
        boolean(),
        binary(),
        list(binary()),
        boolean(),
        boolean()}.

-type start_options() :: {start_options,
        list(binary()),
        lustre_dev_tools@dev@proxy:proxy(),
        binary(),
        gleam@option:option(binary()),
        binary(),
        integer()}.

-file("src/lustre/dev.gleam", 133).
-spec add(lustre_dev_tools@project:project()) -> glint:command({ok, nil} |
    {error, lustre_dev_tools@error:error()}).
add(Project) ->
    glint:command_help(
        (<<"
Add various binary dependencies to your project. Lustre uses various external
tools to provide core functionality such as bundling JavaScript with Bun or
building styles with Tailwind. This command can be used to download these
integrations from GitHub for dev tools to use.
    "/utf8>>),
        fun() ->
            glint:named_arg(
                <<"integration"/utf8>>,
                fun(Get_integration) ->
                    glint:unnamed_args(
                        {min_args, 0},
                        fun() ->
                            glint:command(
                                fun(Arg, Args, _) ->
                                    Options = {add_options,
                                        [Get_integration(Arg) | Args]},
                                    gleam@list:try_each(
                                        erlang:element(2, Options),
                                        fun(Integration) -> case Integration of
                                                <<"bun"/utf8>> ->
                                                    lustre_dev_tools@bin@bun:download(
                                                        Project,
                                                        false
                                                    );

                                                <<"tailwind"/utf8>> ->
                                                    lustre_dev_tools@bin@tailwind:download(
                                                        Project,
                                                        false
                                                    );

                                                <<"tailwindcss"/utf8>> ->
                                                    lustre_dev_tools@bin@tailwind:download(
                                                        Project,
                                                        false
                                                    );

                                                <<"tw"/utf8>> ->
                                                    lustre_dev_tools@bin@tailwind:download(
                                                        Project,
                                                        false
                                                    );

                                                Name ->
                                                    {error,
                                                        {unknown_integration,
                                                            Name}}
                                            end end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/lustre/dev.gleam", 170).
-spec build(lustre_dev_tools@project:project()) -> glint:command({ok, nil} |
    {error, lustre_dev_tools@error:error()}).
build(Project) ->
    glint:command_help(
        (<<"
Build your Gleam project and produce a JavaScript bundle ready to be served and
run in a Web browser. The produced JavaScript bundle(s) will be minified and written
to your project's `dist` directory by default. Some optimisations such as dead-code
elimination may also be performed.
    "/utf8>>),
        fun() ->
            lustre_dev_tools@cli:bool(
                <<"minify"/utf8>>,
                [<<"build"/utf8>>, <<"minify"/utf8>>],
                Project,
                (<<"
Produce a production-ready minified build of the project. This will rename
variables, remove white space, and perform other optimisations to reduce the
size of the JavaScript output.


This option can also be provided in your `gleam.toml` configuration under the
key `tools.lustre.build.minify`.
    "/utf8>>),
                fun(Minify) ->
                    lustre_dev_tools@cli:bool(
                        <<"no-html"/utf8>>,
                        [<<"build"/utf8>>, <<"no_html"/utf8>>],
                        Project,
                        (<<"
Skip automatic generation of an HTML file for this project. You might want to
do this if you have a custom HTML file you want to use instead. HTML generation
is always skipped if there are multiple entry modules.


This option can also be provided in your `gleam.toml` configuration under the
key `tools.lustre.build.no_html`.
    "/utf8>>),
                        fun(Skip_html) ->
                            lustre_dev_tools@cli:bool(
                                <<"no-tailwind"/utf8>>,
                                [<<"build"/utf8>>, <<"no_tailwind"/utf8>>],
                                Project,
                                <<"
Skip automatic detection of Tailwind CSS in this project. This means even if a
valid Tailwind entry point is detected, Lustre will not attempt to download or
run the Tailwind CLI tool during the build process. You might want to do this if
you have a custom CSS build process you want to use instead.

This option can also be provided in your `gleam.toml` configuration under the
key `tools.lustre.build.no_tailwind`.
    "/utf8>>,
                                fun(Skip_tailwind) ->
                                    lustre_dev_tools@cli:string(
                                        <<"outdir"/utf8>>,
                                        [<<"build"/utf8>>, <<"outdir"/utf8>>],
                                        Project,
                                        (<<"
Configure where the build JavaScript bundle will be written to: by default this
is `dist` within the project root. Common alternatives include `docs/` for GitHub
Pages sites, `public/` for hosting with services like Vercel, or the `priv/static`
directory of another Gleam or Elixir project.


This option can also be provided in your `gleam.toml` configuration under the
key `tools.lustre.build.outdir`.
    "/utf8>>),
                                        fun(Outdir) ->
                                            glint:unnamed_args(
                                                {min_args, 0},
                                                fun() ->
                                                    glint:command(
                                                        fun(_, Entries, Flags) ->
                                                            gleam@result:'try'(
                                                                case Entries of
                                                                    [] ->
                                                                        {ok,
                                                                            [erlang:element(
                                                                                    2,
                                                                                    Project
                                                                                )]};

                                                                    _ ->
                                                                        gleam@list:try_map(
                                                                            Entries,
                                                                            fun(
                                                                                Entry
                                                                            ) ->
                                                                                case lustre_dev_tools@project:exists(
                                                                                    Project,
                                                                                    Entry
                                                                                ) of
                                                                                    true ->
                                                                                        {ok,
                                                                                            Entry};

                                                                                    false ->
                                                                                        {error,
                                                                                            {unknown_gleam_module,
                                                                                                Entry}}
                                                                                end
                                                                            end
                                                                        )
                                                                end,
                                                                fun(Entries@1) ->
                                                                    Options = {build_options,
                                                                        begin
                                                                            _pipe = Minify(
                                                                                Flags
                                                                            ),
                                                                            gleam@result:unwrap(
                                                                                _pipe,
                                                                                false
                                                                            )
                                                                        end,
                                                                        filepath:join(
                                                                            erlang:element(
                                                                                4,
                                                                                Project
                                                                            ),
                                                                            begin
                                                                                _pipe@1 = Outdir(
                                                                                    Flags
                                                                                ),
                                                                                gleam@result:unwrap(
                                                                                    _pipe@1,
                                                                                    <<"dist"/utf8>>
                                                                                )
                                                                            end
                                                                        ),
                                                                        Entries@1,
                                                                        case {Skip_html(
                                                                                Flags
                                                                            ),
                                                                            Entries@1} of
                                                                            {{ok,
                                                                                    true},
                                                                                _} ->
                                                                                true;

                                                                            {{ok,
                                                                                    false},
                                                                                [_,
                                                                                    _ |
                                                                                    _]} ->
                                                                                true;

                                                                            {{ok,
                                                                                    false},
                                                                                _} ->
                                                                                false;

                                                                            {{error,
                                                                                    _},
                                                                                _} ->
                                                                                false
                                                                        end,
                                                                        begin
                                                                            _pipe@2 = Skip_tailwind(
                                                                                Flags
                                                                            ),
                                                                            gleam@result:unwrap(
                                                                                _pipe@2,
                                                                                false
                                                                            )
                                                                        end},
                                                                    gleam@result:'try'(
                                                                        begin
                                                                            _pipe@3 = simplifile:create_directory_all(
                                                                                erlang:element(
                                                                                    3,
                                                                                    Options
                                                                                )
                                                                            ),
                                                                            gleam@result:map_error(
                                                                                _pipe@3,
                                                                                fun(
                                                                                    _capture
                                                                                ) ->
                                                                                    {could_not_write_file,
                                                                                        erlang:element(
                                                                                            3,
                                                                                            Options
                                                                                        ),
                                                                                        _capture}
                                                                                end
                                                                            )
                                                                        end,
                                                                        fun(_) ->
                                                                            gleam@result:'try'(
                                                                                lustre_dev_tools@bin@gleam:build(
                                                                                    Project
                                                                                ),
                                                                                fun(
                                                                                    _
                                                                                ) ->
                                                                                    gleam@result:'try'(
                                                                                        begin
                                                                                            gleam@list:try_map(
                                                                                                erlang:element(
                                                                                                    4,
                                                                                                    Options
                                                                                                ),
                                                                                                fun(
                                                                                                    Entry@1
                                                                                                ) ->
                                                                                                    Module = begin
                                                                                                        _pipe@4 = <<"import { main } from '../../build/dev/javascript/${name}/${entry}.mjs'; main();"/utf8>>,
                                                                                                        _pipe@5 = gleam@string:replace(
                                                                                                            _pipe@4,
                                                                                                            <<"${name}"/utf8>>,
                                                                                                            erlang:element(
                                                                                                                2,
                                                                                                                Project
                                                                                                            )
                                                                                                        ),
                                                                                                        gleam@string:replace(
                                                                                                            _pipe@5,
                                                                                                            <<"${entry}"/utf8>>,
                                                                                                            Entry@1
                                                                                                        )
                                                                                                    end,
                                                                                                    Name = <<(justin:snake_case(
                                                                                                            Entry@1
                                                                                                        ))/binary,
                                                                                                        ".mjs"/utf8>>,
                                                                                                    Path = filepath:join(
                                                                                                        erlang:element(
                                                                                                            9,
                                                                                                            Project
                                                                                                        ),
                                                                                                        Name
                                                                                                    ),
                                                                                                    gleam@result:'try'(
                                                                                                        begin
                                                                                                            _pipe@6 = simplifile:write(
                                                                                                                Path,
                                                                                                                Module
                                                                                                            ),
                                                                                                            gleam@result:map_error(
                                                                                                                _pipe@6,
                                                                                                                fun(
                                                                                                                    _capture@1
                                                                                                                ) ->
                                                                                                                    {could_not_write_file,
                                                                                                                        Path,
                                                                                                                        _capture@1}
                                                                                                                end
                                                                                                            )
                                                                                                        end,
                                                                                                        fun(
                                                                                                            _
                                                                                                        ) ->
                                                                                                            {ok,
                                                                                                                Path}
                                                                                                        end
                                                                                                    )
                                                                                                end
                                                                                            )
                                                                                        end,
                                                                                        fun(
                                                                                            Bun_entries
                                                                                        ) ->
                                                                                            gleam@result:'try'(
                                                                                                lustre_dev_tools@bin@bun:build(
                                                                                                    Project,
                                                                                                    Bun_entries,
                                                                                                    erlang:element(
                                                                                                        3,
                                                                                                        Options
                                                                                                    ),
                                                                                                    erlang:element(
                                                                                                        2,
                                                                                                        Options
                                                                                                    ),
                                                                                                    false
                                                                                                ),
                                                                                                fun(
                                                                                                    _
                                                                                                ) ->
                                                                                                    Tailwind_entry = case erlang:element(
                                                                                                        4,
                                                                                                        Options
                                                                                                    ) of
                                                                                                        [Entry@2] ->
                                                                                                            Entry@2;

                                                                                                        [] ->
                                                                                                            erlang:element(
                                                                                                                2,
                                                                                                                Project
                                                                                                            );

                                                                                                        [_ |
                                                                                                            _] ->
                                                                                                            erlang:element(
                                                                                                                2,
                                                                                                                Project
                                                                                                            )
                                                                                                    end,
                                                                                                    gleam@result:'try'(
                                                                                                        case lustre_dev_tools@bin@tailwind:detect(
                                                                                                            Project,
                                                                                                            Tailwind_entry
                                                                                                        ) of
                                                                                                            {ok,
                                                                                                                has_tailwind_entry} when erlang:element(
                                                                                                                6,
                                                                                                                Options
                                                                                                            ) ->
                                                                                                                {ok,
                                                                                                                    none};

                                                                                                            {ok,
                                                                                                                has_tailwind_entry} ->
                                                                                                                gleam@result:'try'(
                                                                                                                    lustre_dev_tools@bin@tailwind:build(
                                                                                                                        Project,
                                                                                                                        Tailwind_entry,
                                                                                                                        erlang:element(
                                                                                                                            3,
                                                                                                                            Options
                                                                                                                        ),
                                                                                                                        erlang:element(
                                                                                                                            2,
                                                                                                                            Options
                                                                                                                        ),
                                                                                                                        false
                                                                                                                    ),
                                                                                                                    fun(
                                                                                                                        _
                                                                                                                    ) ->
                                                                                                                        {ok,
                                                                                                                            {some,
                                                                                                                                Tailwind_entry}}
                                                                                                                    end
                                                                                                                );

                                                                                                            {ok,
                                                                                                                has_viable_entry} ->
                                                                                                                {ok,
                                                                                                                    none};

                                                                                                            {ok,
                                                                                                                nothing} ->
                                                                                                                {ok,
                                                                                                                    none};

                                                                                                            {ok,
                                                                                                                has_legacy_config} ->
                                                                                                                {ok,
                                                                                                                    none};

                                                                                                            {error,
                                                                                                                E} ->
                                                                                                                {error,
                                                                                                                    E}
                                                                                                        end,
                                                                                                        fun(
                                                                                                            Tailwind_entry@1
                                                                                                        ) ->
                                                                                                            gleam@result:'try'(
                                                                                                                case {erlang:element(
                                                                                                                        4,
                                                                                                                        Options
                                                                                                                    ),
                                                                                                                    erlang:element(
                                                                                                                        5,
                                                                                                                        Options
                                                                                                                    )} of
                                                                                                                    {_,
                                                                                                                        true} ->
                                                                                                                        {ok,
                                                                                                                            nil};

                                                                                                                    {[_,
                                                                                                                            _ |
                                                                                                                            _],
                                                                                                                        _} ->
                                                                                                                        lustre_dev_tools@cli:log(
                                                                                                                            <<"Multiple entry modules provided, skipping HTML generation"/utf8>>,
                                                                                                                            false
                                                                                                                        ),
                                                                                                                        {ok,
                                                                                                                            nil};

                                                                                                                    {[],
                                                                                                                        false} ->
                                                                                                                        gleam@result:'try'(
                                                                                                                            begin
                                                                                                                                _pipe@7 = lustre_dev_tools@build@html:generate(
                                                                                                                                    Project,
                                                                                                                                    erlang:element(
                                                                                                                                        2,
                                                                                                                                        Project
                                                                                                                                    ),
                                                                                                                                    Tailwind_entry@1,
                                                                                                                                    erlang:element(
                                                                                                                                        2,
                                                                                                                                        Options
                                                                                                                                    )
                                                                                                                                ),
                                                                                                                                _pipe@8 = simplifile:write(
                                                                                                                                    filepath:join(
                                                                                                                                        erlang:element(
                                                                                                                                            3,
                                                                                                                                            Options
                                                                                                                                        ),
                                                                                                                                        <<"index.html"/utf8>>
                                                                                                                                    ),
                                                                                                                                    _pipe@7
                                                                                                                                ),
                                                                                                                                gleam@result:map_error(
                                                                                                                                    _pipe@8,
                                                                                                                                    fun(
                                                                                                                                        _capture@2
                                                                                                                                    ) ->
                                                                                                                                        {could_not_write_file,
                                                                                                                                            filepath:join(
                                                                                                                                                erlang:element(
                                                                                                                                                    3,
                                                                                                                                                    Options
                                                                                                                                                ),
                                                                                                                                                <<"index.html"/utf8>>
                                                                                                                                            ),
                                                                                                                                            _capture@2}
                                                                                                                                    end
                                                                                                                                )
                                                                                                                            end,
                                                                                                                            fun(
                                                                                                                                _
                                                                                                                            ) ->
                                                                                                                                lustre_dev_tools@cli:success(
                                                                                                                                    <<"HTML generated."/utf8>>,
                                                                                                                                    false
                                                                                                                                ),
                                                                                                                                {ok,
                                                                                                                                    nil}
                                                                                                                            end
                                                                                                                        );

                                                                                                                    {[Entry@3],
                                                                                                                        false} ->
                                                                                                                        gleam@result:'try'(
                                                                                                                            begin
                                                                                                                                _pipe@9 = lustre_dev_tools@build@html:generate(
                                                                                                                                    Project,
                                                                                                                                    Entry@3,
                                                                                                                                    Tailwind_entry@1,
                                                                                                                                    erlang:element(
                                                                                                                                        2,
                                                                                                                                        Options
                                                                                                                                    )
                                                                                                                                ),
                                                                                                                                _pipe@10 = simplifile:write(
                                                                                                                                    filepath:join(
                                                                                                                                        erlang:element(
                                                                                                                                            3,
                                                                                                                                            Options
                                                                                                                                        ),
                                                                                                                                        <<"index.html"/utf8>>
                                                                                                                                    ),
                                                                                                                                    _pipe@9
                                                                                                                                ),
                                                                                                                                gleam@result:map_error(
                                                                                                                                    _pipe@10,
                                                                                                                                    fun(
                                                                                                                                        _capture@3
                                                                                                                                    ) ->
                                                                                                                                        {could_not_write_file,
                                                                                                                                            filepath:join(
                                                                                                                                                erlang:element(
                                                                                                                                                    3,
                                                                                                                                                    Options
                                                                                                                                                ),
                                                                                                                                                <<"index.html"/utf8>>
                                                                                                                                            ),
                                                                                                                                            _capture@3}
                                                                                                                                    end
                                                                                                                                )
                                                                                                                            end,
                                                                                                                            fun(
                                                                                                                                _
                                                                                                                            ) ->
                                                                                                                                lustre_dev_tools@cli:success(
                                                                                                                                    <<"HTML generated."/utf8>>,
                                                                                                                                    false
                                                                                                                                ),
                                                                                                                                {ok,
                                                                                                                                    nil}
                                                                                                                            end
                                                                                                                        )
                                                                                                                end,
                                                                                                                fun(
                                                                                                                    _
                                                                                                                ) ->
                                                                                                                    gleam@result:'try'(
                                                                                                                        case simplifile_erl:is_directory(
                                                                                                                            erlang:element(
                                                                                                                                7,
                                                                                                                                Project
                                                                                                                            )
                                                                                                                        ) of
                                                                                                                            {ok,
                                                                                                                                true} ->
                                                                                                                                gleam@result:'try'(
                                                                                                                                    begin
                                                                                                                                        _pipe@11 = simplifile:copy_directory(
                                                                                                                                            erlang:element(
                                                                                                                                                7,
                                                                                                                                                Project
                                                                                                                                            ),
                                                                                                                                            filepath:join(
                                                                                                                                                erlang:element(
                                                                                                                                                    3,
                                                                                                                                                    Options
                                                                                                                                                ),
                                                                                                                                                <<"assets"/utf8>>
                                                                                                                                            )
                                                                                                                                        ),
                                                                                                                                        gleam@result:map_error(
                                                                                                                                            _pipe@11,
                                                                                                                                            fun(
                                                                                                                                                _capture@4
                                                                                                                                            ) ->
                                                                                                                                                {could_not_write_file,
                                                                                                                                                    filepath:join(
                                                                                                                                                        erlang:element(
                                                                                                                                                            3,
                                                                                                                                                            Options
                                                                                                                                                        ),
                                                                                                                                                        <<"assets"/utf8>>
                                                                                                                                                    ),
                                                                                                                                                    _capture@4}
                                                                                                                                            end
                                                                                                                                        )
                                                                                                                                    end,
                                                                                                                                    fun(
                                                                                                                                        _
                                                                                                                                    ) ->
                                                                                                                                        lustre_dev_tools@cli:success(
                                                                                                                                            <<"Assets copied."/utf8>>,
                                                                                                                                            false
                                                                                                                                        ),
                                                                                                                                        {ok,
                                                                                                                                            nil}
                                                                                                                                    end
                                                                                                                                );

                                                                                                                            {ok,
                                                                                                                                false} ->
                                                                                                                                {ok,
                                                                                                                                    nil};

                                                                                                                            {error,
                                                                                                                                _} ->
                                                                                                                                {ok,
                                                                                                                                    nil}
                                                                                                                        end,
                                                                                                                        fun(
                                                                                                                            _
                                                                                                                        ) ->
                                                                                                                            lustre_dev_tools@cli:success(
                                                                                                                                <<"Build complete!"/utf8>>,
                                                                                                                                false
                                                                                                                            ),
                                                                                                                            {ok,
                                                                                                                                nil}
                                                                                                                        end
                                                                                                                    )
                                                                                                                end
                                                                                                            )
                                                                                                        end
                                                                                                    )
                                                                                                end
                                                                                            )
                                                                                        end
                                                                                    )
                                                                                end
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/lustre/dev.gleam", 412).
-spec start(lustre_dev_tools@project:project()) -> glint:command({ok, nil} |
    {error, lustre_dev_tools@error:error()}).
start(Project) ->
    glint:command_help(
        (<<"
Start a development server to run your Lustre app locally. This will watch your
source files for changes and automatically rebuild and reload the app in your
browser.
    "/utf8>>),
        fun() ->
            lustre_dev_tools@cli:string(
                <<"host"/utf8>>,
                [<<"dev"/utf8>>, <<"host"/utf8>>],
                Project,
                (<<"
Configure the host address the development server will listen on: by default this
is `localhost`. You can set this to `0.0.0.0` to allow access from other devices
on yoru local network. This can be useful for testing with real mobile devices.
    "/utf8>>),
                fun(Host) ->
                    lustre_dev_tools@cli:int(
                        <<"port"/utf8>>,
                        [<<"dev"/utf8>>, <<"port"/utf8>>],
                        Project,
                        (<<"
Configure the port the development server will listen on: by default this is
`1234`. If this port is already in use the server will fail to start.
    "/utf8>>),
                        fun(Port) ->
                            lustre_dev_tools@cli:string(
                                <<""/utf8>>,
                                [<<"dev"/utf8>>,
                                    <<"proxy"/utf8>>,
                                    <<"from"/utf8>>],
                                Project,
                                <<""/utf8>>,
                                fun(Proxy_from) ->
                                    lustre_dev_tools@cli:string(
                                        <<""/utf8>>,
                                        [<<"dev"/utf8>>,
                                            <<"proxy"/utf8>>,
                                            <<"to"/utf8>>],
                                        Project,
                                        <<""/utf8>>,
                                        fun(Proxy_to) ->
                                            lustre_dev_tools@cli:string_list(
                                                <<"watch"/utf8>>,
                                                [<<"dev"/utf8>>,
                                                    <<"watch"/utf8>>],
                                                Project,
                                                (<<"
Configure additional directories to watch for changes. The `src/` and `assets/`
directories are always watched and do not need to be specified here.
    "/utf8>>),
                                                fun(Watch) ->
                                                    glint:unnamed_args(
                                                        {min_args, 0},
                                                        fun() ->
                                                            glint:command(
                                                                fun(
                                                                    _,
                                                                    Entries,
                                                                    Flags
                                                                ) ->
                                                                    gleam@result:'try'(
                                                                        lustre_dev_tools@dev@proxy:new(
                                                                            begin
                                                                                _pipe = Proxy_from(
                                                                                    Flags
                                                                                ),
                                                                                gleam@result:unwrap(
                                                                                    _pipe,
                                                                                    <<""/utf8>>
                                                                                )
                                                                            end,
                                                                            begin
                                                                                _pipe@1 = Proxy_to(
                                                                                    Flags
                                                                                ),
                                                                                gleam@result:unwrap(
                                                                                    _pipe@1,
                                                                                    <<""/utf8>>
                                                                                )
                                                                            end
                                                                        ),
                                                                        fun(
                                                                            Proxy
                                                                        ) ->
                                                                            Entry@1 = case Entries of
                                                                                [] ->
                                                                                    erlang:element(
                                                                                        2,
                                                                                        Project
                                                                                    );

                                                                                [Entry |
                                                                                    _] ->
                                                                                    Entry
                                                                            end,
                                                                            gleam@result:'try'(
                                                                                case lustre_dev_tools@bin@tailwind:detect(
                                                                                    Project,
                                                                                    Entry@1
                                                                                ) of
                                                                                    {ok,
                                                                                        has_tailwind_entry} ->
                                                                                        {ok,
                                                                                            {some,
                                                                                                Entry@1}};

                                                                                    {ok,
                                                                                        has_viable_entry} ->
                                                                                        {ok,
                                                                                            none};

                                                                                    {ok,
                                                                                        nothing} ->
                                                                                        {ok,
                                                                                            none};

                                                                                    {ok,
                                                                                        has_legacy_config} ->
                                                                                        {ok,
                                                                                            none};

                                                                                    {error,
                                                                                        E} ->
                                                                                        {error,
                                                                                            E}
                                                                                end,
                                                                                fun(
                                                                                    Tailwind_entry
                                                                                ) ->
                                                                                    Options = {start_options,
                                                                                        begin
                                                                                            _pipe@2 = Watch(
                                                                                                Flags
                                                                                            ),
                                                                                            _pipe@3 = gleam@result:unwrap(
                                                                                                _pipe@2,
                                                                                                []
                                                                                            ),
                                                                                            _pipe@4 = gleam@list:filter(
                                                                                                _pipe@3,
                                                                                                fun(
                                                                                                    Dir
                                                                                                ) ->
                                                                                                    case simplifile_erl:is_directory(
                                                                                                        Dir
                                                                                                    ) of
                                                                                                        {ok,
                                                                                                            true} ->
                                                                                                            true;

                                                                                                        {ok,
                                                                                                            false} ->
                                                                                                            false;

                                                                                                        {error,
                                                                                                            _} ->
                                                                                                            false
                                                                                                    end
                                                                                                end
                                                                                            ),
                                                                                            lists:append(
                                                                                                _pipe@4,
                                                                                                [erlang:element(
                                                                                                        5,
                                                                                                        Project
                                                                                                    ),
                                                                                                    erlang:element(
                                                                                                        7,
                                                                                                        Project
                                                                                                    )]
                                                                                            )
                                                                                        end,
                                                                                        Proxy,
                                                                                        Entry@1,
                                                                                        Tailwind_entry,
                                                                                        begin
                                                                                            _pipe@5 = Host(
                                                                                                Flags
                                                                                            ),
                                                                                            gleam@result:unwrap(
                                                                                                _pipe@5,
                                                                                                <<"localhost"/utf8>>
                                                                                            )
                                                                                        end,
                                                                                        begin
                                                                                            _pipe@6 = Port(
                                                                                                Flags
                                                                                            ),
                                                                                            gleam@result:unwrap(
                                                                                                _pipe@6,
                                                                                                1234
                                                                                            )
                                                                                        end},
                                                                                    gleam@result:'try'(
                                                                                        lustre_dev_tools@bin@gleam:build(
                                                                                            Project
                                                                                        ),
                                                                                        fun(
                                                                                            _
                                                                                        ) ->
                                                                                            gleam@result:'try'(
                                                                                                case erlang:element(
                                                                                                    10,
                                                                                                    Project
                                                                                                ) of
                                                                                                    true ->
                                                                                                        Module = begin
                                                                                                            _pipe@7 = <<"import { main } from '../../build/dev/javascript/${name}/${entry}.mjs'; main();"/utf8>>,
                                                                                                            _pipe@8 = gleam@string:replace(
                                                                                                                _pipe@7,
                                                                                                                <<"${name}"/utf8>>,
                                                                                                                erlang:element(
                                                                                                                    2,
                                                                                                                    Project
                                                                                                                )
                                                                                                            ),
                                                                                                            gleam@string:replace(
                                                                                                                _pipe@8,
                                                                                                                <<"${entry}"/utf8>>,
                                                                                                                erlang:element(
                                                                                                                    2,
                                                                                                                    Project
                                                                                                                )
                                                                                                            )
                                                                                                        end,
                                                                                                        Name = <<(justin:snake_case(
                                                                                                                erlang:element(
                                                                                                                    2,
                                                                                                                    Project
                                                                                                                )
                                                                                                            ))/binary,
                                                                                                            ".dev.mjs"/utf8>>,
                                                                                                        Path = filepath:join(
                                                                                                            erlang:element(
                                                                                                                9,
                                                                                                                Project
                                                                                                            ),
                                                                                                            Name
                                                                                                        ),
                                                                                                        gleam@result:'try'(
                                                                                                            begin
                                                                                                                _pipe@9 = simplifile:write(
                                                                                                                    Path,
                                                                                                                    Module
                                                                                                                ),
                                                                                                                gleam@result:map_error(
                                                                                                                    _pipe@9,
                                                                                                                    fun(
                                                                                                                        _capture
                                                                                                                    ) ->
                                                                                                                        {could_not_write_file,
                                                                                                                            Path,
                                                                                                                            _capture}
                                                                                                                    end
                                                                                                                )
                                                                                                            end,
                                                                                                            fun(
                                                                                                                _
                                                                                                            ) ->
                                                                                                                lustre_dev_tools@bin@bun:build(
                                                                                                                    Project,
                                                                                                                    [Path],
                                                                                                                    filepath:join(
                                                                                                                        erlang:element(
                                                                                                                            4,
                                                                                                                            Project
                                                                                                                        ),
                                                                                                                        <<"build/dev/javascript"/utf8>>
                                                                                                                    ),
                                                                                                                    false,
                                                                                                                    false
                                                                                                                )
                                                                                                            end
                                                                                                        );

                                                                                                    false ->
                                                                                                        {ok,
                                                                                                            nil}
                                                                                                end,
                                                                                                fun(
                                                                                                    _
                                                                                                ) ->
                                                                                                    gleam@result:'try'(
                                                                                                        case erlang:element(
                                                                                                            5,
                                                                                                            Options
                                                                                                        ) of
                                                                                                            {some,
                                                                                                                Tailwind_entry@1} ->
                                                                                                                lustre_dev_tools@bin@tailwind:build(
                                                                                                                    Project,
                                                                                                                    Tailwind_entry@1,
                                                                                                                    filepath:join(
                                                                                                                        erlang:element(
                                                                                                                            4,
                                                                                                                            Project
                                                                                                                        ),
                                                                                                                        <<"build/dev/javascript"/utf8>>
                                                                                                                    ),
                                                                                                                    false,
                                                                                                                    false
                                                                                                                );

                                                                                                            none ->
                                                                                                                {ok,
                                                                                                                    nil}
                                                                                                        end,
                                                                                                        fun(
                                                                                                            _
                                                                                                        ) ->
                                                                                                            Error = booklet_ffi:make(
                                                                                                                none
                                                                                                            ),
                                                                                                            gleam@result:'try'(
                                                                                                                lustre_dev_tools@dev@watcher:start(
                                                                                                                    Project,
                                                                                                                    Error,
                                                                                                                    erlang:element(
                                                                                                                        2,
                                                                                                                        Options
                                                                                                                    ),
                                                                                                                    erlang:element(
                                                                                                                        5,
                                                                                                                        Options
                                                                                                                    )
                                                                                                                ),
                                                                                                                fun(
                                                                                                                    Watcher
                                                                                                                ) ->
                                                                                                                    gleam@result:'try'(
                                                                                                                        lustre_dev_tools@dev@server:start(
                                                                                                                            Project,
                                                                                                                            Error,
                                                                                                                            Watcher,
                                                                                                                            erlang:element(
                                                                                                                                3,
                                                                                                                                Options
                                                                                                                            ),
                                                                                                                            erlang:element(
                                                                                                                                4,
                                                                                                                                Options
                                                                                                                            ),
                                                                                                                            erlang:element(
                                                                                                                                5,
                                                                                                                                Options
                                                                                                                            ),
                                                                                                                            erlang:element(
                                                                                                                                6,
                                                                                                                                Options
                                                                                                                            ),
                                                                                                                            erlang:element(
                                                                                                                                7,
                                                                                                                                Options
                                                                                                                            )
                                                                                                                        ),
                                                                                                                        fun(
                                                                                                                            _
                                                                                                                        ) ->
                                                                                                                            {ok,
                                                                                                                                gleam_erlang_ffi:sleep_forever(
                                                                                                                                    
                                                                                                                                )}
                                                                                                                        end
                                                                                                                    )
                                                                                                                end
                                                                                                            )
                                                                                                        end
                                                                                                    )
                                                                                                end
                                                                                            )
                                                                                        end
                                                                                    )
                                                                                end
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/lustre/dev.gleam", 92).
-spec main() -> nil.
main() ->
    Result = begin
        gleam@result:'try'(
            lustre_dev_tools@project:initialise(),
            fun(Project) ->
                Args = erlang:element(4, argv:load()),
                Cli = begin
                    _pipe = glint:new(),
                    _pipe@1 = glint:as_module(_pipe),
                    _pipe@2 = glint:with_name(_pipe@1, <<"lustre/dev"/utf8>>),
                    _pipe@3 = glint:pretty_help(
                        _pipe@2,
                        glint:default_pretty_help()
                    ),
                    _pipe@4 = glint:add(_pipe@3, [<<"add"/utf8>>], add(Project)),
                    _pipe@5 = glint:add(
                        _pipe@4,
                        [<<"build"/utf8>>],
                        build(Project)
                    ),
                    glint:add(_pipe@5, [<<"start"/utf8>>], start(Project))
                end,
                case glint:execute(Cli, Args) of
                    {ok, {help, Help}} ->
                        {ok, gleam_stdlib:println(Help)};

                    {ok, {out, {ok, _}}} ->
                        {ok, nil};

                    {ok, {out, {error, Reason}}} ->
                        {error, Reason};

                    {error, Message} ->
                        {ok, gleam_stdlib:println_error(Message)}
                end
            end
        )
    end,
    case Result of
        {ok, _} ->
            nil;

        {error, Reason@1} ->
            gleam_stdlib:println_error(lustre_dev_tools@error:explain(Reason@1)),
            system_ffi:exit(1)
    end.
