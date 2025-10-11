-module(lustre_dev_tools@build@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/build/html.gleam").
-export([generate/4, dev/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre_dev_tools/build/html.gleam", 140).
?DOC(false).
-spec lang(lustre_dev_tools@project:project()) -> lustre@vdom@vattr:attribute(any()).
lang(Project) ->
    lustre@attribute:lang(
        begin
            _pipe = tom:get_string(
                erlang:element(3, Project),
                [<<"html"/utf8>>, <<"lang"/utf8>>]
            ),
            gleam@result:unwrap(_pipe, <<"en"/utf8>>)
        end
    ).

-file("src/lustre_dev_tools/build/html.gleam", 149).
?DOC(false).
-spec title(lustre_dev_tools@project:project(), binary()) -> lustre@vdom@vnode:element(any()).
title(Project, Entry) ->
    Name = filepath:base_name(Entry),
    lustre@element@html:title(
        [],
        begin
            _pipe = tom:get_string(
                erlang:element(3, Project),
                [<<"html"/utf8>>, <<"title"/utf8>>]
            ),
            gleam@result:unwrap(_pipe, Name)
        end
    ).

-file("src/lustre_dev_tools/build/html.gleam", 210).
?DOC(false).
-spec stylesheets(lustre_dev_tools@project:project()) -> lustre@vdom@vnode:element(any()).
stylesheets(Project) ->
    lustre@element:fragment(
        begin
            _pipe = tom:get_array(
                erlang:element(3, Project),
                [<<"html"/utf8>>, <<"stylesheets"/utf8>>]
            ),
            _pipe@1 = gleam@result:map(
                _pipe,
                fun(_capture) ->
                    gleam@list:filter_map(_capture, fun tom:as_table/1)
                end
            ),
            _pipe@2 = gleam@result:map(
                _pipe@1,
                fun(_capture@1) ->
                    gleam@list:filter_map(
                        _capture@1,
                        fun(Toml) ->
                            Href = tom:get_string(Toml, [<<"href"/utf8>>]),
                            Content = tom:get_string(Toml, [<<"content"/utf8>>]),
                            case {Href, Content} of
                                {{ok, Href@1}, _} ->
                                    {ok,
                                        lustre@element@html:link(
                                            [lustre@attribute:href(Href@1),
                                                lustre@attribute:rel(
                                                    <<"stylesheet"/utf8>>
                                                )]
                                        )};

                                {_, {ok, Content@1}} ->
                                    {ok,
                                        lustre@element@html:style([], Content@1)};

                                {_, _} ->
                                    {error, nil}
                            end
                        end
                    )
                end
            ),
            gleam@result:unwrap(_pipe@2, [])
        end
    ).

-file("src/lustre_dev_tools/build/html.gleam", 231).
?DOC(false).
-spec scripts(lustre_dev_tools@project:project()) -> lustre@vdom@vnode:element(any()).
scripts(Project) ->
    lustre@element:fragment(
        begin
            _pipe = tom:get_array(
                erlang:element(3, Project),
                [<<"html"/utf8>>, <<"scripts"/utf8>>]
            ),
            _pipe@1 = gleam@result:map(
                _pipe,
                fun(_capture) ->
                    gleam@list:filter_map(_capture, fun tom:as_table/1)
                end
            ),
            _pipe@2 = gleam@result:map(
                _pipe@1,
                fun(_capture@1) ->
                    gleam@list:filter_map(
                        _capture@1,
                        fun(Toml) ->
                            Src = tom:get_string(Toml, [<<"src"/utf8>>]),
                            Content = tom:get_string(Toml, [<<"content"/utf8>>]),
                            Type_ = tom:get_string(Toml, [<<"type"/utf8>>]),
                            case {Src, Content, Type_} of
                                {{ok, Src@1}, _, {ok, Type_value}} ->
                                    {ok,
                                        lustre@element@html:script(
                                            [lustre@attribute:src(Src@1),
                                                lustre@attribute:type_(
                                                    Type_value
                                                )],
                                            <<""/utf8>>
                                        )};

                                {{ok, Src@2}, _, _} ->
                                    {ok,
                                        lustre@element@html:script(
                                            [lustre@attribute:src(Src@2)],
                                            <<""/utf8>>
                                        )};

                                {_, {ok, Content@1}, {ok, Type_value@1}} ->
                                    {ok,
                                        lustre@element@html:script(
                                            [lustre@attribute:type_(
                                                    Type_value@1
                                                )],
                                            Content@1
                                        )};

                                {_, {ok, Content@2}, _} ->
                                    {ok,
                                        lustre@element@html:script(
                                            [],
                                            Content@2
                                        )};

                                {_, _, _} ->
                                    {error, nil}
                            end
                        end
                    )
                end
            ),
            gleam@result:unwrap(_pipe@2, [])
        end
    ).

-file("src/lustre_dev_tools/build/html.gleam", 261).
?DOC(false).
-spec as_attribute(binary(), tom:toml()) -> {ok,
        lustre@vdom@vattr:attribute(any())} |
    {error, nil}.
as_attribute(Key, Toml) ->
    case {tom:as_string(Toml), tom:as_bool(Toml)} of
        {{ok, Value}, _} ->
            {ok, lustre@attribute:attribute(Key, Value)};

        {_, {ok, true}} ->
            {ok, lustre@attribute:attribute(Key, <<""/utf8>>)};

        {_, {ok, false}} ->
            {error, nil};

        {_, _} ->
            {error, nil}
    end.

-file("src/lustre_dev_tools/build/html.gleam", 158).
?DOC(false).
-spec meta(lustre_dev_tools@project:project()) -> lustre@vdom@vnode:element(any()).
meta(Project) ->
    lustre@element:fragment(
        begin
            _pipe = tom:get_array(
                erlang:element(3, Project),
                [<<"html"/utf8>>, <<"meta"/utf8>>]
            ),
            _pipe@1 = gleam@result:map(
                _pipe,
                fun(_capture) ->
                    gleam@list:filter_map(_capture, fun tom:as_table/1)
                end
            ),
            _pipe@2 = gleam@result:map(
                _pipe@1,
                fun(_capture@1) ->
                    gleam@list:map(
                        _capture@1,
                        (fun(_capture@2) ->
                            gleam@dict:fold(
                                _capture@2,
                                [],
                                fun(Attributes, Key, Toml) ->
                                    case as_attribute(Key, Toml) of
                                        {ok, Attribute} ->
                                            [Attribute | Attributes];

                                        {error, _} ->
                                            Attributes
                                    end
                                end
                            )
                        end)
                    )
                end
            ),
            _pipe@3 = gleam@result:map(
                _pipe@2,
                fun(_capture@3) ->
                    gleam@list:filter_map(
                        _capture@3,
                        fun(Attributes@1) -> case Attributes@1 of
                                [_ | _] ->
                                    {ok, lustre@element@html:meta(Attributes@1)};

                                [] ->
                                    {error, nil}
                            end end
                    )
                end
            ),
            gleam@result:unwrap(_pipe@3, [])
        end
    ).

-file("src/lustre_dev_tools/build/html.gleam", 184).
?DOC(false).
-spec links(lustre_dev_tools@project:project()) -> lustre@vdom@vnode:element(any()).
links(Project) ->
    lustre@element:fragment(
        begin
            _pipe = tom:get_array(
                erlang:element(3, Project),
                [<<"html"/utf8>>, <<"links"/utf8>>]
            ),
            _pipe@1 = gleam@result:map(
                _pipe,
                fun(_capture) ->
                    gleam@list:filter_map(_capture, fun tom:as_table/1)
                end
            ),
            _pipe@2 = gleam@result:map(
                _pipe@1,
                fun(_capture@1) ->
                    gleam@list:map(
                        _capture@1,
                        (fun(_capture@2) ->
                            gleam@dict:fold(
                                _capture@2,
                                [],
                                fun(Attributes, Key, Toml) ->
                                    case as_attribute(Key, Toml) of
                                        {ok, Attribute} ->
                                            [Attribute | Attributes];

                                        {error, _} ->
                                            Attributes
                                    end
                                end
                            )
                        end)
                    )
                end
            ),
            _pipe@3 = gleam@result:map(
                _pipe@2,
                fun(_capture@3) ->
                    gleam@list:filter_map(
                        _capture@3,
                        fun(Attributes@1) -> case Attributes@1 of
                                [_ | _] ->
                                    {ok, lustre@element@html:link(Attributes@1)};

                                [] ->
                                    {error, nil}
                            end end
                    )
                end
            ),
            gleam@result:unwrap(_pipe@3, [])
        end
    ).

-file("src/lustre_dev_tools/build/html.gleam", 270).
?DOC(false).
-spec body(lustre_dev_tools@project:project()) -> lustre@vdom@vnode:element(any()).
body(Project) ->
    lustre@element:unsafe_raw_html(
        <<""/utf8>>,
        <<"body"/utf8>>,
        [],
        begin
            _pipe = tom:get_string(
                erlang:element(3, Project),
                [<<"html"/utf8>>, <<"body"/utf8>>]
            ),
            gleam@result:unwrap(_pipe, <<"<div id=\"app\"></div>"/utf8>>)
        end
    ).

-file("src/lustre_dev_tools/build/html.gleam", 17).
?DOC(false).
-spec generate(
    lustre_dev_tools@project:project(),
    binary(),
    gleam@option:option(binary()),
    boolean()
) -> binary().
generate(Project, Entry, Tailwind_entry, Minify) ->
    Name = filepath:base_name(Entry),
    Html = lustre@element@html:html(
        [lang(Project)],
        [lustre@element@html:head(
                [],
                [lustre@element@html:meta(
                        [lustre@attribute:charset(<<"utf-8"/utf8>>)]
                    ),
                    lustre@element@html:meta(
                        [lustre@attribute:name(<<"viewport"/utf8>>),
                            lustre@attribute:content(
                                <<"width=device-width, initial-scale=1"/utf8>>
                            )]
                    ),
                    meta(Project),
                    title(Project, Entry),
                    links(Project),
                    stylesheets(Project),
                    case Tailwind_entry of
                        {some, Entry@1} ->
                            lustre@element@html:link(
                                [lustre@attribute:rel(<<"stylesheet"/utf8>>),
                                    lustre@attribute:href(
                                        <<<<"/"/utf8,
                                                (filepath:base_name(Entry@1))/binary>>/binary,
                                            ".css"/utf8>>
                                    )]
                            );

                        none ->
                            lustre@element:none()
                    end,
                    scripts(Project),
                    lustre@element@html:script(
                        [lustre@attribute:type_(<<"module"/utf8>>),
                            lustre@attribute:src(
                                <<<<"/"/utf8, Name/binary>>/binary, ".js"/utf8>>
                            )],
                        <<""/utf8>>
                    )]
            ),
            body(Project)]
    ),
    case Minify of
        true ->
            lustre@element:to_document_string(Html);

        false ->
            <<"<!doctype html>\n"/utf8,
                (lustre@element:to_readable_string(Html))/binary>>
    end.

-file("src/lustre_dev_tools/build/html.gleam", 76).
?DOC(false).
-spec dev(
    lustre_dev_tools@project:project(),
    binary(),
    gleam@option:option(binary())
) -> binary().
dev(Project, Entry, Tailwind_entry) ->
    Html = lustre@element@html:html(
        [lang(Project)],
        [lustre@element@html:head(
                [],
                [lustre@element@html:meta(
                        [lustre@attribute:charset(<<"utf-8"/utf8>>)]
                    ),
                    lustre@element@html:meta(
                        [lustre@attribute:name(<<"viewport"/utf8>>),
                            lustre@attribute:content(
                                <<"width=device-width, initial-scale=1"/utf8>>
                            )]
                    ),
                    meta(Project),
                    title(Project, Entry),
                    links(Project),
                    stylesheets(Project),
                    case Tailwind_entry of
                        {some, Entry@1} ->
                            lustre@element@html:link(
                                [lustre@attribute:rel(<<"stylesheet"/utf8>>),
                                    lustre@attribute:href(
                                        <<<<"/"/utf8, Entry@1/binary>>/binary,
                                            ".css"/utf8>>
                                    )]
                            );

                        none ->
                            lustre@element:none()
                    end,
                    scripts(Project),
                    lustre@element@html:script(
                        [lustre@attribute:src(
                                <<"/.lustre/server-hot-reload.js"/utf8>>
                            )],
                        <<""/utf8>>
                    ),
                    case erlang:element(10, Project) of
                        true ->
                            lustre@element@html:script(
                                [lustre@attribute:type_(<<"module"/utf8>>),
                                    lustre@attribute:src(
                                        <<Entry/binary, ".dev.js"/utf8>>
                                    )],
                                <<""/utf8>>
                            );

                        false ->
                            lustre@element@html:script(
                                [lustre@attribute:type_(<<"module"/utf8>>)],
                                begin
                                    _pipe = <<"
              import { main } from '/${name}/${entry}.mjs';

              main();
              "/utf8>>,
                                    _pipe@1 = gleam@string:replace(
                                        _pipe,
                                        <<"${name}"/utf8>>,
                                        erlang:element(2, Project)
                                    ),
                                    gleam@string:replace(
                                        _pipe@1,
                                        <<"${entry}"/utf8>>,
                                        Entry
                                    )
                                end
                            )
                    end]
            ),
            body(Project)]
    ),
    lustre@element:to_document_string(Html).
