-module(lustre@dev@query).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/dev/query.gleam").
-export([element/1, child/2, descendant/2, 'and'/2, tag/1, namespaced/2, attribute/2, class/1, style/2, id/1, data/2, test_id/1, aria/2, text/1, matches/2, to_readable_string/1, find_all/2, find_path/4, find/2, has/2]).
-export_type(['query'/0, selector/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque 'query'() :: {find_element, selector()} |
    {find_child, 'query'(), selector()} |
    {find_descendant, 'query'(), selector()}.

-opaque selector() :: {all, list(selector())} |
    {type, binary(), binary()} |
    {has_attribute, binary(), binary()} |
    {has_class, binary()} |
    {has_style, binary(), binary()} |
    {contains, binary()}.

-file("src/lustre/dev/query.gleam", 41).
?DOC(" Find any elements in a view that match the given [`Selector`](#Selector).\n").
-spec element(selector()) -> 'query'().
element(Selector) ->
    {find_element, Selector}.

-file("src/lustre/dev/query.gleam", 49).
?DOC(
    " Given a `Query` that finds an element, find any of that element's _direct_\n"
    " children that match the given [`Selector`](#Selector). This is similar to the\n"
    " CSS `>` combinator.\n"
).
-spec child('query'(), selector()) -> 'query'().
child(Parent, Selector) ->
    {find_child, Parent, Selector}.

-file("src/lustre/dev/query.gleam", 57).
?DOC(
    " Given a `Query` that finds an element, find any of that element's _descendants_\n"
    " that match the given [`Selector`](#Selector). This will walk the entire tree\n"
    " from the matching parent.\n"
).
-spec descendant('query'(), selector()) -> 'query'().
descendant(Parent, Selector) ->
    {find_descendant, Parent, Selector}.

-file("src/lustre/dev/query.gleam", 95).
?DOC(
    " Combine two selectors into one that must match both. For example, if you have\n"
    " a selector for div elements and a selector for elements with the class \"wibble\"\n"
    " then they can be combined into a selector that matches only div elements with\n"
    " the class \"wibble\".\n"
    "\n"
    " ```gleam\n"
    " import lustre/dev/query\n"
    "\n"
    " pub fn example() {\n"
    "   let div = query.tag(\"div\")\n"
    "   let wibble = query.class(\"wibble\")\n"
    "\n"
    "   query.element(matching: div |> query.and(wibble))\n"
    " }\n"
    " ```\n"
    "\n"
    " You can chain multiple `and` calls together to combine many selectors into\n"
    " something more specific.\n"
    "\n"
    " ```gleam\n"
    " import lustre/dev/query\n"
    "\n"
    " pub fn example() {\n"
    "   query.tag(\"div\")\n"
    "   |> query.and(query.class(\"wibble\"))\n"
    "   |> query.and(query.data(\"open\", \"true\"))\n"
    " }\n"
    " ```\n"
    "\n"
    " > **Note**: if you find yourself crafting complex selectors, consider using\n"
    " > a test id on the element(s) you want to find instead.\n"
).
-spec 'and'(selector(), selector()) -> selector().
'and'(First, Second) ->
    case First of
        {all, []} ->
            {all, [Second]};

        {all, Others} ->
            {all, [Second | Others]};

        _ ->
            {all, [First, Second]}
    end.

-file("src/lustre/dev/query.gleam", 107).
?DOC(
    " Select elements based on their tag name, like `\"div\"`, `\"span\"`, or `\"a\"`.\n"
    " To select elements with an XML namespace - such as SVG elements - use the\n"
    " [`namespaced`](#namespaced) selector instead.\n"
).
-spec tag(binary()) -> selector().
tag(Value) ->
    {type, <<""/utf8>>, Value}.

-file("src/lustre/dev/query.gleam", 125).
?DOC(
    " Select elements based on their tag name and XML namespace. This is useful\n"
    " for selecting SVG elements or other XML elements that have a namespace.\n"
    " For example, to select an SVG circle element, you would use:\n"
    "\n"
    " ```gleam\n"
    " import lustre/dev/query\n"
    "\n"
    " pub fn example() {\n"
    "   let svg = \"http://www.w3.org/2000/svg\"\n"
    "\n"
    "   query.element(matching: query.namespaced(svg, \"circle\"))\n"
    " }\n"
    " ```\n"
).
-spec namespaced(binary(), binary()) -> selector().
namespaced(Namespace, Tag) ->
    {type, Namespace, Tag}.

-file("src/lustre/dev/query.gleam", 154).
?DOC(
    " Select elements that have the specified attribute with the given value. If\n"
    " the value is left blank, this selector will match any element that has the\n"
    " attribute, _regardless of its value_.\n"
    "\n"
    " For example, to select a form input with the name \"username\", you would\n"
    " use:\n"
    "\n"
    " ```gleam\n"
    " import lustre/dev/query\n"
    "\n"
    " pub fn example() {\n"
    "   query.element(matching: query.attribute(\"name\", \"username\"))\n"
    " }\n"
    " ```\n"
    "\n"
    " Or to select elements with the `disabled` attribute:\n"
    "\n"
    " ```gleam\n"
    " import lustre/dev/query\n"
    "\n"
    " pub fn example() {\n"
    "   query.element(matching: query.attribute(\"disabled\", \"\"))\n"
    " }\n"
    " ```\n"
).
-spec attribute(binary(), binary()) -> selector().
attribute(Name, Value) ->
    {has_attribute, Name, Value}.

-file("src/lustre/dev/query.gleam", 169).
?DOC(
    " Select elements that include the given space-separated class name(s). For\n"
    " example given the element `<div class=\"foo bar baz\">`, the following selectors\n"
    " would match:\n"
    "\n"
    " - `query.class(\"foo\")`\n"
    "\n"
    " - `query.class(\"bar baz\")`\n"
    "\n"
    " If you need to match the class attribute exactly, you can use the [`attribute`](#attribute)\n"
    " selector instead.\n"
).
-spec class(binary()) -> selector().
class(Name) ->
    {has_class, Name}.

-file("src/lustre/dev/query.gleam", 177).
?DOC(
    " Select elements that have the specified inline style with the given value.\n"
    " If the value is left blank, this selector will match any element that has\n"
    " the given style, _regardless of its value_.\n"
).
-spec style(binary(), binary()) -> selector().
style(Name, Value) ->
    {has_style, Name, Value}.

-file("src/lustre/dev/query.gleam", 184).
?DOC(
    " Select an element based on its `id` attribute. Well-formed HTML means that\n"
    " only one element should have a given id.\n"
).
-spec id(binary()) -> selector().
id(Name) ->
    {has_attribute, <<"id"/utf8>>, Name}.

-file("src/lustre/dev/query.gleam", 199).
?DOC(
    " Select elements that have the given `data-*` attribute. For example you can\n"
    " select a custom disclosure element that is currently open with:\n"
    "\n"
    " ```gleam\n"
    " import lustre/dev/query\n"
    "\n"
    " pub fn example() {\n"
    "   query.element(matching: query.data(\"open\", \"true\"))\n"
    " }\n"
    " ```\n"
).
-spec data(binary(), binary()) -> selector().
data(Name, Value) ->
    {has_attribute, <<"data-"/utf8, Name/binary>>, Value}.

-file("src/lustre/dev/query.gleam", 207).
?DOC(
    " It is a common convention to use the `data-test-id` attribute to mark elements\n"
    " for easy selection in tests. This function is a shorthand for writing\n"
    " `query.data(\"test-id\", value)`\n"
).
-spec test_id(binary()) -> selector().
test_id(Value) ->
    data(<<"test-id"/utf8>>, Value).

-file("src/lustre/dev/query.gleam", 222).
?DOC(
    " Select elements that have the given `aria-*` attribute. For example you can\n"
    " select the trigger of a dropdown menu with:\n"
    "\n"
    " ```gleam\n"
    " import lustre/dev/query\n"
    "\n"
    " pub fn example() {\n"
    "   query.element(matching: query.aria(\"expanded\", \"true\"))\n"
    " }\n"
    " ```\n"
).
-spec aria(binary(), binary()) -> selector().
aria(Name, Value) ->
    {has_attribute, <<"aria-"/utf8, Name/binary>>, Value}.

-file("src/lustre/dev/query.gleam", 254).
?DOC(
    " Select elements whose text content matches the given string exactly. This\n"
    " includes text from **inline** children, but not from **block** children. For\n"
    " example, given the following HTML:\n"
    "\n"
    " ```html\n"
    " <p>Hello, <span class=\"font-bold\">Joe</span>!</p>\n"
    " ```\n"
    "\n"
    " The selector `query.text(\"Hello, Joe!\")` would match the `<p>` element because\n"
    " the text content of the inline `<span>` element is included in the paragraph's\n"
    " text content.\n"
    "\n"
    " Whitespace must match exactly, so the selector `query.text(\"Hello, Joe!\")`\n"
    " would not match an element like:\n"
    "\n"
    " ```gleam\n"
    " html.p([], [html.text(\"Hello,     Joe!\")])\n"
    " ```\n"
    "\n"
    " > **Note**: while this selector makes a best-effort attempt to include the\n"
    " > text content of inline children, this cannot account for block elements that\n"
    " > are styled as inline by CSS stylesheets.\n"
    "\n"
    " > **Note**: often it is better to use more precise selectors such as\n"
    " > [`id`](#id), [`class`](#class), or [`test_id`](#test_id). You should reach\n"
    " > for this selector only when you want to assert that an element contains\n"
    " > some specific text, such as in a hero banner or a copyright notice.\n"
).
-spec text(binary()) -> selector().
text(Content) ->
    {contains, Content}.

-file("src/lustre/dev/query.gleam", 610).
-spec text_content(lustre@vdom@vnode:element(any()), boolean(), binary()) -> binary().
text_content(Element, Inline, Content) ->
    case Element of
        {fragment, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(5, Element),
                Content,
                fun(Content@1, Child) ->
                    text_content(Child, true, Content@1)
                end
            );

        {element, _, _, _, _, _, _, _, _, _, _} when not Inline orelse (erlang:element(
            5,
            Element
        ) =/= <<""/utf8>>) ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@2, Child@1) ->
                    text_content(Child@1, true, Content@2)
                end
            );

        {element, _, _, _, _, <<"a"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"abbr"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"acronym"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"b"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"bdo"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"big"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"br"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"button"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"cite"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"code"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"dfn"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"em"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"i"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"img"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"input"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"kbd"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"label"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"map"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"object"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"output"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"q"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"samp"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"script"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"select"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"small"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"span"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"strong"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"sub"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"sup"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"textarea"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"time"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"tt"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, <<"var"/utf8>>, _, _, _, _, _} ->
            gleam@list:fold(
                erlang:element(8, Element),
                Content,
                fun(Content@3, Child@2) ->
                    text_content(Child@2, true, Content@3)
                end
            );

        {element, _, _, _, _, _, _, _, _, _, _} ->
            Rule = <<"display:inline"/utf8>>,
            Is_inline = gleam@list:any(
                erlang:element(7, Element),
                fun(Attribute) -> case Attribute of
                        {attribute, _, <<"style"/utf8>>, Value} ->
                            gleam_stdlib:contains_string(Value, Rule);

                        _ ->
                            false
                    end end
            ),
            case Is_inline of
                true ->
                    gleam@list:fold(
                        erlang:element(8, Element),
                        Content,
                        fun(Content@4, Child@3) ->
                            text_content(Child@3, true, Content@4)
                        end
                    );

                false ->
                    Content
            end;

        {text, _, _, _, _} ->
            <<Content/binary, (erlang:element(5, Element))/binary>>;

        {unsafe_inner_html, _, _, _, _, _, _, _} ->
            Content
    end.

-file("src/lustre/dev/query.gleam", 536).
?DOC(" Check if the given target element matches the given [`Selector`](#Selector).\n").
-spec matches(lustre@vdom@vnode:element(any()), selector()) -> boolean().
matches(Element, Selector) ->
    case {Element, Selector} of
        {_, {all, Selectors}} ->
            gleam@list:all(
                Selectors,
                fun(_capture) -> matches(Element, _capture) end
            );

        {{element, _, _, _, Namespace, Tag, _, _, _, _, _}, {type, _, _}} ->
            (Namespace =:= erlang:element(2, Selector)) andalso (Tag =:= erlang:element(
                3,
                Selector
            ));

        {{unsafe_inner_html, _, _, _, Namespace, Tag, _, _}, {type, _, _}} ->
            (Namespace =:= erlang:element(2, Selector)) andalso (Tag =:= erlang:element(
                3,
                Selector
            ));

        {{element, _, _, _, _, _, Attributes, _, _, _, _},
            {has_attribute, Name, <<""/utf8>>}} ->
            gleam@list:any(Attributes, fun(Attribute) -> case Attribute of
                        {attribute, _, _, _} ->
                            erlang:element(3, Attribute) =:= Name;

                        _ ->
                            false
                    end end);

        {{unsafe_inner_html, _, _, _, _, _, Attributes, _},
            {has_attribute, Name, <<""/utf8>>}} ->
            gleam@list:any(Attributes, fun(Attribute) -> case Attribute of
                        {attribute, _, _, _} ->
                            erlang:element(3, Attribute) =:= Name;

                        _ ->
                            false
                    end end);

        {{element, _, _, _, _, _, Attributes@1, _, _, _, _},
            {has_attribute, Name@1, Value}} ->
            gleam@list:contains(
                Attributes@1,
                lustre@attribute:attribute(Name@1, Value)
            );

        {{unsafe_inner_html, _, _, _, _, _, Attributes@1, _},
            {has_attribute, Name@1, Value}} ->
            gleam@list:contains(
                Attributes@1,
                lustre@attribute:attribute(Name@1, Value)
            );

        {{element, _, _, _, _, _, Attributes@2, _, _, _, _},
            {has_class, Name@2}} ->
            gleam@list:fold_until(
                gleam@string:split(Name@2, <<" "/utf8>>),
                true,
                fun(_, Class) ->
                    Name@3 = gleam@string:trim_end(Class),
                    Matches = gleam@list:any(
                        Attributes@2,
                        fun(Attribute@1) -> case Attribute@1 of
                                {attribute, _, <<"class"/utf8>>, Value@1} ->
                                    (((Value@1 =:= Name@3) orelse gleam_stdlib:string_starts_with(
                                        Value@1,
                                        <<Name@3/binary, " "/utf8>>
                                    ))
                                    orelse gleam_stdlib:string_ends_with(
                                        Value@1,
                                        <<" "/utf8, Name@3/binary>>
                                    ))
                                    orelse gleam_stdlib:contains_string(
                                        Value@1,
                                        <<<<" "/utf8, Name@3/binary>>/binary,
                                            " "/utf8>>
                                    );

                                _ ->
                                    false
                            end end
                    ),
                    case Matches of
                        true ->
                            {continue, true};

                        false ->
                            {stop, false}
                    end
                end
            );

        {{unsafe_inner_html, _, _, _, _, _, Attributes@2, _},
            {has_class, Name@2}} ->
            gleam@list:fold_until(
                gleam@string:split(Name@2, <<" "/utf8>>),
                true,
                fun(_, Class) ->
                    Name@3 = gleam@string:trim_end(Class),
                    Matches = gleam@list:any(
                        Attributes@2,
                        fun(Attribute@1) -> case Attribute@1 of
                                {attribute, _, <<"class"/utf8>>, Value@1} ->
                                    (((Value@1 =:= Name@3) orelse gleam_stdlib:string_starts_with(
                                        Value@1,
                                        <<Name@3/binary, " "/utf8>>
                                    ))
                                    orelse gleam_stdlib:string_ends_with(
                                        Value@1,
                                        <<" "/utf8, Name@3/binary>>
                                    ))
                                    orelse gleam_stdlib:contains_string(
                                        Value@1,
                                        <<<<" "/utf8, Name@3/binary>>/binary,
                                            " "/utf8>>
                                    );

                                _ ->
                                    false
                            end end
                    ),
                    case Matches of
                        true ->
                            {continue, true};

                        false ->
                            {stop, false}
                    end
                end
            );

        {{element, _, _, _, _, _, Attributes@3, _, _, _, _},
            {has_style, Name@4, Value@2}} ->
            Rule = <<<<<<Name@4/binary, ":"/utf8>>/binary, Value@2/binary>>/binary,
                ";"/utf8>>,
            gleam@list:any(Attributes@3, fun(Attribute@2) -> case Attribute@2 of
                        {attribute, _, <<"style"/utf8>>, Value@3} ->
                            gleam_stdlib:contains_string(Value@3, Rule);

                        _ ->
                            false
                    end end);

        {{unsafe_inner_html, _, _, _, _, _, Attributes@3, _},
            {has_style, Name@4, Value@2}} ->
            Rule = <<<<<<Name@4/binary, ":"/utf8>>/binary, Value@2/binary>>/binary,
                ";"/utf8>>,
            gleam@list:any(Attributes@3, fun(Attribute@2) -> case Attribute@2 of
                        {attribute, _, <<"style"/utf8>>, Value@3} ->
                            gleam_stdlib:contains_string(Value@3, Rule);

                        _ ->
                            false
                    end end);

        {{element, _, _, _, _, _, _, _, _, _, _}, {contains, Content}} ->
            _pipe = Element,
            _pipe@1 = text_content(_pipe, false, <<""/utf8>>),
            gleam_stdlib:contains_string(_pipe@1, Content);

        {_, _} ->
            false
    end.

-file("src/lustre/dev/query.gleam", 349).
-spec find_matching_in_list(
    list(lustre@vdom@vnode:element(SUC)),
    selector(),
    lustre@vdom@path:path(),
    integer()
) -> {ok, {lustre@vdom@vnode:element(SUC), lustre@vdom@path:path()}} |
    {error, nil}.
find_matching_in_list(Elements, Selector, Path, Index) ->
    case Elements of
        [] ->
            {error, nil};

        [{fragment, _, _, _, _, _} = First | Rest] ->
            find_matching_in_list(
                lists:append(erlang:element(5, First), Rest),
                Selector,
                lustre@vdom@path:add(Path, Index, erlang:element(3, First)),
                0
            );

        [First@1 | Rest@1] ->
            case matches(First@1, Selector) of
                true ->
                    {ok,
                        {First@1,
                            lustre@vdom@path:add(
                                Path,
                                Index,
                                erlang:element(3, First@1)
                            )}};

                false ->
                    find_matching_in_list(Rest@1, Selector, Path, Index + 1)
            end
    end.

-file("src/lustre/dev/query.gleam", 336).
-spec find_direct_child(
    lustre@vdom@vnode:element(STX),
    selector(),
    lustre@vdom@path:path()
) -> {ok, {lustre@vdom@vnode:element(STX), lustre@vdom@path:path()}} |
    {error, nil}.
find_direct_child(Parent, Selector, Path) ->
    case Parent of
        {element, _, _, _, _, _, _, Children, _, _, _} ->
            find_matching_in_list(Children, Selector, Path, 0);

        {fragment, _, _, _, Children, _} ->
            find_matching_in_list(Children, Selector, Path, 0);

        {unsafe_inner_html, _, _, _, _, _, _, _} ->
            {error, nil};

        {text, _, _, _, _} ->
            {error, nil}
    end.

-file("src/lustre/dev/query.gleam", 479).
-spec find_all_matching_in_list(
    list(lustre@vdom@vnode:element(SVK)),
    selector()
) -> list(lustre@vdom@vnode:element(SVK)).
find_all_matching_in_list(Elements, Selector) ->
    case Elements of
        [] ->
            [];

        [First | Rest] ->
            case matches(First, Selector) of
                true ->
                    [First | find_all_matching_in_list(Rest, Selector)];

                false ->
                    find_all_matching_in_list(Rest, Selector)
            end
    end.

-file("src/lustre/dev/query.gleam", 468).
-spec find_all_direct_children(lustre@vdom@vnode:element(SVG), selector()) -> list(lustre@vdom@vnode:element(SVG)).
find_all_direct_children(Parent, Selector) ->
    case Parent of
        {element, _, _, _, _, _, _, Children, _, _, _} ->
            find_all_matching_in_list(Children, Selector);

        {fragment, _, _, _, Children, _} ->
            find_all_matching_in_list(Children, Selector);

        {unsafe_inner_html, _, _, _, _, _, _, _} ->
            [];

        {text, _, _, _, _} ->
            []
    end.

-file("src/lustre/dev/query.gleam", 745).
-spec sort_selectors(list(selector())) -> list(selector()).
sort_selectors(Selectors) ->
    gleam@list:sort(
        begin
            gleam@list:flat_map(Selectors, fun(Selector) -> case Selector of
                        {all, Selectors@1} ->
                            Selectors@1;

                        _ ->
                            [Selector]
                    end end)
        end,
        fun(A, B) -> case {A, B} of
                {{all, _}, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"`All` selectors should be flattened"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lustre/dev/query"/utf8>>,
                            function => <<"sort_selectors"/utf8>>,
                            line => 756});

                {_, {all, _}} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"`All` selectors should be flattened"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"lustre/dev/query"/utf8>>,
                            function => <<"sort_selectors"/utf8>>,
                            line => 756});

                {{type, _, _}, {type, _, _}} ->
                    case gleam@string:compare(
                        erlang:element(2, A),
                        erlang:element(2, B)
                    ) of
                        eq ->
                            gleam@string:compare(
                                erlang:element(3, A),
                                erlang:element(3, B)
                            );

                        Order ->
                            Order
                    end;

                {{type, _, _}, _} ->
                    lt;

                {_, {type, _, _}} ->
                    gt;

                {{has_attribute, <<"id"/utf8>>, _},
                    {has_attribute, <<"id"/utf8>>, _}} ->
                    gleam@string:compare(
                        erlang:element(3, A),
                        erlang:element(3, B)
                    );

                {{has_attribute, <<"id"/utf8>>, _}, _} ->
                    lt;

                {_, {has_attribute, <<"id"/utf8>>, _}} ->
                    gt;

                {{has_attribute, _, _}, {has_attribute, _, _}} ->
                    case gleam@string:compare(
                        erlang:element(2, A),
                        erlang:element(2, B)
                    ) of
                        eq ->
                            gleam@string:compare(
                                erlang:element(3, A),
                                erlang:element(3, B)
                            );

                        Order@1 ->
                            Order@1
                    end;

                {{has_attribute, _, _}, _} ->
                    lt;

                {_, {has_attribute, _, _}} ->
                    gt;

                {{has_style, _, _}, {has_style, _, _}} ->
                    gleam@string:compare(
                        erlang:element(2, A),
                        erlang:element(2, B)
                    );

                {{has_style, _, _}, _} ->
                    lt;

                {_, {has_style, _, _}} ->
                    gt;

                {{has_class, _}, {has_class, _}} ->
                    gleam@string:compare(
                        erlang:element(2, A),
                        erlang:element(2, B)
                    );

                {{has_class, _}, _} ->
                    lt;

                {_, {has_class, _}} ->
                    gt;

                {{contains, _}, {contains, _}} ->
                    gleam@string:compare(
                        erlang:element(2, A),
                        erlang:element(2, B)
                    )
            end end
    ).

-file("src/lustre/dev/query.gleam", 718).
-spec selector_to_readable_string(selector()) -> binary().
selector_to_readable_string(Selector) ->
    case Selector of
        {all, []} ->
            <<""/utf8>>;

        {type, <<""/utf8>>, <<""/utf8>>} ->
            <<""/utf8>>;

        {has_attribute, <<""/utf8>>, _} ->
            <<""/utf8>>;

        {has_class, <<""/utf8>>} ->
            <<""/utf8>>;

        {has_style, <<""/utf8>>, _} ->
            <<""/utf8>>;

        {has_style, _, <<""/utf8>>} ->
            <<""/utf8>>;

        {contains, <<""/utf8>>} ->
            <<""/utf8>>;

        {all, Selectors} ->
            _pipe = Selectors,
            _pipe@1 = sort_selectors(_pipe),
            _pipe@2 = gleam@list:map(_pipe@1, fun selector_to_readable_string/1),
            erlang:list_to_binary(_pipe@2);

        {type, <<""/utf8>>, Tag} ->
            Tag;

        {type, Namespace, Tag@1} ->
            <<<<Namespace/binary, ":"/utf8>>/binary, Tag@1/binary>>;

        {has_attribute, <<"id"/utf8>>, Value} ->
            <<"#"/utf8, Value/binary>>;

        {has_attribute, Name, <<""/utf8>>} ->
            <<<<"["/utf8, Name/binary>>/binary, "]"/utf8>>;

        {has_attribute, Name@1, Value@1} ->
            <<<<<<<<"["/utf8, Name@1/binary>>/binary, "=\""/utf8>>/binary,
                    Value@1/binary>>/binary,
                "\"]"/utf8>>;

        {has_class, Name@2} ->
            <<"."/utf8, Name@2/binary>>;

        {has_style, Name@3, Value@2} ->
            <<<<<<<<"[style*=\""/utf8, Name@3/binary>>/binary, ":"/utf8>>/binary,
                    Value@2/binary>>/binary,
                "\"]"/utf8>>;

        {contains, Content} ->
            <<<<":contains(\""/utf8, Content/binary>>/binary, "\")"/utf8>>
    end.

-file("src/lustre/dev/query.gleam", 704).
?DOC(
    " Print a `Query` as a human-readable string similar to a CSS selector. This\n"
    " function is primarily intended for debugging and testing purposes: for example,\n"
    " you might use this to include the selector in a snapshot test for easier\n"
    " review.\n"
    "\n"
    " > **Note**: while similar, this function is not guaranteed to produce a valid\n"
    " > CSS selector. Specifically, queries that use the [`text`](#text) selector\n"
    " > will not be valid CSS selectors as they use the `:contains` pseudo-class,\n"
    " > which is not part of the CSS spec!\n"
).
-spec to_readable_string('query'()) -> binary().
to_readable_string(Query) ->
    case Query of
        {find_element, Selector} ->
            selector_to_readable_string(Selector);

        {find_child, Parent, Selector@1} ->
            <<<<(to_readable_string(Parent))/binary, " > "/utf8>>/binary,
                (selector_to_readable_string(Selector@1))/binary>>;

        {find_descendant, Parent@1, Selector@2} ->
            <<<<(to_readable_string(Parent@1))/binary, " "/utf8>>/binary,
                (selector_to_readable_string(Selector@2))/binary>>
    end.

-file("src/lustre/dev/query.gleam", 453).
-spec find_all_in_list(list(lustre@vdom@vnode:element(SVB)), 'query'()) -> list(lustre@vdom@vnode:element(SVB)).
find_all_in_list(Elements, Query) ->
    case Elements of
        [] ->
            [];

        [First | Rest] ->
            First_matches = find_all(First, Query),
            Rest_matches = find_all_in_list(Rest, Query),
            lists:append(First_matches, Rest_matches)
    end.

-file("src/lustre/dev/query.gleam", 418).
?DOC(
    " Like [`find`](#find) but returns every element in the view that matches the\n"
    " given query.\n"
).
-spec find_all(lustre@vdom@vnode:element(SUT), 'query'()) -> list(lustre@vdom@vnode:element(SUT)).
find_all(Root, Query) ->
    case Query of
        {find_element, Selector} ->
            case matches(Root, Selector) of
                true ->
                    [Root | find_all_in_children(Root, Query)];

                false ->
                    find_all_in_children(Root, Query)
            end;

        {find_child, Parent, Selector@1} ->
            _pipe = Root,
            _pipe@1 = find_all(_pipe, Parent),
            gleam@list:flat_map(
                _pipe@1,
                fun(_capture) ->
                    find_all_direct_children(_capture, Selector@1)
                end
            );

        {find_descendant, Parent@1, Selector@2} ->
            _pipe@2 = Root,
            _pipe@3 = find_all(_pipe@2, Parent@1),
            gleam@list:flat_map(
                _pipe@3,
                fun(_capture@1) ->
                    find_all_descendants(_capture@1, Selector@2)
                end
            )
    end.

-file("src/lustre/dev/query.gleam", 441).
-spec find_all_in_children(lustre@vdom@vnode:element(SUX), 'query'()) -> list(lustre@vdom@vnode:element(SUX)).
find_all_in_children(Element, Query) ->
    case Element of
        {element, _, _, _, _, _, _, Children, _, _, _} ->
            find_all_in_list(Children, Query);

        {fragment, _, _, _, Children, _} ->
            find_all_in_list(Children, Query);

        {unsafe_inner_html, _, _, _, _, _, _, _} ->
            [];

        {text, _, _, _, _} ->
            []
    end.

-file("src/lustre/dev/query.gleam", 509).
-spec find_all_descendants_in_list(
    list(lustre@vdom@vnode:element(SVT)),
    selector()
) -> list(lustre@vdom@vnode:element(SVT)).
find_all_descendants_in_list(Elements, Selector) ->
    case Elements of
        [] ->
            [];

        [First | Rest] ->
            First_matches = find_all_descendants(First, Selector),
            Rest_matches = find_all_descendants_in_list(Rest, Selector),
            lists:append(First_matches, Rest_matches)
    end.

-file("src/lustre/dev/query.gleam", 493).
-spec find_all_descendants(lustre@vdom@vnode:element(SVP), selector()) -> list(lustre@vdom@vnode:element(SVP)).
find_all_descendants(Parent, Selector) ->
    Direct_matches = find_all_direct_children(Parent, Selector),
    Descendant_matches = case Parent of
        {element, _, _, _, _, _, _, Children, _, _, _} ->
            find_all_descendants_in_list(Children, Selector);

        {fragment, _, _, _, Children, _} ->
            find_all_descendants_in_list(Children, Selector);

        {unsafe_inner_html, _, _, _, _, _, _, _} ->
            [];

        {text, _, _, _, _} ->
            []
    end,
    lists:append(Direct_matches, Descendant_matches).

-file("src/lustre/dev/query.gleam", 318).
-spec find_in_list(
    list(lustre@vdom@vnode:element(STR)),
    'query'(),
    lustre@vdom@path:path(),
    integer()
) -> {ok, {lustre@vdom@vnode:element(STR), lustre@vdom@path:path()}} |
    {error, nil}.
find_in_list(Elements, Query, Path, Index) ->
    case Elements of
        [] ->
            {error, nil};

        [First | Rest] ->
            case find_path(First, Query, Index, Path) of
                {ok, Element} ->
                    {ok, Element};

                {error, _} ->
                    find_in_list(Rest, Query, Path, Index + 1)
            end
    end.

-file("src/lustre/dev/query.gleam", 277).
?DOC(false).
-spec find_path(
    lustre@vdom@vnode:element(STH),
    'query'(),
    integer(),
    lustre@vdom@path:path()
) -> {ok, {lustre@vdom@vnode:element(STH), lustre@vdom@path:path()}} |
    {error, nil}.
find_path(Root, Query, Index, Path) ->
    case Query of
        {find_element, Selector} ->
            case matches(Root, Selector) of
                true ->
                    {ok,
                        {Root,
                            begin
                                _pipe = Path,
                                lustre@vdom@path:add(
                                    _pipe,
                                    Index,
                                    erlang:element(3, Root)
                                )
                            end}};

                false ->
                    find_in_children(Root, Query, Index, Path)
            end;

        {find_child, Parent, Selector@1} ->
            case find_path(Root, Parent, Index, Path) of
                {ok, {Element, Path@1}} ->
                    find_direct_child(Element, Selector@1, Path@1);

                {error, _} ->
                    {error, nil}
            end;

        {find_descendant, Parent@1, Selector@2} ->
            case find_path(Root, Parent@1, Index, Path) of
                {ok, {Element@1, Path@2}} ->
                    find_descendant(Element@1, Selector@2, Path@2);

                {error, _} ->
                    {error, nil}
            end
    end.

-file("src/lustre/dev/query.gleam", 304).
-spec find_in_children(
    lustre@vdom@vnode:element(STM),
    'query'(),
    integer(),
    lustre@vdom@path:path()
) -> {ok, {lustre@vdom@vnode:element(STM), lustre@vdom@path:path()}} |
    {error, nil}.
find_in_children(Element, Query, Index, Path) ->
    case Element of
        {element, _, _, _, _, _, _, Children, _, _, _} ->
            find_in_list(
                Children,
                Query,
                begin
                    _pipe = Path,
                    lustre@vdom@path:add(
                        _pipe,
                        Index,
                        erlang:element(3, Element)
                    )
                end,
                0
            );

        {fragment, _, _, _, Children, _} ->
            find_in_list(
                Children,
                Query,
                begin
                    _pipe = Path,
                    lustre@vdom@path:add(
                        _pipe,
                        Index,
                        erlang:element(3, Element)
                    )
                end,
                0
            );

        {unsafe_inner_html, _, _, _, _, _, _, _} ->
            {error, nil};

        {text, _, _, _, _} ->
            {error, nil}
    end.

-file("src/lustre/dev/query.gleam", 264).
?DOC(
    " Find the first element in a view that matches the given [`Query`](#Query).\n"
    " This is useful for tests when combined with [`element.to_readable_string`](../element.html#to_readable_string),\n"
    " allowing you to render large views but take more precise snapshots.\n"
).
-spec find(lustre@vdom@vnode:element(STC), 'query'()) -> {ok,
        lustre@vdom@vnode:element(STC)} |
    {error, nil}.
find(Root, Query) ->
    case find_path(Root, Query, 0, root) of
        {ok, {Element, _}} ->
            {ok, Element};

        {error, _} ->
            {error, nil}
    end.

-file("src/lustre/dev/query.gleam", 527).
?DOC(
    " Check if an element or any of its descendants match the given\n"
    " [`Selector`](#Selector).\n"
).
-spec has(lustre@vdom@vnode:element(any()), selector()) -> boolean().
has(Element, Selector) ->
    case find(Element, {find_element, Selector}) of
        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

-file("src/lustre/dev/query.gleam", 391).
-spec find_descendant_in_list(
    list(lustre@vdom@vnode:element(SUN)),
    selector(),
    lustre@vdom@path:path(),
    integer()
) -> {ok, {lustre@vdom@vnode:element(SUN), lustre@vdom@path:path()}} |
    {error, nil}.
find_descendant_in_list(Elements, Selector, Path, Index) ->
    case Elements of
        [] ->
            {error, nil};

        [First | Rest] ->
            case matches(First, Selector) of
                true ->
                    {ok,
                        {First,
                            lustre@vdom@path:add(
                                Path,
                                Index,
                                erlang:element(3, First)
                            )}};

                false ->
                    Child = lustre@vdom@path:add(
                        Path,
                        Index,
                        erlang:element(3, First)
                    ),
                    case find_descendant(First, Selector, Child) of
                        {ok, Element} ->
                            {ok, Element};

                        {error, _} ->
                            find_descendant_in_list(
                                Rest,
                                Selector,
                                Path,
                                Index + 1
                            )
                    end
            end
    end.

-file("src/lustre/dev/query.gleam", 374).
-spec find_descendant(
    lustre@vdom@vnode:element(SUI),
    selector(),
    lustre@vdom@path:path()
) -> {ok, {lustre@vdom@vnode:element(SUI), lustre@vdom@path:path()}} |
    {error, nil}.
find_descendant(Parent, Selector, Path) ->
    case find_direct_child(Parent, Selector, Path) of
        {ok, Element} ->
            {ok, Element};

        {error, _} ->
            case Parent of
                {element, _, _, _, _, _, _, Children, _, _, _} ->
                    find_descendant_in_list(Children, Selector, Path, 0);

                {fragment, _, _, _, Children, _} ->
                    find_descendant_in_list(Children, Selector, Path, 0);

                {unsafe_inner_html, _, _, _, _, _, _, _} ->
                    {error, nil};

                {text, _, _, _, _} ->
                    {error, nil}
            end
    end.
