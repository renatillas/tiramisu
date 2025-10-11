-module(lustre@vdom@vnode).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/vdom/vnode.gleam").
-export([to_keyed/2, fragment/4, element/9, text/3, unsafe_inner_html/6, to_snapshot/1, to_string_tree/1, to_string/1, to_json/1]).
-export_type([element/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type element(NZW) :: {fragment,
        integer(),
        binary(),
        fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
        list(element(NZW)),
        lustre@internals@mutable_map:mutable_map(binary(), element(NZW))} |
    {element,
        integer(),
        binary(),
        fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
        binary(),
        binary(),
        list(lustre@vdom@vattr:attribute(NZW)),
        list(element(NZW)),
        lustre@internals@mutable_map:mutable_map(binary(), element(NZW)),
        boolean(),
        boolean()} |
    {text,
        integer(),
        binary(),
        fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
        binary()} |
    {unsafe_inner_html,
        integer(),
        binary(),
        fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
        binary(),
        binary(),
        list(lustre@vdom@vattr:attribute(NZW)),
        binary()}.

-file("src/lustre/vdom/vnode.gleam", 106).
?DOC(false).
-spec is_void_element(binary(), binary()) -> boolean().
is_void_element(Tag, Namespace) ->
    case Namespace of
        <<""/utf8>> ->
            case Tag of
                <<"area"/utf8>> ->
                    true;

                <<"base"/utf8>> ->
                    true;

                <<"br"/utf8>> ->
                    true;

                <<"col"/utf8>> ->
                    true;

                <<"embed"/utf8>> ->
                    true;

                <<"hr"/utf8>> ->
                    true;

                <<"img"/utf8>> ->
                    true;

                <<"input"/utf8>> ->
                    true;

                <<"link"/utf8>> ->
                    true;

                <<"meta"/utf8>> ->
                    true;

                <<"param"/utf8>> ->
                    true;

                <<"source"/utf8>> ->
                    true;

                <<"track"/utf8>> ->
                    true;

                <<"wbr"/utf8>> ->
                    true;

                _ ->
                    false
            end;

        _ ->
            false
    end.

-file("src/lustre/vdom/vnode.gleam", 163).
?DOC(false).
-spec to_keyed(binary(), element(OAT)) -> element(OAT).
to_keyed(Key, Node) ->
    case Node of
        {element, _, _, _, _, _, _, _, _, _, _} ->
            {element,
                erlang:element(2, Node),
                Key,
                erlang:element(4, Node),
                erlang:element(5, Node),
                erlang:element(6, Node),
                erlang:element(7, Node),
                erlang:element(8, Node),
                erlang:element(9, Node),
                erlang:element(10, Node),
                erlang:element(11, Node)};

        {text, _, _, _, _} ->
            {text,
                erlang:element(2, Node),
                Key,
                erlang:element(4, Node),
                erlang:element(5, Node)};

        {unsafe_inner_html, _, _, _, _, _, _, _} ->
            {unsafe_inner_html,
                erlang:element(2, Node),
                Key,
                erlang:element(4, Node),
                erlang:element(5, Node),
                erlang:element(6, Node),
                erlang:element(7, Node),
                erlang:element(8, Node)};

        {fragment, _, _, _, _, _} ->
            {fragment,
                erlang:element(2, Node),
                Key,
                erlang:element(4, Node),
                erlang:element(5, Node),
                erlang:element(6, Node)}
    end.

-file("src/lustre/vdom/vnode.gleam", 210).
?DOC(false).
-spec text_to_json(integer(), binary(), binary()) -> gleam@json:json().
text_to_json(Kind, Key, Content) ->
    _pipe = lustre@internals@json_object_builder:tagged(Kind),
    _pipe@1 = lustre@internals@json_object_builder:string(
        _pipe,
        <<"key"/utf8>>,
        Key
    ),
    _pipe@2 = lustre@internals@json_object_builder:string(
        _pipe@1,
        <<"content"/utf8>>,
        Content
    ),
    lustre@internals@json_object_builder:build(_pipe@2).

-file("src/lustre/vdom/vnode.gleam", 217).
?DOC(false).
-spec unsafe_inner_html_to_json(
    integer(),
    binary(),
    binary(),
    binary(),
    list(lustre@vdom@vattr:attribute(any())),
    binary()
) -> gleam@json:json().
unsafe_inner_html_to_json(Kind, Key, Namespace, Tag, Attributes, Inner_html) ->
    _pipe = lustre@internals@json_object_builder:tagged(Kind),
    _pipe@1 = lustre@internals@json_object_builder:string(
        _pipe,
        <<"key"/utf8>>,
        Key
    ),
    _pipe@2 = lustre@internals@json_object_builder:string(
        _pipe@1,
        <<"namespace"/utf8>>,
        Namespace
    ),
    _pipe@3 = lustre@internals@json_object_builder:string(
        _pipe@2,
        <<"tag"/utf8>>,
        Tag
    ),
    _pipe@4 = lustre@internals@json_object_builder:list(
        _pipe@3,
        <<"attributes"/utf8>>,
        Attributes,
        fun lustre@vdom@vattr:to_json/1
    ),
    _pipe@5 = lustre@internals@json_object_builder:string(
        _pipe@4,
        <<"inner_html"/utf8>>,
        Inner_html
    ),
    lustre@internals@json_object_builder:build(_pipe@5).

-file("src/lustre/vdom/vnode.gleam", 70).
?DOC(false).
-spec fragment(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    list(element(NZX)),
    lustre@internals@mutable_map:mutable_map(binary(), element(NZX))
) -> element(NZX).
fragment(Key, Mapper, Children, Keyed_children) ->
    {fragment, 0, Key, Mapper, Children, Keyed_children}.

-file("src/lustre/vdom/vnode.gleam", 81).
?DOC(false).
-spec element(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    binary(),
    binary(),
    list(lustre@vdom@vattr:attribute(OAE)),
    list(element(OAE)),
    lustre@internals@mutable_map:mutable_map(binary(), element(OAE)),
    boolean(),
    boolean()
) -> element(OAE).
element(
    Key,
    Mapper,
    Namespace,
    Tag,
    Attributes,
    Children,
    Keyed_children,
    Self_closing,
    Void
) ->
    {element,
        1,
        Key,
        Mapper,
        Namespace,
        Tag,
        lustre@vdom@vattr:prepare(Attributes),
        Children,
        Keyed_children,
        Self_closing,
        Void orelse is_void_element(Tag, Namespace)}.

-file("src/lustre/vdom/vnode.gleam", 132).
?DOC(false).
-spec text(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    binary()
) -> element(any()).
text(Key, Mapper, Content) ->
    {text, 2, Key, Mapper, Content}.

-file("src/lustre/vdom/vnode.gleam", 142).
?DOC(false).
-spec unsafe_inner_html(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    binary(),
    binary(),
    list(lustre@vdom@vattr:attribute(OAP)),
    binary()
) -> element(OAP).
unsafe_inner_html(Key, Mapper, Namespace, Tag, Attributes, Inner_html) ->
    {unsafe_inner_html,
        3,
        Key,
        Mapper,
        Namespace,
        Tag,
        lustre@vdom@vattr:prepare(Attributes),
        Inner_html}.

-file("src/lustre/vdom/vnode.gleam", 382).
?DOC(false).
-spec children_to_snapshot_builder(
    gleam@string_tree:string_tree(),
    list(element(any())),
    boolean(),
    integer()
) -> gleam@string_tree:string_tree().
children_to_snapshot_builder(Html, Children, Raw_text, Indent) ->
    case Children of
        [{text, _, _, _, A}, {text, _, _, _, B} | Rest] ->
            children_to_snapshot_builder(
                Html,
                [{text,
                        2,
                        <<""/utf8>>,
                        fun gleam@function:identity/1,
                        <<A/binary, B/binary>>} |
                    Rest],
                Raw_text,
                Indent
            );

        [{fragment, _, _, _, _, _} = Child | Rest@1] ->
            _pipe = Child,
            _pipe@1 = do_to_snapshot_builder(_pipe, Raw_text, Indent),
            _pipe@2 = gleam_stdlib:iodata_append(Html, _pipe@1),
            children_to_snapshot_builder(_pipe@2, Rest@1, Raw_text, Indent);

        [Child@1 | Rest@2] ->
            _pipe@3 = Child@1,
            _pipe@4 = do_to_snapshot_builder(_pipe@3, Raw_text, Indent),
            _pipe@5 = gleam@string_tree:append(_pipe@4, <<"\n"/utf8>>),
            _pipe@6 = gleam_stdlib:iodata_append(Html, _pipe@5),
            children_to_snapshot_builder(_pipe@6, Rest@2, Raw_text, Indent);

        [] ->
            Html
    end.

-file("src/lustre/vdom/vnode.gleam", 304).
?DOC(false).
-spec do_to_snapshot_builder(element(any()), boolean(), integer()) -> gleam@string_tree:string_tree().
do_to_snapshot_builder(Node, Raw_text, Indent) ->
    Spaces = gleam@string:repeat(<<"  "/utf8>>, Indent),
    case Node of
        {text, _, _, _, <<""/utf8>>} ->
            gleam@string_tree:new();

        {text, _, _, _, Content} when Raw_text ->
            gleam_stdlib:identity([Spaces, Content]);

        {text, _, _, _, Content@1} ->
            gleam_stdlib:identity([Spaces, houdini:escape(Content@1)]);

        {fragment, _, _, _, [], _} ->
            gleam@string_tree:new();

        {fragment, _, _, _, Children, _} ->
            _pipe = gleam@string_tree:new(),
            children_to_snapshot_builder(_pipe, Children, Raw_text, Indent);

        {element, _, Key, _, Namespace, Tag, Attributes, _, _, Self_closing, _} when Self_closing ->
            Html = gleam_stdlib:identity(<<"<"/utf8, Tag/binary>>),
            Attributes@1 = lustre@vdom@vattr:to_string_tree(
                Key,
                Namespace,
                Attributes
            ),
            _pipe@1 = Html,
            _pipe@2 = gleam@string_tree:prepend(_pipe@1, Spaces),
            _pipe@3 = gleam_stdlib:iodata_append(_pipe@2, Attributes@1),
            gleam@string_tree:append(_pipe@3, <<"/>"/utf8>>);

        {element, _, Key@1, _, Namespace@1, Tag@1, Attributes@2, _, _, _, Void} when Void ->
            Html@1 = gleam_stdlib:identity(<<"<"/utf8, Tag@1/binary>>),
            Attributes@3 = lustre@vdom@vattr:to_string_tree(
                Key@1,
                Namespace@1,
                Attributes@2
            ),
            _pipe@4 = Html@1,
            _pipe@5 = gleam@string_tree:prepend(_pipe@4, Spaces),
            _pipe@6 = gleam_stdlib:iodata_append(_pipe@5, Attributes@3),
            gleam@string_tree:append(_pipe@6, <<">"/utf8>>);

        {element, _, Key@2, _, Namespace@2, Tag@2, Attributes@4, [], _, _, _} ->
            Html@2 = gleam_stdlib:identity(<<"<"/utf8, Tag@2/binary>>),
            Attributes@5 = lustre@vdom@vattr:to_string_tree(
                Key@2,
                Namespace@2,
                Attributes@4
            ),
            _pipe@7 = Html@2,
            _pipe@8 = gleam@string_tree:prepend(_pipe@7, Spaces),
            _pipe@9 = gleam_stdlib:iodata_append(_pipe@8, Attributes@5),
            _pipe@10 = gleam@string_tree:append(_pipe@9, <<">"/utf8>>),
            gleam@string_tree:append(
                _pipe@10,
                <<<<"</"/utf8, Tag@2/binary>>/binary, ">"/utf8>>
            );

        {element,
            _,
            Key@3,
            _,
            Namespace@3,
            Tag@3,
            Attributes@6,
            Children@1,
            _,
            _,
            _} ->
            Html@3 = gleam_stdlib:identity(<<"<"/utf8, Tag@3/binary>>),
            Attributes@7 = lustre@vdom@vattr:to_string_tree(
                Key@3,
                Namespace@3,
                Attributes@6
            ),
            _pipe@11 = Html@3,
            _pipe@12 = gleam@string_tree:prepend(_pipe@11, Spaces),
            _pipe@13 = gleam_stdlib:iodata_append(_pipe@12, Attributes@7),
            _pipe@14 = gleam@string_tree:append(_pipe@13, <<">\n"/utf8>>),
            _pipe@15 = children_to_snapshot_builder(
                _pipe@14,
                Children@1,
                Raw_text,
                Indent + 1
            ),
            _pipe@16 = gleam@string_tree:append(_pipe@15, Spaces),
            gleam@string_tree:append(
                _pipe@16,
                <<<<"</"/utf8, Tag@3/binary>>/binary, ">"/utf8>>
            );

        {unsafe_inner_html,
            _,
            Key@4,
            _,
            Namespace@4,
            Tag@4,
            Attributes@8,
            Inner_html} ->
            Html@4 = gleam_stdlib:identity(<<"<"/utf8, Tag@4/binary>>),
            Attributes@9 = lustre@vdom@vattr:to_string_tree(
                Key@4,
                Namespace@4,
                Attributes@8
            ),
            _pipe@17 = Html@4,
            _pipe@18 = gleam_stdlib:iodata_append(_pipe@17, Attributes@9),
            _pipe@19 = gleam@string_tree:append(_pipe@18, <<">"/utf8>>),
            _pipe@20 = gleam@string_tree:append(_pipe@19, Inner_html),
            gleam@string_tree:append(
                _pipe@20,
                <<<<"</"/utf8, Tag@4/binary>>/binary, ">"/utf8>>
            )
    end.

-file("src/lustre/vdom/vnode.gleam", 298).
?DOC(false).
-spec to_snapshot(element(any())) -> binary().
to_snapshot(Node) ->
    _pipe = Node,
    _pipe@1 = do_to_snapshot_builder(_pipe, false, 0),
    unicode:characters_to_binary(_pipe@1).

-file("src/lustre/vdom/vnode.gleam", 287).
?DOC(false).
-spec children_to_string_tree(
    gleam@string_tree:string_tree(),
    list(element(any()))
) -> gleam@string_tree:string_tree().
children_to_string_tree(Html, Children) ->
    gleam@list:fold(Children, Html, fun(Html@1, Child) -> _pipe = Child,
            _pipe@1 = to_string_tree(_pipe),
            gleam_stdlib:iodata_append(Html@1, _pipe@1) end).

-file("src/lustre/vdom/vnode.gleam", 235).
?DOC(false).
-spec to_string_tree(element(any())) -> gleam@string_tree:string_tree().
to_string_tree(Node) ->
    case Node of
        {text, _, _, _, <<""/utf8>>} ->
            gleam@string_tree:new();

        {text, _, _, _, Content} ->
            gleam_stdlib:identity(houdini:escape(Content));

        {fragment, _, _, _, Children, _} ->
            children_to_string_tree(gleam@string_tree:new(), Children);

        {element, _, Key, _, Namespace, Tag, Attributes, _, _, Self_closing, _} when Self_closing ->
            Html = gleam_stdlib:identity(<<"<"/utf8, Tag/binary>>),
            Attributes@1 = lustre@vdom@vattr:to_string_tree(
                Key,
                Namespace,
                Attributes
            ),
            _pipe = Html,
            _pipe@1 = gleam_stdlib:iodata_append(_pipe, Attributes@1),
            gleam@string_tree:append(_pipe@1, <<"/>"/utf8>>);

        {element, _, Key@1, _, Namespace@1, Tag@1, Attributes@2, _, _, _, Void} when Void ->
            Html@1 = gleam_stdlib:identity(<<"<"/utf8, Tag@1/binary>>),
            Attributes@3 = lustre@vdom@vattr:to_string_tree(
                Key@1,
                Namespace@1,
                Attributes@2
            ),
            _pipe@2 = Html@1,
            _pipe@3 = gleam_stdlib:iodata_append(_pipe@2, Attributes@3),
            gleam@string_tree:append(_pipe@3, <<">"/utf8>>);

        {element,
            _,
            Key@2,
            _,
            Namespace@2,
            Tag@2,
            Attributes@4,
            Children@1,
            _,
            _,
            _} ->
            Html@2 = gleam_stdlib:identity(<<"<"/utf8, Tag@2/binary>>),
            Attributes@5 = lustre@vdom@vattr:to_string_tree(
                Key@2,
                Namespace@2,
                Attributes@4
            ),
            _pipe@4 = Html@2,
            _pipe@5 = gleam_stdlib:iodata_append(_pipe@4, Attributes@5),
            _pipe@6 = gleam@string_tree:append(_pipe@5, <<">"/utf8>>),
            _pipe@7 = children_to_string_tree(_pipe@6, Children@1),
            gleam@string_tree:append(
                _pipe@7,
                <<<<"</"/utf8, Tag@2/binary>>/binary, ">"/utf8>>
            );

        {unsafe_inner_html,
            _,
            Key@3,
            _,
            Namespace@3,
            Tag@3,
            Attributes@6,
            Inner_html} ->
            Html@3 = gleam_stdlib:identity(<<"<"/utf8, Tag@3/binary>>),
            Attributes@7 = lustre@vdom@vattr:to_string_tree(
                Key@3,
                Namespace@3,
                Attributes@6
            ),
            _pipe@8 = Html@3,
            _pipe@9 = gleam_stdlib:iodata_append(_pipe@8, Attributes@7),
            _pipe@10 = gleam@string_tree:append(_pipe@9, <<">"/utf8>>),
            _pipe@11 = gleam@string_tree:append(_pipe@10, Inner_html),
            gleam@string_tree:append(
                _pipe@11,
                <<<<"</"/utf8, Tag@3/binary>>/binary, ">"/utf8>>
            )
    end.

-file("src/lustre/vdom/vnode.gleam", 229).
?DOC(false).
-spec to_string(element(any())) -> binary().
to_string(Node) ->
    _pipe = Node,
    _pipe@1 = to_string_tree(_pipe),
    unicode:characters_to_binary(_pipe@1).

-file("src/lustre/vdom/vnode.gleam", 193).
?DOC(false).
-spec fragment_to_json(integer(), binary(), list(element(any()))) -> gleam@json:json().
fragment_to_json(Kind, Key, Children) ->
    _pipe = lustre@internals@json_object_builder:tagged(Kind),
    _pipe@1 = lustre@internals@json_object_builder:string(
        _pipe,
        <<"key"/utf8>>,
        Key
    ),
    _pipe@2 = lustre@internals@json_object_builder:list(
        _pipe@1,
        <<"children"/utf8>>,
        Children,
        fun to_json/1
    ),
    lustre@internals@json_object_builder:build(_pipe@2).

-file("src/lustre/vdom/vnode.gleam", 174).
?DOC(false).
-spec to_json(element(any())) -> gleam@json:json().
to_json(Node) ->
    case Node of
        {fragment, Kind, Key, _, Children, _} ->
            fragment_to_json(Kind, Key, Children);

        {element,
            Kind@1,
            Key@1,
            _,
            Namespace,
            Tag,
            Attributes,
            Children@1,
            _,
            _,
            _} ->
            element_to_json(
                Kind@1,
                Key@1,
                Namespace,
                Tag,
                Attributes,
                Children@1
            );

        {text, Kind@2, Key@2, _, Content} ->
            text_to_json(Kind@2, Key@2, Content);

        {unsafe_inner_html,
            Kind@3,
            Key@3,
            _,
            Namespace@1,
            Tag@1,
            Attributes@1,
            Inner_html} ->
            unsafe_inner_html_to_json(
                Kind@3,
                Key@3,
                Namespace@1,
                Tag@1,
                Attributes@1,
                Inner_html
            )
    end.

-file("src/lustre/vdom/vnode.gleam", 200).
?DOC(false).
-spec element_to_json(
    integer(),
    binary(),
    binary(),
    binary(),
    list(lustre@vdom@vattr:attribute(OAW)),
    list(element(OAW))
) -> gleam@json:json().
element_to_json(Kind, Key, Namespace, Tag, Attributes, Children) ->
    _pipe = lustre@internals@json_object_builder:tagged(Kind),
    _pipe@1 = lustre@internals@json_object_builder:string(
        _pipe,
        <<"key"/utf8>>,
        Key
    ),
    _pipe@2 = lustre@internals@json_object_builder:string(
        _pipe@1,
        <<"namespace"/utf8>>,
        Namespace
    ),
    _pipe@3 = lustre@internals@json_object_builder:string(
        _pipe@2,
        <<"tag"/utf8>>,
        Tag
    ),
    _pipe@4 = lustre@internals@json_object_builder:list(
        _pipe@3,
        <<"attributes"/utf8>>,
        Attributes,
        fun lustre@vdom@vattr:to_json/1
    ),
    _pipe@5 = lustre@internals@json_object_builder:list(
        _pipe@4,
        <<"children"/utf8>>,
        Children,
        fun to_json/1
    ),
    lustre@internals@json_object_builder:build(_pipe@5).
