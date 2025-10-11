-module(plinth@browser@element).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/element.gleam").
-export_type([element/0, position/0]).

-type element() :: any().

-type position() :: before_begin | after_begin | before_end | after_end.

-file("src/plinth/browser/element.gleam", 48).
-spec position_to_string(position()) -> binary().
position_to_string(Position) ->
    case Position of
        before_begin ->
            <<"beforebegin"/utf8>>;

        after_begin ->
            <<"afterbegin"/utf8>>;

        before_end ->
            <<"beforeend"/utf8>>;

        after_end ->
            <<"afterend"/utf8>>
    end.
