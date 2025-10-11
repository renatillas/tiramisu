-module(plinth@browser@event).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/event.gleam").
-export_type([event/1, u_i_event/1, keyboard_event/0]).

-type event(ALIZ) :: any() | {gleam_phantom, ALIZ}.

-type u_i_event(ALJA) :: any() | {gleam_phantom, ALJA}.

-type keyboard_event() :: any().

-file("src/plinth/browser/event.gleam", 4).
-spec wrap_cast(
    gleam@dynamic:dynamic_(),
    fun((gleam@dynamic:dynamic_()) -> {ok, ALKV} | {error, nil})
) -> {ok, ALKV} | {error, gleam@dynamic@decode:decode_error()}.
wrap_cast(Raw, F) ->
    case F(Raw) of
        {ok, Event} ->
            {ok, Event};

        {error, nil} ->
            {error,
                {decode_error,
                    <<"Event"/utf8>>,
                    gleam_stdlib:classify_dynamic(Raw),
                    []}}
    end.
