-module(tiramisu@internal@audio_manager).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/internal/audio_manager.gleam").
-export([init/0, get_group_volume/2]).
-export_type([three_audio_source/0, audio_source_data/0, pending_playback/0, audio_manager_state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-opaque three_audio_source() :: {three_audio_source, gleam@dynamic:dynamic_()}.

-type audio_source_data() :: {audio_source_data,
        three_audio_source(),
        float(),
        gleam@option:option(binary()),
        gleam@option:option(tiramisu@audio:audio_state()),
        gleam@option:option(fun(() -> nil))}.

-type pending_playback() :: {pending_playback,
        binary(),
        audio_source_data(),
        integer()}.

-type audio_manager_state() :: {audio_manager_state,
        gleam@dict:dict(binary(), audio_source_data()),
        gleam@dict:dict(binary(), float()),
        list(binary()),
        boolean(),
        list(pending_playback())}.

-file("src/tiramisu/internal/audio_manager.gleam", 69).
?DOC(false).
-spec init() -> audio_manager_state().
init() ->
    {audio_manager_state,
        maps:new(),
        begin
            _pipe = maps:new(),
            _pipe@1 = gleam@dict:insert(_pipe, <<"sfx"/utf8>>, 1.0),
            _pipe@2 = gleam@dict:insert(_pipe@1, <<"music"/utf8>>, 1.0),
            _pipe@3 = gleam@dict:insert(_pipe@2, <<"voice"/utf8>>, 1.0),
            gleam@dict:insert(_pipe@3, <<"ambient"/utf8>>, 1.0)
        end,
        [],
        false,
        []}.

-file("src/tiramisu/internal/audio_manager.gleam", 499).
?DOC(false).
-spec get_group_volume(audio_manager_state(), binary()) -> float().
get_group_volume(State, Group_name) ->
    case gleam_stdlib:map_get(erlang:element(3, State), Group_name) of
        {ok, Volume} ->
            Volume;

        {error, _} ->
            1.0
    end.

-file("src/tiramisu/internal/audio_manager.gleam", 627).
?DOC(false).
-spec calculate_effective_volume(
    audio_manager_state(),
    float(),
    gleam@option:option(binary())
) -> float().
calculate_effective_volume(State, Base_volume, Group) ->
    case Group of
        none ->
            Base_volume;

        {some, Group_name} ->
            Group_volume = get_group_volume(State, Group_name),
            Is_muted = gleam@list:contains(erlang:element(4, State), Group_name),
            case Is_muted of
                true ->
                    +0.0;

                false ->
                    Base_volume * Group_volume
            end
    end.

-file("src/tiramisu/internal/audio_manager.gleam", 647).
?DOC(false).
-spec clamp_volume(float()) -> float().
clamp_volume(Volume) ->
    case Volume < +0.0 of
        true ->
            +0.0;

        false ->
            case Volume > 1.0 of
                true ->
                    1.0;

                false ->
                    Volume
            end
    end.

-file("src/tiramisu/internal/audio_manager.gleam", 659).
?DOC(false).
-spec audio_group_to_string(tiramisu@audio:audio_group()) -> binary().
audio_group_to_string(Group) ->
    case Group of
        s_f_x ->
            <<"sfx"/utf8>>;

        music ->
            <<"music"/utf8>>;

        voice ->
            <<"voice"/utf8>>;

        ambient ->
            <<"ambient"/utf8>>;

        {custom, Name} ->
            Name
    end.

-file("src/tiramisu/internal/audio_manager.gleam", 673).
?DOC(false).
-spec wrap_audio_source(gleam@dynamic:dynamic_()) -> three_audio_source().
wrap_audio_source(Source) ->
    {three_audio_source, Source}.

-file("src/tiramisu/internal/audio_manager.gleam", 677).
?DOC(false).
-spec unwrap_audio_source(three_audio_source()) -> gleam@dynamic:dynamic_().
unwrap_audio_source(Source) ->
    erlang:element(2, Source).
