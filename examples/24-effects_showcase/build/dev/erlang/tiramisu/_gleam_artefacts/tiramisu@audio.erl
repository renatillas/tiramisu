-module(tiramisu@audio).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/audio.gleam").
-export([config/0, config_with_group/1, playing/0, with_state/2, with_playing/1, with_stopped/1, with_paused/1, with_fade/2, with_no_fade/1, with_volume/2, with_loop/2, with_playback_rate/2, with_group/2, with_on_end/2, global/2, positional/2, with_ref_distance/2, with_rolloff_factor/2, with_max_distance/2]).
-export_type([audio_buffer/0, audio_group/0, audio_state/0, fade_config/0, audio_config/0, audio/0, audio_source/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type audio_buffer() :: any().

-type audio_group() :: s_f_x | music | voice | ambient | {custom, binary()}.

-type audio_state() :: playing | stopped | paused.

-type fade_config() :: no_fade | {fade, integer()}.

-type audio_config() :: {audio_config,
        audio_state(),
        float(),
        boolean(),
        float(),
        fade_config(),
        gleam@option:option(audio_group()),
        gleam@option:option(fun(() -> nil))}.

-type audio() :: {global_audio, audio_buffer(), audio_config()} |
    {positional_audio,
        audio_buffer(),
        audio_config(),
        float(),
        float(),
        float()}.

-type audio_source() :: any().

-file("src/tiramisu/audio.gleam", 83).
?DOC(" Create default audio config (stopped, no fade)\n").
-spec config() -> audio_config().
config() ->
    {audio_config, stopped, 1.0, false, 1.0, no_fade, none, none}.

-file("src/tiramisu/audio.gleam", 96).
?DOC(" Create audio config with a group\n").
-spec config_with_group(audio_group()) -> audio_config().
config_with_group(Group) ->
    {audio_config, stopped, 1.0, false, 1.0, no_fade, {some, Group}, none}.

-file("src/tiramisu/audio.gleam", 109).
?DOC(" Create audio config that starts playing\n").
-spec playing() -> audio_config().
playing() ->
    {audio_config, playing, 1.0, false, 1.0, no_fade, none, none}.

-file("src/tiramisu/audio.gleam", 122).
?DOC(" Set playback state (Playing, Stopped, Paused)\n").
-spec with_state(audio_config(), audio_state()) -> audio_config().
with_state(Config, State) ->
    {audio_config,
        State,
        erlang:element(3, Config),
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(6, Config),
        erlang:element(7, Config),
        erlang:element(8, Config)}.

-file("src/tiramisu/audio.gleam", 127).
?DOC(" Set audio to playing\n").
-spec with_playing(audio_config()) -> audio_config().
with_playing(Config) ->
    {audio_config,
        playing,
        erlang:element(3, Config),
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(6, Config),
        erlang:element(7, Config),
        erlang:element(8, Config)}.

-file("src/tiramisu/audio.gleam", 132).
?DOC(" Set audio to stopped\n").
-spec with_stopped(audio_config()) -> audio_config().
with_stopped(Config) ->
    {audio_config,
        stopped,
        erlang:element(3, Config),
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(6, Config),
        erlang:element(7, Config),
        erlang:element(8, Config)}.

-file("src/tiramisu/audio.gleam", 137).
?DOC(" Set audio to paused\n").
-spec with_paused(audio_config()) -> audio_config().
with_paused(Config) ->
    {audio_config,
        paused,
        erlang:element(3, Config),
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(6, Config),
        erlang:element(7, Config),
        erlang:element(8, Config)}.

-file("src/tiramisu/audio.gleam", 142).
?DOC(" Set fade configuration\n").
-spec with_fade(audio_config(), integer()) -> audio_config().
with_fade(Config, Duration_ms) ->
    {audio_config,
        erlang:element(2, Config),
        erlang:element(3, Config),
        erlang:element(4, Config),
        erlang:element(5, Config),
        {fade, Duration_ms},
        erlang:element(7, Config),
        erlang:element(8, Config)}.

-file("src/tiramisu/audio.gleam", 147).
?DOC(" Set no fade (instant transitions)\n").
-spec with_no_fade(audio_config()) -> audio_config().
with_no_fade(Config) ->
    {audio_config,
        erlang:element(2, Config),
        erlang:element(3, Config),
        erlang:element(4, Config),
        erlang:element(5, Config),
        no_fade,
        erlang:element(7, Config),
        erlang:element(8, Config)}.

-file("src/tiramisu/audio.gleam", 152).
?DOC(" Set volume in config (0.0 to 1.0)\n").
-spec with_volume(audio_config(), float()) -> audio_config().
with_volume(Config, Volume) ->
    {audio_config,
        erlang:element(2, Config),
        Volume,
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(6, Config),
        erlang:element(7, Config),
        erlang:element(8, Config)}.

-file("src/tiramisu/audio.gleam", 157).
?DOC(" Set looping in config\n").
-spec with_loop(audio_config(), boolean()) -> audio_config().
with_loop(Config, Loop) ->
    {audio_config,
        erlang:element(2, Config),
        erlang:element(3, Config),
        Loop,
        erlang:element(5, Config),
        erlang:element(6, Config),
        erlang:element(7, Config),
        erlang:element(8, Config)}.

-file("src/tiramisu/audio.gleam", 162).
?DOC(" Set playback rate in config (1.0 = normal, 2.0 = double speed, etc.)\n").
-spec with_playback_rate(audio_config(), float()) -> audio_config().
with_playback_rate(Config, Rate) ->
    {audio_config,
        erlang:element(2, Config),
        erlang:element(3, Config),
        erlang:element(4, Config),
        Rate,
        erlang:element(6, Config),
        erlang:element(7, Config),
        erlang:element(8, Config)}.

-file("src/tiramisu/audio.gleam", 167).
?DOC(" Set audio group in config\n").
-spec with_group(audio_config(), audio_group()) -> audio_config().
with_group(Config, Group) ->
    {audio_config,
        erlang:element(2, Config),
        erlang:element(3, Config),
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(6, Config),
        {some, Group},
        erlang:element(8, Config)}.

-file("src/tiramisu/audio.gleam", 186).
?DOC(
    " Set callback to be called when audio ends (for non-looping audio)\n"
    "\n"
    " This is useful for one-shot sounds like SFX where you need to know\n"
    " when the sound has finished playing.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " audio.config()\n"
    " |> audio.with_state(audio.Playing)\n"
    " |> audio.with_on_end(fn() {\n"
    "   // Audio finished playing\n"
    "   io.println(\"SFX finished!\")\n"
    " })\n"
    " ```\n"
).
-spec with_on_end(audio_config(), fun(() -> nil)) -> audio_config().
with_on_end(Config, Callback) ->
    {audio_config,
        erlang:element(2, Config),
        erlang:element(3, Config),
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(6, Config),
        erlang:element(7, Config),
        {some, Callback}}.

-file("src/tiramisu/audio.gleam", 191).
?DOC(" Create global audio (2D, same volume everywhere)\n").
-spec global(audio_buffer(), audio_config()) -> audio().
global(Buffer, Config) ->
    {global_audio, Buffer, Config}.

-file("src/tiramisu/audio.gleam", 196).
?DOC(" Create a default positional audio configuration\n").
-spec positional(audio_buffer(), audio_config()) -> audio().
positional(Buffer, Config) ->
    {positional_audio, Buffer, Config, 1.0, 1.0, 10000.0}.

-file("src/tiramisu/audio.gleam", 207).
?DOC(" Set reference distance for positional audio\n").
-spec with_ref_distance(audio(), float()) -> audio().
with_ref_distance(Audio, Distance) ->
    case Audio of
        {positional_audio, Buffer, Config, _, Rolloff, Max} ->
            {positional_audio, Buffer, Config, Distance, Rolloff, Max};

        {global_audio, _, _} ->
            Audio
    end.

-file("src/tiramisu/audio.gleam", 222).
?DOC(" Set rolloff factor for positional audio\n").
-spec with_rolloff_factor(audio(), float()) -> audio().
with_rolloff_factor(Audio, Factor) ->
    case Audio of
        {positional_audio, Buffer, Config, Ref, _, Max} ->
            {positional_audio, Buffer, Config, Ref, Factor, Max};

        {global_audio, _, _} ->
            Audio
    end.

-file("src/tiramisu/audio.gleam", 237).
?DOC(" Set maximum distance for positional audio\n").
-spec with_max_distance(audio(), float()) -> audio().
with_max_distance(Audio, Distance) ->
    case Audio of
        {positional_audio, Buffer, Config, Ref, Rolloff, _} ->
            {positional_audio, Buffer, Config, Ref, Rolloff, Distance};

        {global_audio, _, _} ->
            Audio
    end.

-file("src/tiramisu/audio.gleam", 280).
?DOC(" Convert AudioGroup to string for FFI\n").
-spec audio_group_to_string(audio_group()) -> binary().
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
