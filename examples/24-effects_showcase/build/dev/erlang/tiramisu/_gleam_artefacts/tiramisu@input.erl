-module(tiramisu@input).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/input.gleam").
-export([new/0, mouse_position/1, mouse_delta/1, is_left_button_pressed/1, is_left_button_just_pressed/1, is_right_button_pressed/1, is_right_button_just_pressed/1, mouse_wheel_delta/1, touches/1, touches_just_started/1, touches_just_ended/1, touch_count/1, is_gamepad_connected/2, gamepad_button/3, is_gamepad_button_pressed/3, gamepad_axis/3, is_key_pressed/2, is_key_just_pressed/2, is_key_just_released/2, get_axis_with_deadzone/4, is_left_stick_active/3, is_right_stick_active/3, is_primary_connected/1, is_primary_gamepad_button_pressed/2, get_primary_button/2, get_primary_axis/2, new_bindings/0, bind_key/3, bind_mouse_button/3, bind_gamepad_button/3, is_action_pressed/3, is_action_just_pressed/3, is_action_just_released/3, get_action_value/3, with_buffer/1, update_buffer/3, was_action_pressed_buffered/2, consume_buffered_action/2, clear_buffer/1]).
-export_type([input_state/0, keyboard_state/0, mouse_state/0, button_state/0, gamepad_state/0, touch_state/0, touch/0, key/0, gamepad_button/0, gamepad_axis/0, mouse_button/0, input_bindings/1, buffered_input/1, buffered_action/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Input module - keyboard, mouse, gamepad, and touch input handling.\n"
    "\n"
    " Input state is automatically updated each frame and passed to your `update` function\n"
    " via the `Context`. Query the input state to respond to player actions.\n"
    "\n"
    " ## Quick Example\n"
    "\n"
    " ```gleam\n"
    " import tiramisu/input\n"
    "\n"
    " fn update(model, msg, ctx) {\n"
    "   // Check if player is pressing W to move forward\n"
    "   let move_forward = case input.is_key_pressed(ctx.input, input.KeyW) {\n"
    "     True -> 1.0\n"
    "     False -> 0.0\n"
    "   }\n"
    "\n"
    "   // Check mouse button\n"
    "   let shooting = input.is_left_button_pressed(ctx.input)\n"
    "\n"
    "   // Update model based on input\n"
    "   Model(..model, position: move_player(model.position, move_forward))\n"
    " }\n"
    " ```\n"
).

-opaque input_state() :: {input_state,
        keyboard_state(),
        mouse_state(),
        list(gamepad_state()),
        touch_state()}.

-opaque keyboard_state() :: {keyboard_state,
        list(binary()),
        list(binary()),
        list(binary())}.

-opaque mouse_state() :: {mouse_state,
        float(),
        float(),
        float(),
        float(),
        float(),
        button_state(),
        button_state(),
        button_state()}.

-type button_state() :: {button_state, boolean(), boolean(), boolean()}.

-type gamepad_state() :: {gamepad_state,
        boolean(),
        list(float()),
        list(float())}.

-type touch_state() :: {touch_state,
        list(touch()),
        list(touch()),
        list(touch())}.

-type touch() :: {touch, integer(), float(), float()}.

-type key() :: key_a |
    key_b |
    key_c |
    key_d |
    key_e |
    key_f |
    key_g |
    key_h |
    key_i |
    key_j |
    key_k |
    key_l |
    key_m |
    key_n |
    key_o |
    key_p |
    key_q |
    key_r |
    key_s |
    key_t |
    key_u |
    key_v |
    key_w |
    key_x |
    key_y |
    key_z |
    digit0 |
    digit1 |
    digit2 |
    digit3 |
    digit4 |
    digit5 |
    digit6 |
    digit7 |
    digit8 |
    digit9 |
    f1 |
    f2 |
    f3 |
    f4 |
    f5 |
    f6 |
    f7 |
    f8 |
    f9 |
    f10 |
    f11 |
    f12 |
    arrow_up |
    arrow_down |
    arrow_left |
    arrow_right |
    shift_left |
    shift_right |
    control_left |
    control_right |
    alt_left |
    alt_right |
    meta_left |
    meta_right |
    space |
    enter |
    escape |
    tab |
    backspace |
    delete |
    insert |
    home |
    'end' |
    page_up |
    page_down |
    caps_lock |
    minus |
    equal |
    bracket_left |
    bracket_right |
    backslash |
    semicolon |
    quote |
    comma |
    period |
    slash |
    backquote |
    numpad0 |
    numpad1 |
    numpad2 |
    numpad3 |
    numpad4 |
    numpad5 |
    numpad6 |
    numpad7 |
    numpad8 |
    numpad9 |
    numpad_add |
    numpad_subtract |
    numpad_multiply |
    numpad_divide |
    numpad_decimal |
    numpad_enter |
    num_lock |
    audio_volume_up |
    audio_volume_down |
    audio_volume_mute |
    media_play_pause |
    media_stop |
    media_track_next |
    media_track_previous |
    print_screen |
    scroll_lock |
    pause |
    context_menu |
    {custom, binary()}.

-type gamepad_button() :: button_a |
    button_b |
    button_x |
    button_y |
    left_bumper |
    right_bumper |
    left_trigger |
    right_trigger |
    select |
    start |
    left_stick |
    right_stick |
    d_pad_up |
    d_pad_down |
    d_pad_left |
    d_pad_right |
    home_button.

-type gamepad_axis() :: left_stick_x |
    left_stick_y |
    right_stick_x |
    right_stick_y.

-type mouse_button() :: left_button | right_button | middle_button.

-opaque input_bindings(APHO) :: {input_bindings,
        list({key(), APHO}),
        list({mouse_button(), APHO}),
        list({gamepad_button(), APHO})}.

-opaque buffered_input(APHP) :: {buffered_input,
        list(buffered_action(APHP)),
        integer(),
        integer()}.

-type buffered_action(APHQ) :: {buffered_action, APHQ, integer()}.

-file("src/tiramisu/input.gleam", 87).
-spec new() -> input_state().
new() ->
    {input_state,
        {keyboard_state, [], [], []},
        {mouse_state,
            +0.0,
            +0.0,
            +0.0,
            +0.0,
            +0.0,
            {button_state, false, false, false},
            {button_state, false, false, false},
            {button_state, false, false, false}},
        [],
        {touch_state, [], [], []}}.

-file("src/tiramisu/input.gleam", 148).
?DOC(" Get mouse position\n").
-spec mouse_position(input_state()) -> {float(), float()}.
mouse_position(Input) ->
    {erlang:element(2, erlang:element(3, Input)),
        erlang:element(3, erlang:element(3, Input))}.

-file("src/tiramisu/input.gleam", 153).
?DOC(" Get mouse delta\n").
-spec mouse_delta(input_state()) -> {float(), float()}.
mouse_delta(Input) ->
    {erlang:element(4, erlang:element(3, Input)),
        erlang:element(5, erlang:element(3, Input))}.

-file("src/tiramisu/input.gleam", 158).
?DOC(" Check if left mouse button is pressed\n").
-spec is_left_button_pressed(input_state()) -> boolean().
is_left_button_pressed(Input) ->
    erlang:element(2, erlang:element(7, erlang:element(3, Input))).

-file("src/tiramisu/input.gleam", 163).
?DOC(" Check if left mouse button was just pressed\n").
-spec is_left_button_just_pressed(input_state()) -> boolean().
is_left_button_just_pressed(Input) ->
    erlang:element(3, erlang:element(7, erlang:element(3, Input))).

-file("src/tiramisu/input.gleam", 168).
?DOC(" Check if right mouse button is pressed\n").
-spec is_right_button_pressed(input_state()) -> boolean().
is_right_button_pressed(Input) ->
    erlang:element(2, erlang:element(9, erlang:element(3, Input))).

-file("src/tiramisu/input.gleam", 173).
?DOC(" Check if right mouse button was just pressed\n").
-spec is_right_button_just_pressed(input_state()) -> boolean().
is_right_button_just_pressed(Input) ->
    erlang:element(3, erlang:element(9, erlang:element(3, Input))).

-file("src/tiramisu/input.gleam", 178).
?DOC(" Get mouse wheel delta\n").
-spec mouse_wheel_delta(input_state()) -> float().
mouse_wheel_delta(Input) ->
    erlang:element(6, erlang:element(3, Input)).

-file("src/tiramisu/input.gleam", 234).
?DOC(" Get current touches\n").
-spec touches(input_state()) -> list(touch()).
touches(Input) ->
    erlang:element(2, erlang:element(5, Input)).

-file("src/tiramisu/input.gleam", 239).
?DOC(" Get touches that just started\n").
-spec touches_just_started(input_state()) -> list(touch()).
touches_just_started(Input) ->
    erlang:element(3, erlang:element(5, Input)).

-file("src/tiramisu/input.gleam", 244).
?DOC(" Get touches that just ended\n").
-spec touches_just_ended(input_state()) -> list(touch()).
touches_just_ended(Input) ->
    erlang:element(4, erlang:element(5, Input)).

-file("src/tiramisu/input.gleam", 249).
?DOC(" Get touch count\n").
-spec touch_count(input_state()) -> integer().
touch_count(Input) ->
    erlang:length(erlang:element(2, erlang:element(5, Input))).

-file("src/tiramisu/input.gleam", 253).
-spec list_get(list(APHU), integer()) -> {ok, APHU} | {error, nil}.
list_get(List, Index) ->
    case {List, Index} of
        {[], _} ->
            {error, nil};

        {[X | _], 0} ->
            {ok, X};

        {[_ | Rest], N} ->
            list_get(Rest, N - 1)
    end.

-file("src/tiramisu/input.gleam", 185).
?DOC(" Check if gamepad at index is connected\n").
-spec is_gamepad_connected(input_state(), integer()) -> boolean().
is_gamepad_connected(Input, Index) ->
    case list_get(erlang:element(4, Input), Index) of
        {ok, Gamepad} ->
            erlang:element(2, Gamepad);

        {error, _} ->
            false
    end.

-file("src/tiramisu/input.gleam", 262).
-spec gamepad_button_to_index(gamepad_button()) -> integer().
gamepad_button_to_index(Button) ->
    case Button of
        button_a ->
            0;

        button_b ->
            1;

        button_x ->
            2;

        button_y ->
            3;

        left_bumper ->
            4;

        right_bumper ->
            5;

        left_trigger ->
            6;

        right_trigger ->
            7;

        select ->
            8;

        start ->
            9;

        left_stick ->
            10;

        right_stick ->
            11;

        d_pad_up ->
            12;

        d_pad_down ->
            13;

        d_pad_left ->
            14;

        d_pad_right ->
            15;

        home_button ->
            16
    end.

-file("src/tiramisu/input.gleam", 193).
?DOC(" Get gamepad button value\n").
-spec gamepad_button(input_state(), integer(), gamepad_button()) -> float().
gamepad_button(Input, Gamepad_index, Button) ->
    Button_index = gamepad_button_to_index(Button),
    case list_get(erlang:element(4, Input), Gamepad_index) of
        {ok, Gamepad} ->
            _pipe = list_get(erlang:element(3, Gamepad), Button_index),
            gleam@result:unwrap(_pipe, +0.0);

        {error, _} ->
            +0.0
    end.

-file("src/tiramisu/input.gleam", 208).
?DOC(" Check if gamepad button is pressed\n").
-spec is_gamepad_button_pressed(input_state(), integer(), gamepad_button()) -> boolean().
is_gamepad_button_pressed(Input, Gamepad_index, Button) ->
    gamepad_button(Input, Gamepad_index, Button) > 0.5.

-file("src/tiramisu/input.gleam", 284).
-spec gamepad_axis_to_index(gamepad_axis()) -> integer().
gamepad_axis_to_index(Axis) ->
    case Axis of
        left_stick_x ->
            0;

        left_stick_y ->
            1;

        right_stick_x ->
            2;

        right_stick_y ->
            3
    end.

-file("src/tiramisu/input.gleam", 217).
?DOC(" Get gamepad axis value\n").
-spec gamepad_axis(input_state(), integer(), gamepad_axis()) -> float().
gamepad_axis(Input, Gamepad_index, Axis) ->
    Axis_index = gamepad_axis_to_index(Axis),
    case list_get(erlang:element(4, Input), Gamepad_index) of
        {ok, Gamepad} ->
            _pipe = list_get(erlang:element(4, Gamepad), Axis_index),
            gleam@result:unwrap(_pipe, +0.0);

        {error, _} ->
            +0.0
    end.

-file("src/tiramisu/input.gleam", 421).
?DOC(" Convert Key to JavaScript KeyboardEvent.code string\n").
-spec key_to_code(key()) -> binary().
key_to_code(Key) ->
    case Key of
        key_a ->
            <<"KeyA"/utf8>>;

        key_b ->
            <<"KeyB"/utf8>>;

        key_c ->
            <<"KeyC"/utf8>>;

        key_d ->
            <<"KeyD"/utf8>>;

        key_e ->
            <<"KeyE"/utf8>>;

        key_f ->
            <<"KeyF"/utf8>>;

        key_g ->
            <<"KeyG"/utf8>>;

        key_h ->
            <<"KeyH"/utf8>>;

        key_i ->
            <<"KeyI"/utf8>>;

        key_j ->
            <<"KeyJ"/utf8>>;

        key_k ->
            <<"KeyK"/utf8>>;

        key_l ->
            <<"KeyL"/utf8>>;

        key_m ->
            <<"KeyM"/utf8>>;

        key_n ->
            <<"KeyN"/utf8>>;

        key_o ->
            <<"KeyO"/utf8>>;

        key_p ->
            <<"KeyP"/utf8>>;

        key_q ->
            <<"KeyQ"/utf8>>;

        key_r ->
            <<"KeyR"/utf8>>;

        key_s ->
            <<"KeyS"/utf8>>;

        key_t ->
            <<"KeyT"/utf8>>;

        key_u ->
            <<"KeyU"/utf8>>;

        key_v ->
            <<"KeyV"/utf8>>;

        key_w ->
            <<"KeyW"/utf8>>;

        key_x ->
            <<"KeyX"/utf8>>;

        key_y ->
            <<"KeyY"/utf8>>;

        key_z ->
            <<"KeyZ"/utf8>>;

        digit0 ->
            <<"Digit0"/utf8>>;

        digit1 ->
            <<"Digit1"/utf8>>;

        digit2 ->
            <<"Digit2"/utf8>>;

        digit3 ->
            <<"Digit3"/utf8>>;

        digit4 ->
            <<"Digit4"/utf8>>;

        digit5 ->
            <<"Digit5"/utf8>>;

        digit6 ->
            <<"Digit6"/utf8>>;

        digit7 ->
            <<"Digit7"/utf8>>;

        digit8 ->
            <<"Digit8"/utf8>>;

        digit9 ->
            <<"Digit9"/utf8>>;

        f1 ->
            <<"F1"/utf8>>;

        f2 ->
            <<"F2"/utf8>>;

        f3 ->
            <<"F3"/utf8>>;

        f4 ->
            <<"F4"/utf8>>;

        f5 ->
            <<"F5"/utf8>>;

        f6 ->
            <<"F6"/utf8>>;

        f7 ->
            <<"F7"/utf8>>;

        f8 ->
            <<"F8"/utf8>>;

        f9 ->
            <<"F9"/utf8>>;

        f10 ->
            <<"F10"/utf8>>;

        f11 ->
            <<"F11"/utf8>>;

        f12 ->
            <<"F12"/utf8>>;

        arrow_up ->
            <<"ArrowUp"/utf8>>;

        arrow_down ->
            <<"ArrowDown"/utf8>>;

        arrow_left ->
            <<"ArrowLeft"/utf8>>;

        arrow_right ->
            <<"ArrowRight"/utf8>>;

        shift_left ->
            <<"ShiftLeft"/utf8>>;

        shift_right ->
            <<"ShiftRight"/utf8>>;

        control_left ->
            <<"ControlLeft"/utf8>>;

        control_right ->
            <<"ControlRight"/utf8>>;

        alt_left ->
            <<"AltLeft"/utf8>>;

        alt_right ->
            <<"AltRight"/utf8>>;

        meta_left ->
            <<"MetaLeft"/utf8>>;

        meta_right ->
            <<"MetaRight"/utf8>>;

        space ->
            <<"Space"/utf8>>;

        enter ->
            <<"Enter"/utf8>>;

        escape ->
            <<"Escape"/utf8>>;

        tab ->
            <<"Tab"/utf8>>;

        backspace ->
            <<"Backspace"/utf8>>;

        delete ->
            <<"Delete"/utf8>>;

        insert ->
            <<"Insert"/utf8>>;

        home ->
            <<"Home"/utf8>>;

        'end' ->
            <<"End"/utf8>>;

        page_up ->
            <<"PageUp"/utf8>>;

        page_down ->
            <<"PageDown"/utf8>>;

        caps_lock ->
            <<"CapsLock"/utf8>>;

        minus ->
            <<"Minus"/utf8>>;

        equal ->
            <<"Equal"/utf8>>;

        bracket_left ->
            <<"BracketLeft"/utf8>>;

        bracket_right ->
            <<"BracketRight"/utf8>>;

        backslash ->
            <<"Backslash"/utf8>>;

        semicolon ->
            <<"Semicolon"/utf8>>;

        quote ->
            <<"Quote"/utf8>>;

        comma ->
            <<"Comma"/utf8>>;

        period ->
            <<"Period"/utf8>>;

        slash ->
            <<"Slash"/utf8>>;

        backquote ->
            <<"Backquote"/utf8>>;

        numpad0 ->
            <<"Numpad0"/utf8>>;

        numpad1 ->
            <<"Numpad1"/utf8>>;

        numpad2 ->
            <<"Numpad2"/utf8>>;

        numpad3 ->
            <<"Numpad3"/utf8>>;

        numpad4 ->
            <<"Numpad4"/utf8>>;

        numpad5 ->
            <<"Numpad5"/utf8>>;

        numpad6 ->
            <<"Numpad6"/utf8>>;

        numpad7 ->
            <<"Numpad7"/utf8>>;

        numpad8 ->
            <<"Numpad8"/utf8>>;

        numpad9 ->
            <<"Numpad9"/utf8>>;

        numpad_add ->
            <<"NumpadAdd"/utf8>>;

        numpad_subtract ->
            <<"NumpadSubtract"/utf8>>;

        numpad_multiply ->
            <<"NumpadMultiply"/utf8>>;

        numpad_divide ->
            <<"NumpadDivide"/utf8>>;

        numpad_decimal ->
            <<"NumpadDecimal"/utf8>>;

        numpad_enter ->
            <<"NumpadEnter"/utf8>>;

        num_lock ->
            <<"NumLock"/utf8>>;

        audio_volume_up ->
            <<"AudioVolumeUp"/utf8>>;

        audio_volume_down ->
            <<"AudioVolumeDown"/utf8>>;

        audio_volume_mute ->
            <<"AudioVolumeMute"/utf8>>;

        media_play_pause ->
            <<"MediaPlayPause"/utf8>>;

        media_stop ->
            <<"MediaStop"/utf8>>;

        media_track_next ->
            <<"MediaTrackNext"/utf8>>;

        media_track_previous ->
            <<"MediaTrackPrevious"/utf8>>;

        print_screen ->
            <<"PrintScreen"/utf8>>;

        scroll_lock ->
            <<"ScrollLock"/utf8>>;

        pause ->
            <<"Pause"/utf8>>;

        context_menu ->
            <<"ContextMenu"/utf8>>;

        {custom, Code} ->
            Code
    end.

-file("src/tiramisu/input.gleam", 128).
?DOC(" Check if a key is currently pressed\n").
-spec is_key_pressed(input_state(), key()) -> boolean().
is_key_pressed(Input, Key) ->
    Key_code = key_to_code(Key),
    gleam@list:contains(erlang:element(2, erlang:element(2, Input)), Key_code).

-file("src/tiramisu/input.gleam", 134).
?DOC(" Check if a key was just pressed this frame\n").
-spec is_key_just_pressed(input_state(), key()) -> boolean().
is_key_just_pressed(Input, Key) ->
    Key_code = key_to_code(Key),
    gleam@list:contains(erlang:element(3, erlang:element(2, Input)), Key_code).

-file("src/tiramisu/input.gleam", 140).
?DOC(" Check if a key was just released this frame\n").
-spec is_key_just_released(input_state(), key()) -> boolean().
is_key_just_released(Input, Key) ->
    Key_code = key_to_code(Key),
    gleam@list:contains(erlang:element(4, erlang:element(2, Input)), Key_code).

-file("src/tiramisu/input.gleam", 578).
?DOC(" Get axis value with dead zone applied\n").
-spec get_axis_with_deadzone(input_state(), integer(), gamepad_axis(), float()) -> float().
get_axis_with_deadzone(Input, Gamepad_index, Axis, Deadzone) ->
    Value = gamepad_axis(Input, Gamepad_index, Axis),
    case (Value > Deadzone) orelse (Value < (+0.0 - Deadzone)) of
        true ->
            Value;

        false ->
            +0.0
    end.

-file("src/tiramisu/input.gleam", 592).
?DOC(" Check if left stick is moved in any direction\n").
-spec is_left_stick_active(input_state(), integer(), float()) -> boolean().
is_left_stick_active(Input, Gamepad_index, Threshold) ->
    X = gamepad_axis(Input, Gamepad_index, left_stick_x),
    Y = gamepad_axis(Input, Gamepad_index, left_stick_y),
    (((X > Threshold) orelse (X < (+0.0 - Threshold))) orelse (Y > Threshold))
    orelse (Y < (+0.0 - Threshold)).

-file("src/tiramisu/input.gleam", 606).
?DOC(" Check if right stick is moved in any direction\n").
-spec is_right_stick_active(input_state(), integer(), float()) -> boolean().
is_right_stick_active(Input, Gamepad_index, Threshold) ->
    X = gamepad_axis(Input, Gamepad_index, right_stick_x),
    Y = gamepad_axis(Input, Gamepad_index, right_stick_y),
    (((X > Threshold) orelse (X < (+0.0 - Threshold))) orelse (Y > Threshold))
    orelse (Y < (+0.0 - Threshold)).

-file("src/tiramisu/input.gleam", 620).
?DOC(" Convenience: Check if primary gamepad (index 0) is connected\n").
-spec is_primary_connected(input_state()) -> boolean().
is_primary_connected(Input) ->
    is_gamepad_connected(Input, 0).

-file("src/tiramisu/input.gleam", 625).
?DOC(" Convenience: Check button on primary gamepad\n").
-spec is_primary_gamepad_button_pressed(input_state(), gamepad_button()) -> boolean().
is_primary_gamepad_button_pressed(Input, Button) ->
    is_gamepad_button_pressed(Input, 0, Button).

-file("src/tiramisu/input.gleam", 633).
?DOC(" Convenience: Get button value on primary gamepad\n").
-spec get_primary_button(input_state(), gamepad_button()) -> float().
get_primary_button(Input, Button) ->
    gamepad_button(Input, 0, Button).

-file("src/tiramisu/input.gleam", 638).
?DOC(" Convenience: Get axis value on primary gamepad\n").
-spec get_primary_axis(input_state(), gamepad_axis()) -> float().
get_primary_axis(Input, Axis) ->
    gamepad_axis(Input, 0, Axis).

-file("src/tiramisu/input.gleam", 676).
?DOC(" Create a new empty input bindings configuration\n").
-spec new_bindings() -> input_bindings(any()).
new_bindings() ->
    {input_bindings, [], [], []}.

-file("src/tiramisu/input.gleam", 689).
?DOC(
    " Bind a keyboard key to an action\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let bindings = input.new_bindings()\n"
    "   |> input.bind_key(input.Space, Jump)\n"
    "   |> input.bind_key(input.KeyW, MoveForward)\n"
    " ```\n"
).
-spec bind_key(input_bindings(APIA), key(), APIA) -> input_bindings(APIA).
bind_key(Bindings, Key, Action) ->
    {input_bindings,
        [{Key, Action} | erlang:element(2, Bindings)],
        erlang:element(3, Bindings),
        erlang:element(4, Bindings)}.

-file("src/tiramisu/input.gleam", 701).
?DOC(" Bind a mouse button to an action\n").
-spec bind_mouse_button(input_bindings(APID), mouse_button(), APID) -> input_bindings(APID).
bind_mouse_button(Bindings, Button, Action) ->
    {input_bindings,
        erlang:element(2, Bindings),
        [{Button, Action} | erlang:element(3, Bindings)],
        erlang:element(4, Bindings)}.

-file("src/tiramisu/input.gleam", 713).
?DOC(" Bind a gamepad button to an action\n").
-spec bind_gamepad_button(input_bindings(APIG), gamepad_button(), APIG) -> input_bindings(APIG).
bind_gamepad_button(Bindings, Button, Action) ->
    {input_bindings,
        erlang:element(2, Bindings),
        erlang:element(3, Bindings),
        [{Button, Action} | erlang:element(4, Bindings)]}.

-file("src/tiramisu/input.gleam", 735).
?DOC(
    " Check if an action is currently pressed\n"
    "\n"
    " Returns True if any input bound to this action is pressed.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " if input.is_action_pressed(ctx.input, bindings, Jump) {\n"
    "   // Player wants to jump\n"
    " }\n"
    " ```\n"
).
-spec is_action_pressed(input_state(), input_bindings(APIJ), APIJ) -> boolean().
is_action_pressed(Input, Bindings, Action) ->
    Key_pressed = gleam@list:any(
        erlang:element(2, Bindings),
        fun(Binding) ->
            {Key, Bound_action} = Binding,
            (Bound_action =:= Action) andalso is_key_pressed(Input, Key)
        end
    ),
    Mouse_pressed = gleam@list:any(
        erlang:element(3, Bindings),
        fun(Binding@1) ->
            {Button, Bound_action@1} = Binding@1,
            (Bound_action@1 =:= Action) andalso case Button of
                left_button ->
                    is_left_button_pressed(Input);

                right_button ->
                    is_right_button_pressed(Input);

                middle_button ->
                    erlang:element(
                        2,
                        erlang:element(8, erlang:element(3, Input))
                    )
            end
        end
    ),
    Gamepad_pressed = gleam@list:any(
        erlang:element(4, Bindings),
        fun(Binding@2) ->
            {Button@1, Bound_action@2} = Binding@2,
            (Bound_action@2 =:= Action) andalso is_gamepad_button_pressed(
                Input,
                0,
                Button@1
            )
        end
    ),
    (Key_pressed orelse Mouse_pressed) orelse Gamepad_pressed.

-file("src/tiramisu/input.gleam", 772).
?DOC(
    " Check if an action was just pressed this frame\n"
    "\n"
    " Returns True if any input bound to this action was just pressed.\n"
).
-spec is_action_just_pressed(input_state(), input_bindings(APIL), APIL) -> boolean().
is_action_just_pressed(Input, Bindings, Action) ->
    Key_just_pressed = gleam@list:any(
        erlang:element(2, Bindings),
        fun(Binding) ->
            {Key, Bound_action} = Binding,
            (Bound_action =:= Action) andalso is_key_just_pressed(Input, Key)
        end
    ),
    Mouse_just_pressed = gleam@list:any(
        erlang:element(3, Bindings),
        fun(Binding@1) ->
            {Button, Bound_action@1} = Binding@1,
            (Bound_action@1 =:= Action) andalso case Button of
                left_button ->
                    is_left_button_just_pressed(Input);

                right_button ->
                    is_right_button_just_pressed(Input);

                middle_button ->
                    erlang:element(
                        3,
                        erlang:element(8, erlang:element(3, Input))
                    )
            end
        end
    ),
    Gamepad_just_pressed = gleam@list:any(
        erlang:element(4, Bindings),
        fun(Binding@2) ->
            {Button@1, Bound_action@2} = Binding@2,
            (Bound_action@2 =:= Action) andalso is_gamepad_button_pressed(
                Input,
                0,
                Button@1
            )
        end
    ),
    (Key_just_pressed orelse Mouse_just_pressed) orelse Gamepad_just_pressed.

-file("src/tiramisu/input.gleam", 808).
?DOC(" Check if an action was just released this frame\n").
-spec is_action_just_released(input_state(), input_bindings(APIN), APIN) -> boolean().
is_action_just_released(Input, Bindings, Action) ->
    Key_just_released = gleam@list:any(
        erlang:element(2, Bindings),
        fun(Binding) ->
            {Key, Bound_action} = Binding,
            (Bound_action =:= Action) andalso is_key_just_released(Input, Key)
        end
    ),
    Mouse_just_released = gleam@list:any(
        erlang:element(3, Bindings),
        fun(Binding@1) ->
            {Button, Bound_action@1} = Binding@1,
            (Bound_action@1 =:= Action) andalso case Button of
                left_button ->
                    erlang:element(
                        4,
                        erlang:element(7, erlang:element(3, Input))
                    );

                right_button ->
                    erlang:element(
                        4,
                        erlang:element(9, erlang:element(3, Input))
                    );

                middle_button ->
                    erlang:element(
                        4,
                        erlang:element(8, erlang:element(3, Input))
                    )
            end
        end
    ),
    _ = false,
    Key_just_released orelse Mouse_just_released.

-file("src/tiramisu/input.gleam", 843).
?DOC(
    " Get the analog value (0.0 to 1.0) for an action\n"
    "\n"
    " Useful for actions that can have analog input like gamepad triggers.\n"
    " Returns 1.0 for digital inputs (keyboard/mouse) when pressed, 0.0 when not pressed.\n"
).
-spec get_action_value(input_state(), input_bindings(APIP), APIP) -> float().
get_action_value(Input, Bindings, Action) ->
    Key_value = begin
        _pipe = gleam@list:find_map(
            erlang:element(2, Bindings),
            fun(Binding) ->
                {Key, Bound_action} = Binding,
                case (Bound_action =:= Action) andalso is_key_pressed(
                    Input,
                    Key
                ) of
                    true ->
                        {ok, 1.0};

                    false ->
                        {error, nil}
                end
            end
        ),
        gleam@result:unwrap(_pipe, +0.0)
    end,
    Mouse_value = begin
        _pipe@1 = gleam@list:find_map(
            erlang:element(3, Bindings),
            fun(Binding@1) ->
                {Button, Bound_action@1} = Binding@1,
                case Bound_action@1 =:= Action of
                    true ->
                        case Button of
                            left_button ->
                                case is_left_button_pressed(Input) of
                                    true ->
                                        {ok, 1.0};

                                    false ->
                                        {error, nil}
                                end;

                            right_button ->
                                case is_right_button_pressed(Input) of
                                    true ->
                                        {ok, 1.0};

                                    false ->
                                        {error, nil}
                                end;

                            middle_button ->
                                case erlang:element(
                                    2,
                                    erlang:element(8, erlang:element(3, Input))
                                ) of
                                    true ->
                                        {ok, 1.0};

                                    false ->
                                        {error, nil}
                                end
                        end;

                    false ->
                        {error, nil}
                end
            end
        ),
        gleam@result:unwrap(_pipe@1, +0.0)
    end,
    Gamepad_value = begin
        _pipe@2 = gleam@list:find_map(
            erlang:element(4, Bindings),
            fun(Binding@2) ->
                {Button@1, Bound_action@2} = Binding@2,
                case Bound_action@2 =:= Action of
                    true ->
                        {ok, gamepad_button(Input, 0, Button@1)};

                    false ->
                        {error, nil}
                end
            end
        ),
        gleam@result:unwrap(_pipe@2, +0.0)
    end,
    case Key_value > +0.0 of
        true ->
            Key_value;

        false ->
            case Mouse_value > +0.0 of
                true ->
                    Mouse_value;

                false ->
                    Gamepad_value
            end
    end.

-file("src/tiramisu/input.gleam", 965).
?DOC(
    " Create a new buffered input system\n"
    "\n"
    " ## Arguments\n"
    "\n"
    " - `buffer_frames`: Number of frames to keep actions in buffer (e.g., 5 frames = ~83ms at 60fps)\n"
).
-spec with_buffer(integer()) -> buffered_input(any()).
with_buffer(Buffer_frames) ->
    {buffered_input, [], Buffer_frames, 0}.

-file("src/tiramisu/input.gleam", 984).
?DOC(
    " Update the input buffer each frame\n"
    "\n"
    " Call this once per frame in your update function to:\n"
    " 1. Add newly pressed actions to the buffer\n"
    " 2. Remove expired actions from the buffer\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let buffered = input.update_buffer(\n"
    "   model.buffered_input,\n"
    "   ctx.input,\n"
    "   bindings,\n"
    " )\n"
    " ```\n"
).
-spec update_buffer(buffered_input(APIT), input_state(), input_bindings(APIT)) -> buffered_input(APIT).
update_buffer(Buffered, Input, Bindings) ->
    Frame = erlang:element(4, Buffered) + 1,
    New_actions = gleam@list:filter_map(
        lists:append(
            [gleam@list:map(
                    erlang:element(2, Bindings),
                    fun(Pair) -> erlang:element(2, Pair) end
                ),
                gleam@list:map(
                    erlang:element(3, Bindings),
                    fun(Pair@1) -> erlang:element(2, Pair@1) end
                ),
                gleam@list:map(
                    erlang:element(4, Bindings),
                    fun(Pair@2) -> erlang:element(2, Pair@2) end
                )]
        ),
        fun(Action) -> case is_action_just_pressed(Input, Bindings, Action) of
                true ->
                    {ok, {buffered_action, Action, Frame}};

                false ->
                    {error, nil}
            end end
    ),
    Updated_buffer = lists:append(erlang:element(2, Buffered), New_actions),
    Cutoff_frame = Frame - erlang:element(3, Buffered),
    Cleaned_buffer = gleam@list:filter(
        Updated_buffer,
        fun(Buffered_action) ->
            erlang:element(3, Buffered_action) >= Cutoff_frame
        end
    ),
    {buffered_input, Cleaned_buffer, erlang:element(3, Buffered), Frame}.

-file("src/tiramisu/input.gleam", 1038).
?DOC(
    " Check if an action was pressed within the buffer window\n"
    "\n"
    " Returns True if the action was pressed in the last N frames (where N is buffer_frames).\n"
    " This allows for more forgiving input timing.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " // Allow jump input to be buffered - player can press jump slightly\n"
    " // before landing and it will still work\n"
    " let can_jump = is_grounded\n"
    "   && input.was_action_pressed_buffered(buffered, bindings, Jump)\n"
    " ```\n"
).
-spec was_action_pressed_buffered(buffered_input(APIX), APIX) -> boolean().
was_action_pressed_buffered(Buffered, Action) ->
    gleam@list:any(
        erlang:element(2, Buffered),
        fun(Buffered_action) ->
            erlang:element(2, Buffered_action) =:= Action
        end
    ).

-file("src/tiramisu/input.gleam", 1067).
?DOC(
    " Consume a buffered action (remove it from buffer)\n"
    "\n"
    " Use this when you've acted on a buffered input to prevent it from being\n"
    " used multiple times.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let can_jump = is_grounded\n"
    "   && input.was_action_pressed_buffered(buffered, Jump)\n"
    "\n"
    " case can_jump {\n"
    "   True -> {\n"
    "     // Perform jump\n"
    "     let buffered = input.consume_buffered_action(buffered, Jump)\n"
    "     // ...\n"
    "   }\n"
    "   False -> // ...\n"
    " }\n"
    " ```\n"
).
-spec consume_buffered_action(buffered_input(APIZ), APIZ) -> buffered_input(APIZ).
consume_buffered_action(Buffered, Action) ->
    Updated_buffer = case gleam@list:split_while(
        erlang:element(2, Buffered),
        fun(Buffered_action) -> erlang:element(2, Buffered_action) /= Action end
    ) of
        {Before, []} ->
            Before;

        {Before@1, [_ | After]} ->
            lists:append(Before@1, After)
    end,
    {buffered_input,
        Updated_buffer,
        erlang:element(3, Buffered),
        erlang:element(4, Buffered)}.

-file("src/tiramisu/input.gleam", 1087).
?DOC(
    " Clear all buffered actions\n"
    "\n"
    " Useful when switching game states or when you want to reset the buffer.\n"
).
-spec clear_buffer(buffered_input(APJC)) -> buffered_input(APJC).
clear_buffer(Buffered) ->
    {buffered_input,
        [],
        erlang:element(3, Buffered),
        erlang:element(4, Buffered)}.
