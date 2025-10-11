-module(gramps@websocket).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gramps/websocket.gleam").
-export([apply_mask/2, encode_close_frame/2, encode_continuation_frame/3, apply_deflate/2, apply_inflate/2, encode_text_frame/3, encode_binary_frame/3, encode_ping_frame/2, encode_pong_frame/2, aggregate_frames/3, make_client_key/0, decode_frame/2, decode_many_frames/3, has_deflate/1, get_context_takeovers/1, parse_websocket_key/1]).
-export_type([data_frame/0, close_reason/0, control_frame/0, frame/0, frame_parse_error/0, parsed_frame/0, compression/0, sha_hash/0]).

-type data_frame() :: {text_frame, bitstring()} | {binary_frame, bitstring()}.

-type close_reason() :: not_provided |
    {normal, bitstring()} |
    {going_away, bitstring()} |
    {protocol_error, bitstring()} |
    {unexpected_data_type, bitstring()} |
    {inconsistent_data_type, bitstring()} |
    {policy_violation, bitstring()} |
    {message_too_big, bitstring()} |
    {missing_extensions, bitstring()} |
    {unexpected_condition, bitstring()} |
    {custom_close_reason, integer(), bitstring()}.

-type control_frame() :: {close_frame, close_reason()} |
    {ping_frame, bitstring()} |
    {pong_frame, bitstring()}.

-type frame() :: {data, data_frame()} |
    {control, control_frame()} |
    {continuation, integer(), bitstring()}.

-type frame_parse_error() :: {need_more_data, bitstring()} | invalid_frame.

-type parsed_frame() :: {complete, frame()} | {incomplete, frame()}.

-type compression() :: compressed | uncompressed.

-type sha_hash() :: sha.

-file("src/gramps/websocket.gleam", 52).
-spec mask_data(bitstring(), list(bitstring()), integer(), bitstring()) -> bitstring().
mask_data(Data, Masks, Index, Resp) ->
    case Data of
        <<Masked:8/bitstring, Rest/bitstring>> ->
            {One@1, Two@1, Three@1, Four@1} = case Masks of
                [One, Two, Three, Four] -> {One, Two, Three, Four};
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"gramps/websocket"/utf8>>,
                                function => <<"mask_data"/utf8>>,
                                line => 60,
                                value => _assert_fail,
                                start => 1398,
                                'end' => 1440,
                                pattern_start => 1409,
                                pattern_end => 1432})
            end,
            Mask_value = case Index rem 4 of
                0 ->
                    One@1;

                1 ->
                    Two@1;

                2 ->
                    Three@1;

                3 ->
                    Four@1;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Somehow a value mod 4 is not 0, 1, 2, or 3"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"gramps/websocket"/utf8>>,
                            function => <<"mask_data"/utf8>>,
                            line => 66})
            end,
            Unmasked = crypto:exor(Mask_value, Masked),
            mask_data(
                Rest,
                Masks,
                Index + 1,
                <<Resp/bitstring, Unmasked/bitstring>>
            );

        _ ->
            Resp
    end.

-file("src/gramps/websocket.gleam", 329).
-spec make_length(integer()) -> bitstring().
make_length(Length) ->
    case Length of
        Length@1 when Length@1 > 65535 ->
            <<127:7, Length@1:64/integer>>;

        Length@2 when Length@2 >= 126 ->
            <<126:7, Length@2:16/integer>>;

        _ ->
            <<Length:7>>
    end.

-file("src/gramps/websocket.gleam", 342).
-spec make_frame(
    integer(),
    integer(),
    bitstring(),
    compression(),
    gleam@option:option(bitstring())
) -> gleam@bytes_tree:bytes_tree().
make_frame(Opcode, Length, Payload, Compressed, Mask) ->
    Length_section = make_length(Length),
    Masked = case gleam@option:is_some(Mask) of
        true ->
            1;

        false ->
            0
    end,
    Mask_key = gleam@option:unwrap(Mask, <<>>),
    Compressed@1 = case Compressed of
        compressed ->
            1;

        uncompressed ->
            0
    end,
    _pipe = <<1:1,
        Compressed@1:1,
        0:2,
        Opcode:4,
        Masked:1,
        Length_section/bitstring,
        Mask_key/bitstring,
        Payload/bitstring>>,
    gleam@bytes_tree:from_bit_array(_pipe).

-file("src/gramps/websocket.gleam", 376).
-spec apply_mask(bitstring(), gleam@option:option(bitstring())) -> bitstring().
apply_mask(Data, Mask) ->
    case Mask of
        {some, Mask@1} ->
            {Mask1@1, Mask2@1, Mask3@1, Mask4@1} = case Mask@1 of
                <<Mask1:1/binary,
                    Mask2:1/binary,
                    Mask3:1/binary,
                    Mask4:1/binary>> -> {Mask1, Mask2, Mask3, Mask4};
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"gramps/websocket"/utf8>>,
                                function => <<"apply_mask"/utf8>>,
                                line => 379,
                                value => _assert_fail,
                                start => 10538,
                                'end' => 10683,
                                pattern_start => 10549,
                                pattern_end => 10676})
            end,
            mask_data(Data, [Mask1@1, Mask2@1, Mask3@1, Mask4@1], 0, <<>>);

        none ->
            Data
    end.

-file("src/gramps/websocket.gleam", 251).
-spec encode_frame(frame(), compression(), gleam@option:option(bitstring())) -> gleam@bytes_tree:bytes_tree().
encode_frame(Frame, Compressed, Mask) ->
    case Frame of
        {data, {text_frame, Payload}} ->
            Payload_length = erlang:byte_size(Payload),
            make_frame(1, Payload_length, Payload, Compressed, Mask);

        {control, {close_frame, Reason}} ->
            {Payload_length@1, Payload@1} = case Reason of
                not_provided ->
                    {0, <<>>};

                {going_away, Body} ->
                    Payload_size = erlang:byte_size(Body) + 2,
                    {Payload_size, <<1001:16, Body/bitstring>>};

                {inconsistent_data_type, Body@1} ->
                    Payload_size@1 = erlang:byte_size(Body@1) + 2,
                    {Payload_size@1, <<1007:16, Body@1/bitstring>>};

                {message_too_big, Body@2} ->
                    Payload_size@2 = erlang:byte_size(Body@2) + 2,
                    {Payload_size@2, <<1009:16, Body@2/bitstring>>};

                {missing_extensions, Body@3} ->
                    Payload_size@3 = erlang:byte_size(Body@3) + 2,
                    {Payload_size@3, <<1010:16, Body@3/bitstring>>};

                {normal, Body@4} ->
                    Payload_size@4 = erlang:byte_size(Body@4) + 2,
                    {Payload_size@4, <<1000:16, Body@4/bitstring>>};

                {policy_violation, Body@5} ->
                    Payload_size@5 = erlang:byte_size(Body@5) + 2,
                    {Payload_size@5, <<1008:16, Body@5/bitstring>>};

                {protocol_error, Body@6} ->
                    Payload_size@6 = erlang:byte_size(Body@6) + 2,
                    {Payload_size@6, <<1002:16, Body@6/bitstring>>};

                {unexpected_condition, Body@7} ->
                    Payload_size@7 = erlang:byte_size(Body@7) + 2,
                    {Payload_size@7, <<1011:16, Body@7/bitstring>>};

                {unexpected_data_type, Body@8} ->
                    Payload_size@8 = erlang:byte_size(Body@8) + 2,
                    {Payload_size@8, <<1003:16, Body@8/bitstring>>};

                {custom_close_reason, Code, Body@9} ->
                    Payload_size@9 = erlang:byte_size(Body@9) + 2,
                    Code@1 = case Code < 5000 of
                        true ->
                            Code;

                        false ->
                            1000
                    end,
                    {Payload_size@9, <<Code@1:16, Body@9/bitstring>>}
            end,
            make_frame(
                8,
                Payload_length@1,
                apply_mask(Payload@1, Mask),
                Compressed,
                Mask
            );

        {data, {binary_frame, Payload@2}} ->
            Payload_length@2 = erlang:byte_size(Payload@2),
            make_frame(2, Payload_length@2, Payload@2, Compressed, Mask);

        {control, {pong_frame, Payload@3}} ->
            Payload_length@3 = erlang:byte_size(Payload@3),
            make_frame(10, Payload_length@3, Payload@3, Compressed, Mask);

        {control, {ping_frame, Payload@4}} ->
            Payload_length@4 = erlang:byte_size(Payload@4),
            make_frame(9, Payload_length@4, Payload@4, Compressed, Mask);

        {continuation, Length, Payload@5} ->
            make_frame(0, Length, Payload@5, Compressed, Mask)
    end.

-file("src/gramps/websocket.gleam", 227).
-spec encode_close_frame(close_reason(), gleam@option:option(bitstring())) -> gleam@bytes_tree:bytes_tree().
encode_close_frame(Reason, Mask) ->
    encode_frame({control, {close_frame, Reason}}, uncompressed, Mask).

-file("src/gramps/websocket.gleam", 242).
-spec encode_continuation_frame(
    bitstring(),
    integer(),
    gleam@option:option(bitstring())
) -> gleam@bytes_tree:bytes_tree().
encode_continuation_frame(Data, Total_size, Mask) ->
    Payload = apply_mask(Data, Mask),
    encode_frame({continuation, Total_size, Payload}, uncompressed, Mask).

-file("src/gramps/websocket.gleam", 391).
-spec apply_deflate(
    bitstring(),
    gleam@option:option(gramps@websocket@compression:context())
) -> bitstring().
apply_deflate(Data, Context) ->
    case Context of
        {some, Context@1} ->
            gramps@websocket@compression:deflate(Context@1, Data);

        _ ->
            Data
    end.

-file("src/gramps/websocket.gleam", 398).
-spec apply_inflate(
    bitstring(),
    gleam@option:option(gramps@websocket@compression:context())
) -> bitstring().
apply_inflate(Data, Context) ->
    case Context of
        {some, Context@1} ->
            gramps@websocket@compression:deflate(Context@1, Data);

        _ ->
            Data
    end.

-file("src/gramps/websocket.gleam", 405).
-spec to_frame(
    bitstring(),
    gleam@option:option(gramps@websocket@compression:context()),
    gleam@option:option(bitstring()),
    fun((bitstring()) -> FUC),
    fun((FUC) -> frame())
) -> gleam@bytes_tree:bytes_tree().
to_frame(Data, Context, Mask, Create_inner_frame, Create_frame) ->
    Frame = begin
        _pipe = Data,
        _pipe@1 = apply_deflate(_pipe, Context),
        _pipe@2 = apply_mask(_pipe@1, Mask),
        _pipe@3 = Create_inner_frame(_pipe@2),
        Create_frame(_pipe@3)
    end,
    Compress = case Context of
        {some, _} ->
            compressed;

        _ ->
            uncompressed
    end,
    encode_frame(Frame, Compress, Mask).

-file("src/gramps/websocket.gleam", 211).
-spec encode_text_frame(
    binary(),
    gleam@option:option(gramps@websocket@compression:context()),
    gleam@option:option(bitstring())
) -> gleam@bytes_tree:bytes_tree().
encode_text_frame(Data, Context, Mask) ->
    to_frame(
        gleam_stdlib:identity(Data),
        Context,
        Mask,
        fun(Field@0) -> {text_frame, Field@0} end,
        fun(Field@0) -> {data, Field@0} end
    ).

-file("src/gramps/websocket.gleam", 219).
-spec encode_binary_frame(
    bitstring(),
    gleam@option:option(gramps@websocket@compression:context()),
    gleam@option:option(bitstring())
) -> gleam@bytes_tree:bytes_tree().
encode_binary_frame(Data, Context, Mask) ->
    to_frame(
        Data,
        Context,
        Mask,
        fun(Field@0) -> {binary_frame, Field@0} end,
        fun(Field@0) -> {data, Field@0} end
    ).

-file("src/gramps/websocket.gleam", 234).
-spec encode_ping_frame(bitstring(), gleam@option:option(bitstring())) -> gleam@bytes_tree:bytes_tree().
encode_ping_frame(Data, Mask) ->
    to_frame(
        Data,
        none,
        Mask,
        fun(Field@0) -> {ping_frame, Field@0} end,
        fun(Field@0) -> {control, Field@0} end
    ).

-file("src/gramps/websocket.gleam", 238).
-spec encode_pong_frame(bitstring(), gleam@option:option(bitstring())) -> gleam@bytes_tree:bytes_tree().
encode_pong_frame(Data, Mask) ->
    to_frame(
        Data,
        none,
        Mask,
        fun(Field@0) -> {pong_frame, Field@0} end,
        fun(Field@0) -> {control, Field@0} end
    ).

-file("src/gramps/websocket.gleam", 438).
-spec append_frame(frame(), bitstring()) -> frame().
append_frame(Left, Data) ->
    case Left of
        {data, {text_frame, Payload}} ->
            {data, {text_frame, <<Payload/bitstring, Data/bitstring>>}};

        {data, {binary_frame, Payload@1}} ->
            {data, {binary_frame, <<Payload@1/bitstring, Data/bitstring>>}};

        {control, {close_frame, _}} ->
            Left;

        {control, {ping_frame, Payload@2}} ->
            {control, {ping_frame, <<Payload@2/bitstring, Data/bitstring>>}};

        {control, {pong_frame, Payload@3}} ->
            {control, {pong_frame, <<Payload@3/bitstring, Data/bitstring>>}};

        {continuation, _, _} ->
            Left
    end.

-file("src/gramps/websocket.gleam", 451).
-spec aggregate_frames(
    list(parsed_frame()),
    gleam@option:option(frame()),
    list(frame())
) -> {ok, list(frame())} | {error, nil}.
aggregate_frames(Frames, Previous, Joined) ->
    case {Frames, Previous} of
        {[], _} ->
            {ok, lists:reverse(Joined)};

        {[{complete, {continuation, _, Data}} | Rest], {some, Prev}} ->
            Next = append_frame(Prev, Data),
            aggregate_frames(Rest, none, [Next | Joined]);

        {[{incomplete, {continuation, _, Data@1}} | Rest@1], {some, Prev@1}} ->
            Next@1 = append_frame(Prev@1, Data@1),
            aggregate_frames(Rest@1, {some, Next@1}, Joined);

        {[{incomplete, Frame} | Rest@2], none} ->
            aggregate_frames(Rest@2, {some, Frame}, Joined);

        {[{complete, Frame@1} | Rest@3], none} ->
            aggregate_frames(Rest@3, none, [Frame@1 | Joined]);

        {_, _} ->
            {error, nil}
    end.

-file("src/gramps/websocket.gleam", 478).
-spec make_client_key() -> binary().
make_client_key() ->
    Bytes = crypto:strong_rand_bytes(16),
    gleam_stdlib:bit_array_base64_encode(Bytes, true).

-file("src/gramps/websocket.gleam", 500).
-spec inflate(
    boolean(),
    gleam@option:option(gramps@websocket@compression:context()),
    bitstring()
) -> {ok, bitstring()} | {error, frame_parse_error()}.
inflate(Compressed, Context, Data) ->
    case {Compressed, Context} of
        {true, {some, Context@1}} ->
            {ok, gramps@websocket@compression:inflate(Context@1, Data)};

        {true, none} ->
            {error, invalid_frame};

        {_, _} ->
            {ok, Data}
    end.

-file("src/gramps/websocket.gleam", 85).
-spec decode_frame(
    bitstring(),
    gleam@option:option(gramps@websocket@compression:context())
) -> {ok, {parsed_frame(), bitstring()}} | {error, frame_parse_error()}.
decode_frame(Message, Context) ->
    case Message of
        <<Complete:1,
            Compressed:1,
            _:2,
            Opcode:4/integer,
            Masked:1,
            Payload_length:7/integer,
            Rest/bitstring>> ->
            Compressed@1 = Compressed =:= 1,
            Masked@1 = Masked =:= 1,
            gleam@bool:guard(
                Compressed@1 andalso gleam@option:is_none(Context),
                {error, invalid_frame},
                fun() ->
                    Payload_size = case Payload_length of
                        126 ->
                            16;

                        127 ->
                            64;

                        _ ->
                            0
                    end,
                    Maybe_pair = case {Masked@1, Rest} of
                        {true,
                            <<Length:Payload_size/integer,
                                Mask1:1/binary,
                                Mask2:1/binary,
                                Mask3:1/binary,
                                Mask4:1/binary,
                                Rest@1/bitstring>>} ->
                            Payload_byte_size = case Length of
                                0 ->
                                    Payload_length;

                                N ->
                                    N
                            end,
                            case Rest@1 of
                                <<Payload:Payload_byte_size/binary,
                                    Rest@2/bitstring>> ->
                                    Data = mask_data(
                                        Payload,
                                        [Mask1, Mask2, Mask3, Mask4],
                                        0,
                                        <<>>
                                    ),
                                    {ok, {Data, Rest@2}};

                                _ ->
                                    {error, {need_more_data, Message}}
                            end;

                        {false,
                            <<Length@1:Payload_size/integer, Rest@3/bitstring>>} ->
                            Payload_byte_size@1 = case Length@1 of
                                0 ->
                                    Payload_length;

                                N@1 ->
                                    N@1
                            end,
                            case Rest@3 of
                                <<Payload@1:Payload_byte_size@1/binary,
                                    Rest@4/bitstring>> ->
                                    {ok, {Payload@1, Rest@4}};

                                _ ->
                                    {error, {need_more_data, Message}}
                            end;

                        {_, _} ->
                            {error, invalid_frame}
                    end,
                    gleam@result:'try'(
                        Maybe_pair,
                        fun(_use0) ->
                            {Data@1, Rest@5} = _use0,
                            _pipe@6 = case Opcode of
                                0 ->
                                    _pipe = Data@1,
                                    _pipe@1 = inflate(
                                        Compressed@1,
                                        Context,
                                        _pipe
                                    ),
                                    gleam@result:map(
                                        _pipe@1,
                                        fun(Decompressed) ->
                                            {continuation,
                                                Payload_size,
                                                Decompressed}
                                        end
                                    );

                                1 ->
                                    _pipe@2 = Data@1,
                                    _pipe@3 = inflate(
                                        Compressed@1,
                                        Context,
                                        _pipe@2
                                    ),
                                    gleam@result:map(
                                        _pipe@3,
                                        fun(Decompressed@1) ->
                                            {data, {text_frame, Decompressed@1}}
                                        end
                                    );

                                2 ->
                                    _pipe@4 = Data@1,
                                    _pipe@5 = inflate(
                                        Compressed@1,
                                        Context,
                                        _pipe@4
                                    ),
                                    gleam@result:map(
                                        _pipe@5,
                                        fun(Decompressed@2) ->
                                            {data,
                                                {binary_frame, Decompressed@2}}
                                        end
                                    );

                                8 ->
                                    case Data@1 of
                                        <<1000:16, Rest@6/bitstring>> ->
                                            {ok,
                                                {control,
                                                    {close_frame,
                                                        {normal, Rest@6}}}};

                                        <<1001:16, Rest@7/bitstring>> ->
                                            {ok,
                                                {control,
                                                    {close_frame,
                                                        {going_away, Rest@7}}}};

                                        <<1002:16, Rest@8/bitstring>> ->
                                            {ok,
                                                {control,
                                                    {close_frame,
                                                        {protocol_error, Rest@8}}}};

                                        <<1003:16, Rest@9/bitstring>> ->
                                            {ok,
                                                {control,
                                                    {close_frame,
                                                        {unexpected_data_type,
                                                            Rest@9}}}};

                                        <<1007:16, Rest@10/bitstring>> ->
                                            {ok,
                                                {control,
                                                    {close_frame,
                                                        {inconsistent_data_type,
                                                            Rest@10}}}};

                                        <<1008:16, Rest@11/bitstring>> ->
                                            {ok,
                                                {control,
                                                    {close_frame,
                                                        {policy_violation,
                                                            Rest@11}}}};

                                        <<1009:16, Rest@12/bitstring>> ->
                                            {ok,
                                                {control,
                                                    {close_frame,
                                                        {message_too_big,
                                                            Rest@12}}}};

                                        <<1010:16, Rest@13/bitstring>> ->
                                            {ok,
                                                {control,
                                                    {close_frame,
                                                        {missing_extensions,
                                                            Rest@13}}}};

                                        <<1011:16, Rest@14/bitstring>> ->
                                            {ok,
                                                {control,
                                                    {close_frame,
                                                        {unexpected_condition,
                                                            Rest@14}}}};

                                        <<Code:16, Rest@15/bitstring>> ->
                                            {ok,
                                                {control,
                                                    {close_frame,
                                                        {custom_close_reason,
                                                            Code,
                                                            Rest@15}}}};

                                        _ ->
                                            {ok,
                                                {control,
                                                    {close_frame, not_provided}}}
                                    end;

                                9 ->
                                    {ok, {control, {ping_frame, Data@1}}};

                                10 ->
                                    {ok, {control, {pong_frame, Data@1}}};

                                _ ->
                                    {error, invalid_frame}
                            end,
                            gleam@result:'try'(
                                _pipe@6,
                                fun(Frame) -> case Complete of
                                        1 ->
                                            {ok, {{complete, Frame}, Rest@5}};

                                        0 ->
                                            {ok, {{incomplete, Frame}, Rest@5}};

                                        _ ->
                                            {error, invalid_frame}
                                    end end
                            )
                        end
                    )
                end
            );

        _ ->
            {error, invalid_frame}
    end.

-file("src/gramps/websocket.gleam", 425).
-spec decode_many_frames(
    bitstring(),
    gleam@option:option(gramps@websocket@compression:context()),
    list(parsed_frame())
) -> {list(parsed_frame()), bitstring()}.
decode_many_frames(Data, Context, Frames) ->
    case decode_frame(Data, Context) of
        {ok, {Frame, <<>>}} ->
            {lists:reverse([Frame | Frames]), <<>>};

        {ok, {Frame@1, Rest}} ->
            decode_many_frames(Rest, Context, [Frame@1 | Frames]);

        {error, {need_more_data, Rest@1}} ->
            {lists:reverse(Frames), Rest@1};

        {error, invalid_frame} ->
            {lists:reverse(Frames), Data}
    end.

-file("src/gramps/websocket.gleam", 514).
-spec has_deflate(list(binary())) -> boolean().
has_deflate(Extensions) ->
    gleam@list:any(
        Extensions,
        fun(Str) -> Str =:= <<"permessage-deflate"/utf8>> end
    ).

-file("src/gramps/websocket.gleam", 518).
-spec get_context_takeovers(list(binary())) -> gramps@websocket@compression:context_takeover().
get_context_takeovers(Extensions) ->
    No_client_context_takeover = gleam@list:any(
        Extensions,
        fun(Str) -> Str =:= <<"client_no_context_takeover"/utf8>> end
    ),
    No_server_context_takeover = gleam@list:any(
        Extensions,
        fun(Str@1) -> Str@1 =:= <<"server_no_context_takeover"/utf8>> end
    ),
    {context_takeover, No_client_context_takeover, No_server_context_takeover}.

-file("src/gramps/websocket.gleam", 487).
-spec parse_websocket_key(binary()) -> binary().
parse_websocket_key(Key) ->
    _pipe = Key,
    _pipe@1 = gleam@string:append(
        _pipe,
        <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"/utf8>>
    ),
    _pipe@2 = crypto:hash(sha, _pipe@1),
    base64:encode(_pipe@2).
