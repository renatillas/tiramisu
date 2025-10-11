-module(houdini@internal@escape_erl).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/houdini/internal/escape_erl.gleam").
-export([escape/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/houdini/internal/escape_erl.gleam", 75).
?DOC(false).
-spec do_escape_normal(
    bitstring(),
    integer(),
    bitstring(),
    bitstring(),
    integer()
) -> bitstring().
do_escape_normal(Bin, Skip, Original, Acc, Len) ->
    case Bin of
        <<"<"/utf8, Rest/bitstring>> ->
            Acc@1 = <<Acc/bitstring,
                (binary:part(Original, Skip, Len))/bitstring,
                "&lt;"/utf8>>,
            do_escape(Rest, (Skip + Len) + 1, Original, Acc@1);

        <<">"/utf8, Rest@1/bitstring>> ->
            Acc@2 = <<Acc/bitstring,
                (binary:part(Original, Skip, Len))/bitstring,
                "&gt;"/utf8>>,
            do_escape(Rest@1, (Skip + Len) + 1, Original, Acc@2);

        <<"&"/utf8, Rest@2/bitstring>> ->
            Acc@3 = <<Acc/bitstring,
                (binary:part(Original, Skip, Len))/bitstring,
                "&amp;"/utf8>>,
            do_escape(Rest@2, (Skip + Len) + 1, Original, Acc@3);

        <<"\""/utf8, Rest@3/bitstring>> ->
            Acc@4 = <<Acc/bitstring,
                (binary:part(Original, Skip, Len))/bitstring,
                "&quot;"/utf8>>,
            do_escape(Rest@3, (Skip + Len) + 1, Original, Acc@4);

        <<"'"/utf8, Rest@4/bitstring>> ->
            Acc@5 = <<Acc/bitstring,
                (binary:part(Original, Skip, Len))/bitstring,
                "&#39;"/utf8>>,
            do_escape(Rest@4, (Skip + Len) + 1, Original, Acc@5);

        <<_, B, C, D, E, F, G, H, Rest@5/bitstring>> when ((((((((((((((((((((((((((((((((((B =/= 34) andalso (B =/= 38)) andalso (B =/= 39)) andalso (B =/= 60)) andalso (B =/= 62)) andalso (C =/= 34)) andalso (C =/= 38)) andalso (C =/= 39)) andalso (C =/= 60)) andalso (C =/= 62)) andalso (D =/= 34)) andalso (D =/= 38)) andalso (D =/= 39)) andalso (D =/= 60)) andalso (D =/= 62)) andalso (E =/= 34)) andalso (E =/= 38)) andalso (E =/= 39)) andalso (E =/= 60)) andalso (E =/= 62)) andalso (F =/= 34)) andalso (F =/= 38)) andalso (F =/= 39)) andalso (F =/= 60)) andalso (F =/= 62)) andalso (G =/= 34)) andalso (G =/= 38)) andalso (G =/= 39)) andalso (G =/= 60)) andalso (G =/= 62)) andalso (H =/= 34)) andalso (H =/= 38)) andalso (H =/= 39)) andalso (H =/= 60)) andalso (H =/= 62) ->
            do_escape_normal(Rest@5, Skip, Original, Acc, Len + 8);

        <<_, Rest@6/bitstring>> ->
            do_escape_normal(Rest@6, Skip, Original, Acc, Len + 1);

        <<>> ->
            case Skip of
                0 ->
                    Original;

                _ ->
                    <<Acc/bitstring,
                        (binary:part(Original, Skip, Len))/bitstring>>
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"do_escape_normal: non byte aligned string, all strings should be byte aligned"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"houdini/internal/escape_erl"/utf8>>,
                    function => <<"do_escape_normal"/utf8>>,
                    line => 195})
    end.

-file("src/houdini/internal/escape_erl.gleam", 28).
?DOC(false).
-spec do_escape(bitstring(), integer(), bitstring(), bitstring()) -> bitstring().
do_escape(Bin, Skip, Original, Acc) ->
    case Bin of
        <<"<"/utf8, Rest/bitstring>> ->
            Acc@1 = <<Acc/bitstring, "&lt;"/utf8>>,
            do_escape(Rest, Skip + 1, Original, Acc@1);

        <<">"/utf8, Rest@1/bitstring>> ->
            Acc@2 = <<Acc/bitstring, "&gt;"/utf8>>,
            do_escape(Rest@1, Skip + 1, Original, Acc@2);

        <<"&"/utf8, Rest@2/bitstring>> ->
            Acc@3 = <<Acc/bitstring, "&amp;"/utf8>>,
            do_escape(Rest@2, Skip + 1, Original, Acc@3);

        <<"\""/utf8, Rest@3/bitstring>> ->
            Acc@4 = <<Acc/bitstring, "&quot;"/utf8>>,
            do_escape(Rest@3, Skip + 1, Original, Acc@4);

        <<"'"/utf8, Rest@4/bitstring>> ->
            Acc@5 = <<Acc/bitstring, "&#39;"/utf8>>,
            do_escape(Rest@4, Skip + 1, Original, Acc@5);

        <<_, Rest@5/bitstring>> ->
            do_escape_normal(Rest@5, Skip, Original, Acc, 1);

        <<>> ->
            Acc;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"do_escape: non byte aligned string, all strings should be byte aligned"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"houdini/internal/escape_erl"/utf8>>,
                    function => <<"do_escape"/utf8>>,
                    line => 70})
    end.

-file("src/houdini/internal/escape_erl.gleam", 2).
?DOC(false).
-spec escape(binary()) -> binary().
escape(Text) ->
    Bits = <<Text/binary>>,
    Result = do_escape(Bits, 0, Bits, <<>>),
    houdini_ffi:coerce(Result).
