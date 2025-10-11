-module(gleam_community@colour).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gleam_community/colour.gleam").
-export([from_rgb255/3, from_rgb/3, from_rgba/4, from_hsla/4, from_hsl/3, from_rgb_hex/1, from_rgb_hex_string/1, from_rgba_hex/1, from_rgba_hex_string/1, to_rgba/1, to_hsla/1, to_css_rgba_string/1, to_rgba_hex/1, to_rgba_hex_string/1, to_rgb_hex/1, to_rgb_hex_string/1, encode/1, decoder/0]).
-export_type([colour/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    "\n"
    " - **Types**\n"
    "   - [`Colour`](#Colour)\n"
    "   - [`Color`](#Color)\n"
    " - **Constructors**\n"
    "   - [`from_rgb255`](#from_rgb255)\n"
    "   - [`from_rgb`](#from_rgb)\n"
    "   - [`from_rgba`](#from_rgba)\n"
    "   - [`from_hsl`](#from_hsl)\n"
    "   - [`from_hsla`](#from_hsla)\n"
    "   - [`from_rgb_hex`](#from_rgb_hex)\n"
    "   - [`from_rgba_hex`](#from_rgba_hex)\n"
    "   - [`from_rgb_hex_string`](#from_rgb_hex_string)\n"
    "   - [`from_rgba_hex_string`](#from_rgba_hex_string)\n"
    " - **Conversions**\n"
    "   - [`to_rgba`](#to_rgba)\n"
    "   - [`to_hsla`](#hsla)\n"
    "   - [`to_css_rgba_string`](#to_css_rgba_string)\n"
    "   - [`to_rgba_hex_string`](#to_rgba_hex_string)\n"
    "   - [`to_rgb_hex_string`](#to_rgb_hex_string)\n"
    "   - [`to_rgba_hex`](#to_rgba_hex)\n"
    "   - [`to_rgb_hex`](#to_rgb_hex)\n"
    " - **JSON**\n"
    "   - [`encode`](#encode)\n"
    "   - [`decoder`](#decoder)\n"
    " - **Colours**\n"
    "   - [`light_red`](#light_red)\n"
    "   - [`red`](#red)\n"
    "   - [`dark_red`](#dark_red)\n"
    "   - [`light_orange`](#light_orange)\n"
    "   - [`orange`](#orange)\n"
    "   - [`dark_orange`](#dark_orange)\n"
    "   - [`light_yellow`](#light_yellow)\n"
    "   - [`yellow`](#yellow)\n"
    "   - [`dark_yellow`](#dark_yellow)\n"
    "   - [`light_green`](#light_green)\n"
    "   - [`green`](#green)\n"
    "   - [`dark_green`](#dark_green)\n"
    "   - [`light_blue`](#light_blue)\n"
    "   - [`blue`](#blue)\n"
    "   - [`dark_blue`](#dark_blue)\n"
    "   - [`light_purple`](#light_purple)\n"
    "   - [`purple`](#purple)\n"
    "   - [`dark_purple`](#dark_purple)\n"
    "   - [`light_brown`](#light_brown)\n"
    "   - [`brown`](#brown)\n"
    "   - [`dark_brown`](#dark_brown)\n"
    "   - [`black`](#black)\n"
    "   - [`white`](#white)\n"
    "   - [`light_grey`](#light_grey)\n"
    "   - [`grey`](#grey)\n"
    "   - [`dark_grey`](#dark_grey)\n"
    "   - [`light_gray`](#light_gray)\n"
    "   - [`gray`](#gray)\n"
    "   - [`dark_gray`](#dark_gray)\n"
    "   - [`light_charcoal`](#light_charcoal)\n"
    "   - [`charcoal`](#charcoal)\n"
    "   - [`dark_charcoal`](#dark_charcoal)\n"
    "   - [`pink`](#pink)\n"
    "\n"
    " ---\n"
    "\n"
    " This package was heavily inspired by the `elm-color` module.\n"
    " The original source code can be found\n"
    " <a href=\"https://github.com/avh4/elm-color/\">here</a>.\n"
    "\n"
    " <details>\n"
    " <summary>The license of that package is produced below:</summary>\n"
    "\n"
    "\n"
    " > MIT License\n"
    "\n"
    " > Copyright 2018 Aaron VonderHaar\n"
    "\n"
    " > Redistribution and use in source and binary forms, with or without modification,\n"
    " are permitted provided that the following conditions are met:\n"
    "\n"
    "   1. Redistributions of source code must retain the above copyright notice,\n"
    "    this list of conditions and the following disclaimer.\n"
    "\n"
    "   2. Redistributions in binary form must reproduce the above copyright notice,\n"
    "    this list of conditions and the following disclaimer in the documentation\n"
    "    and/or other materials provided with the distribution.\n"
    "\n"
    "   3. Neither the name of the copyright holder nor the names of its contributors\n"
    "    may be used to endorse or promote products derived from this software without\n"
    "    specific prior written permission.\n"
    "\n"
    " > THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND\n"
    " ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES\n"
    " OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL\n"
    " THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,\n"
    " EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS\n"
    " OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY\n"
    " THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\n"
    " ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"
    "\n"
    " > The above copyright notice and this permission notice shall be included in all\n"
    " copies or substantial portions of the Software.\n"
    " </details>\n"
    "\n"
).

-opaque colour() :: {rgba, float(), float(), float(), float()} |
    {hsla, float(), float(), float(), float()}.

-file("src/gleam_community/colour.gleam", 155).
-spec valid_colour_value(float()) -> {ok, float()} | {error, nil}.
valid_colour_value(C) ->
    case (C > 1.0) orelse (C < +0.0) of
        true ->
            {error, nil};

        false ->
            {ok, C}
    end.

-file("src/gleam_community/colour.gleam", 162).
-spec hue_to_rgb(float(), float(), float()) -> float().
hue_to_rgb(Hue, M1, M2) ->
    H = case Hue of
        _ when Hue < +0.0 ->
            Hue + 1.0;

        _ when Hue > 1.0 ->
            Hue - 1.0;

        _ ->
            Hue
    end,
    H_t_6 = H * 6.0,
    H_t_2 = H * 2.0,
    H_t_3 = H * 3.0,
    case H of
        _ when H_t_6 < 1.0 ->
            M1 + (((M2 - M1) * H) * 6.0);

        _ when H_t_2 < 1.0 ->
            M2;

        _ when H_t_3 < 2.0 ->
            M1 + (((M2 - M1) * ((2.0 / 3.0) - H)) * 6.0);

        _ ->
            M1
    end.

-file("src/gleam_community/colour.gleam", 181).
-spec hex_string_to_int(binary()) -> {ok, integer()} | {error, nil}.
hex_string_to_int(Hex_string) ->
    Hex = case Hex_string of
        <<"#"/utf8, Hex_number/binary>> ->
            Hex_number;

        <<"0x"/utf8, Hex_number@1/binary>> ->
            Hex_number@1;

        _ ->
            Hex_string
    end,
    _pipe = Hex,
    _pipe@1 = string:lowercase(_pipe),
    _pipe@2 = gleam@string:to_graphemes(_pipe@1),
    _pipe@3 = lists:reverse(_pipe@2),
    gleam@list:index_fold(
        _pipe@3,
        {ok, 0},
        fun(Total, Char, Index) -> case Total of
                {error, nil} ->
                    {error, nil};

                {ok, V} ->
                    gleam@result:'try'(case Char of
                            <<"a"/utf8>> ->
                                {ok, 10};

                            <<"b"/utf8>> ->
                                {ok, 11};

                            <<"c"/utf8>> ->
                                {ok, 12};

                            <<"d"/utf8>> ->
                                {ok, 13};

                            <<"e"/utf8>> ->
                                {ok, 14};

                            <<"f"/utf8>> ->
                                {ok, 15};

                            _ ->
                                gleam_stdlib:parse_int(Char)
                        end, fun(Num) ->
                            gleam@result:'try'(
                                gleam@int:power(16, erlang:float(Index)),
                                fun(Base) ->
                                    {ok,
                                        V + erlang:round(
                                            erlang:float(Num) * Base
                                        )}
                                end
                            )
                        end)
            end end
    ).

-file("src/gleam_community/colour.gleam", 212).
-spec hsla_to_rgba(float(), float(), float(), float()) -> {float(),
    float(),
    float(),
    float()}.
hsla_to_rgba(H, S, L, A) ->
    M2 = case L =< 0.5 of
        true ->
            L * (S + 1.0);

        false ->
            (L + S) - (L * S)
    end,
    M1 = (L * 2.0) - M2,
    R = hue_to_rgb(H + (1.0 / 3.0), M1, M2),
    G = hue_to_rgb(H, M1, M2),
    B = hue_to_rgb(H - (1.0 / 3.0), M1, M2),
    {R, G, B, A}.

-file("src/gleam_community/colour.gleam", 232).
-spec rgba_to_hsla(float(), float(), float(), float()) -> {float(),
    float(),
    float(),
    float()}.
rgba_to_hsla(R, G, B, A) ->
    Min_colour = gleam@float:min(R, gleam@float:min(G, B)),
    Max_colour = gleam@float:max(R, gleam@float:max(G, B)),
    H1 = case true of
        _ when Max_colour =:= R ->
            gleam@float:divide(G - B, Max_colour - Min_colour);

        _ when Max_colour =:= G ->
            _pipe = gleam@float:divide(B - R, Max_colour - Min_colour),
            gleam@result:'try'(_pipe, fun(D) -> {ok, 2.0 + D} end);

        _ ->
            _pipe@1 = gleam@float:divide(R - G, Max_colour - Min_colour),
            gleam@result:'try'(_pipe@1, fun(D@1) -> {ok, 4.0 + D@1} end)
    end,
    H2 = case H1 of
        {ok, V} ->
            {ok, V * (1.0 / 6.0)};

        _ ->
            H1
    end,
    H3 = case H2 of
        {ok, V@1} when V@1 < +0.0 ->
            V@1 + 1.0;

        {ok, V@2} ->
            V@2;

        _ ->
            +0.0
    end,
    L = (Min_colour + Max_colour) / 2.0,
    S = case true of
        _ when Min_colour =:= Max_colour ->
            +0.0;

        _ when L < 0.5 ->
            case (Max_colour + Min_colour) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> (Max_colour - Min_colour) / Gleam@denominator
            end;

        _ ->
            case ((2.0 - Max_colour) - Min_colour) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> (Max_colour - Min_colour) / Gleam@denominator@1
            end
    end,
    {H3, S, L, A}.

-file("src/gleam_community/colour.gleam", 300).
?DOC(
    " Returns a `Result(Colour)` created from the given 8 bit RGB values.\n"
    "\n"
    " Returns `Error(Nil)` if the supplied RGB values are greater than 255 or less than 0.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgb255(255, 0, 0)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec from_rgb255(integer(), integer(), integer()) -> {ok, colour()} |
    {error, nil}.
from_rgb255(Red, Green, Blue) ->
    gleam@result:'try'(
        begin
            _pipe = Red,
            _pipe@1 = erlang:float(_pipe),
            _pipe@2 = gleam@float:divide(_pipe@1, 255.0),
            gleam@result:'try'(_pipe@2, fun valid_colour_value/1)
        end,
        fun(R) ->
            gleam@result:'try'(
                begin
                    _pipe@3 = Green,
                    _pipe@4 = erlang:float(_pipe@3),
                    _pipe@5 = gleam@float:divide(_pipe@4, 255.0),
                    gleam@result:'try'(_pipe@5, fun valid_colour_value/1)
                end,
                fun(G) ->
                    gleam@result:'try'(
                        begin
                            _pipe@6 = Blue,
                            _pipe@7 = erlang:float(_pipe@6),
                            _pipe@8 = gleam@float:divide(_pipe@7, 255.0),
                            gleam@result:'try'(
                                _pipe@8,
                                fun valid_colour_value/1
                            )
                        end,
                        fun(B) -> {ok, {rgba, R, G, B, 1.0}} end
                    )
                end
            )
        end
    ).

-file("src/gleam_community/colour.gleam", 348).
?DOC(
    " Returns `Result(Colour)` created from the given RGB values.\n"
    "\n"
    " If the supplied RGB values are greater than 1.0 or less than 0.0 returns `Error(Nil)`\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgb(1.0, 0.0, 0.0)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec from_rgb(float(), float(), float()) -> {ok, colour()} | {error, nil}.
from_rgb(Red, Green, Blue) ->
    gleam@result:'try'(
        valid_colour_value(Red),
        fun(R) ->
            gleam@result:'try'(
                valid_colour_value(Green),
                fun(G) ->
                    gleam@result:'try'(
                        valid_colour_value(Blue),
                        fun(B) -> {ok, {rgba, R, G, B, 1.0}} end
                    )
                end
            )
        end
    ).

-file("src/gleam_community/colour.gleam", 383).
?DOC(
    " Returns `Result(Colour)` created from the given RGBA values.\n"
    "\n"
    " Returns `Error(Nil)` if the supplied RGBA values are greater than 1.0 or less than 0.0.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red_half_opacity) = from_rbga(1.0, 0.0, 0.0, 0.5)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec from_rgba(float(), float(), float(), float()) -> {ok, colour()} |
    {error, nil}.
from_rgba(Red, Green, Blue, Alpha) ->
    gleam@result:'try'(
        valid_colour_value(Red),
        fun(R) ->
            gleam@result:'try'(
                valid_colour_value(Green),
                fun(G) ->
                    gleam@result:'try'(
                        valid_colour_value(Blue),
                        fun(B) ->
                            gleam@result:'try'(
                                valid_colour_value(Alpha),
                                fun(A) -> {ok, {rgba, R, G, B, A}} end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam_community/colour.gleam", 420).
?DOC(
    " Returns `Result(Colour)` created from the given HSLA values.\n"
    "\n"
    " Returns `Error(Nil)`f the supplied HSLA values are greater than 1.0 or less than 0.0.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red_half_opacity) = from_hsla(0.0, 1.0, 0.5, 0.5)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec from_hsla(float(), float(), float(), float()) -> {ok, colour()} |
    {error, nil}.
from_hsla(Hue, Saturation, Lightness, Alpha) ->
    gleam@result:'try'(
        valid_colour_value(Hue),
        fun(H) ->
            gleam@result:'try'(
                valid_colour_value(Saturation),
                fun(S) ->
                    gleam@result:'try'(
                        valid_colour_value(Lightness),
                        fun(L) ->
                            gleam@result:'try'(
                                valid_colour_value(Alpha),
                                fun(A) -> {ok, {hsla, H, S, L, A}} end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam_community/colour.gleam", 457).
?DOC(
    " Returns `Result(Colour)` created from the given HSL values.\n"
    "\n"
    " Returns `Error(Nil)` if the supplied HSL values are greater than 1.0 or less than 0.0.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_hsla(0.0, 1.0, 0.5)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec from_hsl(float(), float(), float()) -> {ok, colour()} | {error, nil}.
from_hsl(Hue, Saturation, Lightness) ->
    from_hsla(Hue, Saturation, Lightness, 1.0).

-file("src/gleam_community/colour.gleam", 488).
?DOC(
    " Returns a `Result(Colour)` created from the given hex `Int`.\n"
    "\n"
    " Returns `Error(Nil)` if the supplied hex `Int is greater than 0xffffff or less than 0x0.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgb_hex(0xff0000)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec from_rgb_hex(integer()) -> {ok, colour()} | {error, nil}.
from_rgb_hex(Hex) ->
    case (Hex > 16#ffffff) orelse (Hex < 0) of
        true ->
            {error, nil};

        false ->
            R = begin
                _pipe = erlang:'bsr'(Hex, 16),
                erlang:'band'(_pipe, 16#ff)
            end,
            G = begin
                _pipe@1 = erlang:'bsr'(Hex, 8),
                erlang:'band'(_pipe@1, 16#ff)
            end,
            B = erlang:'band'(Hex, 16#ff),
            from_rgb255(R, G, B)
    end.

-file("src/gleam_community/colour.gleam", 527).
?DOC(
    " Returns a `Result(Colour)` created from the given RGB hex `String`.\n"
    "\n"
    " Returns `Error(Nil)` if the supplied hex `String` is invalid, or greater than `\"#ffffff\" or less than `\"#0\"`\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgb_hex_string(\"#ff0000\")\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec from_rgb_hex_string(binary()) -> {ok, colour()} | {error, nil}.
from_rgb_hex_string(Hex_string) ->
    gleam@result:'try'(
        hex_string_to_int(Hex_string),
        fun(Hex_int) -> from_rgb_hex(Hex_int) end
    ).

-file("src/gleam_community/colour.gleam", 585).
?DOC(
    " Returns a `Result(Colour)` created from the given hex `Int`.\n"
    "\n"
    " Returns `Error(Nil)` if the supplied hex `Int is greater than 0xffffffff or less than 0x0.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red_half_opacity) = from_rgba_hex(0xff00007f)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec from_rgba_hex(integer()) -> {ok, colour()} | {error, nil}.
from_rgba_hex(Hex) ->
    case (Hex > 16#ffffffff) orelse (Hex < 0) of
        true ->
            {error, nil};

        false ->
            R@1 = case begin
                _pipe = erlang:'bsr'(Hex, 24),
                _pipe@1 = erlang:'band'(_pipe, 16#ff),
                _pipe@2 = erlang:float(_pipe@1),
                gleam@float:divide(_pipe@2, 255.0)
            end of
                {ok, R} -> R;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"gleam_community/colour"/utf8>>,
                                function => <<"from_rgba_hex"/utf8>>,
                                line => 590,
                                value => _assert_fail,
                                start => 17111,
                                'end' => 17260,
                                pattern_start => 17122,
                                pattern_end => 17127})
            end,
            G@1 = case begin
                _pipe@3 = erlang:'bsr'(Hex, 16),
                _pipe@4 = erlang:'band'(_pipe@3, 16#ff),
                _pipe@5 = erlang:float(_pipe@4),
                gleam@float:divide(_pipe@5, 255.0)
            end of
                {ok, G} -> G;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"gleam_community/colour"/utf8>>,
                                function => <<"from_rgba_hex"/utf8>>,
                                line => 596,
                                value => _assert_fail@1,
                                start => 17332,
                                'end' => 17481,
                                pattern_start => 17343,
                                pattern_end => 17348})
            end,
            B@1 = case begin
                _pipe@6 = erlang:'bsr'(Hex, 8),
                _pipe@7 = erlang:'band'(_pipe@6, 16#ff),
                _pipe@8 = erlang:float(_pipe@7),
                gleam@float:divide(_pipe@8, 255.0)
            end of
                {ok, B} -> B;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"gleam_community/colour"/utf8>>,
                                function => <<"from_rgba_hex"/utf8>>,
                                line => 602,
                                value => _assert_fail@2,
                                start => 17553,
                                'end' => 17701,
                                pattern_start => 17564,
                                pattern_end => 17569})
            end,
            A@1 = case begin
                _pipe@9 = erlang:'band'(Hex, 16#ff),
                _pipe@10 = erlang:float(_pipe@9),
                gleam@float:divide(_pipe@10, 255.0)
            end of
                {ok, A} -> A;
                _assert_fail@3 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"gleam_community/colour"/utf8>>,
                                function => <<"from_rgba_hex"/utf8>>,
                                line => 608,
                                value => _assert_fail@3,
                                start => 17773,
                                'end' => 17883,
                                pattern_start => 17784,
                                pattern_end => 17789})
            end,
            from_rgba(R@1, G@1, B@1, A@1)
    end.

-file("src/gleam_community/colour.gleam", 556).
?DOC(
    " Returns a `Result(Colour)` created from the given RGBA hex `String`.\n"
    "\n"
    " Returns `Error(Nil)` if the supplied hex `String` is invalid, or greater than `\"#ffffffff\" or less than `\"#0\"`\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red_half_opacity) = from_rgba_hex_string(\"#ff00007f\")\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec from_rgba_hex_string(binary()) -> {ok, colour()} | {error, nil}.
from_rgba_hex_string(Hex_string) ->
    gleam@result:'try'(
        hex_string_to_int(Hex_string),
        fun(Hex_int) -> from_rgba_hex(Hex_int) end
    ).

-file("src/gleam_community/colour.gleam", 642).
?DOC(
    " Returns `#(Float, Float, Float, Float)` representing the given `Colour`'s\n"
    " R, G, B, and A values respectively.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgb255(255, 0, 0)\n"
    "   let #(r, g, b, a) = to_rgba(red)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec to_rgba(colour()) -> {float(), float(), float(), float()}.
to_rgba(Colour) ->
    case Colour of
        {rgba, R, G, B, A} ->
            {R, G, B, A};

        {hsla, H, S, L, A@1} ->
            hsla_to_rgba(H, S, L, A@1)
    end.

-file("src/gleam_community/colour.gleam", 672).
?DOC(
    " Returns `#(Float, Float, Float, Float)` representing the given `Colour`'s\n"
    " H, S, L, and A values respectively.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgb255(255, 0, 0)\n"
    "   let #(h, s, l, a) = to_hsla(red)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec to_hsla(colour()) -> {float(), float(), float(), float()}.
to_hsla(Colour) ->
    case Colour of
        {hsla, H, S, L, A} ->
            {H, S, L, A};

        {rgba, R, G, B, A@1} ->
            rgba_to_hsla(R, G, B, A@1)
    end.

-file("src/gleam_community/colour.gleam", 701).
?DOC(
    " Returns an rgba formatted CSS `String` created from the given `Colour`.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgb255(255, 0, 0)\n"
    "   let css_red = to_css_rgba_string(red)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec to_css_rgba_string(colour()) -> binary().
to_css_rgba_string(Colour) ->
    {R, G, B, A} = to_rgba(Colour),
    Percent = fun(X) ->
        P@1 = case begin
            _pipe = X,
            _pipe@1 = gleam@float:multiply(_pipe, 10000.0),
            _pipe@2 = erlang:round(_pipe@1),
            _pipe@3 = erlang:float(_pipe@2),
            gleam@float:divide(_pipe@3, 100.0)
        end of
            {ok, P} -> P;
            _assert_fail ->
                erlang:error(#{gleam_error => let_assert,
                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"gleam_community/colour"/utf8>>,
                            function => <<"to_css_rgba_string"/utf8>>,
                            line => 706,
                            value => _assert_fail,
                            start => 20510,
                            'end' => 20646,
                            pattern_start => 20521,
                            pattern_end => 20526})
        end,
        P@1
    end,
    Round_to = fun(X@1) ->
        R@2 = case begin
            _pipe@4 = X@1,
            _pipe@5 = gleam@float:multiply(_pipe@4, 1000.0),
            _pipe@6 = erlang:round(_pipe@5),
            _pipe@7 = erlang:float(_pipe@6),
            gleam@float:divide(_pipe@7, 1000.0)
        end of
            {ok, R@1} -> R@1;
            _assert_fail@1 ->
                erlang:error(#{gleam_error => let_assert,
                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"gleam_community/colour"/utf8>>,
                            function => <<"to_css_rgba_string"/utf8>>,
                            line => 718,
                            value => _assert_fail@1,
                            start => 20768,
                            'end' => 20903,
                            pattern_start => 20779,
                            pattern_end => 20784})
        end,
        R@2
    end,
    gleam@string:join(
        [<<"rgba("/utf8>>,
            <<(gleam_stdlib:float_to_string(Percent(R)))/binary, "%,"/utf8>>,
            <<(gleam_stdlib:float_to_string(Percent(G)))/binary, "%,"/utf8>>,
            <<(gleam_stdlib:float_to_string(Percent(B)))/binary, "%,"/utf8>>,
            gleam_stdlib:float_to_string(Round_to(A)),
            <<")"/utf8>>],
        <<""/utf8>>
    ).

-file("src/gleam_community/colour.gleam", 829).
?DOC(
    " Returns an hex `Int` created from the given `Colour`.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgba(1.0, 0.0, 0.0, 1.0)\n"
    "   let red_hex_int = to_rgba_hex(red)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec to_rgba_hex(colour()) -> integer().
to_rgba_hex(Colour) ->
    {R, G, B, A} = to_rgba(Colour),
    Red = begin
        _pipe = R * 255.0,
        _pipe@1 = erlang:round(_pipe),
        erlang:'bsl'(_pipe@1, 24)
    end,
    Green = begin
        _pipe@2 = G * 255.0,
        _pipe@3 = erlang:round(_pipe@2),
        erlang:'bsl'(_pipe@3, 16)
    end,
    Blue = begin
        _pipe@4 = B * 255.0,
        _pipe@5 = erlang:round(_pipe@4),
        erlang:'bsl'(_pipe@5, 8)
    end,
    Alpha = begin
        _pipe@6 = A * 255.0,
        erlang:round(_pipe@6)
    end,
    ((Red + Green) + Blue) + Alpha.

-file("src/gleam_community/colour.gleam", 763).
?DOC(
    " Returns an rgba hex formatted `String` created from the given `Colour`.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgba(1.0, 0.0, 0.0, 1.0)\n"
    "   let red_hex = to_rgba_hex_string(red)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec to_rgba_hex_string(colour()) -> binary().
to_rgba_hex_string(Colour) ->
    Hex_string = begin
        _pipe = to_rgba_hex(Colour),
        gleam@int:to_base16(_pipe)
    end,
    case string:length(Hex_string) of
        8 ->
            Hex_string;

        L ->
            <<(gleam@string:repeat(<<"0"/utf8>>, 8 - L))/binary,
                Hex_string/binary>>
    end.

-file("src/gleam_community/colour.gleam", 876).
?DOC(
    " Returns a rgb hex `Int` created from the given `Colour`.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgba(255, 0, 0)\n"
    "   let red_hex_int = to_rgb_hex(red)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec to_rgb_hex(colour()) -> integer().
to_rgb_hex(Colour) ->
    {R, G, B, _} = to_rgba(Colour),
    Red = begin
        _pipe = R * 255.0,
        _pipe@1 = erlang:round(_pipe),
        erlang:'bsl'(_pipe@1, 16)
    end,
    Green = begin
        _pipe@2 = G * 255.0,
        _pipe@3 = erlang:round(_pipe@2),
        erlang:'bsl'(_pipe@3, 8)
    end,
    Blue = begin
        _pipe@4 = B * 255.0,
        erlang:round(_pipe@4)
    end,
    (Red + Green) + Blue.

-file("src/gleam_community/colour.gleam", 796).
?DOC(
    " Returns an rgb hex formatted `String` created from the given `Colour`.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   assert Ok(red) = from_rgba(255, 0, 0)\n"
    "   let red_hex = to_rgb_hex_string(red)\n"
    " }\n"
    " ```\n"
    " </details>\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec to_rgb_hex_string(colour()) -> binary().
to_rgb_hex_string(Colour) ->
    Hex_string = begin
        _pipe = to_rgb_hex(Colour),
        gleam@int:to_base16(_pipe)
    end,
    case string:length(Hex_string) of
        6 ->
            Hex_string;

        L ->
            <<(gleam@string:repeat(<<"0"/utf8>>, 6 - L))/binary,
                Hex_string/binary>>
    end.

-file("src/gleam_community/colour.gleam", 918).
-spec encode_rgba(float(), float(), float(), float()) -> gleam@json:json().
encode_rgba(R, G, B, A) ->
    gleam@json:object(
        [{<<"r"/utf8>>, gleam@json:float(R)},
            {<<"g"/utf8>>, gleam@json:float(G)},
            {<<"b"/utf8>>, gleam@json:float(B)},
            {<<"a"/utf8>>, gleam@json:float(A)}]
    ).

-file("src/gleam_community/colour.gleam", 927).
-spec encode_hsla(float(), float(), float(), float()) -> gleam@json:json().
encode_hsla(H, S, L, A) ->
    gleam@json:object(
        [{<<"h"/utf8>>, gleam@json:float(H)},
            {<<"s"/utf8>>, gleam@json:float(S)},
            {<<"l"/utf8>>, gleam@json:float(L)},
            {<<"a"/utf8>>, gleam@json:float(A)}]
    ).

-file("src/gleam_community/colour.gleam", 911).
?DOC(
    " Encodes a `Colour` value as a Gleam [`Json`](https://hexdocs.pm/gleam_json/gleam/json.html#Json)\n"
    " value. You'll need this if you want to send a `Colour` value over the network\n"
    " in a HTTP request or response, for example.\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec encode(colour()) -> gleam@json:json().
encode(Colour) ->
    case Colour of
        {rgba, R, G, B, A} ->
            encode_rgba(R, G, B, A);

        {hsla, H, S, L, A@1} ->
            encode_hsla(H, S, L, A@1)
    end.

-file("src/gleam_community/colour.gleam", 952).
-spec rgba_decoder() -> gleam@dynamic@decode:decoder(colour()).
rgba_decoder() ->
    gleam@dynamic@decode:field(
        <<"r"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_float/1},
        fun(R) ->
            gleam@dynamic@decode:field(
                <<"g"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_float/1},
                fun(G) ->
                    gleam@dynamic@decode:field(
                        <<"b"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_float/1},
                        fun(B) ->
                            gleam@dynamic@decode:field(
                                <<"a"/utf8>>,
                                {decoder,
                                    fun gleam@dynamic@decode:decode_float/1},
                                fun(A) ->
                                    gleam@dynamic@decode:success(
                                        {rgba, R, G, B, A}
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam_community/colour.gleam", 961).
-spec hsla_decoder() -> gleam@dynamic@decode:decoder(colour()).
hsla_decoder() ->
    gleam@dynamic@decode:field(
        <<"h"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_float/1},
        fun(H) ->
            gleam@dynamic@decode:field(
                <<"s"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_float/1},
                fun(S) ->
                    gleam@dynamic@decode:field(
                        <<"l"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_float/1},
                        fun(L) ->
                            gleam@dynamic@decode:field(
                                <<"a"/utf8>>,
                                {decoder,
                                    fun gleam@dynamic@decode:decode_float/1},
                                fun(A) ->
                                    gleam@dynamic@decode:success(
                                        {hsla, H, S, L, A}
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam_community/colour.gleam", 948).
?DOC(
    " Attempt to decode some [`Dynamic`](https://hexdocs.pm/gleam_stdlib/gleam/dynamic.html#Dynamic)\n"
    " value into a `Colour`. Most often you'll use this to decode some JSON.\n"
    "\n"
    " <div style=\"position: relative;\">\n"
    "     <a style=\"position: absolute; left: 0;\" href=\"https://github.com/gleam-community/colour/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    "     <a style=\"position: absolute; right: 0;\" href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec decoder() -> gleam@dynamic@decode:decoder(colour()).
decoder() ->
    gleam@dynamic@decode:one_of(rgba_decoder(), [hsla_decoder()]).
