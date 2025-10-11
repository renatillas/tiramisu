-module(gleam_community@colour@accessibility).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gleam_community/colour/accessibility.gleam").
-export([luminance/1, contrast_ratio/2, maximum_contrast/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " \n"
    " - **Accessibility**\n"
    "   - [`luminance`](#luminance)\n"
    "   - [`contrast_ratio`](#contrast_ratio)\n"
    "   - [`maximum_contrast`](#maximum_contrast)\n"
    "\n"
    " ---\n"
    "\n"
    " This package was heavily inspired by the `elm-color-extra` module.\n"
    " The original source code can be found\n"
    " <a href=\"https://github.com/noahzgordon/elm-color-extra\">here</a>.\n"
    "\n"
    " <details>\n"
    " <summary>The license of that package is produced below:</summary>\n"
    " \n"
    " \n"
    " > MIT License\n"
    "\n"
    " > Copyright (c) 2016 Andreas Köberle\n"
    "\n"
    " > Permission is hereby granted, free of charge, to any person obtaining a copy\n"
    " of this software and associated documentation files (the \"Software\"), to deal\n"
    " in the Software without restriction, including without limitation the rights\n"
    " to use, copy, modify, merge, publish, distribute, sublicense, and/or sell\n"
    " copies of the Software, and to permit persons to whom the Software is\n"
    " furnished to do so, subject to the following conditions:\n"
    "\n"
    " > The above copyright notice and this permission notice shall be included in all\n"
    " copies or substantial portions of the Software.\n"
    "\n"
    " > THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\n"
    " IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\n"
    " FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE\n"
    " AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\n"
    " LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,\n"
    " OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE\n"
    " SOFTWARE.\n"
    "\n"
    " </details>\n"
    "\n"
).

-file("src/gleam_community/colour/accessibility.gleam", 56).
-spec intensity(float()) -> float().
intensity(Colour_value) ->
    case true of
        _ when Colour_value =< 0.03928 ->
            Colour_value / 12.92;

        _ ->
            I@1 = case gleam@float:power((Colour_value + 0.055) / 1.055, 2.4) of
                {ok, I} -> I;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"gleam_community/colour/accessibility"/utf8>>,
                                function => <<"intensity"/utf8>>,
                                line => 62,
                                value => _assert_fail,
                                start => 2399,
                                'end' => 2470,
                                pattern_start => 2410,
                                pattern_end => 2415})
            end,
            I@1
    end.

-file("src/gleam_community/colour/accessibility.gleam", 92).
?DOC(
    " Returns the relative brightness of the given `Colour` as a `Float` between\n"
    " 0.0, and 1.0 with 0.0 being the darkest possible colour and 1.0 being the lightest.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   luminance(colour.white) // 1.0\n"
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
-spec luminance(gleam_community@colour:colour()) -> float().
luminance(Colour) ->
    {R, G, B, _} = gleam_community@colour:to_rgba(Colour),
    R_intensity = intensity(R),
    G_intensity = intensity(G),
    B_intensity = intensity(B),
    ((0.2126 * R_intensity) + (0.7152 * G_intensity)) + (0.0722 * B_intensity).

-file("src/gleam_community/colour/accessibility.gleam", 125).
?DOC(
    " Returns the contrast between two `Colour` values as a `Float` between 1.0,\n"
    " and 21.0 with 1.0 being no contrast and, 21.0 being the highest possible contrast.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   contrast_ratio(between: colour.white, and: colour.black) // 21.0\n"
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
-spec contrast_ratio(
    gleam_community@colour:colour(),
    gleam_community@colour:colour()
) -> float().
contrast_ratio(Colour_a, Colour_b) ->
    Luminance_a = luminance(Colour_a) + 0.05,
    Luminance_b = luminance(Colour_b) + 0.05,
    case Luminance_a > Luminance_b of
        true ->
            case Luminance_b of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> Luminance_a / Gleam@denominator
            end;

        false ->
            case Luminance_a of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> Luminance_b / Gleam@denominator@1
            end
    end.

-file("src/gleam_community/colour/accessibility.gleam", 161).
?DOC(
    " Returns the `Colour` with the highest contrast between the base `Colour`,\n"
    " and and the other provided `Colour` values.\n"
    "\n"
    " <details>\n"
    " <summary>Example:</summary>\n"
    "\n"
    " ```gleam\n"
    " fn example() {\n"
    "   maximum_contrast(\n"
    "    colour.yellow,\n"
    "    [colour.white, colour.dark_blue, colour.green],\n"
    "  )\n"
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
-spec maximum_contrast(
    gleam_community@colour:colour(),
    list(gleam_community@colour:colour())
) -> {ok, gleam_community@colour:colour()} | {error, nil}.
maximum_contrast(Base, Colours) ->
    _pipe = Colours,
    _pipe@1 = gleam@list:sort(
        _pipe,
        fun(Colour_a, Colour_b) ->
            Contrast_a = contrast_ratio(Base, Colour_a),
            Contrast_b = contrast_ratio(Base, Colour_b),
            gleam@float:compare(Contrast_b, Contrast_a)
        end
    ),
    gleam@list:first(_pipe@1).
