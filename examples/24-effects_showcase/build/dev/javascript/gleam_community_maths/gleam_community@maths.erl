-module(gleam_community@maths).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([gcd/2, euclidean_modulo/2, lcm/2, divisors/1, proper_divisors/1, weighted_sum/1, weighted_product/1, cumulative_sum/1, int_cumulative_sum/1, cumulative_product/1, int_cumulative_product/1, acos/1, acosh/1, asin/1, asinh/1, atan/1, atan2/2, cartesian_to_polar/2, atanh/1, cos/1, cosh/1, sin/1, polar_to_cartesian/2, sinh/1, tan/1, tanh/1, exponential/1, natural_logarithm/1, logarithm_2/1, logarithm_10/1, logarithm/2, nth_root/2, degrees_to_radians/1, radians_to_degrees/1, pi/0, tau/0, golden_ratio/0, e/0, round_to_zero/2, round_down/2, round_up/2, absolute_difference/2, int_absolute_difference/2, sign/1, round_to_nearest/2, round_ties_away/2, round_ties_up/2, int_sign/1, flip_sign/1, copy_sign/2, int_flip_sign/1, int_copy_sign/2, minmax/3, list_minimum/2, list_maximum/2, arg_minimum/2, arg_maximum/2, extrema/2, combination/2, combination_with_repetitions/2, factorial/1, permutation/2, permutation_with_repetitions/2, list_combination/2, list_combination_with_repetitions/2, list_permutation/2, list_permutation_with_repetitions/2, cartesian_product/2, norm/2, norm_with_weights/2, minkowski_distance/2, manhattan_distance/1, minkowski_distance_with_weights/2, manhattan_distance_with_weights/1, euclidean_distance/1, euclidean_distance_with_weights/1, chebyshev_distance/1, chebyshev_distance_with_weights/1, mean/1, moment/2, harmonic_mean/1, geometric_mean/1, median/1, variance/2, standard_deviation/2, kurtosis/1, skewness/1, percentile/2, zscore/2, interquartile_range/1, correlation/1, tversky_index/4, jaccard_index/2, sorensen_dice_coefficient/2, overlap_coefficient/2, cosine_similarity/1, cosine_similarity_with_weights/1, canberra_distance/1, canberra_distance_with_weights/1, braycurtis_distance/1, braycurtis_distance_with_weights/1, is_close/4, all_close/3, is_fractional/1, is_power/2, is_perfect/1, is_prime/1, is_between/3, is_divisible/2, is_multiple/2, erf/1, incomplete_gamma/2, step_range/3, yield_step_range/3, linear_space/4, yield_linear_space/4, logarithmic_space/5, yield_logarithmic_space/5, geometric_space/4, yield_geometric_space/4, symmetric_space/3, yield_symmetric_space/3, gamma/1, beta/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css\" integrity=\"sha384-nB0miv6/jRmo5UMMR1wu3Gz6NLsoTkbqJghGIsx//Rlm+ZU03BU6SQNC66uf4l5+\" crossorigin=\"anonymous\">\n"
    "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js\" integrity=\"sha384-7zkQWkzuo3B5mTepMUcHkMB5jZaolc2xDwL6VFqjFALcbeS9Ggm/Yr2r3Dy4lfFg\" crossorigin=\"anonymous\"></script>\n"
    "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js\" integrity=\"sha384-43gviWU0YVjaDtb/GhzOouOXtZMP/7XUzwPTstBeZFe/+rCMvRwr4yROQP43s0Xk\" crossorigin=\"anonymous\"></script>\n"
    "<script>\n"
    "    document.addEventListener(\"DOMContentLoaded\", function() {\n"
    "        renderMathInElement(document.body, {\n"
    "          // customised options\n"
    "          // • auto-render specific keys, e.g.:\n"
    "          delimiters: [\n"
    "              {left: '$$', right: '$$', display: false},\n"
    "              {left: '$', right: '$', display: false},\n"
    "              {left: '\\\\(', right: '\\\\)', display: false},\n"
    "              {left: '\\\\[', right: '\\\\]', display: true}\n"
    "          ],\n"
    "          // • rendering keys, e.g.:\n"
    "          throwOnError : true\n"
    "        });\n"
    "    });\n"
    "</script>\n"
    "<style>\n"
    "    .katex { font-size: 1.10em; }\n"
    "</style>\n"
    "\n"
).

-file("src/gleam_community/maths.gleam", 74).
-spec do_gcd(integer(), integer()) -> integer().
do_gcd(X, Y) ->
    case X =:= 0 of
        true ->
            Y;

        false ->
            do_gcd(case X of
                    0 -> 0;
                    Gleam@denominator -> Y rem Gleam@denominator
                end, X)
    end.

-file("src/gleam_community/maths.gleam", 67).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function calculates the greatest common divisor of two integers\n"
    " \\\\(x, y \\in \\mathbb{Z}\\\\). The greatest common divisor is the largest positive\n"
    " integer that is divisible by both \\\\(x\\\\) and \\\\(y\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.gcd(1, 1)\n"
    "       |> should.equal(1)\n"
    "\n"
    "       maths.gcd(100, 10)\n"
    "       |> should.equal(10)\n"
    "\n"
    "       maths.gcd(-36, -17)\n"
    "       |> should.equal(1)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec gcd(integer(), integer()) -> integer().
gcd(X, Y) ->
    Absx = gleam@int:absolute_value(X),
    Absy = gleam@int:absolute_value(Y),
    do_gcd(Absx, Absy).

-file("src/gleam_community/maths.gleam", 125).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    "\n"
    " Given two integers, \\\\(x\\\\) (dividend) and \\\\(y\\\\) (divisor), the Euclidean modulo\n"
    " of \\\\(x\\\\) by \\\\(y\\\\), denoted as \\\\(x \\mod y\\\\), is the remainder \\\\(r\\\\) of the\n"
    " division of \\\\(x\\\\) by \\\\(y\\\\), such that:\n"
    "\n"
    " \\\\[\n"
    " x = q \\cdot y + r \\quad \\text{and} \\quad 0 \\leq r < |y|,\n"
    " \\\\]\n"
    "\n"
    " where \\\\(q\\\\) is an integer that represents the quotient of the division.\n"
    "\n"
    " Note that like the Gleam division operator `/` this will return `0` if one of\n"
    " the arguments is `0`.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.euclidean_modulo(15, 4)\n"
    "       |> should.equal(3)\n"
    "\n"
    "       maths.euclidean_modulo(-3, -2)\n"
    "       |> should.equal(1)\n"
    "\n"
    "       maths.euclidean_modulo(5, 0)\n"
    "       |> should.equal(0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec euclidean_modulo(integer(), integer()) -> integer().
euclidean_modulo(X, Y) ->
    case {case Y of
            0 -> 0;
            Gleam@denominator -> X rem Gleam@denominator
        end, X, Y} of
        {_, 0, _} ->
            0;

        {_, _, 0} ->
            0;

        {Md, _, _} when Md < 0 ->
            Md + gleam@int:absolute_value(Y);

        {Md@1, _, _} ->
            Md@1
    end.

-file("src/gleam_community/maths.gleam", 168).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function calculates the least common multiple of two integers\n"
    " \\\\(x, y \\in \\mathbb{Z}\\\\). The least common multiple is the smallest positive\n"
    " integer that has both \\\\(x\\\\) and \\\\(y\\\\) as factors.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.lcm(1, 1)\n"
    "       |> should.equal(1)\n"
    "\n"
    "       maths.lcm(100, 10)\n"
    "       |> should.equal(100)\n"
    "\n"
    "       maths.lcm(-36, -17)\n"
    "       |> should.equal(612)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec lcm(integer(), integer()) -> integer().
lcm(X, Y) ->
    Absx = gleam@int:absolute_value(X),
    Absy = gleam@int:absolute_value(Y),
    case do_gcd(Absx, Absy) of
        0 -> 0;
        Gleam@denominator -> Absx * Absy div Gleam@denominator
    end.

-file("src/gleam_community/maths.gleam", 225).
-spec do_find_divisors(
    integer(),
    integer(),
    gleam@set:set(integer()),
    integer()
) -> gleam@set:set(integer()).
do_find_divisors(N, Max, Acc, I) ->
    case I =< Max of
        false ->
            Acc;

        true ->
            Updated_acc = case (case I of
                0 -> 0;
                Gleam@denominator -> N rem Gleam@denominator
            end) =:= 0 of
                true ->
                    _pipe = gleam@set:insert(Acc, I),
                    gleam@set:insert(_pipe, case I of
                            0 -> 0;
                            Gleam@denominator@1 -> N div Gleam@denominator@1
                        end);

                false ->
                    Acc
            end,
            do_find_divisors(N, Max, Updated_acc, I + 1)
    end.

-file("src/gleam_community/maths.gleam", 214).
-spec find_divisors(integer()) -> gleam@set:set(integer()).
find_divisors(N) ->
    Nabs = gleam@float:absolute_value(erlang:float(N)),
    _assert_subject = gleam@float:square_root(Nabs),
    {ok, Sqrt_result} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"find_divisors"/utf8>>,
                        line => 219})
    end,
    Max = erlang:round(Sqrt_result) + 1,
    do_find_divisors(N, Max, gleam@set:new(), 1).

-file("src/gleam_community/maths.gleam", 208).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function returns all the positive divisors of an integer, including the\n"
    " number itself.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.divisors(4)\n"
    "       |> should.equal([1, 2, 4])\n"
    "\n"
    "       maths.divisors(6)\n"
    "       |> should.equal([1, 2, 3, 6])\n"
    "\n"
    "       maths.divisors(13)\n"
    "       |> should.equal([1, 13])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec divisors(integer()) -> list(integer()).
divisors(N) ->
    _pipe = find_divisors(N),
    _pipe@1 = gleam@set:to_list(_pipe),
    gleam@list:sort(_pipe@1, fun gleam@int:compare/2).

-file("src/gleam_community/maths.gleam", 272).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function returns all the positive divisors of an integer, excluding the\n"
    " number itself.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.proper_divisors(4)\n"
    "       |> should.equal([1, 2])\n"
    "\n"
    "       maths.proper_divisors(6)\n"
    "       |> should.equal([1, 2, 3])\n"
    "\n"
    "       maths.proper_divisors(13)\n"
    "       |> should.equal([1])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec proper_divisors(integer()) -> list(integer()).
proper_divisors(N) ->
    _pipe = find_divisors(N),
    _pipe@1 = gleam@set:delete(_pipe, N),
    _pipe@2 = gleam@set:to_list(_pipe@1),
    gleam@list:sort(_pipe@2, fun gleam@int:compare/2).

-file("src/gleam_community/maths.gleam", 323).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the weighted sum of the elements in a list:\n"
    "\n"
    " \\\\[\n"
    " \\sum_{i=1}^n w_i x_i\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the list and \\\\(x_i \\in \\mathbb{R}\\\\)\n"
    " is the value in the input list indexed by \\\\(i\\\\), while the \\\\(w_i \\in \\mathbb{R}\\\\)\n"
    " are corresponding positive weights.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/float\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.weighted_sum()\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       [#(1.0, 1.0), #(2.0, 1.0), #(3.0, 1.0)]\n"
    "       |> maths.weighted_sum()\n"
    "       |> should.equal(Ok(6.0))\n"
    "\n"
    "       [#(9.0, 0.5), #(10.0, 0.5), #(10.0, 0.5)]\n"
    "       |> maths.weighted_sum()\n"
    "       |> should.equal(Ok(14.5))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec weighted_sum(list({float(), float()})) -> {ok, float()} | {error, nil}.
weighted_sum(Arr) ->
    case Arr of
        [] ->
            {ok, +0.0};

        _ ->
            gleam@list:try_fold(
                Arr,
                +0.0,
                fun(Acc, Tuple) -> case erlang:element(2, Tuple) < +0.0 of
                        true ->
                            {error, nil};

                        false ->
                            {ok,
                                (erlang:element(1, Tuple) * erlang:element(
                                    2,
                                    Tuple
                                ))
                                + Acc}
                    end end
            )
    end.

-file("src/gleam_community/maths.gleam", 385).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the weighted product of the elements in a list:\n"
    "\n"
    " \\\\[\n"
    " \\prod_{i=1}^n x_i^{w_i}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the list and \\\\(x_i \\in \\mathbb{R}\\\\) is\n"
    " the value in the input list indexed by \\\\(i\\\\), while the \\\\(w_i \\in \\mathbb{R}\\\\)\n"
    " are corresponding positive weights.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/float\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.weighted_product()\n"
    "       |> should.equal(Ok(1.0))\n"
    "\n"
    "       [#(1.0, 1.0), #(2.0, 1.0), #(3.0, 1.0)]\n"
    "       |> maths.weighted_product()\n"
    "       |> should.equal(Ok(6.0))\n"
    "\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "       let assert Ok(result) =\n"
    "         [#(9.0, 0.5), #(10.0, 0.5), #(10.0, 0.5)]\n"
    "         |> maths.weighted_product()\n"
    "       result\n"
    "       |> maths.is_close(30.0, 0.0, tolerance)\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec weighted_product(list({float(), float()})) -> {ok, float()} | {error, nil}.
weighted_product(Arr) ->
    case Arr of
        [] ->
            {ok, 1.0};

        _ ->
            gleam@list:try_fold(
                Arr,
                1.0,
                fun(Acc, Tuple) -> case erlang:element(2, Tuple) < +0.0 of
                        true ->
                            {error, nil};

                        false ->
                            case gleam@float:power(
                                erlang:element(1, Tuple),
                                erlang:element(2, Tuple)
                            ) of
                                {error, nil} ->
                                    {error, nil};

                                {ok, Value} ->
                                    {ok, Value * Acc}
                            end
                    end end
            )
    end.

-file("src/gleam_community/maths.gleam", 443).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the cumulative sum of the elements in a list:\n"
    "\n"
    " \\\\[\n"
    " v_j = \\sum_{i=1}^j x_i \\\\;\\\\; \\forall j = 1,\\dots, n\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(v_j\\\\) is the \\\\(j\\\\)'th element in the cumulative sum of \\\\(n\\\\)\n"
    " elements. That is, \\\\(n\\\\) is the length of the list and \\\\(x_i \\in \\mathbb{R}\\\\)\n"
    " is the value in the input list indexed by \\\\(i\\\\). The value \\\\(v_j\\\\) is thus the\n"
    " sum of the \\\\(1\\\\) to \\\\(j\\\\) first elements in the given list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.cumulative_sum()\n"
    "       |> should.equal([])\n"
    "\n"
    "       [1.0, 2.0, 3.0]\n"
    "       |> maths.cumulative_sum()\n"
    "       |> should.equal([1.0, 3.0, 6.0])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec cumulative_sum(list(float())) -> list(float()).
cumulative_sum(Arr) ->
    case Arr of
        [] ->
            [];

        _ ->
            gleam@list:scan(Arr, +0.0, fun(Acc, Element) -> Element + Acc end)
    end.

-file("src/gleam_community/maths.gleam", 490).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the cumulative sum of the elements in a list:\n"
    "\n"
    " \\\\[\n"
    " v_j = \\sum_{i=1}^j x_i \\\\;\\\\; \\forall j = 1,\\dots, n\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(v_j\\\\) is the \\\\(j\\\\)'th element in the cumulative sum of \\\\(n\\\\)\n"
    " elements. That is, \\\\(n\\\\) is the length of the list and \\\\(x_i \\in \\mathbb{Z}\\\\)\n"
    " is the value in the input list indexed by \\\\(i\\\\). The value \\\\(v_j\\\\) is thus the\n"
    " sum of the \\\\(1\\\\) to \\\\(j\\\\) first elements in the given list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.int_cumulative_sum()\n"
    "       |> should.equal([])\n"
    "\n"
    "       [1, 2, 3]\n"
    "       |> maths.int_cumulative_sum()\n"
    "       |> should.equal([1, 3, 6])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec int_cumulative_sum(list(integer())) -> list(integer()).
int_cumulative_sum(Arr) ->
    case Arr of
        [] ->
            [];

        _ ->
            gleam@list:scan(Arr, 0, fun(Acc, Element) -> Element + Acc end)
    end.

-file("src/gleam_community/maths.gleam", 538).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the cumulative product of the elements in a list:\n"
    "\n"
    " \\\\[\n"
    " v_j = \\prod_{i=1}^j x_i \\\\;\\\\; \\forall j = 1,\\dots, n\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(v_j\\\\) is the \\\\(j\\\\)'th element in the cumulative product\n"
    " of \\\\(n\\\\) elements. That is, \\\\(n\\\\) is the length of the list and\n"
    " \\\\(x_i \\in \\mathbb{R}\\\\) is the value in the input list indexed by \\\\(i\\\\).\n"
    " The value \\\\(v_j\\\\) is thus the sum of the \\\\(1\\\\) to \\\\(j\\\\) first elements\n"
    " in the given list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.cumulative_product()\n"
    "       |> should.equal([])\n"
    "\n"
    "       [1.0, 2.0, 3.0]\n"
    "       |> maths.cumulative_product()\n"
    "       |> should.equal([1.0, 2.0, 6.0])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec cumulative_product(list(float())) -> list(float()).
cumulative_product(Arr) ->
    case Arr of
        [] ->
            [];

        _ ->
            gleam@list:scan(Arr, 1.0, fun(Acc, Element) -> Element * Acc end)
    end.

-file("src/gleam_community/maths.gleam", 586).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the cumulative product of the elements in a list:\n"
    "\n"
    " \\\\[\n"
    " v_j = \\prod_{i=1}^j x_i \\\\;\\\\; \\forall j = 1,\\dots, n\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(v_j\\\\) is the \\\\(j\\\\)'th element in the cumulative product\n"
    " of \\\\(n\\\\) elements. That is, \\\\(n\\\\) is the length of the list and\n"
    " \\\\(x_i \\in \\mathbb{Z}\\\\) is the value in the input list indexed by \\\\(i\\\\).\n"
    " The value \\\\(v_j\\\\) is thus the product of the \\\\(1\\\\) to \\\\(j\\\\) first elements\n"
    " in the given list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.int_cumulative_product()\n"
    "       |> should.equal([])\n"
    "\n"
    "       [1, 2, 3]\n"
    "       |> maths.int_cumulative_product()\n"
    "       |> should.equal([1, 2, 6])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec int_cumulative_product(list(integer())) -> list(integer()).
int_cumulative_product(Arr) ->
    case Arr of
        [] ->
            [];

        _ ->
            gleam@list:scan(Arr, 1, fun(Acc, Element) -> Element * Acc end)
    end.

-file("src/gleam_community/maths.gleam", 776).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The inverse cosine function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\[-1, 1\\],   \\\\; \\cos^{-1}{(x)} = y \\in \\[0, \\pi \\]\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\[-1, 1\\]\\\\) as input and\n"
    " returns a numeric value \\\\(y\\\\) that lies in the range \\\\(\\[0, \\pi \\]\\\\) (an\n"
    " angle in radians). If the input value is outside the domain of the function\n"
    " an error is returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.acos(1.0)\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       maths.acos(1.1)\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.acos(-1.1)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec acos(float()) -> {ok, float()} | {error, nil}.
acos(X) ->
    case (X >= -1.0) andalso (X =< 1.0) of
        true ->
            {ok, math:acos(X)};

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 825).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The inverse hyperbolic cosine function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in [1, +\\infty\\),   \\\\; \\cosh^{-1}{(x)} = y \\in \\[0, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\[1, +\\infty\\)\\\\) as input\n"
    " and returns a numeric value \\\\(y\\\\) that lies in the range \\\\(\\[0, +\\infty\\)\\\\)\n"
    " (an angle in radians). If the input value is outside the domain of the function\n"
    " an error is returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.acosh(1.0)\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       maths.acosh(0.0)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec acosh(float()) -> {ok, float()} | {error, nil}.
acosh(X) ->
    case X >= 1.0 of
        true ->
            {ok, math:acosh(X)};

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 876).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The inverse sine function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\[-1, 1\\],   \\\\; \\sin^{-1}{(x)} = y \\in \\(-\\infty, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\[-1, 1\\]\\\\) as input and returns a numeric\n"
    " value \\\\(y\\\\) that lies in the range \\\\(\\[-\\frac{\\pi}{2}, \\frac{\\pi}{2}\\]\\\\) (an angle in\n"
    " radians). If the input value is outside the domain of the function an error is returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.asin(0.0)\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       maths.asin(1.1)\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.asin(-1.1)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec asin(float()) -> {ok, float()} | {error, nil}.
asin(X) ->
    case (X >= -1.0) andalso (X =< 1.0) of
        true ->
            {ok, math:asin(X)};

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 921).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The inverse hyperbolic sine function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(-\\infty, \\infty\\),   \\\\; \\sinh^{-1}{(x)} = y \\in \\(-\\infty, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(-\\infty, +\\infty\\)\\\\)\n"
    " as input and returns a numeric value \\\\(y\\\\) that lies in the range\n"
    " \\\\(\\(-\\infty, +\\infty\\)\\\\) (an angle in radians).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.asinh(0.0)\n"
    "       |> should.equal(0.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec asinh(float()) -> float().
asinh(X) ->
    math:asinh(X).

-file("src/gleam_community/maths.gleam", 963).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The inverse tangent function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(-\\infty, \\infty\\),  \\\\; \\tan^{-1}{(x)} = y \\in \\[-\\frac{\\pi}{2}, \\frac{\\pi}{2}\\]\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(-\\infty, +\\infty\\)\\\\) as input and\n"
    " returns a numeric value \\\\(y\\\\) that lies in the range \\\\(\\[-\\frac{\\pi}{2}, \\frac{\\pi}{2}\\]\\\\)\n"
    " (an angle in radians).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.atan(0.0)\n"
    "       |> should.equal(0.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec atan(float()) -> float().
atan(X) ->
    math:atan(X).

-file("src/gleam_community/maths.gleam", 1014).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The inverse 2-argument tangent function:\n"
    "\n"
    " \\\\[\n"
    " \\text{atan2}(y, x) =\n"
    " \\begin{cases}\n"
    "  \\tan^{-1}(\\frac y x) &\\text{if } x > 0, \\\\\\\\\n"
    "  \\tan^{-1}(\\frac y x) + \\pi &\\text{if } x < 0 \\text{ and } y \\ge 0, \\\\\\\\\n"
    "  \\tan^{-1}(\\frac y x) - \\pi &\\text{if } x < 0 \\text{ and } y < 0, \\\\\\\\\n"
    "  +\\frac{\\pi}{2} &\\text{if } x = 0 \\text{ and } y > 0, \\\\\\\\\n"
    "  -\\frac{\\pi}{2} &\\text{if } x = 0 \\text{ and } y < 0, \\\\\\\\\n"
    "  \\text{undefined} &\\text{if } x = 0 \\text{ and } y = 0.\n"
    " \\end{cases}\n"
    " \\\\]\n"
    "\n"
    " The function returns the angle in radians from the x-axis to the line containing\n"
    " the origin \\\\(\\(0, 0\\)\\\\) and a point given as input with coordinates \\\\(\\(x, y\\)\\\\).\n"
    " The numeric value returned by \\\\(\\text{atan2}(y, x)\\\\) is in the range\n"
    " \\\\(\\[-\\pi, \\pi\\]\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.atan2(0.0, 0.0)\n"
    "       |> should.equal(0.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec atan2(float(), float()) -> float().
atan2(Y, X) ->
    math:atan2(Y, X).

-file("src/gleam_community/maths.gleam", 724).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Converts Cartesian coordinates \\\\((x, y)\\\\) to polar coordinates \\\\((r, \\theta)\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.cartesian_to_polar(1.0, 0.0)\n"
    "       |> should.equal((1.0, 0.0))\n"
    "\n"
    "       maths.cartesian_to_polar(0.0, 1.0)\n"
    "       |> should.equal((1.0, float.pi() /. 2.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec cartesian_to_polar(float(), float()) -> {float(), float()}.
cartesian_to_polar(X, Y) ->
    _assert_subject = gleam@float:square_root((X * X) + (Y * Y)),
    {ok, R} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"cartesian_to_polar"/utf8>>,
                        line => 729})
    end,
    Theta = atan2(Y, X),
    {R, Theta}.

-file("src/gleam_community/maths.gleam", 1062).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The inverse hyperbolic tangent function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(-1, 1\\),   \\\\; \\tanh^{-1}{(x)} = y \\in \\(-\\infty, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(-1, 1\\)\\\\) as input and returns\n"
    " a numeric value \\\\(y\\\\) that lies in the range \\\\(\\(-\\infty, \\infty\\)\\\\) (an angle in radians).\n"
    " If the input value is outside the domain of the function an error is returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.atanh(0.0)\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       maths.atanh(1.0)\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.atanh(-1.0)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec atanh(float()) -> {ok, float()} | {error, nil}.
atanh(X) ->
    case (X > -1.0) andalso (X < 1.0) of
        true ->
            {ok, math:atanh(X)};

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 1109).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The cosine function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(-\\infty, +\\infty\\),   \\\\; \\cos{(x)} = y \\in \\[-1, 1\\]\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(-\\infty, \\infty\\)\\\\) (an angle in\n"
    " radians) as input and returns a numeric value \\\\(y\\\\) that lies in the range \\\\(\\[-1, 1\\]\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.cos(0.0)\n"
    "       |> should.equal(1.0)\n"
    "\n"
    "       maths.cos(maths.pi())\n"
    "       |> should.equal(-1.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec cos(float()) -> float().
cos(X) ->
    math:cos(X).

-file("src/gleam_community/maths.gleam", 1151).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The hyperbolic cosine function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(-\\infty, \\infty\\),   \\\\; \\cosh{(x)} = y \\in \\(-\\infty, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(-\\infty, \\infty\\)\\\\) as input (an angle\n"
    " in radians) and returns a numeric value \\\\(y\\\\) that lies in the range\n"
    " \\\\(\\(-\\infty, \\infty\\)\\\\). If the input value is too large an overflow error might occur.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.cosh(0.0)\n"
    "       |> should.equal(0.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec cosh(float()) -> float().
cosh(X) ->
    math:cosh(X).

-file("src/gleam_community/maths.gleam", 1195).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The sine function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(-\\infty, +\\infty\\),   \\\\; \\sin{(x)} = y \\in \\[-1, 1\\]\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(-\\infty, \\infty\\)\\\\) (an angle in\n"
    " radians) as input and returns a numeric value \\\\(y\\\\) that lies in the range \\\\(\\[-1, 1\\]\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.sin(0.0)\n"
    "       |> should.equal(0.0)\n"
    "\n"
    "       maths.sin(0.5 *. maths.pi())\n"
    "       |> should.equal(1.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec sin(float()) -> float().
sin(X) ->
    math:sin(X).

-file("src/gleam_community/maths.gleam", 687).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Converts polar coordinates \\\\((r, \\theta)\\\\) to Cartesian coordinates \\\\((x, y)\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.polar_to_cartesian(1.0, 0.0)\n"
    "       |> should.equal(#(1.0, 0.0))\n"
    "\n"
    "       maths.polar_to_cartesian(1.0, float.pi() /. 2.0)\n"
    "       |> should.equal(#(0.0, 1.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec polar_to_cartesian(float(), float()) -> {float(), float()}.
polar_to_cartesian(R, Theta) ->
    X = R * cos(Theta),
    Y = R * sin(Theta),
    {X, Y}.

-file("src/gleam_community/maths.gleam", 1237).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The hyperbolic sine function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(-\\infty, +\\infty\\),   \\\\; \\sinh{(x)} = y \\in \\(-\\infty, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(-\\infty, +\\infty\\)\\\\) as input\n"
    " (an angle in radians) and returns a numeric value \\\\(y\\\\) that lies in the range\n"
    " \\\\(\\(-\\infty, +\\infty\\)\\\\). If the input value is too large an overflow error might occur.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.sinh(0.0)\n"
    "       |> should.equal(0.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec sinh(float()) -> float().
sinh(X) ->
    math:sinh(X).

-file("src/gleam_community/maths.gleam", 1279).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The tangent function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(-\\infty, +\\infty\\),   \\\\; \\tan{(x)} = y \\in \\(-\\infty, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(-\\infty, +\\infty\\)\\\\) as input\n"
    " (an angle in radians) and returns a numeric value \\\\(y\\\\) that lies in the range\n"
    " \\\\(\\(-\\infty, +\\infty\\)\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.tan(0.0)\n"
    "       |> should.equal(0.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec tan(float()) -> float().
tan(X) ->
    math:tan(X).

-file("src/gleam_community/maths.gleam", 1326).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The hyperbolic tangent function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(-\\infty, \\infty\\),   \\\\; \\tanh{(x)} = y \\in \\[-1, 1\\]\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(-\\infty, \\infty\\)\\\\) as input (an angle\n"
    " in radians) and returns a numeric value \\\\(y\\\\) that lies in the range \\\\(\\[-1, 1\\]\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       maths.tanh(0.0)\n"
    "       |> should.equal(0.0)\n"
    "\n"
    "       maths.tanh(25.0)\n"
    "       |> should.equal(1.0)\n"
    "\n"
    "       maths.tanh(-25.0)\n"
    "       |> should.equal(-1.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec tanh(float()) -> float().
tanh(X) ->
    math:tanh(X).

-file("src/gleam_community/maths.gleam", 1368).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The exponential function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(-\\infty, \\infty\\),   \\\\; e^{x} = y \\in \\(0, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " where \\\\(e \\approx 2.71828\\dots\\\\) is Eulers' number.\n"
    "\n"
    " Note: If the input value \\\\(x\\\\) is too large an overflow error might occur.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.exponential(0.0)\n"
    "       |> should.equal(1.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec exponential(float()) -> float().
exponential(X) ->
    math:exp(X).

-file("src/gleam_community/maths.gleam", 1417).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The natural logarithm function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(0, \\infty\\),   \\\\; \\log_{e}{(x)} = y \\in \\(-\\infty, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(0, \\infty\\)\\\\) as input and returns\n"
    " a numeric value \\\\(y\\\\) that lies in the range \\\\(\\(-\\infty, \\infty\\)\\\\).\n"
    " If the input value is outside the domain of the function an error is returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       maths.natural_logarithm(1.0)\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       maths.natural_logarithm(maths.e())\n"
    "       |> should.equal(Ok(1.0))\n"
    "\n"
    "       maths.natural_logarithm(-1.0)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec natural_logarithm(float()) -> {ok, float()} | {error, nil}.
natural_logarithm(X) ->
    case X > +0.0 of
        true ->
            {ok, math:log(X)};

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 1524).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The base-2 logarithm function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(0, \\infty),   \\\\; \\log_{2}{(x)} = y \\in \\(-\\infty, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(0, \\infty\\)\\\\) as input and returns a\n"
    " numeric value \\\\(y\\\\) that lies in the range \\\\(\\(-\\infty, \\infty\\)\\\\).\n"
    " If the input value is outside the domain of the function an error is returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       maths.logarithm_2(1.0)\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       maths.logarithm_2(2.0)\n"
    "       |> should.equal(Ok(1.0))\n"
    "\n"
    "       maths.logarithm_2(-1.0)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec logarithm_2(float()) -> {ok, float()} | {error, nil}.
logarithm_2(X) ->
    case X > +0.0 of
        true ->
            {ok, math:log2(X)};

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 1575).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The base-10 logarithm function:\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(0, \\infty),   \\\\; \\log_{10}{(x)} = y \\in \\(-\\infty, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(0, \\infty\\)\\\\) as input and returns a\n"
    " numeric value \\\\(y\\\\) that lies in the range \\\\(\\(-\\infty, \\infty\\)\\\\).\n"
    " If the input value is outside the domain of the function an error is returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       maths.logarithm_10(1.0)\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       maths.logarithm_10(10.0)\n"
    "       |> should.equal(Ok(1.0))\n"
    "\n"
    "       maths.logarithm_10(-1.0)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec logarithm_10(float()) -> {ok, float()} | {error, nil}.
logarithm_10(X) ->
    case X > +0.0 of
        true ->
            {ok, math:log10(X)};

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 1469).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The base \\\\(b\\\\) logarithm function (computed through the \"change of base\" formula):\n"
    "\n"
    " \\\\[\n"
    " \\forall x \\in \\(0, \\infty\\) \\textnormal{ and } b > 1,  \\\\; \\log_{b}{(x)} = y \\in \\(-\\infty, +\\infty\\)\n"
    " \\\\]\n"
    "\n"
    " The function takes a number \\\\(x\\\\) in its domain \\\\(\\(0, \\infty\\)\\\\) and a base \\\\(b > 1\\\\)\n"
    " as input and returns a numeric value \\\\(y\\\\) that lies in the range \\\\(\\(-\\infty, \\infty\\)\\\\).\n"
    " If the input value is outside the domain of the function an error is returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       maths.logarithm(1.0, 10.0)\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       maths.logarithm(maths.e(), maths.e())\n"
    "       |> should.equal(Ok(1.0))\n"
    "\n"
    "       maths.logarithm(-1.0, 2.0)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec logarithm(float(), float()) -> {ok, float()} | {error, nil}.
logarithm(X, Base) ->
    case ((X > +0.0) andalso (Base > +0.0)) andalso (Base /= 1.0) of
        true ->
            _assert_subject = logarithm_10(X),
            {ok, Numerator} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"logarithm"/utf8>>,
                                line => 1475})
            end,
            _assert_subject@1 = logarithm_10(Base),
            {ok, Denominator} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"logarithm"/utf8>>,
                                line => 1476})
            end,
            {ok, case Denominator of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> Numerator / Gleam@denominator
                end};

        _ ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 1624).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The \\\\(n\\\\)'th root function: \\\\(y = \\sqrt[n]{x} = x^{\\frac{1}{n}}\\\\).\n"
    "\n"
    " Note that the function is not defined if the input is negative (\\\\(x < 0\\\\)). An error will be\n"
    " returned as an imaginary number will otherwise have to be returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.nth_root(-1.0, 2)\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.nth_root(1.0, 2)\n"
    "       |> should.equal(Ok(1.0))\n"
    "\n"
    "       maths.nth_root(27.0, 3)\n"
    "       |> should.equal(Ok(3.0))\n"
    "\n"
    "       maths.nth_root(256.0, 4)\n"
    "       |> should.equal(Ok(4.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec nth_root(float(), integer()) -> {ok, float()} | {error, nil}.
nth_root(X, N) ->
    case (X >= +0.0) andalso (N >= 1) of
        true ->
            gleam@float:power(X, case erlang:float(N) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> 1.0 / Gleam@denominator
                end);

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 620).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Convert a value in degrees to a value measured in radians.\n"
    " That is, \\\\(1 \\text{ degrees } = \\frac{\\pi}{180} \\text{ radians }\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.degrees_to_radians(360.)\n"
    "       |> should.equal(2. *. maths.pi())\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec degrees_to_radians(float()) -> float().
degrees_to_radians(X) ->
    (X * math:pi()) / 180.0.

-file("src/gleam_community/maths.gleam", 654).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Convert a value in degrees to a value measured in radians.\n"
    " That is, \\\\(1 \\text{ radians } = \\frac{180}{\\pi} \\text{ degrees }\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.radians_to_degrees(0.0)\n"
    "       |> should.equal(0.0)\n"
    "\n"
    "       maths.radians_to_degrees(2. *. maths.pi())\n"
    "       |> should.equal(360.)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec radians_to_degrees(float()) -> float().
radians_to_degrees(X) ->
    case math:pi() of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> X * 180.0 / Gleam@denominator
    end.

-file("src/gleam_community/maths.gleam", 1648).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The mathematical constant pi: \\\\(\\pi \\approx 3.1415\\dots\\\\)\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec pi() -> float().
pi() ->
    math:pi().

-file("src/gleam_community/maths.gleam", 1670).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The mathematical (circle) constant tau: \\\\(\\tau = 2 \\cdot \\pi \\approx 6.283\\dots\\\\)\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec tau() -> float().
tau() ->
    2.0 * pi().

-file("src/gleam_community/maths.gleam", 1700).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The golden ratio: \\\\(\\phi = \\frac{1 + \\sqrt{5}}{2}\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.golden_ratio()\n"
    "       |> should.equal(1.618033988749895)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec golden_ratio() -> float().
golden_ratio() ->
    _assert_subject = gleam@float:square_root(5.0),
    {ok, Sqrt5} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"golden_ratio"/utf8>>,
                        line => 1705})
    end,
    (1.0 + Sqrt5) / 2.0.

-file("src/gleam_community/maths.gleam", 1739).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Euler's number \\\\(e \\approx 2.71828\\dots\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "\n"
    "       // Test that the constant is approximately equal to 2.7128...\n"
    "       maths.e()\n"
    "       |> maths.is_close(2.7182818284590452353602, 0.0, tolerance)\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec e() -> float().
e() ->
    exponential(1.0).

-file("src/gleam_community/maths.gleam", 1988).
-spec truncate_float(float()) -> float().
truncate_float(X) ->
    erlang:trunc(X).

-file("src/gleam_community/maths.gleam", 1979).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function rounds a float to a specific number of digits (after the decimal place or before\n"
    " if negative). In particular, the input \\\\(x\\\\) is rounded to the nearest integer value (at the\n"
    " specified digit) that is less than or equal to the absolute value of the input \\\\(x\\\\). This\n"
    " rounding behaviour is similar to behaviour of the Gleam stdlib `truncate` function.\n"
    "\n"
    " <details>\n"
    " <summary>Details</summary>\n"
    "\n"
    "   The rounding mode rounds \\\\(12.0654\\\\) to:\n"
    "   - \\\\(12.0\\\\) for 0 digits after the decimal point (`digits = 0`)\n"
    "   - \\\\(12.0\\\\) for 1 digit after the decimal point (`digits = 1`)\n"
    "   - \\\\(12.06\\\\) for 2 digits after the decimal point (`digits = 2`)\n"
    "   - \\\\(12.065\\\\) for 3 digits after the decimal point (`digits = 3`)\n"
    "\n"
    "   It is also possible to specify a negative number of digits. In that case, the negative\n"
    "   number refers to the digits before the decimal point.\n"
    "   - \\\\(10.0\\\\) for 1 digit before the decimal point (`digits = -1`)\n"
    "   - \\\\(0.0\\\\) for 2 digits before the decimal point (`digits = -2`)\n"
    "   - \\\\(0.0\\\\) for 3 digits before the decimal point (`digits = -3`)\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.round_to_zero(12.0654, 2)\n"
    "       |> should.equal(12.06)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec round_to_zero(float(), integer()) -> float().
round_to_zero(X, P) ->
    _assert_subject = gleam@float:power(10.0, erlang:float(P)),
    {ok, P@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"round_to_zero"/utf8>>,
                        line => 1984})
    end,
    case P@1 of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> truncate_float(X * P@1) / Gleam@denominator
    end.

-file("src/gleam_community/maths.gleam", 2042).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function rounds a float to a specific number of digits (after the decimal place or before\n"
    " if negative). In particular, the input \\\\(x\\\\) is rounded to the nearest integer value (at the\n"
    " specified digit) that is less than or equal to the input \\\\(x\\\\). This rounding behaviour is\n"
    " similar to behaviour of the Gleam stdlib `floor` function.\n"
    "\n"
    " <details>\n"
    " <summary>Details</summary>\n"
    "\n"
    "   The rounding mode rounds \\\\(12.0654\\\\) to:\n"
    "   - \\\\(12.0\\\\) for 0 digits after the decimal point (`digits = 0`)\n"
    "   - \\\\(12.0\\\\) for 1 digits after the decimal point (`digits = 1`)\n"
    "   - \\\\(12.06\\\\) for 2 digits after the decimal point (`digits = 2`)\n"
    "   - \\\\(12.065\\\\) for 3 digits after the decimal point (`digits = 3`)\n"
    "\n"
    "   It is also possible to specify a negative number of digits. In that case, the negative\n"
    "   number refers to the digits before the decimal point.\n"
    "   - \\\\(10.0\\\\) for 1 digit before the decimal point (`digits = -1`)\n"
    "   - \\\\(0.0\\\\) for 2 digits before the decimal point (`digits = -2`)\n"
    "   - \\\\(0.0\\\\) for 3 digits before the decimal point (`digits = -3`)\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.round_down(12.0654, 2)\n"
    "       |> should.equal(12.06)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec round_down(float(), integer()) -> float().
round_down(X, P) ->
    _assert_subject = gleam@float:power(10.0, erlang:float(P)),
    {ok, P@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"round_down"/utf8>>,
                        line => 2047})
    end,
    case P@1 of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> math:floor(X * P@1) / Gleam@denominator
    end.

-file("src/gleam_community/maths.gleam", 2101).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function rounds a float to a specific number of digits (after the decimal place or before\n"
    " if negative). In particular, the input \\\\(x\\\\) is rounded to the nearest integer value (at the\n"
    " specified digit) that is larger than or equal to the input \\\\(x\\\\). This rounding behaviour is\n"
    " similar to behaviour of the Gleam stdlib `ceiling` function.\n"
    "\n"
    " <details>\n"
    " <summary>Details</summary>\n"
    "\n"
    "   The rounding mode rounds \\\\(12.0654\\\\) to:\n"
    "   - \\\\(13.0\\\\) for 0 digits after the decimal point (`digits = 0`)\n"
    "   - \\\\(12.1\\\\) for 1 digit after the decimal point (`digits = 1`)\n"
    "   - \\\\(12.07\\\\) for 2 digits after the decimal point (`digits = 2`)\n"
    "   - \\\\(12.066\\\\) for 3 digits after the decimal point (`digits = 3`)\n"
    "\n"
    "   It is also possible to specify a negative number of digits. In that case, the negative\n"
    "   number refers to the digits before the decimal point.\n"
    "   - \\\\(20.0\\\\) for 1 digit places before the decimal point (`digit = -1`)\n"
    "   - \\\\(100.0\\\\) for 2 digits before the decimal point (`digits = -2`)\n"
    "   - \\\\(1000.0\\\\) for 3 digits before the decimal point (`digits = -3`)\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.round_up(12.0654, 2)\n"
    "       |> should.equal(12.07)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec round_up(float(), integer()) -> float().
round_up(X, P) ->
    _assert_subject = gleam@float:power(10.0, erlang:float(P)),
    {ok, P@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"round_up"/utf8>>,
                        line => 2106})
    end,
    case P@1 of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> math:ceil(X * P@1) / Gleam@denominator
    end.

-file("src/gleam_community/maths.gleam", 2150).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The absolute difference:\n"
    "\n"
    " \\\\[\n"
    "  \\forall x, y \\in \\mathbb{R}, \\\\; |x - y|  \\in \\mathbb{R}_{+}.\n"
    " \\\\]\n"
    "\n"
    " The function takes two inputs \\\\(x\\\\) and \\\\(y\\\\) and returns a positive float\n"
    " value which is the absolute difference of the inputs.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.absolute_difference(-10.0, 10.0)\n"
    "       |> should.equal(20.0)\n"
    "\n"
    "       maths.absolute_difference(0.0, -2.0)\n"
    "       |> should.equal(2.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec absolute_difference(float(), float()) -> float().
absolute_difference(A, B) ->
    gleam@float:absolute_value(A - B).

-file("src/gleam_community/maths.gleam", 2190).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The absolute difference:\n"
    "\n"
    " \\\\[\n"
    "  \\forall x, y \\in \\mathbb{Z}, \\\\; |x - y|  \\in \\mathbb{Z}_{+}.\n"
    " \\\\]\n"
    "\n"
    " The function takes two inputs \\\\(x\\\\) and \\\\(y\\\\) and returns a positive integer\n"
    " value which is the absolute difference of the inputs.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.absolute_difference(-10, 10)\n"
    "       |> should.equal(20)\n"
    "\n"
    "       maths.absolute_difference(0, -2)\n"
    "       |> should.equal(2)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec int_absolute_difference(integer(), integer()) -> integer().
int_absolute_difference(A, B) ->
    gleam@int:absolute_value(A - B).

-file("src/gleam_community/maths.gleam", 2215).
-spec do_sign(float()) -> float().
do_sign(X) ->
    case X of
        _ when X < +0.0 ->
            -1.0;

        _ when X > +0.0 ->
            1.0;

        _ ->
            +0.0
    end.

-file("src/gleam_community/maths.gleam", 2210).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function takes an input \\\\(x \\in \\mathbb{R}\\\\) and returns the sign of\n"
    " the input, indicating whether it is positive (+1.0), negative (-1.0), or\n"
    " zero (0.0).\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec sign(float()) -> float().
sign(X) ->
    do_sign(X).

-file("src/gleam_community/maths.gleam", 1789).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function rounds a float to a specific number of digits (after the decimal place or before\n"
    " if negative). In particular, the input \\\\(x\\\\) is rounded to the nearest integer value (at the\n"
    " specified digit) with ties (fractional values of 0.5) being rounded to the nearest even\n"
    " integer.\n"
    "\n"
    " <details>\n"
    " <summary>Details</summary>\n"
    "\n"
    "   The rounding mode rounds \\\\(12.0654\\\\) to:\n"
    "   - \\\\(12.0\\\\) for 0 digits after the decimal point (`digits = 0`)\n"
    "   - \\\\(12.1\\\\) for 1 digit after the decimal point (`digits = 1`)\n"
    "   - \\\\(12.07\\\\) for 2 digits after the decimal point (`digits = 2`)\n"
    "   - \\\\(12.065\\\\) for 3 digits after the decimal point (`digits = 3`)\n"
    "\n"
    "   It is also possible to specify a negative number of digits. In that case, the negative\n"
    "   number refers to the digits before the decimal point.\n"
    "   - \\\\(10.0\\\\) for 1 digit before the decimal point (`digits = -1`)\n"
    "   - \\\\(0.0\\\\) for 2 digits before the decimal point (`digits = -2`)\n"
    "   - \\\\(0.0\\\\) for 3 digits before the decimal point (`digits = -3`)\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.round_to_nearest(12.0654, 2)\n"
    "       |> should.equal(12.07)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec round_to_nearest(float(), integer()) -> float().
round_to_nearest(X, P) ->
    _assert_subject = gleam@float:power(10.0, erlang:float(P)),
    {ok, P@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"round_to_nearest"/utf8>>,
                        line => 1794})
    end,
    Xabs = gleam@float:absolute_value(X) * P@1,
    Xabs_truncated = truncate_float(Xabs),
    Remainder = Xabs - Xabs_truncated,
    case Remainder of
        _ when Remainder > 0.5 ->
            case P@1 of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> sign(X) * truncate_float(Xabs + 1.0) / Gleam@denominator
            end;

        _ when Remainder =:= 0.5 ->
            Is_even = erlang:trunc(Xabs) rem 2,
            case Is_even =:= 0 of
                true ->
                    case P@1 of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator@1 -> sign(X) * Xabs_truncated / Gleam@denominator@1
                    end;

                false ->
                    case P@1 of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator@2 -> sign(X) * truncate_float(
                            Xabs + 1.0
                        )
                        / Gleam@denominator@2
                    end
            end;

        _ ->
            case P@1 of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@3 -> sign(X) * Xabs_truncated / Gleam@denominator@3
            end
    end.

-file("src/gleam_community/maths.gleam", 1857).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function rounds a float to a specific number of digits (after the decimal place or before\n"
    " if negative). In particular, the input \\\\(x\\\\) is rounded to the nearest integer value (at the\n"
    " specified digit) with ties (fractional values of 0.5) being rounded away from zero (C/C++\n"
    " rounding behaviour).\n"
    "\n"
    " <details>\n"
    " <summary>Details</summary>\n"
    "\n"
    "   The rounding mode rounds \\\\(12.0654\\\\) to:\n"
    "   - \\\\(12.0\\\\) for 0 digits after the decimal point (`digits = 0`)\n"
    "   - \\\\(12.1\\\\) for 1 digit after the decimal point (`digits = 1`)\n"
    "   - \\\\(12.07\\\\) for 2 digits after the decimal point (`digits = 2`)\n"
    "   - \\\\(12.065\\\\) for 3 digits after the decimal point (`digits = 3`)\n"
    "\n"
    "   It is also possible to specify a negative number of digits. In that case, the negative\n"
    "   number refers to the digits before the decimal point.\n"
    "   - \\\\(10.0\\\\) for 1 digit before the decimal point (`digits = -1`)\n"
    "   - \\\\(0.0\\\\) for 2 digits before the decimal point (`digits = -2`)\n"
    "   - \\\\(0.0\\\\) for 3 digits before the decimal point (`digits = -3`)\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.round_ties_away(12.0654, 2)\n"
    "       |> should.equal(12.07)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec round_ties_away(float(), integer()) -> float().
round_ties_away(X, P) ->
    _assert_subject = gleam@float:power(10.0, erlang:float(P)),
    {ok, P@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"round_ties_away"/utf8>>,
                        line => 1862})
    end,
    Xabs = gleam@float:absolute_value(X) * P@1,
    Remainder = Xabs - truncate_float(Xabs),
    case Remainder of
        _ when Remainder >= 0.5 ->
            case P@1 of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> sign(X) * truncate_float(Xabs + 1.0) / Gleam@denominator
            end;

        _ ->
            case P@1 of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> sign(X) * truncate_float(Xabs) / Gleam@denominator@1
            end
    end.

-file("src/gleam_community/maths.gleam", 1917).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function rounds a float to a specific number of digits (after the decimal place or before\n"
    " if negative). In particular, the input \\\\(x\\\\) is rounded to the nearest integer value (at the\n"
    " specified digit) with ties (fractional values of 0.5) being rounded towards \\\\(+\\infty\\\\)\n"
    " (Java/JavaScript rounding behaviour).\n"
    "\n"
    " <details>\n"
    " <summary>Details</summary>\n"
    "\n"
    "   The rounding mode rounds \\\\(12.0654\\\\) to:\n"
    "   - \\\\(12.0\\\\) for 0 digits after the decimal point (`digits = 0`)\n"
    "   - \\\\(12.1\\\\) for 1 digits after the decimal point (`digits = 1`)\n"
    "   - \\\\(12.07\\\\) for 2 digits after the decimal point (`digits = 2`)\n"
    "   - \\\\(12.065\\\\) for 3 digits after the decimal point (`digits = 3`)\n"
    "\n"
    "   It is also possible to specify a negative number of digits. In that case, the negative\n"
    "    number refers to the digits before the decimal point.\n"
    "   - \\\\(10.0\\\\) for 1 digit before the decimal point (`digits = -1`)\n"
    "   - \\\\(0.0\\\\) for 2 digits before the decimal point (`digits = -2`)\n"
    "   - \\\\(0.0\\\\) for 3 digits before the decimal point (`digits = -3`)\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.round_ties_up(12.0654, 2)\n"
    "       |> should.equal(12.07)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec round_ties_up(float(), integer()) -> float().
round_ties_up(X, P) ->
    _assert_subject = gleam@float:power(10.0, erlang:float(P)),
    {ok, P@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"round_ties_up"/utf8>>,
                        line => 1922})
    end,
    Xabs = gleam@float:absolute_value(X) * P@1,
    Xabs_truncated = truncate_float(Xabs),
    Remainder = Xabs - Xabs_truncated,
    case Remainder of
        _ when (Remainder >= 0.5) andalso (X >= +0.0) ->
            case P@1 of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> sign(X) * truncate_float(Xabs + 1.0) / Gleam@denominator
            end;

        _ ->
            case P@1 of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> sign(X) * Xabs_truncated / Gleam@denominator@1
            end
    end.

-file("src/gleam_community/maths.gleam", 2244).
-spec do_int_sign(integer()) -> integer().
do_int_sign(X) ->
    case X of
        _ when X < 0 ->
            -1;

        _ when X > 0 ->
            1;

        _ ->
            0
    end.

-file("src/gleam_community/maths.gleam", 2239).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function takes an input \\\\(x \\in \\mathbb{Z}\\\\) and returns the sign of\n"
    " the input, indicating whether it is positive (+1), negative (-1), or zero\n"
    " (0).\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec int_sign(integer()) -> integer().
int_sign(X) ->
    do_int_sign(X).

-file("src/gleam_community/maths.gleam", 2318).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function flips the sign of a given input value \\\\(x \\in \\mathbb{R}\\\\).\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec flip_sign(float()) -> float().
flip_sign(X) ->
    -1.0 * X.

-file("src/gleam_community/maths.gleam", 2267).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function takes two arguments \\\\(x, y \\in \\mathbb{R}\\\\) and returns \\\\(x\\\\)\n"
    " such that it has the same sign as \\\\(y\\\\).\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec copy_sign(float(), float()) -> float().
copy_sign(X, Y) ->
    case sign(X) =:= sign(Y) of
        true ->
            X;

        false ->
            flip_sign(X)
    end.

-file("src/gleam_community/maths.gleam", 2336).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function flips the sign of a given input value \\\\(x \\in \\mathbb{Z}\\\\).\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec int_flip_sign(integer()) -> integer().
int_flip_sign(X) ->
    -1 * X.

-file("src/gleam_community/maths.gleam", 2293).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function takes two arguments \\\\(x, y \\in \\mathbb{Z}\\\\) and returns \\\\(x\\\\)\n"
    " such that it has the same sign as \\\\(y\\\\).\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec int_copy_sign(integer(), integer()) -> integer().
int_copy_sign(X, Y) ->
    case int_sign(X) =:= int_sign(Y) of
        true ->
            X;

        false ->
            int_flip_sign(X)
    end.

-file("src/gleam_community/maths.gleam", 2373).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The minmax function takes two arguments \\\\(x, y\\\\) along with a function\n"
    " for comparing \\\\(x, y\\\\). The function returns a tuple with the smallest\n"
    " value first and largest second.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleam/float\n"
    "     import gleam/int\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.minmax(2.0, 1.5, float.compare)\n"
    "       |> should.equal(#(1.5, 2.0))\n"
    "\n"
    "       maths.minmax(1, 2, int.compare)\n"
    "       |> should.equal(#(1, 2))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec minmax(GFS, GFS, fun((GFS, GFS) -> gleam@order:order())) -> {GFS, GFS}.
minmax(X, Y, Compare) ->
    case Compare(X, Y) of
        lt ->
            {X, Y};

        eq ->
            {X, Y};

        gt ->
            {Y, X}
    end.

-file("src/gleam_community/maths.gleam", 2412).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Returns the minimum value of a given list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/int\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.list_minimum(int.compare)\n"
    "       |> should.be_error()\n"
    "\n"
    "       [4, 4, 3, 2, 1]\n"
    "       |> maths.list_minimum(int.compare)\n"
    "       |> should.equal(Ok(1))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec list_minimum(list(GFU), fun((GFU, GFU) -> gleam@order:order())) -> {ok,
        GFU} |
    {error, nil}.
list_minimum(Arr, Compare) ->
    case Arr of
        [] ->
            {error, nil};

        [X | Rest] ->
            {ok,
                gleam@list:fold(
                    Rest,
                    X,
                    fun(Acc, Element) -> case Compare(Element, Acc) of
                            lt ->
                                Element;

                            _ ->
                                Acc
                        end end
                )}
    end.

-file("src/gleam_community/maths.gleam", 2462).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Returns the maximum value of a given list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/float\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.list_maximum(float.compare)\n"
    "       |> should.be_error()\n"
    "\n"
    "       [4.0, 4.0, 3.0, 2.0, 1.0]\n"
    "       |> maths.list_maximum(float.compare)\n"
    "       |> should.equal(Ok(4.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec list_maximum(list(GFY), fun((GFY, GFY) -> gleam@order:order())) -> {ok,
        GFY} |
    {error, nil}.
list_maximum(Arr, Compare) ->
    case Arr of
        [] ->
            {error, nil};

        [X | Rest] ->
            {ok,
                gleam@list:fold(
                    Rest,
                    X,
                    fun(Acc, Element) -> case Compare(Acc, Element) of
                            lt ->
                                Element;

                            _ ->
                                Acc
                        end end
                )}
    end.

-file("src/gleam_community/maths.gleam", 2518).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Returns the indices of the minimum values in a given list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/float\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.arg_minimum(float.compare)\n"
    "       |> should.be_error()\n"
    "\n"
    "       [4.0, 4.0, 3.0, 2.0, 1.0]\n"
    "       |> maths.arg_minimum(float.compare)\n"
    "       |> should.equal(Ok([4]))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec arg_minimum(list(GGC), fun((GGC, GGC) -> gleam@order:order())) -> {ok,
        list(integer())} |
    {error, nil}.
arg_minimum(Arr, Compare) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            _assert_subject = list_minimum(Arr, Compare),
            {ok, Min} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"arg_minimum"/utf8>>,
                                line => 2526})
            end,
            {ok,
                begin
                    _pipe = gleam@list:index_map(
                        Arr,
                        fun(Element, Index) -> case Compare(Element, Min) of
                                eq ->
                                    Index;

                                _ ->
                                    -1
                            end end
                    ),
                    gleam@list:filter(_pipe, fun(Index@1) -> case Index@1 of
                                -1 ->
                                    false;

                                _ ->
                                    true
                            end end)
                end}
    end.

-file("src/gleam_community/maths.gleam", 2583).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Returns the indices of the maximum values in a given list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/float\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.arg_maximum(float.compare)\n"
    "       |> should.be_error()\n"
    "\n"
    "       [4.0, 4.0, 3.0, 2.0, 1.0]\n"
    "       |> maths.arg_maximum(float.compare)\n"
    "       |> should.equal(Ok([0, 1]))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec arg_maximum(list(GGH), fun((GGH, GGH) -> gleam@order:order())) -> {ok,
        list(integer())} |
    {error, nil}.
arg_maximum(Arr, Compare) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            _assert_subject = list_maximum(Arr, Compare),
            {ok, Max} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"arg_maximum"/utf8>>,
                                line => 2591})
            end,
            {ok,
                begin
                    _pipe = gleam@list:index_map(
                        Arr,
                        fun(Element, Index) -> case Compare(Element, Max) of
                                eq ->
                                    Index;

                                _ ->
                                    -1
                            end end
                    ),
                    gleam@list:filter(_pipe, fun(Index@1) -> case Index@1 of
                                -1 ->
                                    false;

                                _ ->
                                    true
                            end end)
                end}
    end.

-file("src/gleam_community/maths.gleam", 2648).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Returns a tuple consisting of the minimum and maximum values of a given list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/float\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.extrema(float.compare)\n"
    "       |> should.be_error()\n"
    "\n"
    "       [4.0, 4.0, 3.0, 2.0, 1.0]\n"
    "       |> maths.extrema(float.compare)\n"
    "       |> should.equal(Ok(#(1.0, 4.0)))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec extrema(list(GGM), fun((GGM, GGM) -> gleam@order:order())) -> {ok,
        {GGM, GGM}} |
    {error, nil}.
extrema(Arr, Compare) ->
    case Arr of
        [] ->
            {error, nil};

        [X | Rest] ->
            {ok,
                gleam@list:fold(
                    Rest,
                    {X, X},
                    fun(Acc, Element) ->
                        First = erlang:element(1, Acc),
                        Second = erlang:element(2, Acc),
                        case {Compare(Element, First), Compare(Second, Element)} of
                            {lt, lt} ->
                                {Element, Element};

                            {lt, _} ->
                                {Element, Second};

                            {_, lt} ->
                                {First, Element};

                            {_, _} ->
                                {First, Second}
                        end
                    end
                )}
    end.

-file("src/gleam_community/maths.gleam", 2796).
-spec do_combination(integer(), integer(), integer(), integer()) -> integer().
do_combination(N, K, Acc, Element) ->
    case Element > K of
        true ->
            Acc;

        false ->
            do_combination(N, K, case Element of
                    0 -> 0;
                    Gleam@denominator -> Acc * ((N + 1) - Element) div Gleam@denominator
                end, Element + 1)
    end.

-file("src/gleam_community/maths.gleam", 2780).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A combinatorial function for computing the number of \\\\(k\\\\)-combinations of \\\\(n\\\\) elements\n"
    " without repetitions:\n"
    "\n"
    " \\\\[\n"
    " C(n, k) = \\binom{n}{k} = \\frac{n!}{k! (n-k)!}\n"
    " \\\\]\n"
    "\n"
    " Also known as \"\\\\(n\\\\) choose \\\\(k\\\\)\" or the binomial coefficient.\n"
    "\n"
    "\n"
    " <details>\n"
    " <summary>Details</summary>\n"
    "\n"
    " A \\\\(k\\\\)-combination without repetition is a sequence of \\\\(k\\\\) elements selected from\n"
    " \\\\(n\\\\) elements where the order of selection does not matter and elements are not allowed to\n"
    " repeat. For example, consider selecting  2 elements from a list of 3 elements:\n"
    " `[\"A\", \"B\", \"C\"]`. In this case, possible selections are:\n"
    "   - `[\"A\", \"B\"]`\n"
    "   - `[\"A\", \"C\"]`\n"
    "   - `[\"B\", \"C\"]`\n"
    "\n"
    " </details>\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.combination(-1, 1)\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.combination(4, 0)\n"
    "       |> should.equal(Ok(1))\n"
    "\n"
    "       maths.combination(4, 4)\n"
    "       |> should.equal(Ok(1))\n"
    "\n"
    "       maths.combination(13, 5)\n"
    "       |> should.equal(Ok(1287))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec combination(integer(), integer()) -> {ok, integer()} | {error, nil}.
combination(N, K) ->
    case {N, K} of
        {_, _} when N < 0 ->
            {error, nil};

        {_, _} when K < 0 ->
            {error, nil};

        {_, _} when K > N ->
            {ok, 0};

        {_, _} when (K =:= 0) orelse (K =:= N) ->
            {ok, 1};

        {_, _} ->
            Min = case K < (N - K) of
                true ->
                    K;

                false ->
                    N - K
            end,
            {ok, do_combination(N, Min, 1, 1)}
    end.

-file("src/gleam_community/maths.gleam", 2721).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A combinatorial function for computing the number of \\\\(k\\\\)-combinations of \\\\(n\\\\) elements\n"
    " with repetitions:\n"
    "\n"
    " \\\\[\n"
    " C^*(n, k) = \\binom{n + k - 1}{k} = \\frac{(n + k - 1)!}{k! (n - 1)!}\n"
    " \\\\]\n"
    "\n"
    " Also known as the \"stars and bars\" problem in maths. Furthermore, the implementation uses an\n"
    " efficient iterative multiplicative formula for computing the result.\n"
    "\n"
    " <details>\n"
    " <summary>Details</summary>\n"
    "\n"
    " A \\\\(k\\\\)-combination with repetitions is a sequence of \\\\(k\\\\) elements selected from\n"
    " \\\\(n\\\\) elements where the order of selection does not matter and elements are allowed to\n"
    " repeat. For example, consider selecting 2 elements from a list of 3 elements: `[\"A\", \"B\", \"C\"]`.\n"
    " In this case, possible selections are:\n"
    "   - `[\"A\", \"A\"], [\"A\", \"B\"], [\"A\", \"C\"]`\n"
    "   - `[\"B\", \"B\"], [\"B\", \"C\"], [\"C\", \"C\"]`\n"
    "\n"
    " </details>\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.combination_with_repetitions(-1, 1)\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.combination_with_repetitions(2, 3)\n"
    "       |> should.equal(Ok(4))\n"
    "\n"
    "       maths.combination_with_repetitions(13, 5)\n"
    "       |> should.equal(Ok(6188))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec combination_with_repetitions(integer(), integer()) -> {ok, integer()} |
    {error, nil}.
combination_with_repetitions(N, K) ->
    combination((N + K) - 1, K).

-file("src/gleam_community/maths.gleam", 2844).
-spec do_factorial(integer(), integer()) -> integer().
do_factorial(N, Acc) ->
    case N of
        0 ->
            Acc;

        1 ->
            Acc;

        _ ->
            do_factorial(N - 1, Acc * N)
    end.

-file("src/gleam_community/maths.gleam", 2837).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A combinatorial function for computing the total number of combinations of \\\\(n\\\\)\n"
    " elements, that is \\\\(n!\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.factorial(-1)\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.factorial(0)\n"
    "       |> should.equal(Ok(1))\n"
    "\n"
    "       maths.factorial(3)\n"
    "       |> should.equal(Ok(6))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec factorial(integer()) -> {ok, integer()} | {error, nil}.
factorial(N) ->
    case N of
        _ when N < 0 ->
            {error, nil};

        _ ->
            {ok, do_factorial(N, 1)}
    end.

-file("src/gleam_community/maths.gleam", 2917).
-spec do_permutation(integer(), integer(), integer()) -> integer().
do_permutation(N, K, Acc) ->
    case K of
        0 ->
            Acc;

        _ ->
            do_permutation(N - 1, K - 1, Acc * N)
    end.

-file("src/gleam_community/maths.gleam", 2907).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A combinatorial function for computing the number of \\\\(k\\\\)-permutations without\n"
    " repetitions:\n"
    "\n"
    " \\\\[\n"
    " P(n, k) = \\binom{n}{k} \\cdot k! = \\frac{n!}{(n - k)!}\n"
    " \\\\]\n"
    "\n"
    " The implementation uses an efficient iterative multiplicative formula for computing the result.\n"
    "\n"
    " <details>\n"
    " <summary>Details</summary>\n"
    "\n"
    " A \\\\(k\\\\)-permutation without repetitions is a sequence of \\\\(k\\\\) elements selected from \\\n"
    " \\\\(n\\\\) elements where the order of selection matters and elements are not allowed to repeat.\n"
    " For example, consider selecting 2 elements from a list of 3 elements: `[\"A\", \"B\", \"C\"]`. In\n"
    " this case, possible selections are:\n"
    "   - `[\"A\", \"B\"], [\"B\", \"A\"]`\n"
    "   - `[\"A\", \"C\"], [\"C\", \"A\"]`\n"
    "   - `[\"B\", \"C\"], [\"C\", \"B\"]`\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.permutation(-1, 1)\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.permutation(4, 0)\n"
    "       |> should.equal(Ok(1))\n"
    "\n"
    "       maths.permutation(4, 2)\n"
    "       |> should.equal(Ok(12))\n"
    "\n"
    "       maths.permutation(13, 5)\n"
    "       |> should.equal(Ok(154_440))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec permutation(integer(), integer()) -> {ok, integer()} | {error, nil}.
permutation(N, K) ->
    case {N, K} of
        {_, _} when N < 0 ->
            {error, nil};

        {_, _} when K < 0 ->
            {error, nil};

        {_, _} when K > N ->
            {ok, 0};

        {_, _} when K =:= 0 ->
            {ok, 1};

        {_, _} ->
            {ok, do_permutation(N, K, 1)}
    end.

-file("src/gleam_community/maths.gleam", 2976).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A combinatorial function for computing the number of \\\\(k\\\\)-permutations with repetitions:\n"
    "\n"
    " \\\\[\n"
    " P^*(n, k) = n^k\n"
    " \\\\]\n"
    "\n"
    " <details>\n"
    " <summary>Details</summary>\n"
    "\n"
    " A \\\\(k\\\\)-permutation with repetitions is a sequence of \\\\(k\\\\) elements selected from \\\\(n\\\\)\n"
    " elements where the order of selection matters and elements are allowed to repeat. For example,\n"
    " consider selecting 2 elements from a list of 3 elements: `[\"A\", \"B\", \"C\"]`. In this case,\n"
    " possible selections are:\n"
    "   - `[\"A\", \"A\"], [\"A\", \"B\"], [\"A\", \"C\"]`\n"
    "   - `[\"B\", \"A\"], [\"B\", \"B\"], [\"B\", \"C\"]`\n"
    "   - `[\"C\", \"A\"], [\"C\", \"B\"], [\"C\", \"C\"]`\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.permutation_with_repetitions(1, -1)\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.permutation_with_repetitions(2, 3)\n"
    "       |> should.equal(Ok(8))\n"
    "\n"
    "       maths.permutation_with_repetitions(4, 4)\n"
    "       |> should.equal(Ok(256))\n"
    "\n"
    "       maths.permutation_with_repetitions(6, 3)\n"
    "       |> should.equal(Ok(216))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec permutation_with_repetitions(integer(), integer()) -> {ok, integer()} |
    {error, nil}.
permutation_with_repetitions(N, K) ->
    case {N, K} of
        {_, _} when N < 0 ->
            {error, nil};

        {_, _} when K < 0 ->
            {error, nil};

        {_, _} ->
            N_float = erlang:float(N),
            K_float = erlang:float(K),
            _assert_subject = gleam@float:power(N_float, K_float),
            {ok, Result} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"permutation_with_repetitions"/utf8>>,
                                line => 2987})
            end,
            {ok, erlang:round(Result)}
    end.

-file("src/gleam_community/maths.gleam", 3040).
-spec do_list_combination_without_repetitions(
    gleam@yielder:yielder(GHG),
    integer(),
    list(GHG)
) -> gleam@yielder:yielder(list(GHG)).
do_list_combination_without_repetitions(Arr, K, Prefix) ->
    case K of
        0 ->
            gleam@yielder:single(lists:reverse(Prefix));

        _ ->
            case gleam@yielder:step(Arr) of
                done ->
                    gleam@yielder:empty();

                {next, X, Xs} ->
                    With_x = do_list_combination_without_repetitions(
                        Xs,
                        K - 1,
                        [X | Prefix]
                    ),
                    Without_x = do_list_combination_without_repetitions(
                        Xs,
                        K,
                        Prefix
                    ),
                    gleam@yielder:concat([With_x, Without_x])
            end
    end.

-file("src/gleam_community/maths.gleam", 3026).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Generates all possible combinations of \\\\(k\\\\) elements selected from a given list of size\n"
    " \\\\(n\\\\). The function handles the case without repetitions, that is, repeated elements\n"
    " are not treated as distinct.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/yielder\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // All 2-combinations of [1, 2, 3] without repetition\n"
    "       let assert Ok(combinations) = maths.list_combination([1, 2, 3], 2)\n"
    "\n"
    "       combinations\n"
    "       |> yielder.to_list()\n"
    "       |> should.equal([[1, 2], [1, 3], [2, 3]])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec list_combination(list(GHA), integer()) -> {ok,
        gleam@yielder:yielder(list(GHA))} |
    {error, nil}.
list_combination(Arr, K) ->
    case {K, erlang:length(Arr)} of
        {_, _} when K < 0 ->
            {error, nil};

        {_, Arr_length} when K > Arr_length ->
            {error, nil};

        {_, Arr_length@1} when K =:= Arr_length@1 ->
            {ok, gleam@yielder:single(Arr)};

        {_, _} ->
            {ok,
                do_list_combination_without_repetitions(
                    gleam@yielder:from_list(Arr),
                    K,
                    []
                )}
    end.

-file("src/gleam_community/maths.gleam", 3106).
-spec do_list_combination_with_repetitions(
    gleam@yielder:yielder(GHR),
    integer(),
    list(GHR)
) -> gleam@yielder:yielder(list(GHR)).
do_list_combination_with_repetitions(Arr, K, Prefix) ->
    case K of
        0 ->
            gleam@yielder:single(lists:reverse(Prefix));

        _ ->
            case gleam@yielder:step(Arr) of
                done ->
                    gleam@yielder:empty();

                {next, X, Xs} ->
                    With_x = do_list_combination_with_repetitions(
                        Arr,
                        K - 1,
                        [X | Prefix]
                    ),
                    Without_x = do_list_combination_with_repetitions(
                        Xs,
                        K,
                        Prefix
                    ),
                    gleam@yielder:concat([With_x, Without_x])
            end
    end.

-file("src/gleam_community/maths.gleam", 3094).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Generates all possible combinations of \\\\(k\\\\) elements selected from a given list of size\n"
    " \\\\(n\\\\). The function handles the case when the repetition of elements is allowed, that is,\n"
    " repeated elements are treated as distinct.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/yielder\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // All 2-combinations of [1, 2, 3] with repetition\n"
    "       let assert Ok(combinations) =\n"
    "         maths.list_combination_with_repetitions([1, 2, 3], 2)\n"
    "\n"
    "       combinations\n"
    "       |> yielder.to_list()\n"
    "       |> should.equal([[1, 1], [1, 2], [1, 3], [2, 2], [2, 3], [3, 3]])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec list_combination_with_repetitions(list(GHL), integer()) -> {ok,
        gleam@yielder:yielder(list(GHL))} |
    {error, nil}.
list_combination_with_repetitions(Arr, K) ->
    case K of
        _ when K < 0 ->
            {error, nil};

        _ ->
            {ok,
                do_list_combination_with_repetitions(
                    gleam@yielder:from_list(Arr),
                    K,
                    []
                )}
    end.

-file("src/gleam_community/maths.gleam", 3126).
-spec remove_first_by_index(gleam@yielder:yielder({integer(), GHW}), integer()) -> gleam@yielder:yielder({integer(),
    GHW}).
remove_first_by_index(Arr, Index_to_remove) ->
    gleam@yielder:flat_map(
        Arr,
        fun(Tuple) ->
            {Index, Element} = Tuple,
            case Index =:= Index_to_remove of
                true ->
                    gleam@yielder:empty();

                false ->
                    gleam@yielder:single({Index, Element})
            end
        end
    ).

-file("src/gleam_community/maths.gleam", 3188).
-spec do_list_permutation_without_repetitions(
    gleam@yielder:yielder({integer(), GIF}),
    integer()
) -> gleam@yielder:yielder(list(GIF)).
do_list_permutation_without_repetitions(Arr, K) ->
    case K of
        0 ->
            gleam@yielder:single([]);

        _ ->
            gleam@yielder:flat_map(
                Arr,
                fun(Tuple) ->
                    {Index, Element} = Tuple,
                    Remaining = remove_first_by_index(Arr, Index),
                    Permutations = do_list_permutation_without_repetitions(
                        Remaining,
                        K - 1
                    ),
                    gleam@yielder:map(
                        Permutations,
                        fun(Permutation) -> [Element | Permutation] end
                    )
                end
            )
    end.

-file("src/gleam_community/maths.gleam", 3173).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Generates all possible permutations of \\\\(k\\\\) elements selected from a given list of size\n"
    " \\\\(n\\\\). The function handles the case without repetitions, that is, repeated elements are\n"
    " not treated as distinct.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/yielder\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // All 2-permutations of [1, 2] without repetition\n"
    "       let assert Ok(permutations) =\n"
    "         [1, 2]\n"
    "         |> maths.list_permutation(2)\n"
    "       permutations\n"
    "       |> yielder.to_list()\n"
    "       |> should.equal([[1, 2], [2, 1]])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec list_permutation(list(GHZ), integer()) -> {ok,
        gleam@yielder:yielder(list(GHZ))} |
    {error, nil}.
list_permutation(Arr, K) ->
    case {K, erlang:length(Arr)} of
        {_, _} when K < 0 ->
            {error, nil};

        {_, Arr_length} when K > Arr_length ->
            {error, nil};

        {_, _} ->
            Indexed_arr = gleam@list:index_map(
                Arr,
                fun(Element, Index) -> {Index, Element} end
            ),
            {ok,
                do_list_permutation_without_repetitions(
                    gleam@yielder:from_list(Indexed_arr),
                    K
                )}
    end.

-file("src/gleam_community/maths.gleam", 3254).
-spec do_list_permutation_with_repetitions(
    gleam@yielder:yielder({integer(), GIP}),
    integer()
) -> gleam@yielder:yielder(list(GIP)).
do_list_permutation_with_repetitions(Arr, K) ->
    case K of
        0 ->
            gleam@yielder:single([]);

        _ ->
            gleam@yielder:flat_map(
                Arr,
                fun(Tuple) ->
                    {_, Element} = Tuple,
                    Permutations = do_list_permutation_with_repetitions(
                        Arr,
                        K - 1
                    ),
                    gleam@yielder:map(
                        Permutations,
                        fun(Permutation) -> [Element | Permutation] end
                    )
                end
            )
    end.

-file("src/gleam_community/maths.gleam", 3240).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Generates all possible permutations of \\\\(k\\\\) elements selected from a given list of size\n"
    " \\\\(n\\\\). The function handles the case when the repetition of elements is allowed, that is,\n"
    " repeated elements are treated as distinct.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/yielder\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // All 2-permutations of [1, 2] with repetition\n"
    "       let assert Ok(permutations) =\n"
    "         [1, 2]\n"
    "         |> maths.list_permutation_with_repetitions(2)\n"
    "       permutations\n"
    "       |> yielder.to_list()\n"
    "       |> set.from_list()\n"
    "       |> should.equal(set.from_list([[1, 1], [1, 2], [2, 2], [2, 1]]))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec list_permutation_with_repetitions(list(GIJ), integer()) -> {ok,
        gleam@yielder:yielder(list(GIJ))} |
    {error, nil}.
list_permutation_with_repetitions(Arr, K) ->
    case K of
        _ when K < 0 ->
            {error, nil};

        _ ->
            Indexed_arr = gleam@list:index_map(
                Arr,
                fun(Element, Index) -> {Index, Element} end
            ),
            {ok,
                do_list_permutation_with_repetitions(
                    gleam@yielder:from_list(Indexed_arr),
                    K
                )}
    end.

-file("src/gleam_community/maths.gleam", 3305).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Generate a set containing all combinations of pairs of elements coming from two given sets.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/set\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       set.from_list([])\n"
    "       |> maths.cartesian_product(set.from_list([]))\n"
    "       |> should.equal(set.from_list([]))\n"
    "\n"
    "       set.from_list([1.0, 10.0])\n"
    "       |> maths.cartesian_product(set.from_list([1.0, 2.0]))\n"
    "       |> should.equal(\n"
    "         set.from_list([#(1.0, 1.0), #(1.0, 2.0), #(10.0, 1.0), #(10.0, 2.0)]),\n"
    "       )\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec cartesian_product(gleam@set:set(GIT), gleam@set:set(GIV)) -> gleam@set:set({GIT,
    GIV}).
cartesian_product(Xset, Yset) ->
    gleam@set:fold(
        Xset,
        gleam@set:new(),
        fun(Acc0, Element0) ->
            gleam@set:fold(
                Yset,
                Acc0,
                fun(Acc1, Element1) ->
                    gleam@set:insert(Acc1, {Element0, Element1})
                end
            )
        end
    ).

-file("src/gleam_community/maths.gleam", 3355).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the \\\\(p\\\\)-norm of a list (representing a vector):\n"
    "\n"
    " \\\\[\n"
    " \\left( \\sum_{i=1}^n \\left|x_{i}\\right|^{p} \\right)^{\\frac{1}{p}}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the list and \\\\(x_i\\\\) is the value in\n"
    " the input list indexed by \\\\(i\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       [1.0, 1.0, 1.0]\n"
    "       |> maths.norm(1.0)\n"
    "       |> should.equal(Ok(3.0))\n"
    "\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "       let assert Ok(result) =\n"
    "         [1.0, 2.0, 3.0]\n"
    "         |> maths.norm(2.0)\n"
    "       result\n"
    "       |> maths.is_close(3.7416573867739413, 0.0, tolerance)\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec norm(list(float()), float()) -> {ok, float()} | {error, nil}.
norm(Arr, P) ->
    case Arr of
        [] ->
            {ok, +0.0};

        _ ->
            case P of
                +0.0 ->
                    {ok,
                        gleam@list:fold(
                            Arr,
                            +0.0,
                            fun(Acc, Element) -> case Element of
                                    +0.0 ->
                                        Acc;

                                    _ ->
                                        Acc + 1.0
                                end end
                        )};

                _ when P < +0.0 ->
                    Aggregate = gleam@list:try_fold(
                        Arr,
                        +0.0,
                        fun(Acc@1, Element@1) -> case Element@1 of
                                +0.0 ->
                                    {error, +0.0};

                                _ ->
                                    _assert_subject = gleam@float:power(
                                        gleam@float:absolute_value(Element@1),
                                        P
                                    ),
                                    {ok, Result} = case _assert_subject of
                                        {ok, _} -> _assert_subject;
                                        _assert_fail ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                        value => _assert_fail,
                                                        module => <<"gleam_community/maths"/utf8>>,
                                                        function => <<"norm"/utf8>>,
                                                        line => 3385}
                                                )
                                    end,
                                    {ok, Result + Acc@1}
                            end end
                    ),
                    case Aggregate of
                        {ok, Result@1} ->
                            gleam@float:power(Result@1, case P of
                                    +0.0 -> +0.0;
                                    -0.0 -> -0.0;
                                    Gleam@denominator -> 1.0 / Gleam@denominator
                                end);

                        {error, _} ->
                            {ok, +0.0}
                    end;

                _ ->
                    Aggregate@1 = gleam@list:fold(
                        Arr,
                        +0.0,
                        fun(Acc@2, Element@2) ->
                            _assert_subject@1 = gleam@float:power(
                                gleam@float:absolute_value(Element@2),
                                P
                            ),
                            {ok, Result@2} = case _assert_subject@1 of
                                {ok, _} -> _assert_subject@1;
                                _assert_fail@1 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                value => _assert_fail@1,
                                                module => <<"gleam_community/maths"/utf8>>,
                                                function => <<"norm"/utf8>>,
                                                line => 3402})
                            end,
                            Result@2 + Acc@2
                        end
                    ),
                    gleam@float:power(Aggregate@1, case P of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator@1 -> 1.0 / Gleam@denominator@1
                        end)
            end
    end.

-file("src/gleam_community/maths.gleam", 3456).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the weighted \\\\(p\\\\)-norm of a list (representing a vector):\n"
    "\n"
    " \\\\[\n"
    " \\left( \\sum_{i=1}^n w_{i} \\left|x_{i}\\right|^{p} \\right)^{\\frac{1}{p}}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the list and \\\\(x_i\\\\) is the value in\n"
    " the input list indexed by \\\\(i\\\\), while \\\\(w_i \\in \\mathbb{R}_{+}\\\\) is\n"
    " a corresponding positive weight.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       [#(1.0, 0.5), #(1.0, 0.5), #(1.0, 0.5)]\n"
    "       |> maths.norm_with_weights(1.0)\n"
    "       |> should.equal(Ok(1.5))\n"
    "\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "       let assert Ok(result) =\n"
    "         [#(1.0, 0.5), #(2.0, 0.5), #(3.0, 0.5)]\n"
    "         |> maths.norm_with_weights(2.0)\n"
    "       result\n"
    "       |> maths.is_close(2.6457513110645907, 0.0, tolerance)\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec norm_with_weights(list({float(), float()}), float()) -> {ok, float()} |
    {error, nil}.
norm_with_weights(Arr, P) ->
    case Arr of
        [] ->
            {ok, +0.0};

        _ ->
            Weight_is_invalid = gleam@list:any(
                Arr,
                fun(Tuple) -> erlang:element(2, Tuple) < +0.0 end
            ),
            case Weight_is_invalid of
                true ->
                    {error, nil};

                false ->
                    case P of
                        +0.0 ->
                            {ok,
                                gleam@list:fold(
                                    Arr,
                                    +0.0,
                                    fun(Acc, Tuple@1) -> case Tuple@1 of
                                            {+0.0, _} ->
                                                Acc;

                                            _ ->
                                                Acc + 1.0
                                        end end
                                )};

                        _ when P < +0.0 ->
                            Aggregate = gleam@list:try_fold(
                                Arr,
                                +0.0,
                                fun(Acc@1, Tuple@2) -> case Tuple@2 of
                                        {+0.0, _} ->
                                            {error, +0.0};

                                        {_, +0.0} ->
                                            {error, +0.0};

                                        _ ->
                                            _assert_subject = gleam@float:power(
                                                gleam@float:absolute_value(
                                                    erlang:element(1, Tuple@2)
                                                ),
                                                P
                                            ),
                                            {ok, Result} = case _assert_subject of
                                                {ok, _} -> _assert_subject;
                                                _assert_fail ->
                                                    erlang:error(
                                                            #{gleam_error => let_assert,
                                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                                value => _assert_fail,
                                                                module => <<"gleam_community/maths"/utf8>>,
                                                                function => <<"norm_with_weights"/utf8>>,
                                                                line => 3496}
                                                        )
                                            end,
                                            {ok,
                                                (erlang:element(2, Tuple@2) * Result)
                                                + Acc@1}
                                    end end
                            ),
                            case Aggregate of
                                {ok, Result@1} ->
                                    gleam@float:power(Result@1, case P of
                                            +0.0 -> +0.0;
                                            -0.0 -> -0.0;
                                            Gleam@denominator -> 1.0 / Gleam@denominator
                                        end);

                                {error, _} ->
                                    {ok, +0.0}
                            end;

                        _ ->
                            Aggregate@1 = gleam@list:fold(
                                Arr,
                                +0.0,
                                fun(Acc@2, Tuple@3) ->
                                    _assert_subject@1 = gleam@float:power(
                                        gleam@float:absolute_value(
                                            erlang:element(1, Tuple@3)
                                        ),
                                        P
                                    ),
                                    {ok, Result@2} = case _assert_subject@1 of
                                        {ok, _} -> _assert_subject@1;
                                        _assert_fail@1 ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                        value => _assert_fail@1,
                                                        module => <<"gleam_community/maths"/utf8>>,
                                                        function => <<"norm_with_weights"/utf8>>,
                                                        line => 3513}
                                                )
                                    end,
                                    (erlang:element(2, Tuple@3) * Result@2) + Acc@2
                                end
                            ),
                            gleam@float:power(Aggregate@1, case P of
                                    +0.0 -> +0.0;
                                    -0.0 -> -0.0;
                                    Gleam@denominator@1 -> 1.0 / Gleam@denominator@1
                                end)
                    end
            end
    end.

-file("src/gleam_community/maths.gleam", 3647).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the Minkowski distance between two lists (representing\n"
    " vectors):\n"
    "\n"
    " \\\\[\n"
    " \\left( \\sum_{i=1}^n w_{i} \\left|x_i - y_i \\right|^{p} \\right)^{\\frac{1}{p}}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(p >= 1\\\\) is the order, \\\\(n\\\\) is the length of the two lists\n"
    " and \\\\(x_i, y_i\\\\) are the values in the respective input lists indexed by \\\\(i\\\\).\n"
    "\n"
    " The Minkowski distance is a generalization of the Euclidean distance\n"
    " (\\\\(p=2\\\\)) and the Manhattan distance (\\\\(p = 1\\\\)).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "\n"
    "       let assert Ok(result) =\n"
    "         maths.minkowski_distance([#(1.0, 2.0), #(3.0, 4.0), #(5.0, 6.0)], 4.0)\n"
    "       result\n"
    "       |> maths.is_close(1.3160740129524924, 0.0, tolerance)\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec minkowski_distance(list({float(), float()}), float()) -> {ok, float()} |
    {error, nil}.
minkowski_distance(Arr, P) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            case P < 1.0 of
                true ->
                    {error, nil};

                false ->
                    Differences = gleam@list:map(
                        Arr,
                        fun(Tuple) ->
                            erlang:element(1, Tuple) - erlang:element(2, Tuple)
                        end
                    ),
                    norm(Differences, P)
            end
    end.

-file("src/gleam_community/maths.gleam", 3560).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the Manhattan distance between two lists (representing\n"
    " vectors):\n"
    "\n"
    " \\\\[\n"
    " \\sum_{i=1}^n \\left|x_i - y_i \\right|\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists and \\\\(x_i, y_i\\\\) are the\n"
    " values in the respective input lists indexed by \\\\(i\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.manhattan_distance([#(1.0, 2.0), #(2.0, 3.0)])\n"
    "       |> should.equal(Ok(2.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec manhattan_distance(list({float(), float()})) -> {ok, float()} |
    {error, nil}.
manhattan_distance(Arr) ->
    minkowski_distance(Arr, 1.0).

-file("src/gleam_community/maths.gleam", 3711).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the weighted Minkowski distance between two lists (representing\n"
    " vectors):\n"
    "\n"
    " \\\\[\n"
    " \\left( \\sum_{i=1}^n w_{i} \\left|x_i - y_i \\right|^{p} \\right)^{\\frac{1}{p}}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(p >= 1\\\\) is the order, \\\\(n\\\\) is the length of the two lists\n"
    " and \\\\(x_i, y_i\\\\) are the values in the respective input lists indexed by \\\\(i\\\\).\n"
    " The \\\\(w_i \\in \\mathbb{R}_{+}\\\\) are corresponding positive weights.\n"
    "\n"
    " The Minkowski distance is a generalization of the Euclidean distance\n"
    " (\\\\(p=2\\\\)) and the Manhattan distance (\\\\(p = 1\\\\)).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "\n"
    "       let assert Ok(result) =\n"
    "         maths.minkowski_distance_with_weights(\n"
    "           [#(1.0, 2.0, 0.5), #(3.0, 4.0, 1.0), #(5.0, 6.0, 1.0)],\n"
    "           4.0,\n"
    "         )\n"
    "       result\n"
    "       |> maths.is_close(1.2574334296829355, 0.0, tolerance)\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec minkowski_distance_with_weights(
    list({float(), float(), float()}),
    float()
) -> {ok, float()} | {error, nil}.
minkowski_distance_with_weights(Arr, P) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            Weight_is_negative = gleam@list:any(
                Arr,
                fun(Tuple) -> erlang:element(3, Tuple) < +0.0 end
            ),
            case {P < 1.0, Weight_is_negative} of
                {false, false} ->
                    Differences = gleam@list:map(
                        Arr,
                        fun(Tuple@1) ->
                            {erlang:element(1, Tuple@1) - erlang:element(
                                    2,
                                    Tuple@1
                                ),
                                erlang:element(3, Tuple@1)}
                        end
                    ),
                    norm_with_weights(Differences, P);

                {_, _} ->
                    {error, nil}
            end
    end.

-file("src/gleam_community/maths.gleam", 3599).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the weighted Manhattan distance between two lists (representing\n"
    " vectors):\n"
    "\n"
    " \\\\[\n"
    " \\sum_{i=1}^n w_{i} \\left|x_i - y_i \\right|\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists and \\\\(x_i, y_i\\\\) are the\n"
    " values in the respective input lists indexed by \\\\(i\\\\), while the\n"
    " \\\\(w_i \\in \\mathbb{R}_{+}\\\\) are corresponding positive weights.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.manhattan_distance_with_weights([#(1.0, 2.0, 0.5), #(2.0, 3.0, 1.0)])\n"
    "       |> should.equal(Ok(1.5))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec manhattan_distance_with_weights(list({float(), float(), float()})) -> {ok,
        float()} |
    {error, nil}.
manhattan_distance_with_weights(Arr) ->
    minkowski_distance_with_weights(Arr, 1.0).

-file("src/gleam_community/maths.gleam", 3770).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the Euclidean distance between two lists (representing\n"
    " vectors):\n"
    "\n"
    " \\\\[\n"
    " \\left( \\sum_{i=1}^n \\left|x_i - y_i \\right|^{2} \\right)^{\\frac{1}{2}}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists and \\\\(x_i, y_i\\\\) are the\n"
    " values in the respective input lists indexed by \\\\(i\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "\n"
    "       let assert Ok(result) = maths.euclidean_distance([#(1.0, 2.0), #(3.0, 4.0)])\n"
    "       result\n"
    "       |> maths.is_close(1.4142135623730951, 0.0, tolerance)\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec euclidean_distance(list({float(), float()})) -> {ok, float()} |
    {error, nil}.
euclidean_distance(Arr) ->
    minkowski_distance(Arr, 2.0).

-file("src/gleam_community/maths.gleam", 3814).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the weighted Euclidean distance between two lists (representing\n"
    " vectors):\n"
    "\n"
    " \\\\[\n"
    " \\left( \\sum_{i=1}^n w_{i} \\left|x_i - y_i \\right|^{2} \\right)^{\\frac{1}{2}}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists and \\\\(x_i, y_i\\\\) are the\n"
    " values in the respective input lists indexed by \\\\(i\\\\), while the\n"
    " \\\\(w_i \\in \\mathbb{R}_{+}\\\\) are corresponding positive weights.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "\n"
    "       let assert Ok(result) =\n"
    "         maths.euclidean_distance_with_weights([#(1.0, 2.0, 0.5), #(3.0, 4.0, 1.0)])\n"
    "       result\n"
    "       |> maths.is_close(1.224744871391589, 0.0, tolerance)\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec euclidean_distance_with_weights(list({float(), float(), float()})) -> {ok,
        float()} |
    {error, nil}.
euclidean_distance_with_weights(Arr) ->
    minkowski_distance_with_weights(Arr, 2.0).

-file("src/gleam_community/maths.gleam", 3853).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the Chebyshev distance between two lists (representing vectors):\n"
    "\n"
    " \\\\[\n"
    " \\text{max}_{i=1}^n \\left|x_i - y_i \\right|\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists and \\\\(x_i, y_i\\\\) are the\n"
    " values in the respective input lists indexed by \\\\(i\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.chebyshev_distance([#(-5.0, -1.0), #(-10.0, -12.0), #(-3.0, -3.0)])\n"
    "       |> should.equal(Ok(4.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec chebyshev_distance(list({float(), float()})) -> {ok, float()} |
    {error, nil}.
chebyshev_distance(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            _pipe = gleam@list:map(
                Arr,
                fun(Tuple) ->
                    gleam@float:absolute_value(
                        (erlang:element(1, Tuple) - erlang:element(2, Tuple))
                    )
                end
            ),
            list_maximum(_pipe, fun gleam@float:compare/2)
    end.

-file("src/gleam_community/maths.gleam", 3901).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the weighted Chebyshev distance between two lists (representing vectors):\n"
    "\n"
    " \\\\[\n"
    " \\text{max}_{i=1}^n w_i \\left|x_i - y_i \\right|\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists and \\\\(x_i, y_i\\\\) are the\n"
    " values in the respective input lists indexed by \\\\(i\\\\), while the\n"
    " \\\\(w_i \\in \\mathbb{R}_{+}\\\\) are corresponding positive weights.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.chebyshev_distance_with_weights([\n"
    "         #(-5.0, -1.0, 0.5),\n"
    "         #(-10.0, -12.0, 1.0),\n"
    "         #(-3.0, -3.0, 1.0),\n"
    "       ])\n"
    "       |> should.equal(Ok(2.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec chebyshev_distance_with_weights(list({float(), float(), float()})) -> {ok,
        float()} |
    {error, nil}.
chebyshev_distance_with_weights(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            Weight_is_negative = gleam@list:any(
                Arr,
                fun(Tuple) -> erlang:element(3, Tuple) < +0.0 end
            ),
            case Weight_is_negative of
                true ->
                    {error, nil};

                false ->
                    _pipe = gleam@list:map(
                        Arr,
                        fun(Tuple@1) ->
                            gleam@float:absolute_value(
                                (erlang:element(1, Tuple@1) - erlang:element(
                                    2,
                                    Tuple@1
                                ))
                            )
                            * erlang:element(3, Tuple@1)
                        end
                    ),
                    list_maximum(_pipe, fun gleam@float:compare/2)
            end
    end.

-file("src/gleam_community/maths.gleam", 4039).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the arithmetic mean of the elements in a list:\n"
    "\n"
    " \\\\[\n"
    " \\bar{x} = \\frac{1}{n}\\sum_{i=1}^n x_i\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the sample size (the length of the list) and \\\\(x_i\\\\)\n"
    " is the sample point in the input list indexed by \\\\(i\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.mean()\n"
    "       |> should.be_error()\n"
    "\n"
    "       [1.0, 2.0, 3.0]\n"
    "       |> maths.mean()\n"
    "       |> should.equal(Ok(2.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec mean(list(float())) -> {ok, float()} | {error, nil}.
mean(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            {ok, case erlang:float(erlang:length(Arr)) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> gleam@float:sum(Arr) / Gleam@denominator
                end}
    end.

-file("src/gleam_community/maths.gleam", 3965).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the n'th moment about the mean of a list of elements.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // An empty list returns an error\n"
    "       []\n"
    "       |> maths.moment(0)\n"
    "       |> should.be_error()\n"
    "\n"
    "       // 0th moment about the mean is 1. per definition\n"
    "       [0.0, 1.0, 2.0, 3.0, 4.0]\n"
    "       |> maths.moment(0)\n"
    "       |> should.equal(Ok(1.0))\n"
    "\n"
    "       // 1st moment about the mean is 0. per definition\n"
    "       [0.0, 1.0, 2.0, 3.0, 4.0]\n"
    "       |> maths.moment(1)\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       // 2nd moment about the mean\n"
    "       [0.0, 1.0, 2.0, 3.0, 4.0]\n"
    "       |> maths.moment(2)\n"
    "       |> should.equal(Ok(2.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec moment(list(float()), integer()) -> {ok, float()} | {error, nil}.
moment(Arr, N) ->
    case {Arr, N} of
        {[], _} ->
            {error, nil};

        {_, 0} ->
            {ok, 1.0};

        {_, 1} ->
            {ok, +0.0};

        {_, N@1} when N@1 > 1 ->
            _assert_subject = mean(Arr),
            {ok, M1} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"moment"/utf8>>,
                                line => 3976})
            end,
            Result = gleam@list:try_fold(
                Arr,
                +0.0,
                fun(Acc, A) ->
                    case gleam@float:power(A - M1, erlang:float(N@1)) of
                        {error, _} ->
                            {error, nil};

                        {ok, Value} ->
                            {ok, Acc + Value}
                    end
                end
            ),
            case Result of
                {error, _} ->
                    {error, nil};

                {ok, Value@1} ->
                    {ok, case erlang:float(erlang:length(Arr)) of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator -> Value@1 / Gleam@denominator
                        end}
            end;

        {_, _} ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 4092).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the harmonic mean \\\\(\\bar{x}\\\\) of the elements in a list:\n"
    "\n"
    " \\\\[\n"
    "   \\bar{x} = \\frac{n}{\\sum_{i=1}^{n}\\frac{1}{x_i}}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the sample size (the length of the list) and\n"
    " \\\\(x_i\\\\) is the sample point in the input list indexed by \\\\(i\\\\).\n"
    " Note: The harmonic mean is only defined for positive numbers.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // An empty list returns an error\n"
    "       []\n"
    "       |> maths.harmonic_mean()\n"
    "       |> should.be_error()\n"
    "\n"
    "       // List with negative numbers returns an error\n"
    "       [-1.0, -3.0, -6.0]\n"
    "       |> maths.harmonic_mean()\n"
    "       |> should.be_error()\n"
    "\n"
    "       // Valid input returns a result\n"
    "       [1.0, 3.0, 6.0]\n"
    "       |> maths.harmonic_mean()\n"
    "       |> should.equal(Ok(2.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec harmonic_mean(list(float())) -> {ok, float()} | {error, nil}.
harmonic_mean(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            Sum = gleam@list:try_fold(Arr, +0.0, fun(Acc, A) -> case A of
                        A@1 when A@1 > +0.0 ->
                            {ok, (case A@1 of
                                    +0.0 -> +0.0;
                                    -0.0 -> -0.0;
                                    Gleam@denominator -> 1.0 / Gleam@denominator
                                end) + Acc};

                        A@2 when A@2 =:= +0.0 ->
                            {error, +0.0};

                        _ ->
                            {error, -1.0}
                    end end),
            case Sum of
                {ok, Sum@1} ->
                    {ok, case Sum@1 of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator@1 -> erlang:float(
                                erlang:length(Arr)
                            )
                            / Gleam@denominator@1
                        end};

                {error, +0.0} ->
                    {ok, +0.0};

                {error, _} ->
                    {error, nil}
            end
    end.

-file("src/gleam_community/maths.gleam", 4161).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the geometric mean \\\\(\\bar{x}\\\\) of the elements in a list:\n"
    "\n"
    " \\\\[\n"
    "   \\bar{x} = \\left(\\prod^{n}_{i=1} x_i\\right)^{\\frac{1}{n}}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the sample size (the length of the list) and\n"
    " \\\\(x_i\\\\) is the sample point in the input list indexed by \\\\(i\\\\).\n"
    " Note: The geometric mean is only defined for positive numbers.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // An empty list returns an error\n"
    "       []\n"
    "       |> maths.geometric_mean()\n"
    "       |> should.be_error()\n"
    "\n"
    "       // List with negative numbers returns an error\n"
    "       [-1.0, -3.0, -6.0]\n"
    "       |> maths.geometric_mean()\n"
    "       |> should.be_error()\n"
    "\n"
    "       // Valid input returns a result\n"
    "       [1.0, 3.0, 9.0]\n"
    "       |> maths.geometric_mean()\n"
    "       |> should.equal(Ok(3.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec geometric_mean(list(float())) -> {ok, float()} | {error, nil}.
geometric_mean(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            Product = gleam@list:try_fold(Arr, 1.0, fun(Acc, A) -> case A of
                        A@1 when A@1 > +0.0 ->
                            {ok, Acc * A@1};

                        A@2 when A@2 =:= +0.0 ->
                            {error, +0.0};

                        _ ->
                            {error, -1.0}
                    end end),
            case Product of
                {ok, Product@1} ->
                    gleam@float:power(
                        Product@1,
                        case erlang:float(erlang:length(Arr)) of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator -> 1.0 / Gleam@denominator
                        end
                    );

                {error, +0.0} ->
                    {ok, +0.0};

                {error, _} ->
                    {error, nil}
            end
    end.

-file("src/gleam_community/maths.gleam", 4231).
-spec do_median(list(float()), integer(), boolean(), integer()) -> {ok, float()} |
    {error, nil}.
do_median(Xs, Mid, Mean, Index) ->
    gleam@bool:guard(
        Index > Mid,
        {error, nil},
        fun() ->
            Mid_less_one = Mid - 1,
            case Xs of
                [X | _] when not Mean andalso (Index =:= Mid) ->
                    {ok, X};

                [X@1, Y | _] when Mean andalso (Index =:= Mid_less_one) ->
                    {ok, (X@1 + Y) / 2.0};

                [_ | Rest] ->
                    do_median(Rest, Mid, Mean, Index + 1);

                [] ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam_community/maths.gleam", 4219).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the median of the elements in a list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       []\n"
    "       |> maths.median()\n"
    "       |> should.be_error()\n"
    "\n"
    "       [1.0, 2.0, 3.0]\n"
    "       |> maths.median()\n"
    "       |> should.equal(Ok(2.0))\n"
    "\n"
    "       [1.0, 2.0, 3.0, 4.0]\n"
    "       |> maths.median()\n"
    "       |> should.equal(Ok(2.5))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec median(list(float())) -> {ok, float()} | {error, nil}.
median(Arr) ->
    gleam@bool:guard(
        gleam@list:is_empty(Arr),
        {error, nil},
        fun() ->
            Length = erlang:length(Arr),
            Mid = Length div 2,
            Arr_sorted = gleam@list:sort(Arr, fun gleam@float:compare/2),
            case (Length rem 2) =:= 0 of
                true ->
                    do_median(Arr_sorted, Mid, true, 0);

                false ->
                    do_median(Arr_sorted, Mid, false, 0)
            end
        end
    ).

-file("src/gleam_community/maths.gleam", 4291).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the sample variance of the elements in a list:\n"
    "\n"
    " \\\\[\n"
    " s^{2} = \\frac{1}{n - d} \\sum_{i=1}^{n}(x_i - \\bar{x})\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the sample size (the length of the list) and \\\\(x_i\\\\)\n"
    " is the sample point in the input list indexed by \\\\(i\\\\).\n"
    " Furthermore, \\\\(\\bar{x}\\\\) is the sample mean and \\\\(d\\\\) is the \"Delta\n"
    " Degrees of Freedom\". It is typically set to \\\\(d = 1\\\\), which gives an unbiased estimate.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // Degrees of freedom\n"
    "       let ddof = 1\n"
    "\n"
    "       []\n"
    "       |> maths.variance(ddof)\n"
    "       |> should.be_error()\n"
    "\n"
    "       [1.0, 2.0, 3.0]\n"
    "       |> maths.variance(ddof)\n"
    "       |> should.equal(Ok(1.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec variance(list(float()), integer()) -> {ok, float()} | {error, nil}.
variance(Arr, Ddof) ->
    case {Arr, Ddof} of
        {[], _} ->
            {error, nil};

        {_, _} when Ddof < 0 ->
            {error, nil};

        {_, _} ->
            _assert_subject = mean(Arr),
            {ok, Mean} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"variance"/utf8>>,
                                line => 4297})
            end,
            {ok,
                begin
                    _pipe = gleam@list:map(
                        Arr,
                        fun(Element) ->
                            _assert_subject@1 = gleam@float:power(
                                Element - Mean,
                                2.0
                            ),
                            {ok, Result} = case _assert_subject@1 of
                                {ok, _} -> _assert_subject@1;
                                _assert_fail@1 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                value => _assert_fail@1,
                                                module => <<"gleam_community/maths"/utf8>>,
                                                function => <<"variance"/utf8>>,
                                                line => 4304})
                            end,
                            Result
                        end
                    ),
                    _pipe@1 = gleam@float:sum(_pipe),
                    (fun(Element@1) ->
                        case (erlang:float(erlang:length(Arr)) - erlang:float(
                            Ddof
                        )) of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator -> Element@1 / Gleam@denominator
                        end
                    end)(_pipe@1)
                end}
    end.

-file("src/gleam_community/maths.gleam", 4358).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the sample standard deviation of the elements in a list:\n"
    " \\\\[\n"
    " s = \\left(\\frac{1}{n - d} \\sum_{i=1}^{n}(x_i - \\bar{x})\\right)^{\\frac{1}{2}}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the sample size (the length of the list) and \\\\(x_i\\\\)\n"
    " is the sample point in the input list indexed by \\\\(i\\\\).\n"
    " Furthermore, \\\\(\\bar{x}\\\\) is the sample mean and \\\\(d\\\\) is the \"Delta\n"
    " Degrees of Freedom\", and is typically set to \\\\(d = 1\\\\), which gives an unbiased estimate.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // Degrees of freedom\n"
    "       let ddof = 1\n"
    "\n"
    "       []\n"
    "       |> maths.standard_deviation(ddof)\n"
    "       |> should.be_error()\n"
    "\n"
    "       [1.0, 2.0, 3.0]\n"
    "       |> maths.standard_deviation(ddof)\n"
    "       |> should.equal(Ok(1.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec standard_deviation(list(float()), integer()) -> {ok, float()} |
    {error, nil}.
standard_deviation(Arr, Ddof) ->
    case {Arr, Ddof} of
        {[], _} ->
            {error, nil};

        {_, _} when Ddof < 0 ->
            {error, nil};

        {_, _} ->
            _assert_subject = variance(Arr, Ddof),
            {ok, Variance} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"standard_deviation"/utf8>>,
                                line => 4365})
            end,
            gleam@float:square_root(Variance)
    end.

-file("src/gleam_community/maths.gleam", 4409).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the sample kurtosis of a list of elements using the\n"
    " definition of Fisher.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // An empty list returns an error\n"
    "       []\n"
    "       |> maths.kurtosis()\n"
    "       |> should.be_error()\n"
    "\n"
    "       // To calculate kurtosis at least four values are needed\n"
    "       [1.0, 2.0, 3.0]\n"
    "       |> maths.kurtosis()\n"
    "       |> should.be_error()\n"
    "\n"
    "       [1.0, 2.0, 3.0, 4.0]\n"
    "       |> maths.kurtosis()\n"
    "       |> should.equal(Ok(-1.36))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec kurtosis(list(float())) -> {ok, float()} | {error, nil}.
kurtosis(Arr) ->
    case erlang:length(Arr) < 4 of
        true ->
            {error, nil};

        false ->
            case {moment(Arr, 2), moment(Arr, 4)} of
                {{ok, M2}, {ok, M4}} when M2 =/= +0.0 ->
                    case gleam@float:power(M2, 2.0) of
                        {ok, Value} ->
                            {ok, (case Value of
                                    +0.0 -> +0.0;
                                    -0.0 -> -0.0;
                                    Gleam@denominator -> M4 / Gleam@denominator
                                end) - 3.0};

                        {error, nil} ->
                            {error, nil}
                    end;

                {_, _} ->
                    {error, nil}
            end
    end.

-file("src/gleam_community/maths.gleam", 4464).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the sample skewness of a list of elements using the\n"
    " Fisher-Pearson coefficient of skewness.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // An empty list returns an error\n"
    "       []\n"
    "       |> maths.skewness()\n"
    "       |> should.be_error()\n"
    "\n"
    "       // To calculate skewness at least three values are needed\n"
    "       [1.0, 2.0, 3.0]\n"
    "       |> maths.skewness()\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       [1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 3.0, 4.0]\n"
    "       |> maths.skewness()\n"
    "       |> should.equal(Ok(0.6))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec skewness(list(float())) -> {ok, float()} | {error, nil}.
skewness(Arr) ->
    case erlang:length(Arr) < 3 of
        true ->
            {error, nil};

        false ->
            case {moment(Arr, 2), moment(Arr, 3)} of
                {{ok, M2}, {ok, M3}} when M2 =/= +0.0 ->
                    case gleam@float:power(M2, 1.5) of
                        {ok, Value} ->
                            {ok, case Value of
                                    +0.0 -> +0.0;
                                    -0.0 -> -0.0;
                                    Gleam@denominator -> M3 / Gleam@denominator
                                end};

                        {error, nil} ->
                            {error, nil}
                    end;

                {_, _} ->
                    {error, nil}
            end
    end.

-file("src/gleam_community/maths.gleam", 4515).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the n'th percentile of the elements in a list using\n"
    " linear interpolation between closest ranks.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // An empty list returns an error\n"
    "       []\n"
    "       |> maths.percentile(40)\n"
    "       |> should.be_error()\n"
    "\n"
    "       // Calculate 40th percentile\n"
    "       [15.0, 20.0, 35.0, 40.0, 50.0]\n"
    "       |> maths.percentile(40)\n"
    "       |> should.equal(Ok(29.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec percentile(list(float()), integer()) -> {ok, float()} | {error, nil}.
percentile(Arr, N) ->
    case {Arr, N} of
        {[], _} ->
            {error, nil};

        {[Element], _} ->
            {ok, Element};

        {_, N@1} when N@1 =:= 0 ->
            gleam@list:first(gleam@list:sort(Arr, fun gleam@float:compare/2));

        {_, N@2} when N@2 =:= 100 ->
            gleam@list:last(gleam@list:sort(Arr, fun gleam@float:compare/2));

        {_, N@3} when (N@3 > 0) andalso (N@3 < 100) ->
            R = (erlang:float(N@3) / 100.0) * erlang:float(
                erlang:length(Arr) - 1
            ),
            F = erlang:trunc(R),
            Sorted_arr = gleam@list:drop(
                gleam@list:sort(Arr, fun gleam@float:compare/2),
                F
            ),
            case gleam@list:take(Sorted_arr, 2) of
                [Lower, Upper] ->
                    {ok, Lower + ((Upper - Lower) * (R - erlang:float(F)))};

                _ ->
                    {error, nil}
            end;

        {_, _} ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 4578).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the z-score of each value in the list relative to the sample\n"
    " mean and standard deviation.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // An empty list returns an error\n"
    "       []\n"
    "       // Use degrees of freedom = 1\n"
    "       |> maths.zscore(1)\n"
    "       |> should.be_error()\n"
    "\n"
    "       [1.0, 2.0, 3.0]\n"
    "       // Use degrees of freedom = 1\n"
    "       |> maths.zscore(1)\n"
    "       |> should.equal(Ok([-1.0, 0.0, 1.0]))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec zscore(list(float()), integer()) -> {ok, list(float())} | {error, nil}.
zscore(Arr, Ddof) ->
    Length = erlang:length(Arr),
    case {Arr, Ddof} of
        {[], _} ->
            {error, nil};

        {_, Ddof@1} when Ddof@1 < 0 ->
            {error, nil};

        {_, Ddof@2} when Length =< Ddof@2 ->
            {error, nil};

        {_, _} ->
            case {mean(Arr), standard_deviation(Arr, Ddof)} of
                {{ok, Mean}, {ok, Stdev}} when Stdev =/= +0.0 ->
                    {ok, gleam@list:map(Arr, fun(A) -> case Stdev of
                                    +0.0 -> +0.0;
                                    -0.0 -> -0.0;
                                    Gleam@denominator -> (A - Mean) / Gleam@denominator
                                end end)};

                {_, _} ->
                    {error, nil}
            end
    end.

-file("src/gleam_community/maths.gleam", 4632).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the interquartile range (IQR) of the elements in a list.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // An empty list returns an error\n"
    "       []\n"
    "       |> maths.interquartile_range()\n"
    "       |> should.be_error()\n"
    "\n"
    "       // Valid input returns a result\n"
    "       [1.0, 2.0, 3.0, 4.0, 5.0]\n"
    "       |> maths.interquartile_range()\n"
    "       |> should.equal(Ok(3.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec interquartile_range(list(float())) -> {ok, float()} | {error, nil}.
interquartile_range(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            Length = erlang:length(Arr),
            Arr_sorted = gleam@list:sort(Arr, fun gleam@float:compare/2),
            case gleam@int:is_even(Length) of
                true ->
                    {Lower_half, Upper_half} = gleam@list:split(
                        Arr_sorted,
                        Length div 2
                    ),
                    case {median(Lower_half), median(Upper_half)} of
                        {{ok, Q1}, {ok, Q3}} ->
                            {ok, Q3 - Q1};

                        {_, _} ->
                            {error, nil}
                    end;

                false ->
                    {Lower_half@1, _} = gleam@list:split(
                        Arr_sorted,
                        (Length - 1) div 2
                    ),
                    {_, Upper_half@1} = gleam@list:split(
                        Arr_sorted,
                        (Length + 1) div 2
                    ),
                    case {median(Lower_half@1), median(Upper_half@1)} of
                        {{ok, Q1@1}, {ok, Q3@1}} ->
                            {ok, Q3@1 - Q1@1};

                        {_, _} ->
                            {error, nil}
                    end
            end
    end.

-file("src/gleam_community/maths.gleam", 4737).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate Pearson's sample correlation coefficient to determine the linear\n"
    " relationship between the elements in two lists of equal\n"
    " length. The correlation coefficient \\\\(r_{xy} \\in \\[-1, 1\\]\\\\) is calculated\n"
    " as:\n"
    "\n"
    " \\\\[\n"
    " r_{xy} =\\frac{\\sum ^n _{i=1}(x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum^n _{i=1}(x_i - \\bar{x})^2} \\sqrt{\\sum^n _{i=1}(y_i - \\bar{y})^2}}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the sample size (the length of the input lists),\n"
    " \\\\(x_i\\\\), \\\\(y_i\\\\) are the corresponding sample points indexed by \\\\(i\\\\) and\n"
    " \\\\(\\bar{x}\\\\), \\\\(\\bar{y}\\\\) are the sample means.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       // An empty lists returns an error\n"
    "       maths.correlation([], [])\n"
    "       |> should.be_error()\n"
    "\n"
    "       // Perfect positive correlation\n"
    "       let xarr =\n"
    "         list.range(0, 100)\n"
    "         |> list.map(fn(x) { int.to_float(x) })\n"
    "       let yarr =\n"
    "         list.range(0, 100)\n"
    "         |> list.map(fn(y) { int.to_float(y) })\n"
    "       list.zip(xarr, yarr)\n"
    "       |> maths.correlation()\n"
    "       |> should.equal(Ok(1.0))\n"
    "\n"
    "       // Perfect negative correlation\n"
    "       let xarr =\n"
    "         list.range(0, 100)\n"
    "         |> list.map(fn(x) { -1.0 *. int.to_float(x) })\n"
    "       let yarr =\n"
    "         list.range(0, 100)\n"
    "         |> list.map(fn(y) { int.to_float(y) })\n"
    "       list.zip(xarr, yarr)\n"
    "       |> maths.correlation()\n"
    "       |> should.equal(Ok(-1.0))\n"
    "\n"
    "       // No correlation (independent variables)\n"
    "       let xarr = [1.0, 2.0, 3.0, 4.0]\n"
    "       let yarr = [5.0, 5.0, 5.0, 5.0]\n"
    "       list.zip(xarr, yarr)\n"
    "       |> maths.correlation()\n"
    "       |> should.equal(Ok(0.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec correlation(list({float(), float()})) -> {ok, float()} | {error, nil}.
correlation(Arr) ->
    Length = erlang:length(Arr),
    case Length >= 2 of
        false ->
            {error, nil};

        true ->
            {Xarr, Yarr} = gleam@list:unzip(Arr),
            _assert_subject = mean(Xarr),
            {ok, Xmean} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"correlation"/utf8>>,
                                line => 4743})
            end,
            _assert_subject@1 = mean(Yarr),
            {ok, Ymean} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"correlation"/utf8>>,
                                line => 4744})
            end,
            A = begin
                _pipe = gleam@list:map(
                    Arr,
                    fun(Tuple) ->
                        (erlang:element(1, Tuple) - Xmean) * (erlang:element(
                            2,
                            Tuple
                        )
                        - Ymean)
                    end
                ),
                gleam@float:sum(_pipe)
            end,
            B = begin
                _pipe@1 = gleam@list:map(
                    Xarr,
                    fun(X) -> (X - Xmean) * (X - Xmean) end
                ),
                gleam@float:sum(_pipe@1)
            end,
            C = begin
                _pipe@2 = gleam@list:map(
                    Yarr,
                    fun(Y) -> (Y - Ymean) * (Y - Ymean) end
                ),
                gleam@float:sum(_pipe@2)
            end,
            _assert_subject@2 = gleam@float:square_root(B * C),
            {ok, Value} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@2,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"correlation"/utf8>>,
                                line => 4758})
            end,
            {ok, case Value of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> A / Gleam@denominator
                end}
    end.

-file("src/gleam_community/maths.gleam", 4921).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The Tversky index is a generalization of the Jaccard index and Sørensen-Dice\n"
    " coefficient, which adds flexibility in measuring similarity between two sets using two\n"
    " parameters, \\\\(\\alpha\\\\) and \\\\(\\beta\\\\). These parameters allow for asymmetric\n"
    " similarity measures between sets. The Tversky index is defined as:\n"
    "\n"
    " \\\\[\n"
    " \\frac{|X \\cap Y|}{|X \\cap Y| + \\alpha|X - Y| + \\beta|Y - X|}\n"
    " \\\\]\n"
    "\n"
    " where:\n"
    "\n"
    " - \\\\(X\\\\) and \\\\(Y\\\\) are the sets being compared\n"
    " - \\\\(|X - Y|\\\\) and \\\\(|Y - X|\\\\) are the sizes of the relative complements of\n"
    " \\\\(Y\\\\) in \\\\(X\\\\) and \\\\(X\\\\) in \\\\(Y\\\\), respectively,\n"
    " - \\\\(\\alpha\\\\) and \\\\(\\beta\\\\) are parameters that weight the relative importance\n"
    " of the elements unique to \\\\(X\\\\) and \\\\(Y\\\\)\n"
    "\n"
    " The Tversky index reduces to the Jaccard index when \\\\(\\alpha = \\beta = 1\\\\) and\n"
    " to the Sørensen-Dice coefficient when \\\\(\\alpha = \\beta = 0.5\\\\). In general, the\n"
    " Tversky index can take on any non-negative value, including 0. The index equals\n"
    " 0 when there is no intersection between the two sets, indicating no similarity.\n"
    " The Tversky index does not have a strict upper limit of 1 when \\\\(\\alpha \\neq \\beta\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/set\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let yset = set.from_list([\"cat\", \"dog\", \"hippo\", \"monkey\"])\n"
    "       let xset = set.from_list([\"monkey\", \"rhino\", \"ostrich\", \"salmon\"])\n"
    "       // Test Jaccard index (alpha = beta = 1)\n"
    "       maths.tversky_index(xset, yset, 1.0, 1.0)\n"
    "       |> should.equal(Ok(1.0 /. 7.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec tversky_index(gleam@set:set(GLZ), gleam@set:set(GLZ), float(), float()) -> {ok,
        float()} |
    {error, nil}.
tversky_index(Xset, Yset, Alpha, Beta) ->
    case {Alpha >= +0.0, Beta >= +0.0} of
        {true, true} ->
            Intersection = begin
                _pipe = gleam@set:intersection(Xset, Yset),
                _pipe@1 = gleam@set:size(_pipe),
                erlang:float(_pipe@1)
            end,
            Difference1 = begin
                _pipe@2 = gleam@set:difference(Xset, Yset),
                _pipe@3 = gleam@set:size(_pipe@2),
                erlang:float(_pipe@3)
            end,
            Difference2 = begin
                _pipe@4 = gleam@set:difference(Yset, Xset),
                _pipe@5 = gleam@set:size(_pipe@4),
                erlang:float(_pipe@5)
            end,
            {ok,
                case ((Intersection + (Alpha * Difference1)) + (Beta * Difference2)) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> Intersection / Gleam@denominator
                end};

        {_, _} ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 4809).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The Jaccard index measures similarity between two sets of elements. Mathematically, the\n"
    " Jaccard index is defined as:\n"
    "\n"
    " \\\\[\n"
    " \\frac{|X \\cap Y|}{|X \\cup Y|} \\\\; \\in \\\\; \\left[0, 1\\right]\n"
    " \\\\]\n"
    "\n"
    " where:\n"
    "\n"
    " - \\\\(X\\\\) and \\\\(Y\\\\) are two sets being compared\n"
    " - \\\\(|X \\cap Y|\\\\) represents the size of the intersection of the two sets\n"
    " - \\\\(|X \\cup Y|\\\\) denotes the size of the union of the two sets\n"
    "\n"
    " The value of the Jaccard index ranges from 0 to 1, where 0 indicates that the\n"
    " two sets share no elements and 1 indicates that the sets are identical. The\n"
    " Jaccard index is a special case of the  [Tversky index](#tversky_index) (with\n"
    " \\\\(\\alpha=\\beta=1\\\\)).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/set\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let xset = set.from_list([\"cat\", \"dog\", \"hippo\", \"monkey\"])\n"
    "       let yset = set.from_list([\"monkey\", \"rhino\", \"ostrich\", \"salmon\"])\n"
    "       maths.jaccard_index(xset, yset)\n"
    "       |> should.equal(1.0 /. 7.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec jaccard_index(gleam@set:set(GLT), gleam@set:set(GLT)) -> float().
jaccard_index(Xset, Yset) ->
    _assert_subject = tversky_index(Xset, Yset, 1.0, 1.0),
    {ok, Result} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"jaccard_index"/utf8>>,
                        line => 4812})
    end,
    Result.

-file("src/gleam_community/maths.gleam", 4863).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The Sørensen-Dice coefficient measures the similarity between two sets of elements.\n"
    " Mathematically, the coefficient is defined as:\n"
    "\n"
    " \\\\[\n"
    " \\frac{2 |X \\cap Y|}{|X| + |Y|} \\\\; \\in \\\\; \\left[0, 1\\right]\n"
    " \\\\]\n"
    "\n"
    " where:\n"
    "\n"
    " - \\\\(X\\\\) and \\\\(Y\\\\) are two sets being compared\n"
    " - \\\\(|X \\cap Y|\\\\) is the size of the intersection of the two sets (i.e., the\n"
    " number of elements common to both sets)\n"
    " - \\\\(|X|\\\\) and \\\\(|Y|\\\\) are the sizes of the sets \\\\(X\\\\) and \\\\(Y\\\\), respectively\n"
    "\n"
    " The coefficient ranges from 0 to 1, where 0 indicates no similarity (the sets\n"
    " share no elements) and 1 indicates perfect similarity (the sets are identical).\n"
    " The higher the coefficient, the greater the similarity between the two sets.\n"
    " The Sørensen-Dice coefficient is a special case of the\n"
    " [Tversky index](#tversky_index) (with \\\\(\\alpha=\\beta=0.5\\\\)).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/set\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let xset = set.from_list([\"cat\", \"dog\", \"hippo\", \"monkey\"])\n"
    "       let yset = set.from_list([\"monkey\", \"rhino\", \"ostrich\", \"salmon\", \"spider\"])\n"
    "       maths.sorensen_dice_coefficient(xset, yset)\n"
    "       |> should.equal(2.0 *. 1.0 /. { 4.0 +. 5.0 })\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec sorensen_dice_coefficient(gleam@set:set(GLW), gleam@set:set(GLW)) -> float().
sorensen_dice_coefficient(Xset, Yset) ->
    _assert_subject = tversky_index(Xset, Yset, 0.5, 0.5),
    {ok, Result} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam_community/maths"/utf8>>,
                        function => <<"sorensen_dice_coefficient"/utf8>>,
                        line => 4866})
    end,
    Result.

-file("src/gleam_community/maths.gleam", 4998).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The Overlap coefficient, also known as the Szymkiewicz–Simpson coefficient, is\n"
    " a measure of similarity between two sets that focuses on the size of the\n"
    " intersection relative to the smaller of the two sets. It is defined\n"
    " mathematically as:\n"
    "\n"
    " \\\\[\n"
    " \\frac{|X \\cap Y|}{\\min(|X|, |Y|)} \\\\; \\in \\\\; \\left[0, 1\\right]\n"
    " \\\\]\n"
    "\n"
    " where:\n"
    "\n"
    " - \\\\(X\\\\) and \\\\(Y\\\\) are the sets being compared\n"
    " - \\\\(|X \\cap Y|\\\\) is the size of the intersection of the sets\n"
    " - \\\\(\\min(|X|, |Y|)\\\\) is the size of the smaller set among \\\\(X\\\\) and \\\\(Y\\\\)\n"
    "\n"
    " The coefficient ranges from 0 to 1, where 0 indicates no overlap and 1\n"
    " indicates that the smaller set is a suyset of the larger set. This\n"
    " measure is especially useful in situations where the similarity in terms\n"
    " of the proportion of overlap is more relevant than the difference in sizes\n"
    " between the two sets.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/set\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let set_a = set.from_list([\"horse\", \"dog\", \"hippo\", \"monkey\", \"bird\"])\n"
    "       let set_b = set.from_list([\"monkey\", \"bird\", \"ostrich\", \"salmon\"])\n"
    "       maths.overlap_coefficient(set_a, set_b)\n"
    "       |> should.equal(2.0 /. 4.0)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec overlap_coefficient(gleam@set:set(GME), gleam@set:set(GME)) -> float().
overlap_coefficient(Xset, Yset) ->
    Intersection = begin
        _pipe = gleam@set:intersection(Xset, Yset),
        _pipe@1 = gleam@set:size(_pipe),
        erlang:float(_pipe@1)
    end,
    Minsize = begin
        _pipe@2 = gleam@int:min(gleam@set:size(Xset), gleam@set:size(Yset)),
        erlang:float(_pipe@2)
    end,
    case Minsize of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> Intersection / Gleam@denominator
    end.

-file("src/gleam_community/maths.gleam", 5061).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the cosine similarity between two lists (representing\n"
    " vectors):\n"
    "\n"
    " \\\\[\n"
    " \\frac{\\sum_{i=1}^n  x_i \\cdot y_i}\n"
    " {\\left(\\sum_{i=1}^n x_i^2\\right)^{\\frac{1}{2}}\n"
    " \\cdot\n"
    " \\left(\\sum_{i=1}^n y_i^2\\right)^{\\frac{1}{2}}}\n"
    " \\\\; \\in \\\\; \\left[-1, 1\\right]\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists and \\\\(x_i\\\\), \\\\(y_i\\\\) are\n"
    " the values in the respective input lists indexed by \\\\(i\\\\).\n"
    "\n"
    " The cosine similarity provides a value between -1 and 1, where 1 means the\n"
    " vectors are in the same direction, -1 means they are in exactly opposite\n"
    " directions, and 0 indicates orthogonality.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/option\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       // Two orthogonal vectors\n"
    "       maths.cosine_similarity([#(-1.0, 1.0), #(1.0, 1.0), #(0.0, -1.0)])\n"
    "       |> should.equal(Ok(0.0))\n"
    "\n"
    "       // Two identical (parallel) vectors\n"
    "       maths.cosine_similarity([#(1.0, 1.0), #(2.0, 2.0), #(3.0, 3.0)])\n"
    "       |> should.equal(Ok(1.0))\n"
    "\n"
    "       // Two parallel, but oppositely oriented vectors\n"
    "       maths.cosine_similarity([#(-1.0, 1.0), #(-2.0, 2.0), #(-3.0, 3.0)])\n"
    "       |> should.equal(Ok(-1.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec cosine_similarity(list({float(), float()})) -> {ok, float()} |
    {error, nil}.
cosine_similarity(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            Numerator = gleam@list:fold(
                Arr,
                +0.0,
                fun(Acc, Tuple) ->
                    Acc + (erlang:element(1, Tuple) * erlang:element(2, Tuple))
                end
            ),
            Xarr = gleam@list:map(
                Arr,
                fun(Tuple@1) -> erlang:element(1, Tuple@1) end
            ),
            Yarr = gleam@list:map(
                Arr,
                fun(Tuple@2) -> erlang:element(2, Tuple@2) end
            ),
            _assert_subject = norm(Xarr, 2.0),
            {ok, Xarr_norm} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"cosine_similarity"/utf8>>,
                                line => 5073})
            end,
            _assert_subject@1 = norm(Yarr, 2.0),
            {ok, Yarr_norm} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"cosine_similarity"/utf8>>,
                                line => 5074})
            end,
            Denominator = (Xarr_norm * Yarr_norm),
            {ok, case Denominator of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> Numerator / Gleam@denominator
                end}
    end.

-file("src/gleam_community/maths.gleam", 5147).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the weighted cosine similarity between two lists (representing\n"
    " vectors):\n"
    "\n"
    " \\\\[\n"
    " \\frac{\\sum_{i=1}^n w_{i} \\cdot x_i \\cdot y_i}\n"
    " {\\left(\\sum_{i=1}^n w_{i} \\cdot x_i^2\\right)^{\\frac{1}{2}}\n"
    " \\cdot\n"
    " \\left(\\sum_{i=1}^n w_{i} \\cdot y_i^2\\right)^{\\frac{1}{2}}}\n"
    " \\\\; \\in \\\\; \\left[-1, 1\\right]\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists and \\\\(x_i\\\\), \\\\(y_i\\\\) are\n"
    " the values in the respective input lists indexed by \\\\(i\\\\), while the\n"
    " \\\\(w_i \\in \\mathbb{R}_{+}\\\\) are corresponding positive weights.\n"
    "\n"
    " The cosine similarity provides a value between -1 and 1, where 1 means the\n"
    " vectors are in the same direction, -1 means they are in exactly opposite\n"
    " directions, and 0 indicates orthogonality.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/option\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "\n"
    "       let assert Ok(result) =\n"
    "         maths.cosine_similarity_with_weights([\n"
    "           #(1.0, 1.0, 2.0),\n"
    "           #(2.0, 2.0, 3.0),\n"
    "           #(3.0, 3.0, 4.0),\n"
    "         ])\n"
    "       result\n"
    "       |> maths.is_close(1.0, 0.0, tolerance)\n"
    "       |> should.be_true()\n"
    "\n"
    "       let assert Ok(result) =\n"
    "         maths.cosine_similarity_with_weights([\n"
    "           #(-1.0, 1.0, 1.0),\n"
    "           #(-2.0, 2.0, 0.5),\n"
    "           #(-3.0, 3.0, 0.33),\n"
    "         ])\n"
    "       result\n"
    "       |> maths.is_close(-1.0, 0.0, tolerance)\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec cosine_similarity_with_weights(list({float(), float(), float()})) -> {ok,
        float()} |
    {error, nil}.
cosine_similarity_with_weights(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            Weight_is_negative = gleam@list:any(
                Arr,
                fun(Tuple) -> erlang:element(3, Tuple) < +0.0 end
            ),
            case Weight_is_negative of
                false ->
                    Numerator = gleam@list:fold(
                        Arr,
                        +0.0,
                        fun(Acc, Tuple@1) ->
                            Acc + ((erlang:element(1, Tuple@1) * erlang:element(
                                2,
                                Tuple@1
                            ))
                            * erlang:element(3, Tuple@1))
                        end
                    ),
                    Xarr = gleam@list:map(
                        Arr,
                        fun(Tuple@2) ->
                            {erlang:element(1, Tuple@2),
                                erlang:element(3, Tuple@2)}
                        end
                    ),
                    Yarr = gleam@list:map(
                        Arr,
                        fun(Tuple@3) ->
                            {erlang:element(2, Tuple@3),
                                erlang:element(3, Tuple@3)}
                        end
                    ),
                    _assert_subject = norm_with_weights(Xarr, 2.0),
                    {ok, Xarr_norm} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        value => _assert_fail,
                                        module => <<"gleam_community/maths"/utf8>>,
                                        function => <<"cosine_similarity_with_weights"/utf8>>,
                                        line => 5167})
                    end,
                    _assert_subject@1 = norm_with_weights(Yarr, 2.0),
                    {ok, Yarr_norm} = case _assert_subject@1 of
                        {ok, _} -> _assert_subject@1;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        value => _assert_fail@1,
                                        module => <<"gleam_community/maths"/utf8>>,
                                        function => <<"cosine_similarity_with_weights"/utf8>>,
                                        line => 5168})
                    end,
                    Denominator = (Xarr_norm * Yarr_norm),
                    {ok, case Denominator of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator -> Numerator / Gleam@denominator
                        end};

                true ->
                    {error, nil}
            end
    end.

-file("src/gleam_community/maths.gleam", 5218).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the Canberra distance between two lists:\n"
    "\n"
    " \\\\[\n"
    " \\sum_{i=1}^n \\frac{\\left| x_i - y_i \\right|}\n"
    " {\\left| x_i \\right| + \\left| y_i \\right|}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists, and \\\\(x_i, y_i\\\\) are the\n"
    " values in the respective input lists indexed by \\\\(i\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.canberra_distance([])\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.canberra_distance([#(1.0, -2.0), #(2.0, -1.0)])\n"
    "       |> should.equal(Ok(2.0))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec canberra_distance(list({float(), float()})) -> {ok, float()} |
    {error, nil}.
canberra_distance(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            {ok,
                gleam@list:fold(
                    Arr,
                    +0.0,
                    fun(Acc, Tuple) ->
                        Numerator = gleam@float:absolute_value(
                            (erlang:element(1, Tuple) - erlang:element(2, Tuple))
                        ),
                        Denominator = (gleam@float:absolute_value(
                            erlang:element(1, Tuple)
                        )
                        + gleam@float:absolute_value(erlang:element(2, Tuple))),
                        Acc + (case Denominator of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator -> Numerator / Gleam@denominator
                        end)
                    end
                )}
    end.

-file("src/gleam_community/maths.gleam", 5273).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the weighted Canberra distance between two lists:\n"
    "\n"
    " \\\\[\n"
    " \\sum_{i=1}^n w_{i}\\frac{\\left| x_i - y_i \\right|}\n"
    " {\\left| x_i \\right| + \\left| y_i \\right|}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists, and \\\\(x_i, y_i\\\\) are the\n"
    " values in the respective input lists indexed by \\\\(i\\\\), while the\n"
    " \\\\(w_i \\in \\mathbb{R}_{+}\\\\) are corresponding positive weights.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.canberra_distance_with_weights([])\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.canberra_distance_with_weights([#(1.0, -2.0, 0.5), #(2.0, -1.0, 1.0)])\n"
    "       |> should.equal(Ok(1.5))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec canberra_distance_with_weights(list({float(), float(), float()})) -> {ok,
        float()} |
    {error, nil}.
canberra_distance_with_weights(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            Weight_is_negative = gleam@list:any(
                Arr,
                fun(Tuple) -> erlang:element(3, Tuple) < +0.0 end
            ),
            case Weight_is_negative of
                true ->
                    {error, nil};

                false ->
                    {ok,
                        gleam@list:fold(
                            Arr,
                            +0.0,
                            fun(Acc, Tuple@1) ->
                                Numerator = gleam@float:absolute_value(
                                    (erlang:element(1, Tuple@1) - erlang:element(
                                        2,
                                        Tuple@1
                                    ))
                                ),
                                Denominator = (gleam@float:absolute_value(
                                    erlang:element(1, Tuple@1)
                                )
                                + gleam@float:absolute_value(
                                    erlang:element(2, Tuple@1)
                                )),
                                Acc + (case Denominator of
                                    +0.0 -> +0.0;
                                    -0.0 -> -0.0;
                                    Gleam@denominator -> erlang:element(
                                        3,
                                        Tuple@1
                                    )
                                    * Numerator
                                    / Gleam@denominator
                                end)
                            end
                        )}
            end
    end.

-file("src/gleam_community/maths.gleam", 5340).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the Bray-Curtis distance between two lists:\n"
    "\n"
    " \\\\[\n"
    " \\frac{\\sum_{i=1}^n  \\left| x_i - y_i \\right|}\n"
    " {\\sum_{i=1}^n \\left| x_i + y_i \\right|}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists, and \\\\(x_i, y_i\\\\) are the values\n"
    " in the respective input lists indexed by \\\\(i\\\\).\n"
    "\n"
    " The Bray-Curtis distance is in the range \\\\([0, 1]\\\\) if all entries \\\\(x_i, y_i\\\\) are\n"
    " positive.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.braycurtis_distance([])\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.braycurtis_distance([#(1.0, 3.0), #(2.0, 4.0)])\n"
    "       |> should.equal(Ok(0.4))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec braycurtis_distance(list({float(), float()})) -> {ok, float()} |
    {error, nil}.
braycurtis_distance(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            Numerator = gleam@list:fold(
                Arr,
                +0.0,
                fun(Acc, Tuple) ->
                    Acc + gleam@float:absolute_value(
                        (erlang:element(1, Tuple) - erlang:element(2, Tuple))
                    )
                end
            ),
            Denominator = gleam@list:fold(
                Arr,
                +0.0,
                fun(Acc@1, Tuple@1) ->
                    Acc@1 + gleam@float:absolute_value(
                        (erlang:element(1, Tuple@1) + erlang:element(2, Tuple@1))
                    )
                end
            ),
            {ok, (case Denominator of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> Numerator / Gleam@denominator
                end)}
    end.

-file("src/gleam_community/maths.gleam", 5400).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Calculate the weighted Bray-Curtis distance between two lists:\n"
    "\n"
    " \\\\[\n"
    " \\frac{\\sum_{i=1}^n w_{i} \\left| x_i - y_i \\right|}\n"
    " {\\sum_{i=1}^n w_{i}\\left| x_i + y_i \\right|}\n"
    " \\\\]\n"
    "\n"
    " In the formula, \\\\(n\\\\) is the length of the two lists, and \\\\(x_i, y_i\\\\) are the values\n"
    " in the respective input lists indexed by \\\\(i\\\\), while the\n"
    " \\\\(w_i \\in \\mathbb{R}_{+}\\\\) are corresponding positive weights.\n"
    "\n"
    " The Bray-Curtis distance is in the range \\\\([0, 1]\\\\) if all entries \\\\(x_i, y_i\\\\) are\n"
    " positive and \\\\(w_i = 1.0\\\\;\\forall i=1...n\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.braycurtis_distance_with_weights([])\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.braycurtis_distance_with_weights([#(1.0, 3.0, 0.5), #(2.0, 4.0, 1.0)])\n"
    "       |> should.equal(Ok(0.375))\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec braycurtis_distance_with_weights(list({float(), float(), float()})) -> {ok,
        float()} |
    {error, nil}.
braycurtis_distance_with_weights(Arr) ->
    case Arr of
        [] ->
            {error, nil};

        _ ->
            Weight_is_negative = gleam@list:any(
                Arr,
                fun(Tuple) -> erlang:element(3, Tuple) < +0.0 end
            ),
            case Weight_is_negative of
                true ->
                    {error, nil};

                false ->
                    Numerator = gleam@list:fold(
                        Arr,
                        +0.0,
                        fun(Acc, Tuple@1) ->
                            Acc + (erlang:element(3, Tuple@1) * gleam@float:absolute_value(
                                (erlang:element(1, Tuple@1) - erlang:element(
                                    2,
                                    Tuple@1
                                ))
                            ))
                        end
                    ),
                    Denominator = gleam@list:fold(
                        Arr,
                        +0.0,
                        fun(Acc@1, Tuple@2) ->
                            Acc@1 + (erlang:element(3, Tuple@2) * gleam@float:absolute_value(
                                (erlang:element(1, Tuple@2) + erlang:element(
                                    2,
                                    Tuple@2
                                ))
                            ))
                        end
                    ),
                    {ok, (case Denominator of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator -> Numerator / Gleam@denominator
                        end)}
            end
    end.

-file("src/gleam_community/maths.gleam", 5468).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Determine if a given value \\\\(x\\\\) is close to or equivalent to a reference value\n"
    " \\\\(y\\\\) based on supplied relative \\\\(r_{tol}\\\\) and absolute \\\\(a_{tol}\\\\) tolerance\n"
    " values. The equivalance of the two given values are then determined based on\n"
    " the equation:\n"
    "\n"
    " \\\\[\n"
    "     \\|x - y\\| \\leq (a_{tol} + r_{tol} \\cdot \\|y\\|)\n"
    " \\\\]\n"
    "\n"
    " `True` is returned if the statement holds, otherwise `False` is returned.\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let value = 99.\n"
    "       let reference_value = 100.\n"
    "       // We set 'absolute_tolerance' and 'relative_tolerance' such that the values are\n"
    "       // equivalent if 'value' is within 1 percent of 'reference_value' +/- 0.1\n"
    "       let relative_tolerance = 0.01\n"
    "       let absolute_tolerance = 0.10\n"
    "       maths.is_close(value, reference_value, relative_tolerance, absolute_tolerance)\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec is_close(float(), float(), float(), float()) -> boolean().
is_close(X, Y, Rtol, Atol) ->
    X@1 = absolute_difference(X, Y),
    Y@1 = Atol + (Rtol * gleam@float:absolute_value(Y)),
    X@1 =< Y@1.

-file("src/gleam_community/maths.gleam", 5525).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Determine if each value \\\\(x_i\\\\) is close to or equivalent to its corresponding reference value\n"
    " \\\\(y_i\\\\), in a list of value pairs \\\\((x_i, y_i)\\\\), based on supplied relative \\\\(r_{tol}\\\\)\n"
    " and absolute  \\\\(a_{tol}\\\\) tolerance values. The equivalence of each pair \\\\((x_i, y_i)\\\\) is\n"
    " determined by the equation:\n"
    "\n"
    " \\\\[\n"
    "     \\|x_i - y_i\\| \\leq (a_{tol} + r_{tol} \\cdot \\|y_i\\|), \\\\; \\forall i=1,...,n.\n"
    " \\\\]\n"
    "\n"
    " A list of `Bool` values is returned, where each entry indicates if the corresponding pair\n"
    " satisfies the condition. If all conditions are satisfied, the list will contain only `True`\n"
    " values.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/list\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let value = 99.0\n"
    "       let reference_value = 100.0\n"
    "       let xarr = list.repeat(value, 42)\n"
    "       let yarr = list.repeat(reference_value, 42)\n"
    "       let arr = list.zip(xarr, yarr)\n"
    "       // We set 'absolute_tolerance' and 'relative_tolerance' such that\n"
    "       // the values are equivalent if 'value' is within 1 percent of\n"
    "       // 'reference_value' +/- 0.1\n"
    "       let relative_tolerance = 0.01\n"
    "       let absolute_tolerance = 0.1\n"
    "       let assert Ok(result) =\n"
    "         maths.all_close(arr, relative_tolerance, absolute_tolerance)\n"
    "       result\n"
    "       |> list.all(fn(x) { x == True })\n"
    "       |> should.be_true()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec all_close(list({float(), float()}), float(), float()) -> list(boolean()).
all_close(Arr, Rtol, Atol) ->
    gleam@list:map(
        Arr,
        fun(_use0) ->
            {X, Y} = _use0,
            is_close(X, Y, Rtol, Atol)
        end
    ).

-file("src/gleam_community/maths.gleam", 5571).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Determine if a given value \\\\(x\\\\) is fractional, i.e., if it contains a fractional part:\n"
    "\n"
    " \\\\[\n"
    "     x - \\lfloor x \\rfloor > 0\n"
    " \\\\]\n"
    "\n"
    " `True` is returned if the given value is fractional (i.e., it has a non-zero decimal part),\n"
    " otherwise `False` is returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       maths.is_fractional(0.3333)\n"
    "       |> should.equal(True)\n"
    "\n"
    "       maths.is_fractional(1.0)\n"
    "       |> should.equal(False)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec is_fractional(float()) -> boolean().
is_fractional(X) ->
    (math:ceil(X) - X) > +0.0.

-file("src/gleam_community/maths.gleam", 5616).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A function that determines if a given integer value \\\\(x \\in \\mathbb{Z}\\\\) is a power of\n"
    " another integer value \\\\(y \\in \\mathbb{Z}\\\\), i.e., the function evaluates whether \\\\(x\\\\) can\n"
    " be expressed as \\\\(y^n\\\\) for some integer \\\\(n \\geq 0\\\\), by computing the base-\\\\(y\\\\)\n"
    " logarithm of \\\\(x\\\\):\n"
    "\n"
    " \\\\[\n"
    "     n = \\log_y(x)\n"
    " \\\\]\n"
    "\n"
    " If \\\\(n\\\\) is an integer (i.e., it has no fractional part), then \\\\(x\\\\) is a power of \\\\(y\\\\)\n"
    " and `True` is returned. Otherwise `False` is returned.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       // Check if 4 is a power of 2 (it is)\n"
    "       maths.is_power(4, 2)\n"
    "       |> should.equal(True)\n"
    "\n"
    "       // Check if 5 is a power of 2 (it is not)\n"
    "       maths.is_power(5, 2)\n"
    "       |> should.equal(False)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec is_power(integer(), integer()) -> boolean().
is_power(X, Y) ->
    case logarithm(erlang:float(X), erlang:float(Y)) of
        {ok, Value} ->
            Truncated = round_to_zero(Value, 0),
            Remainder = Value - Truncated,
            Remainder =:= +0.0;

        {error, _} ->
            false
    end.

-file("src/gleam_community/maths.gleam", 5667).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A function that tests whether a given integer value \\\\(n \\in \\mathbb{Z}\\\\) is a\n"
    " perfect number. A number is perfect if it is equal to the sum of its proper\n"
    " positive divisors.\n"
    "\n"
    " <details>\n"
    "     <summary>Details</summary>\n"
    "\n"
    "   For example:\n"
    "   - \\\\(6\\\\) is a perfect number since the divisors of 6 are \\\\(1 + 2 + 3 = 6\\\\).\n"
    "   - \\\\(28\\\\) is a perfect number since the divisors of 28 are \\\\(1 + 2 + 4 + 7 + 14 = 28\\\\).\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.is_perfect(6)\n"
    "       |> should.equal(True)\n"
    "\n"
    "       maths.is_perfect(28)\n"
    "       |> should.equal(True)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec is_perfect(integer()) -> boolean().
is_perfect(N) ->
    gleam@int:sum(proper_divisors(N)) =:= N.

-file("src/gleam_community/maths.gleam", 5752).
-spec powmod_with_check(integer(), integer(), integer()) -> integer().
powmod_with_check(Base, Exponent, Modulus) ->
    case {Exponent, (Exponent rem 2) =:= 0} of
        {0, _} ->
            1;

        {_, true} ->
            X = powmod_with_check(Base, Exponent div 2, Modulus),
            case {case Modulus of
                    0 -> 0;
                    Gleam@denominator -> (X * X) rem Gleam@denominator
                end, (X /= 1) andalso (X /= (Modulus - 1))} of
                {1, true} ->
                    0;

                {_, _} ->
                    case Modulus of
                        0 -> 0;
                        Gleam@denominator@1 -> (X * X) rem Gleam@denominator@1
                    end
            end;

        {_, _} ->
            case Modulus of
                0 -> 0;
                Gleam@denominator@2 -> (Base * powmod_with_check(
                    Base,
                    Exponent - 1,
                    Modulus
                ))
                rem Gleam@denominator@2
            end
    end.

-file("src/gleam_community/maths.gleam", 5738).
-spec miller_rabin_test(integer(), integer()) -> boolean().
miller_rabin_test(N, K) ->
    case {N, K} of
        {_, 0} ->
            true;

        {_, _} ->
            Random_candidate = 2 + gleam@int:random(N - 2),
            case powmod_with_check(Random_candidate, N - 1, N) =:= 1 of
                true ->
                    miller_rabin_test(N, K - 1);

                false ->
                    false
            end
    end.

-file("src/gleam_community/maths.gleam", 5724).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A function that tests whether a given integer value \\\\(x \\in \\mathbb{Z}\\\\) is a\n"
    " prime number. A prime number is a natural number greater than 1 that has no\n"
    " positive divisors other than 1 and itself.\n"
    "\n"
    " The function uses the Miller-Rabin primality test to assess if \\\\(x\\\\) is prime.\n"
    " It is a probabilistic test, so it can mistakenly identify a composite number\n"
    " as prime. However, the probability of such errors decreases with more testing\n"
    " iterations (the function uses 64 iterations internally, which is typically\n"
    " more than sufficient). The Miller-Rabin test is particularly useful for large\n"
    " numbers.\n"
    "\n"
    " <details>\n"
    "     <summary>Details</summary>\n"
    "\n"
    "   Examples of prime numbers:\n"
    "   - \\\\(2\\\\) is a prime number since it has only two divisors: \\\\(1\\\\) and \\\\(2\\\\).\n"
    "   - \\\\(7\\\\) is a prime number since it has only two divisors: \\\\(1\\\\) and \\\\(7\\\\).\n"
    "   - \\\\(4\\\\) is not a prime number since it has divisors other than \\\\(1\\\\) and itself, such\n"
    "     as \\\\(2\\\\).\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.is_prime(2)\n"
    "       |> should.equal(True)\n"
    "\n"
    "       maths.is_prime(4)\n"
    "       |> should.equal(False)\n"
    "\n"
    "       // Test the 2nd Carmichael number\n"
    "       maths.is_prime(1105)\n"
    "       |> should.equal(False)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec is_prime(integer()) -> boolean().
is_prime(X) ->
    case X of
        X@1 when X@1 < 2 ->
            false;

        X@2 when X@2 =:= 2 ->
            true;

        _ ->
            miller_rabin_test(X, 64)
    end.

-file("src/gleam_community/maths.gleam", 5799).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A function that tests whether a given real number \\\\(x \\in \\mathbb{R}\\\\) is strictly\n"
    " between two other real numbers, \\\\(a,b \\in \\mathbb{R}\\\\), such that \\\\(a < x < b\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.is_between(5.5, 5.0, 6.0)\n"
    "       |> should.equal(True)\n"
    "\n"
    "       maths.is_between(5.0, 5.0, 6.0)\n"
    "       |> should.equal(False)\n"
    "\n"
    "       maths.is_between(6.0, 5.0, 6.0)\n"
    "       |> should.equal(False)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec is_between(float(), float(), float()) -> boolean().
is_between(X, Lower, Upper) ->
    (Lower < X) andalso (X < Upper).

-file("src/gleam_community/maths.gleam", 5842).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A function that tests whether a given integer \\\\(n \\in \\mathbb{Z}\\\\) is divisible by another\n"
    " integer \\\\(d \\in \\mathbb{Z}\\\\), such that \\\\(n \\mod d = 0\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Details</summary>\n"
    "\n"
    "   For example:\n"
    "   - \\\\(n = 10\\\\) is divisible by \\\\(d = 2\\\\) because \\\\(10 \\mod 2 = 0\\\\).\n"
    "   - \\\\(n = 7\\\\) is not divisible by \\\\(d = 3\\\\) because \\\\(7 \\mod 3 \\neq 0\\\\).\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.is_divisible(10, 2)\n"
    "       |> should.equal(True)\n"
    "\n"
    "       maths.is_divisible(7, 3)\n"
    "       |> should.equal(False)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec is_divisible(integer(), integer()) -> boolean().
is_divisible(N, D) ->
    (case D of
        0 -> 0;
        Gleam@denominator -> N rem Gleam@denominator
    end) =:= 0.

-file("src/gleam_community/maths.gleam", 5886).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " A function that tests whether a given integer \\\\(m \\in \\mathbb{Z}\\\\) is a multiple of another\n"
    " integer \\\\(k \\in \\mathbb{Z}\\\\), such that \\\\(m = k \\cdot q\\\\), with \\\\(q \\in \\mathbb{Z}\\\\).\n"
    "\n"
    " <details>\n"
    "     <summary>Details</summary>\n"
    "\n"
    "   For example:\n"
    "   - \\\\(m = 15\\\\) is a multiple of \\\\(k = 5\\\\) because \\\\(15 = 5 \\cdot 3\\\\).\n"
    "   - \\\\(m = 14\\\\) is not a multiple of \\\\(k = 5\\\\) because \\\\(\\frac{14}{5}\\\\) does not yield an\n"
    "     integer quotient.\n"
    "\n"
    " </details>\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       maths.is_multiple(15, 5)\n"
    "       |> should.equal(True)\n"
    "\n"
    "       maths.is_multiple(14, 5)\n"
    "       |> should.equal(False)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec is_multiple(integer(), integer()) -> boolean().
is_multiple(M, K) ->
    (case K of
        0 -> 0;
        Gleam@denominator -> M rem Gleam@denominator
    end) =:= 0.

-file("src/gleam_community/maths.gleam", 5928).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The error function.\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec erf(float()) -> float().
erf(X) ->
    A1 = 0.254829592,
    A2 = -0.284496736,
    A3 = 1.421413741,
    A4 = -1.453152027,
    A5 = 1.061405429,
    P = 0.3275911,
    Sign = sign(X),
    X@1 = gleam@float:absolute_value(X),
    T = case (1.0 + (P * X@1)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> 1.0 / Gleam@denominator
    end,
    Y = 1.0 - ((((((((((A5 * T) + A4) * T) + A3) * T) + A2) * T) + A1) * T) * exponential(
        (-1.0 * X@1) * X@1
    )),
    Sign * Y.

-file("src/gleam_community/maths.gleam", 6050).
-spec incomplete_gamma_sum(float(), float(), float(), float(), float()) -> float().
incomplete_gamma_sum(A, X, T, S, N) ->
    case T of
        +0.0 ->
            S;

        _ ->
            Ns = S + T,
            Nt = T * (case (A + N) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> X / Gleam@denominator
            end),
            incomplete_gamma_sum(A, X, Nt, Ns, N + 1.0)
    end.

-file("src/gleam_community/maths.gleam", 6030).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The lower incomplete gamma function over the real numbers.\n"
    "\n"
    " The implemented incomplete gamma function is evaluated through a power series\n"
    " expansion.\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec incomplete_gamma(float(), float()) -> {ok, float()} | {error, nil}.
incomplete_gamma(A, X) ->
    case (A > +0.0) andalso (X >= +0.0) of
        true ->
            _assert_subject = gleam@float:power(X, A),
            {ok, V} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"incomplete_gamma"/utf8>>,
                                line => 6038})
            end,
            {ok,
                (V * exponential(-1.0 * X)) * incomplete_gamma_sum(
                    A,
                    X,
                    case A of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> 1.0 / Gleam@denominator
                    end,
                    +0.0,
                    1.0
                )};

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 6130).
-spec do_step_range(float(), float(), integer(), list(float())) -> list(float()).
do_step_range(Current, Increment, Remaining_steps, Acc) ->
    case Remaining_steps of
        0 ->
            Acc;

        _ ->
            do_step_range(
                Current - Increment,
                Increment,
                Remaining_steps - 1,
                [Current | Acc]
            )
    end.

-file("src/gleam_community/maths.gleam", 6107).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function returns a list of evenly spaced values within a specified interval\n"
    " `[start, stop)` based on a given increment size.\n"
    "\n"
    " Note that if `increment > 0`, the sequence progresses from `start`  towards `stop`, while if\n"
    " `increment < 0`, the sequence progresses from `start` towards `stop` in reverse.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       maths.step_range(1.0, 5.0, 1.0)\n"
    "       |> should.equal([1.0, 2.0, 3.0, 4.0])\n"
    "\n"
    "       // No points returned since\n"
    "       // start is smaller than stop and the step is positive\n"
    "       maths.step_range(5.0, 1.0, 1.0)\n"
    "       |> should.equal([])\n"
    "\n"
    "       // Points returned since\n"
    "       // start smaller than stop but negative step\n"
    "       maths.step_range(5.0, 1.0, -1.0)\n"
    "       |> should.equal([5.0, 4.0, 3.0, 2.0])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec step_range(float(), float(), float()) -> list(float()).
step_range(Start, Stop, Increment) ->
    case ((Start >= Stop) andalso (Increment > +0.0)) orelse ((Start =< Stop)
    andalso (Increment < +0.0)) of
        true ->
            [];

        false ->
            Direction = case Start =< Stop of
                true ->
                    1.0;

                false ->
                    -1.0
            end,
            Increment_abs = gleam@float:absolute_value(Increment),
            Distance = gleam@float:absolute_value(Start - Stop),
            Steps = erlang:round(case Increment_abs of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> Distance / Gleam@denominator
                end),
            Adjusted_stop = Stop - (Increment_abs * Direction),
            do_step_range(Adjusted_stop, Increment_abs * Direction, Steps, [])
    end.

-file("src/gleam_community/maths.gleam", 6187).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function is similar to [`step_range`](#step_range) but instead returns a yielder\n"
    " (lazily evaluated sequence of elements). This function can be used whenever there is a need\n"
    " to generate a larger-than-usual sequence of elements.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/yielder.{Next, Done}\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let range = maths.yield_step_range(1.0, 2.5, 0.5)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(range)\n"
    "       should.equal(element, 1.0)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 1.5)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 2.0)\n"
    "\n"
    "       // We have generated 3 values over the interval [1.0, 2.5)\n"
    "       // in increments of 0.5, so the 4th will be 'Done'\n"
    "       should.equal(yielder.step(rest), Done)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec yield_step_range(float(), float(), float()) -> gleam@yielder:yielder(float()).
yield_step_range(Start, Stop, Increment) ->
    case ((Start >= Stop) andalso (Increment > +0.0)) orelse ((Start =< Stop)
    andalso (Increment < +0.0)) of
        true ->
            gleam@yielder:empty();

        false ->
            Direction = case Start =< Stop of
                true ->
                    1.0;

                false ->
                    -1.0
            end,
            Increment_abs = gleam@float:absolute_value(Increment),
            Distance = gleam@float:absolute_value(Start - Stop),
            Num = erlang:round(case Increment_abs of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> Distance / Gleam@denominator
                end),
            gleam@yielder:map(
                gleam@yielder:range(0, Num - 1),
                fun(Index) ->
                    Start + ((erlang:float(Index) * Increment_abs) * Direction)
                end
            )
    end.

-file("src/gleam_community/maths.gleam", 6281).
-spec do_linear_space(float(), float(), integer(), list(float())) -> list(float()).
do_linear_space(Current, Increment, Remaining_steps, Acc) ->
    case Remaining_steps of
        0 ->
            Acc;

        _ ->
            do_linear_space(
                Current - Increment,
                Increment,
                Remaining_steps - 1,
                [Current | Acc]
            )
    end.

-file("src/gleam_community/maths.gleam", 6251).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function returns a list of linearly spaced points over a specified\n"
    " interval. The endpoint of the interval can optionally be included/excluded. The number of\n"
    " points and whether the endpoint is included determine the spacing between values.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "       let assert Ok(linspace) = maths.linear_space(10.0, 20.0, 5, True)\n"
    "       let pairs = linspace |> list.zip([10.0, 12.5, 15.0, 17.5, 20.0])\n"
    "       let assert Ok(result) = maths.all_close(pairs, 0.0, tolerance)\n"
    "       result\n"
    "       |> list.all(fn(x) { x == True })\n"
    "       |> should.be_true()\n"
    "\n"
    "       // A negative number of points (-5) does not work\n"
    "       maths.linear_space(10.0, 50.0, -5, True)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec linear_space(float(), float(), integer(), boolean()) -> {ok,
        list(float())} |
    {error, nil}.
linear_space(Start, Stop, Steps, Endpoint) ->
    Direction = case Start =< Stop of
        true ->
            1.0;

        false ->
            -1.0
    end,
    Increment_abs = case Endpoint of
        true ->
            case erlang:float(Steps - 1) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> gleam@float:absolute_value(Start - Stop) / Gleam@denominator
            end;

        false ->
            case erlang:float(Steps) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> gleam@float:absolute_value(Start - Stop)
                / Gleam@denominator@1
            end
    end,
    Adjusted_stop = case Endpoint of
        true ->
            Stop;

        false ->
            Stop - (Increment_abs * Direction)
    end,
    case Steps > 0 of
        true ->
            {ok,
                do_linear_space(
                    Adjusted_stop,
                    Increment_abs * Direction,
                    Steps,
                    []
                )};

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 6343).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function is similar to [`linear_space`](#linear_space) but instead returns a yielder\n"
    " (lazily evaluated sequence of elements). This function can be used whenever there is a need\n"
    " to generate a larger-than-usual sequence of elements.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/yielder.{Next, Done}\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let assert Ok(linspace) = maths.yield_linear_space(10.0, 20.0, 5, True)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(linspace)\n"
    "       should.equal(element, 10.0)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 12.5)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 15.0)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 17.5)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 20.0)\n"
    "\n"
    "       // We have generated 5 values, so the 6th will be 'Done'\n"
    "       should.equal(yielder.step(rest), Done)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec yield_linear_space(float(), float(), integer(), boolean()) -> {ok,
        gleam@yielder:yielder(float())} |
    {error, nil}.
yield_linear_space(Start, Stop, Steps, Endpoint) ->
    Direction = case Start =< Stop of
        true ->
            1.0;

        false ->
            -1.0
    end,
    Increment = case Endpoint of
        true ->
            case erlang:float(Steps - 1) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> gleam@float:absolute_value(Start - Stop) / Gleam@denominator
            end;

        false ->
            case erlang:float(Steps) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> gleam@float:absolute_value(Start - Stop)
                / Gleam@denominator@1
            end
    end,
    case Steps > 0 of
        false ->
            {error, nil};

        true ->
            {ok,
                begin
                    gleam@yielder:map(
                        gleam@yielder:range(0, Steps - 1),
                        fun(Index) ->
                            Start + ((erlang:float(Index) * Increment) * Direction)
                        end
                    )
                end}
    end.

-file("src/gleam_community/maths.gleam", 6414).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function returns a list of logarithmically spaced points over a specified\n"
    " interval. The endpoint of the interval can optionally be included/excluded.\n"
    " The number of points, base, and whether the endpoint is included determine\n"
    " the spacing between values.\n"
    "\n"
    " The values in the sequence are computed as powers of the given base, where\n"
    " the exponents are evenly spaced between `start` and `stop`. The `base`\n"
    " parameter must be positive, as negative bases lead to undefined behavior when\n"
    " computing fractional exponents. Similarly, the number of points (`steps`) must\n"
    " be positive; specifying zero or a negative value will result in an error.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "       let assert Ok(logspace) = maths.logarithmic_space(1.0, 3.0, 3, True, 10.0)\n"
    "       let pairs = logspace |> list.zip([10.0, 100.0, 1000.0])\n"
    "       let assert Ok(result) = maths.all_close(pairs, 0.0, tolerance)\n"
    "       result\n"
    "       |> list.all(fn(x) { x == True })\n"
    "       |> should.be_true()\n"
    "\n"
    "       // A negative number of points (-3) does not work\n"
    "       maths.logarithmic_space(1.0, 3.0, -3, False, 10.0)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec logarithmic_space(float(), float(), integer(), boolean(), float()) -> {ok,
        list(float())} |
    {error, nil}.
logarithmic_space(Start, Stop, Steps, Endpoint, Base) ->
    case (Steps > 0) andalso (Base > +0.0) of
        true ->
            _assert_subject = linear_space(Start, Stop, Steps, Endpoint),
            {ok, Linspace} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"logarithmic_space"/utf8>>,
                                line => 6425})
            end,
            {ok,
                begin
                    gleam@list:map(
                        Linspace,
                        fun(Value) ->
                            _assert_subject@1 = gleam@float:power(Base, Value),
                            {ok, Result} = case _assert_subject@1 of
                                {ok, _} -> _assert_subject@1;
                                _assert_fail@1 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                value => _assert_fail@1,
                                                module => <<"gleam_community/maths"/utf8>>,
                                                function => <<"logarithmic_space"/utf8>>,
                                                line => 6437})
                            end,
                            Result
                        end
                    )
                end};

        false ->
            {error, nil}
    end.

-file("src/gleam_community/maths.gleam", 6487).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function is similar to [`logarithmic_space`](#logarithmic_space) but instead returns a yielder\n"
    " (lazily evaluated sequence of elements). This function can be used whenever there is a need\n"
    " to generate a larger-than-usual sequence of elements.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/yielder.{Next, Done}\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let assert Ok(logspace) =\n"
    "         maths.yield_logarithmic_space(1.0, 3.0, 3, True, 10.0)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(logspace)\n"
    "       should.equal(element, 10.0)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 100.0)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 1000.0)\n"
    "\n"
    "       // We have generated 3 values, so the 4th will be 'Done'\n"
    "       should.equal(yielder.step(rest), Done)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec yield_logarithmic_space(float(), float(), integer(), boolean(), float()) -> {ok,
        gleam@yielder:yielder(float())} |
    {error, nil}.
yield_logarithmic_space(Start, Stop, Steps, Endpoint, Base) ->
    case (Steps > 0) andalso (Base > +0.0) of
        false ->
            {error, nil};

        true ->
            _assert_subject = yield_linear_space(Start, Stop, Steps, Endpoint),
            {ok, Linspace} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"yield_logarithmic_space"/utf8>>,
                                line => 6499})
            end,
            {ok,
                begin
                    gleam@yielder:map(
                        Linspace,
                        fun(Value) ->
                            _assert_subject@1 = gleam@float:power(Base, Value),
                            {ok, Result} = case _assert_subject@1 of
                                {ok, _} -> _assert_subject@1;
                                _assert_fail@1 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                value => _assert_fail@1,
                                                module => <<"gleam_community/maths"/utf8>>,
                                                function => <<"yield_logarithmic_space"/utf8>>,
                                                line => 6511})
                            end,
                            Result
                        end
                    )
                end}
    end.

-file("src/gleam_community/maths.gleam", 6574).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function returns a list of a geometric progression between two specified\n"
    " values, where each value is a constant multiple of the previous one. Unlike\n"
    " [`logarithmic_space`](#logarithmic_space), this function allows specifying the starting\n"
    " and ending values (`start` and `stop`) directly, without requiring them to be transformed\n"
    " into exponents.\n"
    "\n"
    " Internally, the function computes the logarithms of `start` and `stop` and generates evenly\n"
    " spaced points in the logarithmic domain (using base 10). These points are then transformed back\n"
    " into their original scale to create a sequence of values that grow multiplicatively.\n"
    "\n"
    " The `start` and `stop` values must be positive, as logarithms are undefined for non-positive\n"
    " values. The number of points (`steps`) must also be positive; specifying zero or a negative\n"
    " value will result in an error.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/yielder\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let assert Ok(tolerance) = float.power(10.0, -6.0)\n"
    "       let assert Ok(logspace) = maths.geometric_space(10.0, 1000.0, 3, True)\n"
    "       let pairs = logspace |> list.zip([10.0, 100.0, 1000.0])\n"
    "       let assert Ok(result) = maths.all_close(pairs, 0.0, tolerance)\n"
    "       result\n"
    "       |> list.all(fn(x) { x == True })\n"
    "       |> should.be_true()\n"
    "\n"
    "       // Input (start and stop can't be less than or equal to 0.0)\n"
    "       maths.geometric_space(0.0, 1000.0, 3, False)\n"
    "       |> should.be_error()\n"
    "\n"
    "       maths.geometric_space(-1000.0, 0.0, 3, False)\n"
    "       |> should.be_error()\n"
    "\n"
    "       // A negative number of points (-3) does not work\n"
    "       maths.geometric_space(10.0, 1000.0, -3, False)\n"
    "       |> should.be_error()\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec geometric_space(float(), float(), integer(), boolean()) -> {ok,
        list(float())} |
    {error, nil}.
geometric_space(Start, Stop, Steps, Endpoint) ->
    case ((Start =< +0.0) orelse (Stop =< +0.0)) orelse (Steps < 0) of
        true ->
            {error, nil};

        false ->
            _assert_subject = logarithm_10(Start),
            {ok, Log_start} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"geometric_space"/utf8>>,
                                line => 6586})
            end,
            _assert_subject@1 = logarithm_10(Stop),
            {ok, Log_stop} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"geometric_space"/utf8>>,
                                line => 6587})
            end,
            logarithmic_space(Log_start, Log_stop, Steps, Endpoint, 10.0)
    end.

-file("src/gleam_community/maths.gleam", 6634).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function is similar to [`geometric_space`](#geometric_space) but instead returns a yielder\n"
    " (lazily evaluated sequence of elements). This function can be used whenever there is a need\n"
    " to generate a larger-than-usual sequence of elements.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/yielder.{Next, Done}\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example () {\n"
    "       let assert Ok(logspace) = maths.yield_geometric_space(10.0, 1000.0, 3, True)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(logspace)\n"
    "       should.equal(element, 10.0)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 100.0)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 1000.0)\n"
    "\n"
    "       // We have generated 3 values, so the 4th will be 'Done'\n"
    "       should.equal(yielder.step(rest), Done)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec yield_geometric_space(float(), float(), integer(), boolean()) -> {ok,
        gleam@yielder:yielder(float())} |
    {error, nil}.
yield_geometric_space(Start, Stop, Steps, Endpoint) ->
    case ((Start =< +0.0) orelse (Stop =< +0.0)) orelse (Steps < 0) of
        true ->
            {error, nil};

        false ->
            _assert_subject = logarithm_10(Start),
            {ok, Log_start} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"yield_geometric_space"/utf8>>,
                                line => 6646})
            end,
            _assert_subject@1 = logarithm_10(Stop),
            {ok, Log_stop} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"yield_geometric_space"/utf8>>,
                                line => 6647})
            end,
            yield_logarithmic_space(Log_start, Log_stop, Steps, Endpoint, 10.0)
    end.

-file("src/gleam_community/maths.gleam", 6687).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " Generates evenly spaced points around a center value. The total span (around the center value)\n"
    " is determined by the `radius` argument of the function.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       let assert Ok(symspace) = maths.symmetric_space(0.0, 5.0, 5)\n"
    "       symspace\n"
    "       |> should.equal([-5.0, -2.5, 0.0, 2.5, 5.0])\n"
    "\n"
    "       // A negative radius reverses the order of the values\n"
    "       let assert Ok(symspace) = maths.symmetric_space(0.0, -5.0, 5)\n"
    "       symspace\n"
    "       |> should.equal([5.0, 2.5, 0.0, -2.5, -5.0])\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec symmetric_space(float(), float(), integer()) -> {ok, list(float())} |
    {error, nil}.
symmetric_space(Center, Radius, Steps) ->
    case Steps > 0 of
        false ->
            {error, nil};

        true ->
            Start = Center - Radius,
            Stop = Center + Radius,
            linear_space(Start, Stop, Steps, true)
    end.

-file("src/gleam_community/maths.gleam", 6749).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The function is similar to [`symmetric_space`](#symmetric_space) but instead returns a yielder\n"
    " (lazily evaluated sequence of elements). This function can be used whenever there is a need\n"
    " to generate a larger-than-usual sequence of elements.\n"
    "\n"
    " <details>\n"
    "     <summary>Example:</summary>\n"
    "\n"
    "     import gleam/yielder.{Next, Done}\n"
    "     import gleeunit/should\n"
    "     import gleam_community/maths\n"
    "\n"
    "     pub fn example() {\n"
    "       let assert Ok(symspace) = maths.yield_symmetric_space(0.0, 5.0, 5)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(symspace)\n"
    "       should.equal(element, -5.0)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, -2.5)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 0.0)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 2.5)\n"
    "\n"
    "       let assert Next(element, rest) = yielder.step(rest)\n"
    "       should.equal(element, 5.0)\n"
    "\n"
    "       // We have generated 5 values, so the 6th will be 'Done'\n"
    "       should.equal(yielder.step(rest), Done)\n"
    "     }\n"
    " </details>\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec yield_symmetric_space(float(), float(), integer()) -> {ok,
        gleam@yielder:yielder(float())} |
    {error, nil}.
yield_symmetric_space(Center, Radius, Steps) ->
    case Steps > 0 of
        false ->
            {error, nil};

        true ->
            Start = Center - Radius,
            Stop = Center + Radius,
            yield_linear_space(Start, Stop, Steps, true)
    end.

-file("src/gleam_community/maths.gleam", 5986).
?DOC(
    " Compute the Gamma function using an approximation with the same coefficients used by the GNU\n"
    " Scientific Library. The function handles both the reflection formula for `x < 0.5` and the\n"
    " standard Lanczos computation for `x >= 0.5`.\n"
).
-spec gamma_lanczos(float()) -> float().
gamma_lanczos(X) ->
    case X < 0.5 of
        true ->
            case (sin(pi() * X) * gamma_lanczos(1.0 - X)) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> pi() / Gleam@denominator
            end;

        false ->
            Z = X - 1.0,
            X@1 = gleam@list:index_fold(
                [0.99999999999980993,
                    676.5203681218851,
                    -1259.1392167224028,
                    771.32342877765313,
                    -176.61502916214059,
                    12.507343278686905,
                    -0.13857109526572012,
                    0.0000099843695780195716,
                    0.00000015056327351493116],
                +0.0,
                fun(Acc, V, Index) -> case Index > 0 of
                        true ->
                            Acc + (case (Z + erlang:float(Index)) of
                                +0.0 -> +0.0;
                                -0.0 -> -0.0;
                                Gleam@denominator@1 -> V / Gleam@denominator@1
                            end);

                        false ->
                            V
                    end end
            ),
            T = (Z + 7.0) + 0.5,
            _assert_subject = gleam@float:power(2.0 * pi(), 0.5),
            {ok, V1} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"gamma_lanczos"/utf8>>,
                                line => 6006})
            end,
            _assert_subject@1 = gleam@float:power(T, Z + 0.5),
            {ok, V2} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"gleam_community/maths"/utf8>>,
                                function => <<"gamma_lanczos"/utf8>>,
                                line => 6007})
            end,
            ((V1 * V2) * exponential(-1.0 * T)) * X@1
    end.

-file("src/gleam_community/maths.gleam", 5968).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The gamma function over the real numbers. The function is essentially equal to\n"
    " the factorial for any positive integer argument: \\\\(\\Gamma(n) = (n - 1)!\\\\)\n"
    "\n"
    " The implemented gamma function is approximated through Lanczos approximation\n"
    " using the same coefficients used by the GNU Scientific Library.\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec gamma(float()) -> float().
gamma(X) ->
    gamma_lanczos(X).

-file("src/gleam_community/maths.gleam", 5910).
?DOC(
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"https://github.com/gleam-community/maths/issues\">\n"
    "         <small>Spot a typo? Open an issue!</small>\n"
    "     </a>\n"
    " </div>\n"
    "\n"
    " The beta function over the real numbers:\n"
    "\n"
    " \\\\[\n"
    " \\text{B}(x, y) = \\frac{\\Gamma(x) \\cdot \\Gamma(y)}{\\Gamma(x + y)}\n"
    " \\\\]\n"
    "\n"
    " The beta function is evaluated through the use of the gamma function.\n"
    "\n"
    " <div style=\"text-align: right;\">\n"
    "     <a href=\"#\">\n"
    "         <small>Back to top ↑</small>\n"
    "     </a>\n"
    " </div>\n"
).
-spec beta(float(), float()) -> float().
beta(X, Y) ->
    case gamma(X + Y) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> gamma(X) * gamma(Y) / Gleam@denominator
    end.
