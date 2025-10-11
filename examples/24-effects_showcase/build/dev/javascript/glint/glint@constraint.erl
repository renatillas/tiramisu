-module(glint@constraint).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([one_of/1, none_of/1, each/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/glint/constraint.gleam", 21).
?DOC(
    " Returns a Constraint that ensures the parsed flag value is one of the allowed values.\n"
    "\n"
    " ```gleam\n"
    " import glint\n"
    " import glint/constraint\n"
    " ...\n"
    " glint.int_flag(\"my_flag\")\n"
    " |> glint.constraint(constraint.one_of([1, 2, 3, 4]))\n"
    " ```\n"
).
-spec one_of(list(LYQ)) -> fun((LYQ) -> {ok, LYQ} | {error, snag:snag()}).
one_of(Allowed) ->
    Allowed_set = gleam@set:from_list(Allowed),
    fun(Val) -> case gleam@set:contains(Allowed_set, Val) of
            true ->
                {ok, Val};

            false ->
                snag:error(
                    <<<<<<<<"invalid value '"/utf8,
                                    (gleam@string:inspect(Val))/binary>>/binary,
                                "', must be one of: ["/utf8>>/binary,
                            (begin
                                _pipe = Allowed,
                                _pipe@1 = gleam@list:map(
                                    _pipe,
                                    fun(A) ->
                                        <<<<"'"/utf8,
                                                (gleam@string:inspect(A))/binary>>/binary,
                                            "'"/utf8>>
                                    end
                                ),
                                gleam@string:join(_pipe@1, <<", "/utf8>>)
                            end)/binary>>/binary,
                        "]"/utf8>>
                )
        end end.

-file("src/glint/constraint.gleam", 52).
?DOC(
    " Returns a Constraint that ensures the parsed flag value is not one of the disallowed values.\n"
    "\n"
    " ```gleam\n"
    " import glint\n"
    " import glint/constraint\n"
    " ...\n"
    " glint.int_flag(\"my_flag\")\n"
    " |> glint.constraint(constraint.none_of([1, 2, 3, 4]))\n"
    " ```\n"
).
-spec none_of(list(LYT)) -> fun((LYT) -> {ok, LYT} | {error, snag:snag()}).
none_of(Disallowed) ->
    Disallowed_set = gleam@set:from_list(Disallowed),
    fun(Val) -> case gleam@set:contains(Disallowed_set, Val) of
            false ->
                {ok, Val};

            true ->
                snag:error(
                    <<<<<<"invalid value '"/utf8,
                                (gleam@string:inspect(Val))/binary>>/binary,
                            "', must not be one of: ["/utf8>>/binary,
                        (((<<(begin
                                _pipe = Disallowed,
                                _pipe@1 = gleam@list:map(
                                    _pipe,
                                    fun(A) ->
                                        <<<<"'"/utf8,
                                                (gleam@string:inspect(A))/binary>>/binary,
                                            "'"/utf8>>
                                    end
                                ),
                                gleam@string:join(_pipe@1, <<", "/utf8>>)
                            end)/binary,
                            "]"/utf8>>)))/binary>>
                )
        end end.

-file("src/glint/constraint.gleam", 102).
?DOC(
    " This is a convenience function for applying a Constraint(a) to a List(a).\n"
    " This is useful because the default behaviour for constraints on lists is that they will apply to the list as a whole.\n"
    "\n"
    " For example, to apply one_of to all items in a `List(Int)`:\n"
    "\n"
    " Via `use`:\n"
    " ```gleam\n"
    " import glint\n"
    " import glint/constraint\n"
    " ...\n"
    " use li <- glint.flag_constraint(glint.int_flag(\"my_flag\"))\n"
    " use i <- constraint.each()\n"
    " i |> one_of([1, 2, 3, 4])\n"
    " ```\n"
    "\n"
    " via a pipe:\n"
    " ```gleam\n"
    " import glint\n"
    " import glint/constraint\n"
    " ...\n"
    " glint.int_flag(\"my_flag\")\n"
    " |> glint.flag_constraint(\n"
    "   constraint.one_of([1,2,3,4])\n"
    "   |> constraint.each\n"
    " )\n"
    " ```\n"
).
-spec each(fun((LYW) -> {ok, LYW} | {error, snag:snag()})) -> fun((list(LYW)) -> {ok,
        list(LYW)} |
    {error, snag:snag()}).
each(Constraint) ->
    fun(_capture) -> gleam@list:try_map(_capture, Constraint) end.
