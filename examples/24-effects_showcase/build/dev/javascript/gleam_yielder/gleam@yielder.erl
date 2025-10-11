-module(gleam@yielder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([unfold/2, repeatedly/1, repeat/1, from_list/1, transform/3, fold/3, run/1, to_list/1, step/1, take/2, drop/2, map/2, map2/3, append/2, flatten/1, concat/1, flat_map/2, filter/2, filter_map/2, cycle/1, find/2, find_map/2, index/1, iterate/2, take_while/2, drop_while/2, scan/3, zip/2, chunk/2, sized_chunk/2, intersperse/2, any/2, all/2, group/2, reduce/2, last/1, empty/0, once/1, range/2, single/1, interleave/2, fold_until/3, try_fold/3, first/1, at/2, length/1, each/2, yield/2, prepend/2]).
-export_type([action/1, yielder/1, step/2, chunk/2, sized_chunk/1]).

-type action(FWM) :: stop | {continue, FWM, fun(() -> action(FWM))}.

-opaque yielder(FWN) :: {yielder, fun(() -> action(FWN))}.

-type step(FWO, FWP) :: {next, FWO, FWP} | done.

-type chunk(FWQ, FWR) :: {another_by,
        list(FWQ),
        FWR,
        FWQ,
        fun(() -> action(FWQ))} |
    {last_by, list(FWQ)}.

-type sized_chunk(FWS) :: {another, list(FWS), fun(() -> action(FWS))} |
    {last, list(FWS)} |
    no_more.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 37).
-spec stop() -> action(any()).
stop() ->
    stop.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 72).
-spec unfold_loop(FXA, fun((FXA) -> step(FXB, FXA))) -> fun(() -> action(FXB)).
unfold_loop(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, unfold_loop(Acc, F)};

            done ->
                stop
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 62).
-spec unfold(FWV, fun((FWV) -> step(FWW, FWV))) -> yielder(FWW).
unfold(Initial, F) ->
    _pipe = Initial,
    _pipe@1 = unfold_loop(_pipe, F),
    {yielder, _pipe@1}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 94).
-spec repeatedly(fun(() -> FXF)) -> yielder(FXF).
repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 109).
-spec repeat(FXH) -> yielder(FXH).
repeat(X) ->
    repeatedly(fun() -> X end).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 123).
-spec from_list(list(FXJ)) -> yielder(FXJ).
from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 134).
-spec transform_loop(
    fun(() -> action(FXM)),
    FXO,
    fun((FXO, FXM) -> step(FXP, FXO))
) -> fun(() -> action(FXP)).
transform_loop(Continuation, State, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                case F(State, El) of
                    done ->
                        stop;

                    {next, Yield, Next_state} ->
                        {continue, Yield, transform_loop(Next, Next_state, F)}
                end
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 169).
-spec transform(yielder(FXT), FXV, fun((FXV, FXT) -> step(FXW, FXV))) -> yielder(FXW).
transform(Yielder, Initial, F) ->
    _pipe = transform_loop(erlang:element(2, Yielder), Initial, F),
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 204).
-spec fold_loop(fun(() -> action(FYD)), fun((FYF, FYD) -> FYF), FYF) -> FYF.
fold_loop(Continuation, F, Accumulator) ->
    case Continuation() of
        {continue, Elem, Next} ->
            fold_loop(Next, F, F(Accumulator, Elem));

        stop ->
            Accumulator
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 195).
-spec fold(yielder(FYA), FYC, fun((FYC, FYA) -> FYC)) -> FYC.
fold(Yielder, Initial, F) ->
    _pipe = erlang:element(2, Yielder),
    fold_loop(_pipe, F, Initial).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 220).
-spec run(yielder(any())) -> nil.
run(Yielder) ->
    fold(Yielder, nil, fun(_, _) -> nil end).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 238).
-spec to_list(yielder(FYI)) -> list(FYI).
to_list(Yielder) ->
    _pipe = Yielder,
    _pipe@1 = fold(_pipe, [], fun(Acc, E) -> [E | Acc] end),
    lists:reverse(_pipe@1).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 266).
-spec step(yielder(FYL)) -> step(FYL, yielder(FYL)).
step(Yielder) ->
    case (erlang:element(2, Yielder))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {yielder, A}}
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 299).
-spec take_loop(fun(() -> action(FYT)), integer()) -> fun(() -> action(FYT)).
take_loop(Continuation, Desired) ->
    fun() -> case Desired > 0 of
            false ->
                stop;

            true ->
                case Continuation() of
                    stop ->
                        stop;

                    {continue, E, Next} ->
                        {continue, E, take_loop(Next, Desired - 1)}
                end
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 293).
-spec take(yielder(FYQ), integer()) -> yielder(FYQ).
take(Yielder, Desired) ->
    _pipe = erlang:element(2, Yielder),
    _pipe@1 = take_loop(_pipe, Desired),
    {yielder, _pipe@1}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 342).
-spec drop_loop(fun(() -> action(FYZ)), integer()) -> action(FYZ).
drop_loop(Continuation, Desired) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Desired > 0 of
                true ->
                    drop_loop(Next, Desired - 1);

                false ->
                    {continue, E, Next}
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 337).
-spec drop(yielder(FYW), integer()) -> yielder(FYW).
drop(Yielder, Desired) ->
    _pipe = fun() -> drop_loop(erlang:element(2, Yielder), Desired) end,
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 376).
-spec map_loop(fun(() -> action(FZG)), fun((FZG) -> FZI)) -> fun(() -> action(FZI)).
map_loop(Continuation, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, F(E), map_loop(Continuation@1, F)}
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 370).
-spec map(yielder(FZC), fun((FZC) -> FZE)) -> yielder(FZE).
map(Yielder, F) ->
    _pipe = erlang:element(2, Yielder),
    _pipe@1 = map_loop(_pipe, F),
    {yielder, _pipe@1}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 417).
-spec map2_loop(
    fun(() -> action(FZQ)),
    fun(() -> action(FZS)),
    fun((FZQ, FZS) -> FZU)
) -> fun(() -> action(FZU)).
map2_loop(Continuation1, Continuation2, Fun) ->
    fun() -> case Continuation1() of
            stop ->
                stop;

            {continue, A, Next_a} ->
                case Continuation2() of
                    stop ->
                        stop;

                    {continue, B, Next_b} ->
                        {continue, Fun(A, B), map2_loop(Next_a, Next_b, Fun)}
                end
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 408).
-spec map2(yielder(FZK), yielder(FZM), fun((FZK, FZM) -> FZO)) -> yielder(FZO).
map2(Yielder1, Yielder2, Fun) ->
    _pipe = map2_loop(
        erlang:element(2, Yielder1),
        erlang:element(2, Yielder2),
        Fun
    ),
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 454).
-spec append_loop(fun(() -> action(GAA)), fun(() -> action(GAA))) -> action(GAA).
append_loop(First, Second) ->
    case First() of
        {continue, E, First@1} ->
            {continue, E, fun() -> append_loop(First@1, Second) end};

        stop ->
            Second()
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 449).
-spec append(yielder(FZW), yielder(FZW)) -> yielder(FZW).
append(First, Second) ->
    _pipe = fun() ->
        append_loop(erlang:element(2, First), erlang:element(2, Second))
    end,
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 481).
-spec flatten_loop(fun(() -> action(yielder(GAI)))) -> action(GAI).
flatten_loop(Flattened) ->
    case Flattened() of
        stop ->
            stop;

        {continue, It, Next_yielder} ->
            append_loop(
                erlang:element(2, It),
                fun() -> flatten_loop(Next_yielder) end
            )
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 476).
-spec flatten(yielder(yielder(GAE))) -> yielder(GAE).
flatten(Yielder) ->
    _pipe = fun() -> flatten_loop(erlang:element(2, Yielder)) end,
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 504).
-spec concat(list(yielder(GAM))) -> yielder(GAM).
concat(Yielders) ->
    flatten(from_list(Yielders)).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 526).
-spec flat_map(yielder(GAQ), fun((GAQ) -> yielder(GAS))) -> yielder(GAS).
flat_map(Yielder, F) ->
    _pipe = Yielder,
    _pipe@1 = map(_pipe, F),
    flatten(_pipe@1).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 562).
-spec filter_loop(fun(() -> action(GAY)), fun((GAY) -> boolean())) -> action(GAY).
filter_loop(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Yielder} ->
            case Predicate(E) of
                true ->
                    {continue, E, fun() -> filter_loop(Yielder, Predicate) end};

                false ->
                    filter_loop(Yielder, Predicate)
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 554).
-spec filter(yielder(GAV), fun((GAV) -> boolean())) -> yielder(GAV).
filter(Yielder, Predicate) ->
    _pipe = fun() -> filter_loop(erlang:element(2, Yielder), Predicate) end,
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 606).
-spec filter_map_loop(
    fun(() -> action(GBI)),
    fun((GBI) -> {ok, GBK} | {error, any()})
) -> action(GBK).
filter_map_loop(Continuation, F) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {continue, E@1, fun() -> filter_map_loop(Next, F) end};

                {error, _} ->
                    filter_map_loop(Next, F)
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 598).
-spec filter_map(yielder(GBB), fun((GBB) -> {ok, GBD} | {error, any()})) -> yielder(GBD).
filter_map(Yielder, F) ->
    _pipe = fun() -> filter_map_loop(erlang:element(2, Yielder), F) end,
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 632).
-spec cycle(yielder(GBP)) -> yielder(GBP).
cycle(Yielder) ->
    _pipe = repeat(Yielder),
    flatten(_pipe).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 709).
-spec find_loop(fun(() -> action(GBX)), fun((GBX) -> boolean())) -> {ok, GBX} |
    {error, nil}.
find_loop(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                true ->
                    {ok, E};

                false ->
                    find_loop(Next, F)
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 701).
-spec find(yielder(GBT), fun((GBT) -> boolean())) -> {ok, GBT} | {error, nil}.
find(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    find_loop(_pipe, Is_desired).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 754).
-spec find_map_loop(
    fun(() -> action(GCJ)),
    fun((GCJ) -> {ok, GCL} | {error, any()})
) -> {ok, GCL} | {error, nil}.
find_map_loop(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {ok, E@1};

                {error, _} ->
                    find_map_loop(Next, F)
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 746).
-spec find_map(yielder(GCB), fun((GCB) -> {ok, GCD} | {error, any()})) -> {ok,
        GCD} |
    {error, nil}.
find_map(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    find_map_loop(_pipe, Is_desired).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 783).
-spec index_loop(fun(() -> action(GCU)), integer()) -> fun(() -> action({GCU,
    integer()})).
index_loop(Continuation, Next) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, {E, Next}, index_loop(Continuation@1, Next + 1)}
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 777).
-spec index(yielder(GCR)) -> yielder({GCR, integer()}).
index(Yielder) ->
    _pipe = erlang:element(2, Yielder),
    _pipe@1 = index_loop(_pipe, 0),
    {yielder, _pipe@1}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 805).
-spec iterate(GCX, fun((GCX) -> GCX)) -> yielder(GCX).
iterate(Initial, F) ->
    unfold(Initial, fun(Element) -> {next, Element, F(Element)} end).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 832).
-spec take_while_loop(fun(() -> action(GDC)), fun((GDC) -> boolean())) -> fun(() -> action(GDC)).
take_while_loop(Continuation, Predicate) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Next} ->
                case Predicate(E) of
                    false ->
                        stop;

                    true ->
                        {continue, E, take_while_loop(Next, Predicate)}
                end
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 823).
-spec take_while(yielder(GCZ), fun((GCZ) -> boolean())) -> yielder(GCZ).
take_while(Yielder, Predicate) ->
    _pipe = erlang:element(2, Yielder),
    _pipe@1 = take_while_loop(_pipe, Predicate),
    {yielder, _pipe@1}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 868).
-spec drop_while_loop(fun(() -> action(GDI)), fun((GDI) -> boolean())) -> action(GDI).
drop_while_loop(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Predicate(E) of
                false ->
                    {continue, E, Next};

                true ->
                    drop_while_loop(Next, Predicate)
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 860).
-spec drop_while(yielder(GDF), fun((GDF) -> boolean())) -> yielder(GDF).
drop_while(Yielder, Predicate) ->
    _pipe = fun() -> drop_while_loop(erlang:element(2, Yielder), Predicate) end,
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 906).
-spec scan_loop(fun(() -> action(GDP)), fun((GDR, GDP) -> GDR), GDR) -> fun(() -> action(GDR)).
scan_loop(Continuation, F, Accumulator) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                Accumulated = F(Accumulator, El),
                {continue, Accumulated, scan_loop(Next, F, Accumulated)}
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 896).
-spec scan(yielder(GDL), GDN, fun((GDN, GDL) -> GDN)) -> yielder(GDN).
scan(Yielder, Initial, F) ->
    _pipe = erlang:element(2, Yielder),
    _pipe@1 = scan_loop(_pipe, F, Initial),
    {yielder, _pipe@1}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 939).
-spec zip_loop(fun(() -> action(GDY)), fun(() -> action(GEA))) -> fun(() -> action({GDY,
    GEA})).
zip_loop(Left, Right) ->
    fun() -> case Left() of
            stop ->
                stop;

            {continue, El_left, Next_left} ->
                case Right() of
                    stop ->
                        stop;

                    {continue, El_right, Next_right} ->
                        {continue,
                            {El_left, El_right},
                            zip_loop(Next_left, Next_right)}
                end
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 934).
-spec zip(yielder(GDT), yielder(GDV)) -> yielder({GDT, GDV}).
zip(Left, Right) ->
    _pipe = zip_loop(erlang:element(2, Left), erlang:element(2, Right)),
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1000).
-spec next_chunk(fun(() -> action(GEN)), fun((GEN) -> GEP), GEP, list(GEN)) -> chunk(GEN, GEP).
next_chunk(Continuation, F, Previous_key, Current_chunk) ->
    case Continuation() of
        stop ->
            {last_by, lists:reverse(Current_chunk)};

        {continue, E, Next} ->
            Key = F(E),
            case Key =:= Previous_key of
                true ->
                    next_chunk(Next, F, Key, [E | Current_chunk]);

                false ->
                    {another_by, lists:reverse(Current_chunk), Key, E, Next}
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 987).
-spec chunk_loop(fun(() -> action(GEI)), fun((GEI) -> GEK), GEK, GEI) -> action(list(GEI)).
chunk_loop(Continuation, F, Previous_key, Previous_element) ->
    case next_chunk(Continuation, F, Previous_key, [Previous_element]) of
        {last_by, Chunk} ->
            {continue, Chunk, fun stop/0};

        {another_by, Chunk@1, Key, El, Next} ->
            {continue, Chunk@1, fun() -> chunk_loop(Next, F, Key, El) end}
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 974).
-spec chunk(yielder(GED), fun((GED) -> any())) -> yielder(list(GED)).
chunk(Yielder, F) ->
    _pipe = fun() -> case (erlang:element(2, Yielder))() of
            stop ->
                stop;

            {continue, E, Next} ->
                chunk_loop(Next, F, F(E), E)
        end end,
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1071).
-spec next_sized_chunk(fun(() -> action(GFB)), integer(), list(GFB)) -> sized_chunk(GFB).
next_sized_chunk(Continuation, Left, Current_chunk) ->
    case Continuation() of
        stop ->
            case Current_chunk of
                [] ->
                    no_more;

                Remaining ->
                    {last, lists:reverse(Remaining)}
            end;

        {continue, E, Next} ->
            Chunk = [E | Current_chunk],
            case Left > 1 of
                false ->
                    {another, lists:reverse(Chunk), Next};

                true ->
                    next_sized_chunk(Next, Left - 1, Chunk)
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1050).
-spec sized_chunk_loop(fun(() -> action(GEX)), integer()) -> fun(() -> action(list(GEX))).
sized_chunk_loop(Continuation, Count) ->
    fun() -> case next_sized_chunk(Continuation, Count, []) of
            no_more ->
                stop;

            {last, Chunk} ->
                {continue, Chunk, fun stop/0};

            {another, Chunk@1, Next_element} ->
                {continue, Chunk@1, sized_chunk_loop(Next_element, Count)}
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1041).
-spec sized_chunk(yielder(GET), integer()) -> yielder(list(GET)).
sized_chunk(Yielder, Count) ->
    _pipe = erlang:element(2, Yielder),
    _pipe@1 = sized_chunk_loop(_pipe, Count),
    {yielder, _pipe@1}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1131).
-spec intersperse_loop(fun(() -> action(GFI)), GFI) -> action(GFI).
intersperse_loop(Continuation, Separator) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            Next_interspersed = fun() -> intersperse_loop(Next, Separator) end,
            {continue, Separator, fun() -> {continue, E, Next_interspersed} end}
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1118).
-spec intersperse(yielder(GFF), GFF) -> yielder(GFF).
intersperse(Yielder, Elem) ->
    _pipe = fun() -> case (erlang:element(2, Yielder))() of
            stop ->
                stop;

            {continue, E, Next} ->
                {continue, E, fun() -> intersperse_loop(Next, Elem) end}
        end end,
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1179).
-spec any_loop(fun(() -> action(GFN)), fun((GFN) -> boolean())) -> boolean().
any_loop(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            false;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    true;

                false ->
                    any_loop(Next, Predicate)
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1171).
-spec any(yielder(GFL), fun((GFL) -> boolean())) -> boolean().
any(Yielder, Predicate) ->
    _pipe = erlang:element(2, Yielder),
    any_loop(_pipe, Predicate).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1228).
-spec all_loop(fun(() -> action(GFR)), fun((GFR) -> boolean())) -> boolean().
all_loop(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            true;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    all_loop(Next, Predicate);

                false ->
                    false
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1220).
-spec all(yielder(GFP), fun((GFP) -> boolean())) -> boolean().
all(Yielder, Predicate) ->
    _pipe = erlang:element(2, Yielder),
    all_loop(_pipe, Predicate).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1273).
-spec update_group_with(GGH) -> fun((gleam@option:option(list(GGH))) -> list(GGH)).
update_group_with(El) ->
    fun(Maybe_group) -> case Maybe_group of
            {some, Group} ->
                [El | Group];

            none ->
                [El]
        end end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1264).
-spec group_updater(fun((GFZ) -> GGA)) -> fun((gleam@dict:dict(GGA, list(GFZ)), GFZ) -> gleam@dict:dict(GGA, list(GFZ))).
group_updater(F) ->
    fun(Groups, Elem) -> _pipe = Groups,
        gleam@dict:upsert(_pipe, F(Elem), update_group_with(Elem)) end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1255).
-spec group(yielder(GFT), fun((GFT) -> GFV)) -> gleam@dict:dict(GFV, list(GFT)).
group(Yielder, Key) ->
    _pipe = Yielder,
    _pipe@1 = fold(_pipe, gleam@dict:new(), group_updater(Key)),
    gleam@dict:map_values(_pipe@1, fun(_, Group) -> lists:reverse(Group) end).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1303).
-spec reduce(yielder(GGL), fun((GGL, GGL) -> GGL)) -> {ok, GGL} | {error, nil}.
reduce(Yielder, F) ->
    case (erlang:element(2, Yielder))() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            _pipe = fold_loop(Next, F, E),
            {ok, _pipe}
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1330).
-spec last(yielder(GGP)) -> {ok, GGP} | {error, nil}.
last(Yielder) ->
    _pipe = Yielder,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1344).
-spec empty() -> yielder(any()).
empty() ->
    {yielder, fun stop/0}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1357).
-spec once(fun(() -> GGV)) -> yielder(GGV).
once(F) ->
    _pipe = fun() -> {continue, F(), fun stop/0} end,
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 657).
-spec range(integer(), integer()) -> yielder(integer()).
range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            once(fun() -> Start end);

        gt ->
            unfold(Start, fun(Current) -> case Current < Stop of
                        false ->
                            {next, Current, Current - 1};

                        true ->
                            done
                    end end);

        lt ->
            unfold(Start, fun(Current@1) -> case Current@1 > Stop of
                        false ->
                            {next, Current@1, Current@1 + 1};

                        true ->
                            done
                    end end)
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1371).
-spec single(GGX) -> yielder(GGX).
single(Elem) ->
    once(fun() -> Elem end).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1402).
-spec interleave_loop(fun(() -> action(GHD)), fun(() -> action(GHD))) -> action(GHD).
interleave_loop(Current, Next) ->
    case Current() of
        stop ->
            Next();

        {continue, E, Next_other} ->
            {continue, E, fun() -> interleave_loop(Next, Next_other) end}
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1394).
-spec interleave(yielder(GGZ), yielder(GGZ)) -> yielder(GGZ).
interleave(Left, Right) ->
    _pipe = fun() ->
        interleave_loop(erlang:element(2, Left), erlang:element(2, Right))
    end,
    {yielder, _pipe}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1446).
-spec fold_until_loop(
    fun(() -> action(GHL)),
    fun((GHN, GHL) -> gleam@list:continue_or_stop(GHN)),
    GHN
) -> GHN.
fold_until_loop(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            Accumulator;

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {continue, Accumulator@1} ->
                    fold_until_loop(Next, F, Accumulator@1);

                {stop, Accumulator@2} ->
                    Accumulator@2
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1437).
-spec fold_until(
    yielder(GHH),
    GHJ,
    fun((GHJ, GHH) -> gleam@list:continue_or_stop(GHJ))
) -> GHJ.
fold_until(Yielder, Initial, F) ->
    _pipe = erlang:element(2, Yielder),
    fold_until_loop(_pipe, F, Initial).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1489).
-spec try_fold_loop(
    fun(() -> action(GHX)),
    fun((GHZ, GHX) -> {ok, GHZ} | {error, GIA}),
    GHZ
) -> {ok, GHZ} | {error, GIA}.
try_fold_loop(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            {ok, Accumulator};

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {ok, Result} ->
                    try_fold_loop(Next, F, Result);

                {error, _} = Error ->
                    Error
            end
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1480).
-spec try_fold(yielder(GHP), GHR, fun((GHR, GHP) -> {ok, GHR} | {error, GHS})) -> {ok,
        GHR} |
    {error, GHS}.
try_fold(Yielder, Initial, F) ->
    _pipe = erlang:element(2, Yielder),
    try_fold_loop(_pipe, F, Initial).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1519).
-spec first(yielder(GIF)) -> {ok, GIF} | {error, nil}.
first(Yielder) ->
    case (erlang:element(2, Yielder))() of
        stop ->
            {error, nil};

        {continue, E, _} ->
            {ok, E}
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1549).
-spec at(yielder(GIJ), integer()) -> {ok, GIJ} | {error, nil}.
at(Yielder, Index) ->
    _pipe = Yielder,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1577).
-spec length_loop(fun(() -> action(any())), integer()) -> integer().
length_loop(Continuation, Length) ->
    case Continuation() of
        stop ->
            Length;

        {continue, _, Next} ->
            length_loop(Next, Length + 1)
    end.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1572).
-spec length(yielder(any())) -> integer().
length(Yielder) ->
    _pipe = erlang:element(2, Yielder),
    length_loop(_pipe, 0).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1601).
-spec each(yielder(GIR), fun((GIR) -> any())) -> nil.
each(Yielder, F) ->
    _pipe = Yielder,
    _pipe@1 = map(_pipe, F),
    run(_pipe@1).

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1629).
-spec yield(GIU, fun(() -> yielder(GIU))) -> yielder(GIU).
yield(Element, Next) ->
    {yielder,
        fun() ->
            {continue, Element, fun() -> (erlang:element(2, Next()))() end}
        end}.

-file("/Users/louis/src/gleam/yielder/src/gleam/yielder.gleam", 1644).
-spec prepend(yielder(GIX), GIX) -> yielder(GIX).
prepend(Yielder, Element) ->
    yield(Element, fun() -> Yielder end).
