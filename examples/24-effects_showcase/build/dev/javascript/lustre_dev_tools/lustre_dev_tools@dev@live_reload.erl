-module(lustre_dev_tools@dev@live_reload).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/dev/live_reload.gleam").
-export([start/5]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre_dev_tools/dev/live_reload.gleam", 17).
?DOC(false).
-spec start(
    gleam@http@request:request(mist@internal@http:connection()),
    lustre_dev_tools@project:project(),
    booklet:booklet(gleam@option:option(lustre_dev_tools@error:error())),
    group_registry:group_registry(lustre_dev_tools@dev@watcher:event()),
    gleam@option:option(binary())
) -> gleam@http@response:response(mist:response_data()).
start(Request, Project, Error, Watcher, Tailwind_entry) ->
    mist:websocket(Request, fun(_, Message, Connection) -> case Message of
                {custom, {change, Dir, Path}} when Dir =:= erlang:element(
                    7,
                    Project
                ) ->
                    _ = begin
                        _pipe = gleam@json:object(
                            [{<<"type"/utf8>>,
                                    gleam@json:string(<<"asset-update"/utf8>>)},
                                {<<"asset"/utf8>>, gleam@json:string(Path)}]
                        ),
                        _pipe@1 = gleam@json:to_string(_pipe),
                        mist:send_text_frame(Connection, _pipe@1)
                    end,
                    mist:continue(nil);

                {custom, {change, _, Path@1}} ->
                    case {some, Path@1} =:= gleam@option:map(
                        Tailwind_entry,
                        fun(_capture) ->
                            gleam@string:append(_capture, <<".css"/utf8>>)
                        end
                    ) of
                        true ->
                            _ = begin
                                _pipe@2 = gleam@json:object(
                                    [{<<"type"/utf8>>,
                                            gleam@json:string(
                                                <<"asset-update"/utf8>>
                                            )},
                                        {<<"asset"/utf8>>,
                                            gleam@json:string(Path@1)}]
                                ),
                                _pipe@3 = gleam@json:to_string(_pipe@2),
                                mist:send_text_frame(Connection, _pipe@3)
                            end,
                            mist:continue(nil);

                        false ->
                            _ = begin
                                _pipe@4 = gleam@json:object(
                                    [{<<"type"/utf8>>,
                                            gleam@json:string(<<"reload"/utf8>>)}]
                                ),
                                _pipe@5 = gleam@json:to_string(_pipe@4),
                                mist:send_text_frame(Connection, _pipe@5)
                            end,
                            mist:continue(nil)
                    end;

                {custom, styles} ->
                    mist:continue(nil);

                {custom, {build_error, Reason}} ->
                    Message@1 = lustre_dev_tools@error:explain(Reason),
                    _ = begin
                        _pipe@6 = gleam@json:object(
                            [{<<"type"/utf8>>,
                                    gleam@json:string(<<"error"/utf8>>)},
                                {<<"message"/utf8>>,
                                    gleam@json:string(Message@1)}]
                        ),
                        _pipe@7 = gleam@json:to_string(_pipe@6),
                        mist:send_text_frame(Connection, _pipe@7)
                    end,
                    mist:continue(nil);

                {binary, _} ->
                    mist:continue(nil);

                {text, _} ->
                    mist:continue(nil);

                closed ->
                    mist:stop();

                shutdown ->
                    mist:stop()
            end end, fun(Connection@1) ->
            Self = erlang:self(),
            Subject = lustre_dev_tools@dev@watcher:subscribe(Watcher, Self),
            Selector = begin
                _pipe@8 = gleam_erlang_ffi:new_selector(),
                gleam@erlang@process:select(_pipe@8, Subject)
            end,
            case booklet_ffi:get(Error) of
                {some, Reason@1} ->
                    Message@2 = lustre_dev_tools@error:explain(Reason@1),
                    _ = begin
                        _pipe@9 = gleam@json:object(
                            [{<<"type"/utf8>>,
                                    gleam@json:string(<<"error"/utf8>>)},
                                {<<"message"/utf8>>,
                                    gleam@json:string(Message@2)}]
                        ),
                        _pipe@10 = gleam@json:to_string(_pipe@9),
                        mist:send_text_frame(Connection@1, _pipe@10)
                    end,
                    nil;

                none ->
                    nil
            end,
            {nil, {some, Selector}}
        end, fun(_) ->
            Self@1 = erlang:self(),
            lustre_dev_tools@dev@watcher:unsubscribe(Watcher, Self@1)
        end).
