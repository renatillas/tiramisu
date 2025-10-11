-module(plinth@browser@geolocation).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/geolocation.gleam").
-export([decoder/0]).
-export_type([geolocation_position/0]).

-type geolocation_position() :: {geolocation_position,
        float(),
        float(),
        gleam@option:option(float()),
        float(),
        gleam@option:option(float()),
        gleam@option:option(float()),
        gleam@option:option(float()),
        float()}.

-file("src/plinth/browser/geolocation.gleam", 21).
-spec decoder() -> gleam@dynamic@decode:decoder(geolocation_position()).
decoder() ->
    gleam@dynamic@decode:field(
        <<"timestamp"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_float/1},
        fun(Timestamp) ->
            gleam@dynamic@decode:field(
                <<"coords"/utf8>>,
                begin
                    gleam@dynamic@decode:field(
                        <<"latitude"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_float/1},
                        fun(Latitude) ->
                            gleam@dynamic@decode:field(
                                <<"longitude"/utf8>>,
                                {decoder,
                                    fun gleam@dynamic@decode:decode_float/1},
                                fun(Longitude) ->
                                    gleam@dynamic@decode:field(
                                        <<"altitude"/utf8>>,
                                        gleam@dynamic@decode:optional(
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_float/1}
                                        ),
                                        fun(Altitude) ->
                                            gleam@dynamic@decode:field(
                                                <<"accuracy"/utf8>>,
                                                {decoder,
                                                    fun gleam@dynamic@decode:decode_float/1},
                                                fun(Accuracy) ->
                                                    gleam@dynamic@decode:field(
                                                        <<"altitudeAccuracy"/utf8>>,
                                                        gleam@dynamic@decode:optional(
                                                            {decoder,
                                                                fun gleam@dynamic@decode:decode_float/1}
                                                        ),
                                                        fun(Altitude_accuracy) ->
                                                            gleam@dynamic@decode:field(
                                                                <<"heading"/utf8>>,
                                                                gleam@dynamic@decode:optional(
                                                                    {decoder,
                                                                        fun gleam@dynamic@decode:decode_float/1}
                                                                ),
                                                                fun(Heading) ->
                                                                    gleam@dynamic@decode:field(
                                                                        <<"speed"/utf8>>,
                                                                        gleam@dynamic@decode:optional(
                                                                            {decoder,
                                                                                fun gleam@dynamic@decode:decode_float/1}
                                                                        ),
                                                                        fun(
                                                                            Speed
                                                                        ) ->
                                                                            gleam@dynamic@decode:success(
                                                                                {geolocation_position,
                                                                                    Latitude,
                                                                                    Longitude,
                                                                                    Altitude,
                                                                                    Accuracy,
                                                                                    Altitude_accuracy,
                                                                                    Heading,
                                                                                    Speed,
                                                                                    Timestamp}
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end,
                fun(N) -> gleam@dynamic@decode:success(N) end
            )
        end
    ).
