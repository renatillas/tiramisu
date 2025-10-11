-module(tiramisu@asset).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/asset.gleam").
-export([loaded_model/1, loaded_texture/1, loaded_audio/1, loaded_stl/1, loaded_obj/1, new_cache/0, new_cache_with_size/1, cache_size/1, is_cached/2, clear_cache/1, get_model/2, get_model_scene/2, get_texture/2, get_audio/2, get_stl/2, get_obj/2, try_get/2, cached_urls/1, insert_asset/3]).
-export_type([texture/0, buffer_geometry/0, load_error/0, g_l_t_f_data/0, asset_type/0, loaded_asset/0, asset_error/0, load_progress/0, cache_entry/0, cache_config/0, asset_cache/0, batch_load_result/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Asset Management System - loading and caching for textures, models, and audio.\n"
    "\n"
    " Provides declarative asset loading with progress tracking, caching,\n"
    " and batch loading capabilities with LRU eviction.\n"
    "\n"
    " ## Quick Example\n"
    "\n"
    " ```gleam\n"
    " import tiramisu/asset\n"
    "\n"
    " // Load a texture\n"
    " let load_effect = asset.load_texture(\"player.png\")\n"
    "   |> promise.map(fn(result) {\n"
    "     case result {\n"
    "       Ok(texture) -> TextureLoaded(texture)\n"
    "       Error(err) -> LoadFailed(err)\n"
    "     }\n"
    "   })\n"
    "   |> effect.from_promise\n"
    "\n"
    " // Use with cache\n"
    " let cache = asset.new_cache()\n"
    " let cache = asset.insert_texture(cache, \"player.png\", texture)\n"
    " let texture = asset.get_texture(cache, \"player.png\")\n"
    " ```\n"
).

-type texture() :: any().

-type buffer_geometry() :: any().

-type load_error() :: {load_error, binary()} |
    {invalid_url, binary()} |
    {parse_error, binary()}.

-type g_l_t_f_data() :: {g_l_t_f_data,
        tiramisu@object3d:object3_d(),
        list(tiramisu@object3d:animation_clip())}.

-type asset_type() :: {model_asset, binary()} |
    {texture_asset, binary()} |
    {audio_asset, binary()} |
    {s_t_l_asset, binary()} |
    {o_b_j_asset, binary(), gleam@option:option(binary())}.

-opaque loaded_asset() :: {loaded_model, g_l_t_f_data()} |
    {loaded_texture, texture()} |
    {loaded_audio, tiramisu@audio:audio_buffer()} |
    {loaded_s_t_l, buffer_geometry()} |
    {loaded_o_b_j, tiramisu@object3d:object3_d()}.

-type asset_error() :: {asset_load_error, binary(), binary()} |
    {asset_not_found, binary()} |
    {invalid_asset_type, binary()}.

-type load_progress() :: {load_progress, integer(), integer(), binary()}.

-type cache_entry() :: {cache_entry, loaded_asset(), integer()}.

-type cache_config() :: {cache_config, integer(), integer()}.

-opaque asset_cache() :: {asset_cache,
        gleam@dict:dict(binary(), cache_entry()),
        cache_config()}.

-type batch_load_result() :: {batch_load_result,
        asset_cache(),
        list(asset_error())}.

-file("src/tiramisu/asset.gleam", 95).
?DOC(false).
-spec loaded_model(g_l_t_f_data()) -> loaded_asset().
loaded_model(Data) ->
    {loaded_model, Data}.

-file("src/tiramisu/asset.gleam", 100).
?DOC(false).
-spec loaded_texture(texture()) -> loaded_asset().
loaded_texture(Texture) ->
    {loaded_texture, Texture}.

-file("src/tiramisu/asset.gleam", 105).
?DOC(false).
-spec loaded_audio(tiramisu@audio:audio_buffer()) -> loaded_asset().
loaded_audio(Audio) ->
    {loaded_audio, Audio}.

-file("src/tiramisu/asset.gleam", 110).
?DOC(false).
-spec loaded_stl(buffer_geometry()) -> loaded_asset().
loaded_stl(Geometry) ->
    {loaded_s_t_l, Geometry}.

-file("src/tiramisu/asset.gleam", 115).
?DOC(false).
-spec loaded_obj(tiramisu@object3d:object3_d()) -> loaded_asset().
loaded_obj(Object) ->
    {loaded_o_b_j, Object}.

-file("src/tiramisu/asset.gleam", 149).
?DOC(" Create a new empty asset cache with default max size (100 asset)\n").
-spec new_cache() -> asset_cache().
new_cache() ->
    {asset_cache, maps:new(), {cache_config, 100, 0}}.

-file("src/tiramisu/asset.gleam", 157).
?DOC(" Create a new empty asset cache with custom max size\n").
-spec new_cache_with_size(integer()) -> asset_cache().
new_cache_with_size(Max_size) ->
    {asset_cache, maps:new(), {cache_config, Max_size, 0}}.

-file("src/tiramisu/asset.gleam", 165).
?DOC(" Get the number of cached asset\n").
-spec cache_size(asset_cache()) -> integer().
cache_size(Cache) ->
    maps:size(erlang:element(2, Cache)).

-file("src/tiramisu/asset.gleam", 170).
?DOC(" Check if an asset is cached\n").
-spec is_cached(asset_cache(), binary()) -> boolean().
is_cached(Cache, Url) ->
    gleam@dict:has_key(erlang:element(2, Cache), Url).

-file("src/tiramisu/asset.gleam", 175).
?DOC(" Clear all cached asset\n").
-spec clear_cache(asset_cache()) -> asset_cache().
clear_cache(Cache) ->
    {asset_cache, maps:new(), erlang:element(3, Cache)}.

-file("src/tiramisu/asset.gleam", 271).
?DOC(" Get a GLTF model from the cache (updates LRU timestamp)\n").
-spec get_model(asset_cache(), binary()) -> {ok, g_l_t_f_data()} |
    {error, asset_error()}.
get_model(Cache, Url) ->
    case gleam_stdlib:map_get(erlang:element(2, Cache), Url) of
        {ok, {cache_entry, {loaded_model, Data}, _}} ->
            {ok, Data};

        {ok, {cache_entry, _, _}} ->
            {error, {invalid_asset_type, Url}};

        {error, _} ->
            {error, {asset_not_found, Url}}
    end.

-file("src/tiramisu/asset.gleam", 280).
?DOC(" Get the scene object from a cached GLTF model\n").
-spec get_model_scene(asset_cache(), binary()) -> {ok,
        tiramisu@object3d:object3_d()} |
    {error, asset_error()}.
get_model_scene(Cache, Url) ->
    case get_model(Cache, Url) of
        {ok, Data} ->
            {ok, erlang:element(2, Data)};

        {error, Err} ->
            {error, Err}
    end.

-file("src/tiramisu/asset.gleam", 291).
?DOC(" Get a texture from the cache\n").
-spec get_texture(asset_cache(), binary()) -> {ok, texture()} |
    {error, asset_error()}.
get_texture(Cache, Url) ->
    case gleam_stdlib:map_get(erlang:element(2, Cache), Url) of
        {ok, {cache_entry, {loaded_texture, Tex}, _}} ->
            {ok, Tex};

        {ok, {cache_entry, _, _}} ->
            {error, {invalid_asset_type, Url}};

        {error, _} ->
            {error, {asset_not_found, Url}}
    end.

-file("src/tiramisu/asset.gleam", 303).
?DOC(" Get an audio buffer from the cache\n").
-spec get_audio(asset_cache(), binary()) -> {ok, tiramisu@audio:audio_buffer()} |
    {error, asset_error()}.
get_audio(Cache, Url) ->
    case gleam_stdlib:map_get(erlang:element(2, Cache), Url) of
        {ok, {cache_entry, {loaded_audio, Audio}, _}} ->
            {ok, Audio};

        {ok, {cache_entry, _, _}} ->
            {error, {invalid_asset_type, Url}};

        {error, _} ->
            {error, {asset_not_found, Url}}
    end.

-file("src/tiramisu/asset.gleam", 315).
?DOC(" Get an STL geometry from the cache\n").
-spec get_stl(asset_cache(), binary()) -> {ok, buffer_geometry()} |
    {error, asset_error()}.
get_stl(Cache, Url) ->
    case gleam_stdlib:map_get(erlang:element(2, Cache), Url) of
        {ok, {cache_entry, {loaded_s_t_l, Geom}, _}} ->
            {ok, Geom};

        {ok, {cache_entry, _, _}} ->
            {error, {invalid_asset_type, Url}};

        {error, _} ->
            {error, {asset_not_found, Url}}
    end.

-file("src/tiramisu/asset.gleam", 346).
?DOC(
    " Get an OBJ model from the cache\n"
    "\n"
    " Returns the loaded OBJ model as an Object3D. The model will have:\n"
    " - Materials with textures applied (if MTL file was loaded)\n"
    " - Vertex normals computed\n"
    " - Center position at origin\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(bread_model) = asset.get_obj(cache, \"models/bread.obj\")\n"
    "\n"
    " scene.Model3D(\n"
    "   id: \"bread\",\n"
    "   object: bread_model,\n"
    "   transform: transform.identity,\n"
    "   animation: option.None,\n"
    "   physics: option.None,\n"
    " )\n"
    " ```\n"
).
-spec get_obj(asset_cache(), binary()) -> {ok, tiramisu@object3d:object3_d()} |
    {error, asset_error()}.
get_obj(Cache, Url) ->
    case gleam_stdlib:map_get(erlang:element(2, Cache), Url) of
        {ok, {cache_entry, {loaded_o_b_j, Object}, _}} ->
            {ok, Object};

        {ok, {cache_entry, _, _}} ->
            {error, {invalid_asset_type, Url}};

        {error, _} ->
            {error, {asset_not_found, Url}}
    end.

-file("src/tiramisu/asset.gleam", 355).
?DOC(" Try to get any asset (returns Option)\n").
-spec try_get(asset_cache(), binary()) -> gleam@option:option(loaded_asset()).
try_get(Cache, Url) ->
    case gleam_stdlib:map_get(erlang:element(2, Cache), Url) of
        {ok, {cache_entry, Asset, _}} ->
            {some, Asset};

        {error, _} ->
            none
    end.

-file("src/tiramisu/asset.gleam", 363).
?DOC(" Get all cached URLs\n").
-spec cached_urls(asset_cache()) -> list(binary()).
cached_urls(Cache) ->
    maps:keys(erlang:element(2, Cache)).

-file("src/tiramisu/asset.gleam", 392).
?DOC(" Evict the least recently used asset from the cache\n").
-spec evict_lru(asset_cache()) -> asset_cache().
evict_lru(Cache) ->
    case begin
        _pipe = maps:to_list(erlang:element(2, Cache)),
        _pipe@1 = gleam@list:sort(
            _pipe,
            fun(A, B) ->
                {_, {cache_entry, _, Time_a}} = A,
                {_, {cache_entry, _, Time_b}} = B,
                case Time_a < Time_b of
                    true ->
                        lt;

                    false ->
                        case Time_a > Time_b of
                            true ->
                                gt;

                            false ->
                                eq
                        end
                end
            end
        ),
        gleam@list:first(_pipe@1)
    end of
        {ok, {Url_to_evict, _}} ->
            New_asset = gleam@dict:delete(
                erlang:element(2, Cache),
                Url_to_evict
            ),
            {asset_cache, New_asset, erlang:element(3, Cache)};

        {error, _} ->
            Cache
    end.

-file("src/tiramisu/asset.gleam", 369).
?DOC(
    " Insert a loaded asset into the cache manually\n"
    " If cache exceeds max_size, evicts least recently used asset\n"
).
-spec insert_asset(asset_cache(), binary(), loaded_asset()) -> asset_cache().
insert_asset(Cache, Url, Asset) ->
    New_time = erlang:element(3, erlang:element(3, Cache)) + 1,
    Entry = {cache_entry, Asset, New_time},
    New_asset = gleam@dict:insert(erlang:element(2, Cache), Url, Entry),
    New_cache = {asset_cache,
        New_asset,
        begin
            _record = erlang:element(3, Cache),
            {cache_config, erlang:element(2, _record), New_time}
        end},
    case maps:size(New_asset) > erlang:element(2, erlang:element(3, Cache)) of
        true ->
            evict_lru(New_cache);

        false ->
            New_cache
    end.
