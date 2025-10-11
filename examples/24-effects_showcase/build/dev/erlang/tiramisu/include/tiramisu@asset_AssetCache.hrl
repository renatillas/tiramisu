-record(asset_cache, {
    asset :: gleam@dict:dict(binary(), tiramisu@asset:cache_entry()),
    config :: tiramisu@asset:cache_config()
}).
