-record(batch_load_result, {
    cache :: tiramisu@asset:asset_cache(),
    errors :: list(tiramisu@asset:asset_error())
}).
