-record(rsa_hashed_import_params, {
    name :: binary(),
    hash :: plinth@browser@crypto@subtle:digest_algorithm()
}).
