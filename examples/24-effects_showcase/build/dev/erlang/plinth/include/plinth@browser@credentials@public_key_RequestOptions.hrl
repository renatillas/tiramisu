-record(request_options, {
    allow_credentials :: list({bitstring(),
        list(plinth@browser@credentials@public_key:transport())}),
    challenge :: bitstring(),
    hints :: list(plinth@browser@credentials@public_key:hint()),
    relaying_party_id :: gleam@option:option(binary()),
    timeout :: gleam@option:option(integer()),
    user_verification :: plinth@browser@credentials@public_key:requirement()
}).
