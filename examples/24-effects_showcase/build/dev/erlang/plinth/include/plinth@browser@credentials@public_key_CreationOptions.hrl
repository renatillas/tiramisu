-record(creation_options, {
    attestation :: plinth@browser@credentials@public_key:attestation(),
    attestation_formats :: list(binary()),
    authenticator_attachement :: gleam@option:option(plinth@browser@credentials@public_key:authenticator_attachement()),
    resident_key :: plinth@browser@credentials@public_key:requirement(),
    user_verification :: plinth@browser@credentials@public_key:requirement(),
    challenge :: bitstring(),
    exclude_credentials :: list({bitstring(),
        list(plinth@browser@credentials@public_key:transport())}),
    public_key_credential_parameters :: list(plinth@browser@credentials@public_key:algorithm()),
    relaying_party_id :: gleam@option:option(binary()),
    relaying_party_name :: binary(),
    timeout :: gleam@option:option(integer()),
    user_id :: bitstring(),
    user_name :: binary(),
    user_display_name :: binary(),
    hints :: list(plinth@browser@credentials@public_key:hint())
}).
