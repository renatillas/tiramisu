-record(rsa_hashed_key_gen_params, {
    name :: binary(),
    modulus_length :: integer(),
    public_exponent :: bitstring(),
    hash :: plinth@browser@crypto@subtle:digest_algorithm()
}).
