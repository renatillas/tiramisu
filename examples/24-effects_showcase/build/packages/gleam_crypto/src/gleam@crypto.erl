-module(gleam@crypto).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gleam/crypto.gleam").
-export([strong_random_bytes/1, new_hasher/1, hash_chunk/2, digest/1, hash/2, hmac/3, secure_compare/2, sign_message/3, verify_signed_message/2]).
-export_type([hash_algorithm/0, hasher/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(" Set of cryptographic functions.\n").

-type hash_algorithm() :: sha224 | sha256 | sha384 | sha512 | md5 | sha1.

-type hasher() :: any().

-file("src/gleam/crypto.gleam", 20).
?DOC(
    " Generates a specified number of bytes randomly uniform 0..255, and returns\n"
    " the result in a binary.\n"
    "\n"
    " On Erlang this uses a cryptographically secure prng seeded and periodically\n"
    " mixed with / operating system provided entropy. By default this is the\n"
    " RAND_bytes method from OpenSSL.\n"
    " <https://erlang.org/doc/man/crypto.html#strong_rand_bytes-1>\n"
    "\n"
    " On JavaScript the WebCrypto API is used.\n"
).
-spec strong_random_bytes(integer()) -> bitstring().
strong_random_bytes(A) ->
    crypto:strong_rand_bytes(A).

-file("src/gleam/crypto.gleam", 73).
?DOC(
    " Initializes the state for a streaming hash digest calculation.\n"
    " Then you can add data into the digest algorithm using `hash_update` function.\n"
    " Finally you use `hash_final` to retrieve the digest.\n"
    "\n"
    " It is useful for hashing streams of data or\n"
    " large amount of it without the need to load it all to the memory.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let hash =\n"
    "   new_hasher(Sha512)\n"
    "   |> hash_chunk(<<\"data to hash\":utf8>>)\n"
    "   |> digest\n"
    " ```\n"
).
-spec new_hasher(hash_algorithm()) -> hasher().
new_hasher(Algorithm) ->
    gleam_crypto_ffi:hash_init(Algorithm).

-file("src/gleam/crypto.gleam", 81).
?DOC(
    " Adds data to a streaming digest calculation.\n"
    "\n"
    " See `new_hasher` for more information and examples.\n"
).
-spec hash_chunk(hasher(), bitstring()) -> hasher().
hash_chunk(Hasher, Chunk) ->
    crypto:hash_update(Hasher, Chunk).

-file("src/gleam/crypto.gleam", 89).
?DOC(
    " Finalizes a streaming hash calculation.\n"
    "\n"
    " See `new_hasher` for more information and examples.\n"
).
-spec digest(hasher()) -> bitstring().
digest(Hasher) ->
    crypto:hash_final(Hasher).

-file("src/gleam/crypto.gleam", 49).
?DOC(
    " Computes a digest of the input bit string.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let digest = hash(Sha256, <<\"a\":utf8>>)\n"
    " ```\n"
    " If you wish to to hash content in multiple chunks rather than all at once\n"
    " see the `new_hasher` function.\n"
).
-spec hash(hash_algorithm(), bitstring()) -> bitstring().
hash(Algorithm, Data) ->
    _pipe = gleam_crypto_ffi:hash_init(Algorithm),
    _pipe@1 = crypto:hash_update(_pipe, Data),
    crypto:hash_final(_pipe@1).

-file("src/gleam/crypto.gleam", 100).
?DOC(
    " Calculates the HMAC (hash-based message authentication code) for a bit\n"
    " string.\n"
    "\n"
    " Based on the Erlang [`crypto:mac`](https://www.erlang.org/doc/man/crypto.html#mac-4)\n"
    " function, or the [`node:crypto.createHmac`](https://nodejs.org/api/crypto.html#cryptocreatehmacalgorithm-key-options)\n"
    " function on JavaScript.\n"
).
-spec hmac(bitstring(), hash_algorithm(), bitstring()) -> bitstring().
hmac(Data, Algorithm, Key) ->
    gleam_crypto_ffi:hmac(Data, Algorithm, Key).

-file("src/gleam/crypto.gleam", 113).
-spec do_secure_compare(bitstring(), bitstring(), integer()) -> boolean().
do_secure_compare(Left, Right, Accumulator) ->
    case {Left, Right} of
        {<<X, Left@1/binary>>, <<Y, Right@1/binary>>} ->
            Accumulator@1 = erlang:'bor'(Accumulator, erlang:'bxor'(X, Y)),
            do_secure_compare(Left@1, Right@1, Accumulator@1);

        {_, _} ->
            (Left =:= Right) andalso (Accumulator =:= 0)
    end.

-file("src/gleam/crypto.gleam", 106).
?DOC(
    " Compares the two binaries in constant-time to avoid timing attacks.\n"
    "\n"
    " For more details see: http://codahale.com/a-lesson-in-timing-attacks/\n"
).
-spec secure_compare(bitstring(), bitstring()) -> boolean().
secure_compare(Left, Right) ->
    case erlang:byte_size(Left) =:= erlang:byte_size(Right) of
        true ->
            do_secure_compare(Left, Right, 0);

        false ->
            false
    end.

-file("src/gleam/crypto.gleam", 143).
-spec signing_input(hash_algorithm(), bitstring()) -> binary().
signing_input(Digest_type, Message) ->
    Protected = case Digest_type of
        sha224 ->
            <<"HS224"/utf8>>;

        sha256 ->
            <<"HS256"/utf8>>;

        sha384 ->
            <<"HS384"/utf8>>;

        sha512 ->
            <<"HS512"/utf8>>;

        sha1 ->
            <<"HS1"/utf8>>;

        md5 ->
            <<"HMD5"/utf8>>
    end,
    gleam@string:concat(
        [gleam@bit_array:base64_url_encode(<<Protected/binary>>, false),
            <<"."/utf8>>,
            gleam@bit_array:base64_url_encode(Message, false)]
    ).

-file("src/gleam/crypto.gleam", 132).
?DOC(
    " Sign a message which can later be verified using the `verify_signed_message`\n"
    " function to detect if the message has been tampered with.\n"
    "\n"
    " A web application could use this verifier to sign HTTP cookies. The data can\n"
    " be read by the user, but cannot be tampered with.\n"
).
-spec sign_message(bitstring(), bitstring(), hash_algorithm()) -> binary().
sign_message(Message, Secret, Digest_type) ->
    Input = signing_input(Digest_type, Message),
    Signature = gleam_crypto_ffi:hmac(<<Input/binary>>, Digest_type, Secret),
    gleam@string:concat(
        [Input,
            <<"."/utf8>>,
            gleam@bit_array:base64_url_encode(Signature, false)]
    ).

-file("src/gleam/crypto.gleam", 163).
?DOC(" Verify a message created by the `sign_message` function.\n").
-spec verify_signed_message(binary(), bitstring()) -> {ok, bitstring()} |
    {error, nil}.
verify_signed_message(Message, Secret) ->
    gleam@result:'try'(case gleam@string:split(Message, <<"."/utf8>>) of
            [A, B, C] ->
                {ok, {A, B, C}};

            _ ->
                {error, nil}
        end, fun(_use0) ->
            {Protected, Payload, Signature} = _use0,
            Text = gleam@string:concat([Protected, <<"."/utf8>>, Payload]),
            gleam@result:'try'(
                gleam@bit_array:base64_url_decode(Payload),
                fun(Payload@1) ->
                    gleam@result:'try'(
                        gleam@bit_array:base64_url_decode(Signature),
                        fun(Signature@1) ->
                            gleam@result:'try'(
                                gleam@bit_array:base64_url_decode(Protected),
                                fun(Protected@1) ->
                                    gleam@result:'try'(case Protected@1 of
                                            <<72, 83, 50, 50, 52>> ->
                                                {ok, sha224};

                                            <<72, 83, 50, 53, 54>> ->
                                                {ok, sha256};

                                            <<72, 83, 51, 56, 52>> ->
                                                {ok, sha384};

                                            <<72, 83, 53, 49, 50>> ->
                                                {ok, sha512};

                                            <<72, 83, 49>> ->
                                                {ok, sha1};

                                            <<72, 77, 68, 53>> ->
                                                {ok, md5};

                                            _ ->
                                                {error, nil}
                                        end, fun(Digest_type) ->
                                            Challenge = gleam_crypto_ffi:hmac(
                                                <<Text/binary>>,
                                                Digest_type,
                                                Secret
                                            ),
                                            case secure_compare(
                                                Challenge,
                                                Signature@1
                                            ) of
                                                true ->
                                                    {ok, Payload@1};

                                                false ->
                                                    {error, nil}
                                            end
                                        end)
                                end
                            )
                        end
                    )
                end
            )
        end).
