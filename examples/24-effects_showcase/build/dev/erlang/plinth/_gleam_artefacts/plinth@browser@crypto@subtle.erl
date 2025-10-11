-module(plinth@browser@crypto@subtle).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/crypto/subtle.gleam").
-export([digest_algorithm_to_string/1]).
-export_type([digest_algorithm/0, crypto_key/0, public_key_algorithm/0, key_usage/0, import_algorithm/0, named_curve/0, sign_algorithm/0]).

-type digest_algorithm() :: s_h_a1 | s_h_a256 | s_h_a384 | s_h_a512.

-type crypto_key() :: any().

-type public_key_algorithm() :: {rsa_hashed_key_gen_params,
        binary(),
        integer(),
        bitstring(),
        digest_algorithm()} |
    {ec_key_gen_params, binary(), binary()}.

-type key_usage() :: encrypt |
    decrypt |
    sign |
    verify |
    derive_key |
    derive_bits |
    wrap_key |
    unwrap_key.

-type import_algorithm() :: {rsa_hashed_import_params,
        binary(),
        digest_algorithm()} |
    {ec_key_import_params, binary(), named_curve()} |
    {hmac_import_params, digest_algorithm()} |
    {other_import_params, binary()}.

-type named_curve() :: p256 | p384 | p521.

-type sign_algorithm() :: rsa_ssa_pkcs1v15 |
    {rsa_pss_params, integer()} |
    {ecdsa_params, digest_algorithm()} |
    hmac |
    ed25519.

-file("src/plinth/browser/crypto/subtle.gleam", 13).
-spec digest_algorithm_to_string(digest_algorithm()) -> binary().
digest_algorithm_to_string(Algorithm) ->
    case Algorithm of
        s_h_a1 ->
            <<"SHA-1"/utf8>>;

        s_h_a256 ->
            <<"SHA-256"/utf8>>;

        s_h_a384 ->
            <<"SHA-384"/utf8>>;

        s_h_a512 ->
            <<"SHA-512"/utf8>>
    end.

-file("src/plinth/browser/crypto/subtle.gleam", 76).
-spec key_usage_to_string(key_usage()) -> binary().
key_usage_to_string(Key_usage) ->
    case Key_usage of
        encrypt ->
            <<"encrypt"/utf8>>;

        decrypt ->
            <<"decrypt"/utf8>>;

        sign ->
            <<"sign"/utf8>>;

        verify ->
            <<"verify"/utf8>>;

        derive_key ->
            <<"deriveKey"/utf8>>;

        derive_bits ->
            <<"deriveBits"/utf8>>;

        wrap_key ->
            <<"wrapKey"/utf8>>;

        unwrap_key ->
            <<"unwrapKey"/utf8>>
    end.

-file("src/plinth/browser/crypto/subtle.gleam", 158).
-spec named_curve_to_string(named_curve()) -> binary().
named_curve_to_string(Named_curve) ->
    case Named_curve of
        p256 ->
            <<"P-256"/utf8>>;

        p384 ->
            <<"P-384"/utf8>>;

        p521 ->
            <<"P-521"/utf8>>
    end.

-file("src/plinth/browser/crypto/subtle.gleam", 131).
-spec import_algorithm_to_json(import_algorithm()) -> gleam@json:json().
import_algorithm_to_json(Algorithm_parameters) ->
    case Algorithm_parameters of
        {rsa_hashed_import_params, Name, Hash} ->
            gleam@json:object(
                [{<<"name"/utf8>>, gleam@json:string(Name)},
                    {<<"hash"/utf8>>,
                        gleam@json:string(digest_algorithm_to_string(Hash))}]
            );

        {ec_key_import_params, Name@1, Named_curve} ->
            gleam@json:object(
                [{<<"name"/utf8>>, gleam@json:string(Name@1)},
                    {<<"namedCurve"/utf8>>,
                        gleam@json:string(named_curve_to_string(Named_curve))}]
            );

        {hmac_import_params, Hash@1} ->
            gleam@json:object(
                [{<<"name"/utf8>>, gleam@json:string(<<"HMAC"/utf8>>)},
                    {<<"hash"/utf8>>,
                        gleam@json:string(digest_algorithm_to_string(Hash@1))}]
            );

        {other_import_params, Name@2} ->
            gleam@json:object([{<<"name"/utf8>>, gleam@json:string(Name@2)}])
    end.

-file("src/plinth/browser/crypto/subtle.gleam", 186).
-spec sign_algorithm_to_json(sign_algorithm()) -> gleam@json:json().
sign_algorithm_to_json(Key_algorithm) ->
    case Key_algorithm of
        rsa_ssa_pkcs1v15 ->
            gleam@json:object(
                [{<<"name"/utf8>>,
                        gleam@json:string(<<"RSASSA-PKCS1-v1_5"/utf8>>)}]
            );

        {rsa_pss_params, Salt_length} ->
            gleam@json:object(
                [{<<"name"/utf8>>, gleam@json:string(<<"RSA-PSS"/utf8>>)},
                    {<<"saltLength"/utf8>>, gleam@json:int(Salt_length)}]
            );

        {ecdsa_params, Hash} ->
            gleam@json:object(
                [{<<"name"/utf8>>, gleam@json:string(<<"ECDSA"/utf8>>)},
                    {<<"hash"/utf8>>,
                        gleam@json:string(digest_algorithm_to_string(Hash))}]
            );

        hmac ->
            gleam@json:object(
                [{<<"name"/utf8>>, gleam@json:string(<<"HMAC"/utf8>>)}]
            );

        ed25519 ->
            gleam@json:object(
                [{<<"name"/utf8>>, gleam@json:string(<<"Ed25519"/utf8>>)}]
            )
    end.
