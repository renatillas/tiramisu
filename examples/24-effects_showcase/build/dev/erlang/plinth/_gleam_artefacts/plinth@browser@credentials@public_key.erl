-module(plinth@browser@credentials@public_key).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/credentials/public_key.gleam").
-export([creation/6, request/1, algorithm_to_number/1]).
-export_type([creation_options/0, request_options/0, attestation/0, authenticator_attachement/0, algorithm/0, requirement/0, transport/0, hint/0, native_creation_options/0, native_request_options/0, credential/1, attest/0, assertion/0]).

-type creation_options() :: {creation_options,
        attestation(),
        list(binary()),
        gleam@option:option(authenticator_attachement()),
        requirement(),
        requirement(),
        bitstring(),
        list({bitstring(), list(transport())}),
        list(algorithm()),
        gleam@option:option(binary()),
        binary(),
        gleam@option:option(integer()),
        bitstring(),
        binary(),
        binary(),
        list(hint())}.

-type request_options() :: {request_options,
        list({bitstring(), list(transport())}),
        bitstring(),
        list(hint()),
        gleam@option:option(binary()),
        gleam@option:option(integer()),
        requirement()}.

-type attestation() :: no_attestation | direct | enterprise | indirect.

-type authenticator_attachement() :: platform | cross_platform.

-type algorithm() :: ed25519 | e_s256 | r_s256.

-type requirement() :: required | preferred | discouraged.

-type transport() :: ble | hybrid_transport | internal | nfc | usb.

-type hint() :: security_key | client_device | hybrid_hint.

-type native_creation_options() :: any().

-type native_request_options() :: any().

-type credential(AKYX) :: any() | {gleam_phantom, AKYX}.

-type attest() :: any().

-type assertion() :: any().

-file("src/plinth/browser/credentials/public_key.gleam", 46).
-spec creation(
    bitstring(),
    algorithm(),
    binary(),
    bitstring(),
    binary(),
    binary()
) -> creation_options().
creation(
    Challenge,
    Algorithm,
    Relaying_party_name,
    User_id,
    User_name,
    User_display_name
) ->
    {creation_options,
        no_attestation,
        [],
        none,
        discouraged,
        preferred,
        Challenge,
        [],
        [Algorithm],
        none,
        Relaying_party_name,
        none,
        User_id,
        User_name,
        User_display_name,
        []}.

-file("src/plinth/browser/credentials/public_key.gleam", 85).
-spec request(bitstring()) -> request_options().
request(Challenge) ->
    {request_options, [], Challenge, [], none, none, preferred}.

-file("src/plinth/browser/credentials/public_key.gleam", 96).
-spec attestation_to_string(attestation()) -> binary().
attestation_to_string(Attestation) ->
    case Attestation of
        no_attestation ->
            <<"none"/utf8>>;

        direct ->
            <<"direct"/utf8>>;

        enterprise ->
            <<"enterprise"/utf8>>;

        indirect ->
            <<"indirect"/utf8>>
    end.

-file("src/plinth/browser/credentials/public_key.gleam", 110).
-spec authenticator_attachment_to_string(authenticator_attachement()) -> binary().
authenticator_attachment_to_string(Authenticator_attachment) ->
    case Authenticator_attachment of
        platform ->
            <<"platform"/utf8>>;

        cross_platform ->
            <<"cross-platform"/utf8>>
    end.

-file("src/plinth/browser/credentials/public_key.gleam", 123).
-spec algorithm_to_number(algorithm()) -> integer().
algorithm_to_number(Algorithm) ->
    case Algorithm of
        ed25519 ->
            -8;

        e_s256 ->
            -7;

        r_s256 ->
            -257
    end.

-file("src/plinth/browser/credentials/public_key.gleam", 137).
-spec requirement_to_string(requirement()) -> binary().
requirement_to_string(Requirement) ->
    case Requirement of
        required ->
            <<"required"/utf8>>;

        preferred ->
            <<"preferred"/utf8>>;

        discouraged ->
            <<"discouraged"/utf8>>
    end.

-file("src/plinth/browser/credentials/public_key.gleam", 154).
-spec transport_to_string(transport()) -> binary().
transport_to_string(Transport) ->
    case Transport of
        ble ->
            <<"ble"/utf8>>;

        hybrid_transport ->
            <<"hybrid"/utf8>>;

        internal ->
            <<"internal"/utf8>>;

        nfc ->
            <<"nfc"/utf8>>;

        usb ->
            <<"usb"/utf8>>
    end.

-file("src/plinth/browser/credentials/public_key.gleam", 171).
-spec hint_to_string(hint()) -> binary().
hint_to_string(Hint) ->
    case Hint of
        security_key ->
            <<"security-key"/utf8>>;

        client_device ->
            <<"client-device"/utf8>>;

        hybrid_hint ->
            <<"hybrid"/utf8>>
    end.

-file("src/plinth/browser/credentials/public_key.gleam", 192).
-spec json_bitarry(bitstring()) -> gleam@json:json().
json_bitarry(Bytes) ->
    gleam@json:string(gleam@bit_array:base64_url_encode(Bytes, false)).

-file("src/plinth/browser/credentials/public_key.gleam", 284).
-spec allow_exclude_credential_to_json({bitstring(), list(transport())}) -> gleam@json:json().
allow_exclude_credential_to_json(Credential_id) ->
    {Id, Transports} = Credential_id,
    gleam@json:object(
        [{<<"id"/utf8>>, json_bitarry(Id)},
            {<<"transports"/utf8>>,
                gleam@json:array(
                    Transports,
                    fun(X) -> gleam@json:string(transport_to_string(X)) end
                )},
            {<<"type"/utf8>>, gleam@json:string(<<"public-key"/utf8>>)}]
    ).
