# hpack

HPACK Implementation for Erlang

[HPACK RFC-7541](http://tools.ietf.org/html/rfc7541)

This implementation was designed for use by
[Chatterbox](http://github.com/joedevivo/chatterbox), but could be
used by any HTTP/2 implementation (or, if something other than HTTP/2
has need for HPACK, this would work as well).

## Why Separate?

* Use in other projects
* A separate RFC seemed like a really clear abstraction

## What's Covered

* Compression contexts ([RFC-7541 Section 2.2](http://tools.ietf.org/html/rfc7541#section-2.2))
* Dynamic table management ([RFC-7541 Section 4](http://tools.ietf.org/html/rfc7541#section-4))
* Primitive type representations ([RFC-7541 Section 5](http://tools.ietf.org/html/rfc7541#section-5))
* Binary format ([RFC-7541 Section 6](http://tools.ietf.org/html/rfc7541#section-6))
  * Indexed Header Field Representation
  * Literal Header Field Representation
  * Dynamic Table Size Update

## What's not covered

### HTTP/2 Frames

An HTTP/2 implementation should be responsible for anything you need
to do in order to read a compressed header block from various
PUSH_PROMISE, HEADERS, and/or CONTINUATION frames. Once you have a
complete block, you can use this library to turn it into something you
can use for fulfilling HTTP requests

## API

### Creating Contexts

`hpack` provides a single type that represents both encoding and decoding
contexts. A new context can be created with `hpack:new_context/0` or
`hpack:new_context/1` (passing the size for the `HEADER_TABLE_SIZE` setting).

### Changing table size

If HTTP/2 settings get renegotiated, you can pass that information
along by calling `hpack:new_max_table_size/2`, like this:

``` erlang
NewContext = hpack:new_max_table_size(NewSize, OldContext).
```

### Decoding Headers

Decode a headers binary with `hpack:decode/2`. It's your job to
assemble the binary if it's coming from multiple HTTP/2 frames.

``` erlang
{ok, {Headers, NewContext}} = hpack:decode(Binary, OldContext).
```

`Headers` is a list of headers, where each header is a `{binary(), binary()}`
tuple.

### Encoding Headers

Encoding headers works the same way, only a `[{binary(), binary()}]`
goes in, and a `binary()` comes out.

``` erlang
{Bin, NewContext} = hpack:encode(Headers, OldContext),
```

### Soup to nuts

Here's how to do the whole thing!

``` erlang

DecodeContext1 = hpack:new_context(), %% Server context
EncodeContext1 = hpack:new_context(), %% Client context

ClientRequestHeaders = [
        {<<":method">>, <<"GET">>},
        {<<":path">>, <<"/">>},
        {<<"X-Whatev">>, <<"commands">>}
    ],

%% Client operation
{RequestHeadersBin, EncodeContext2} = hpack:encode(
    ClientRequestHeaders,
    EncodeContext1),

%% Server operation, after receiving RequestHeadersBin
{ok, {ServerRequestHeaders, DecodeContext2}} = hpack:decode(
    RequestHeadersBin,
    DecodeContext1),

%% Note the following truths:
ClientRequestHeaders = ServerRequestHeaders,
EncodeContext1 = DecodeContext1,
EncodeContext2 = DecodeContext2.

```

The whole reason HPACK works is that the client and server both keep
their contexts in sync with each other.

*Note*: I used the terms `client` and `server` here, but it could as
easily be `sender` and `receiver` if you're a proxy.
