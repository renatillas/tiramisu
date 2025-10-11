%% @private

-module(hpack_integer).

%% @doc For encoding/decoding HPACK's primitive integer representation
%% as defined http://tools.ietf.org/html/rfc7541#section-5.1

-export([
         decode/2,
         encode/2
        ]).

%% To save every bit on the wire, an integer can fill a partial octet,
%% and usually does in HPACK. If that integer is small enough to fit,
%% then no further work is required.

%% In the octet below, the first three bits were used for something
%% else. Doesn't matter what. What matters is that we have 5 bits left
%% for the integer, which means if it's less than 2^5-1, we're all
%% good!

%%    0   1   2   3   4   5   6   7
%%   +---+---+---+---+---+---+---+---+
%%   | ? | ? | ? |       Value       |
%%   +---+---+---+-------------------+

%% So what if it's greater than 2^5-1?

%%     0   1   2   3   4   5   6   7
%%   +---+---+---+---+---+---+---+---+
%%   | ? | ? | ? | 1   1   1   1   1 |
%%   +---+---+---+-------------------+
%%   | 1 |    Value-(2^5-1) LSB      |
%%   +---+---------------------------+
%%                  ...
%%   +---+---------------------------+
%%   | 0 |    Value-(2^5-1) MSB      |
%%   +---+---------------------------+

%% Then we don't want to waste those first 5 bits, so they're all set
%% to 1, and we know the integer is greater than 2^5-1. How much
%% greater? That's the question we answer with the following octets.

%% "Value-(2^5-1)" is "how much greater?". Let's call it I.

-spec decode(binary(), pos_integer()) -> {non_neg_integer(), binary()}.
decode(<<1:1,Bin/bits>>, 1) ->
    prefix_plus(1, decode(Bin, 0, 0));
decode(<<3:2,Bin/bits>>, 2) ->
    prefix_plus(3, decode(Bin, 0, 0));
decode(<<7:3,Bin/bits>>, 3) ->
    prefix_plus(7, decode(Bin, 0, 0));
decode(<<15:4,Bin/bits>>, 4) ->
    prefix_plus(15, decode(Bin, 0, 0));
decode(<<31:5,Bin/bits>>, 5) ->
    prefix_plus(31, decode(Bin, 0, 0));
decode(<<63:6,Bin/bits>>, 6) ->
    prefix_plus(63, decode(Bin, 0, 0));
decode(<<127:7,Bin/bits>>, 7) ->
    prefix_plus(127, decode(Bin, 0, 0));
decode(<<255:8,Bin/bits>>, 8) ->
    prefix_plus(255, decode(Bin, 0, 0));
%% This clause means we have something small enough to fit in prefix
decode(Bin, Prefix) ->
    <<Value:Prefix,Rem/bits>> = Bin,
    {Value, Rem}.

-spec decode(binary(), non_neg_integer(), non_neg_integer()) -> {non_neg_integer(), binary()}.
decode(<<1:1,Int:7,Rem/binary>>, M, I) ->
    decode(Rem, M+7, round(I + Int * math:pow(2, M)));
decode(<<0:1,Int:7,Rem/binary>>, M, I) ->
    {round(I + Int * math:pow(2, M)), Rem}.

-spec prefix_plus(pos_integer(), {non_neg_integer(), binary()}) -> {non_neg_integer(), binary()}.
prefix_plus(Prefix, {I, Rem}) ->
    {Prefix+I, Rem}.

-spec encode(non_neg_integer(), pos_integer()) -> binary().
%% First clauses are performance optimizations for Int == 2^Prefix - 1
encode(  1,1) -> <<  1:1,0:8>>;
encode(  3,2) -> <<  3:2,0:8>>;
encode(  7,3) -> <<  7:3,0:8>>;
encode( 15,4) -> << 15:4,0:8>>;
encode( 31,5) -> << 31:5,0:8>>;
encode( 63,6) -> << 63:6,0:8>>;
encode(127,7) -> <<127:7,0:8>>;
encode(255,8) -> <<255,0>>;
encode(Int, N) when Int < (1 bsl N - 1) ->
    <<Int:N>>;
encode(Int, N) ->
    Prefix = 1 bsl N - 1,
    Remaining = Int - Prefix,
    Bin = encode_(Remaining, <<>>),
    <<Prefix:N, Bin/binary>>.

-spec encode_(non_neg_integer(), binary()) -> binary().
encode_(I, BinAcc) ->
    LeastSigSeven = (I rem 128),
    RestToEncode = I bsr 7,
    case RestToEncode of
        0 ->
            <<BinAcc/binary, LeastSigSeven>>;
        _ -> %% Adds the continuation bit
            ThisByte = 128 + LeastSigSeven,
            encode_(RestToEncode, <<BinAcc/binary, ThisByte>>)
    end.
