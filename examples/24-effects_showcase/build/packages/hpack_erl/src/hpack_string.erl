%% @private

-module(hpack_string).

-export([
         decode/1
        ]).

-spec decode(binary()) -> {binary(), binary()}.
decode(<<>>) ->
    {<<>>, <<>>};
decode(<<Huff:1,Bin/bits>>) ->
    {Length, Rem} = hpack_integer:decode(Bin, 7),
    <<RawLiteral:Length/binary,B2/bits>> = Rem,
    Literal = case Huff of
                  1 ->
                      huffman:decode(RawLiteral);
                  0 ->
                      RawLiteral
              end,
    {Literal, B2}.

%% As the Huffman-encoded data doesn't always end at an octet
%% boundary, some padding is inserted after it, up to the next octet
%% boundary.  To prevent this padding from being misinterpreted as
%% part of the string literal, the most significant bits of the code
%% corresponding to the EOS (end-of-string) symbol are used.
%% EOS (256)  |11111111|11111111|11111111|111111      3fffffff  [30]
