%% @doc
%% The `hpack' module provides functions for working with HPACK as described in
%% <a href="https://tools.ietf.org/html/rfc7541">RFC 7541</a>.

%% @reference <a href="https://tools.ietf.org/html/rfc7541">RFC 7541</a>

-module(hpack).

%% API Exports
-export([
         new_context/0,
         new_context/1,
         decode/2,
         encode/2,
         new_max_table_size/2,
         all_fields_indexed/2
        ]).

%% Datatypes for the real world:
-record(hpack_context,
        {
          dynamic_table = hpack_index:new(),
          connection_max_table_size = 4096 :: non_neg_integer()
        }).
-type context() :: #hpack_context{}.

-export_type([context/0]).

-type header_name() :: binary().
-type header_value() :: binary().
-type header() :: {header_name(), header_value()}.
-type headers() :: [header()].

-export_type([
              header_name/0,
              header_value/0,
              header/0,
              headers/0
             ]).

%% @equiv new_context(4096)
-spec new_context() -> context().
new_context() -> #hpack_context{}.

%% @doc
%% Returns a new HPACK context with the given `MaxTableSize' as the max table size.
-spec new_context(non_neg_integer()) -> context().
new_context(MaxTableSize) ->
    #hpack_context{
       connection_max_table_size=MaxTableSize
      }.

%% @doc
%% Updates the max table size of the given HPACK context (`Context') to the given
%% `NewSize'.
%%
%% Useful when HTTP/2 settings are renegotiated.
-spec new_max_table_size(non_neg_integer(), context())
                        -> context().
new_max_table_size(NewSize,
                   #hpack_context{
                      dynamic_table=T,
                      connection_max_table_size=OldSize
                     }=Context) ->
    NewT = case OldSize > NewSize of
               true ->
                   hpack_index:resize(NewSize, T);
               _ ->
                   T
           end,
    Context#hpack_context{
      dynamic_table=NewT,
      connection_max_table_size=NewSize
     }.

%% @doc
%% Encodes the given `Headers' using the given `Context'.
%%
%% When successful, returns a `{ok, {EncodedHeaders, NewContext}}' tuple where
%% `EncodedHeaders' is a binary representing the encoded headers and `NewContext'
%% is the new HPACK context.
%%
%% For example:
%% ```
%% Headers = [{<<":method">>, <<"GET">>}],
%% {ok, {EncodedHeaders, NewContext}} = hpack:encode(Headers, hpack:new_context()).
%% '''
-spec encode(headers(),
             context())
            -> {ok, {binary(), context()}}
                   | {error, term()}.
encode(Headers, Context) ->
    encode(Headers, <<>>, Context).

%% @doc
%% Decodes the given binary into a list of headers using the given HPACK
%% context.
%%
%% If successful, returns a `{ok, {Headers, NewContext}}' tuple where `Headers'
%% are the decoded headers and `NewContext' is the new HPACK context.
%%
%% For example:
%% ```
%% {Headers, NewContext} = hpack:decode(Binary, OldContext).
%% '''
-spec decode(binary(), context()) ->
                    {ok, {headers(), context()}}
                  | {error, compression_error}
                  | {error, {compression_error, {bad_header_packet, binary()}}}.
decode(Bin, Context) ->
    decode(Bin, [], Context).

%% Private Functions

-spec decode(binary(), headers(), context())
            ->
                    {ok, {headers(), context()}}
                  | {error, compression_error}
                  | {error, {compression_error, {bad_header_packet, binary()}}}.
%% We're done decoding, return headers
decode(<<>>, HeadersAcc, C) ->
    {ok, {HeadersAcc, C}};
%% First bit is '1', so it's an 'Indexed Header Feild'
%% http://http2.github.io/http2-spec/compression.html#rfc.section.6.1
decode(<<2#1:1,_/bits>>=B, HeaderAcc, Context) ->
    decode_indexed_header(B, HeaderAcc, Context);
%% First two bits are '01' so it's a 'Literal Header Field with Incremental Indexing'
%% http://http2.github.io/http2-spec/compression.html#rfc.section.6.2.1
decode(<<2#01:2,_/bits>>=B, HeaderAcc, Context) ->
    decode_literal_header_with_indexing(B, HeaderAcc, Context);

%% First four bits are '0000' so it's a 'Literal Header Field without Indexing'
%% http://http2.github.io/http2-spec/compression.html#rfc.section.6.2.2
decode(<<2#0000:4,_/bits>>=B, HeaderAcc, Context) ->
    decode_literal_header_without_indexing(B, HeaderAcc, Context);

%% First four bits are '0001' so it's a 'Literal Header Field never Indexed'
%% http://http2.github.io/http2-spec/compression.html#rfc.section.6.2.3
decode(<<2#0001:4,_/bits>>=B, HeaderAcc, Context) ->
    decode_literal_header_never_indexed(B, HeaderAcc, Context);

%% First three bits are '001' so it's a 'Dynamic Table Size Update'
%% http://http2.github.io/http2-spec/compression.html#rfc.section.6.3
decode(<<2#001:3,_/bits>>=B, HeaderAcc, Context) ->
    decode_dynamic_table_size_update(B, HeaderAcc, Context);

%% Oops!
decode(<<B:1,_/binary>>, _HeaderAcc, _Context) ->
    {error, {compression_error, {bad_header_packet, B}}}.

decode_indexed_header(<<2#1:1,B1/bits>>,
                      Acc,
                      Context = #hpack_context{dynamic_table=T}) ->
    {Index, B2} = hpack_integer:decode(B1, 7),
    decode(B2, Acc ++ [hpack_index:lookup(Index, T)], Context).

%% The case where the field isn't indexed yet, but should be.
decode_literal_header_with_indexing(<<2#01:2,2#000000:6>>, _Acc, _Context) ->
    {error, compression_error};
decode_literal_header_with_indexing(<<2#01:2,2#000000:6,B1/bits>>, Acc,
                                    #hpack_context{
                                       dynamic_table=T
                                      }=Context) ->
    {Str, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    decode(B3,
           Acc ++ [{Str, Value}],
           Context#hpack_context{dynamic_table=hpack_index:add(Str, Value, T)});
%% This is the case when the index is greater than 0, 0 being not yet
%% indexed
decode_literal_header_with_indexing(<<2#01:2,B1/bits>>, Acc,
    Context = #hpack_context{dynamic_table=T}) ->
    {Index, Rem} = hpack_integer:decode(B1,6),
    {Str, B2} = hpack_string:decode(Rem),
    {Name,_} =
        case hpack_index:lookup(Index, T) of
            undefined ->
                throw(undefined);
            {N, V} ->
                {N, V}
    end,
    decode(B2,
           Acc ++ [{Name, Str}],
           Context#hpack_context{dynamic_table=hpack_index:add(Name, Str, T)}).

decode_literal_header_without_indexing(<<2#0000:4,2#0000:4,B1/bits>>, Acc,
    Context) ->
    {Str, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    decode(B3, Acc ++ [{Str, Value}], Context);
decode_literal_header_without_indexing(<<2#0000:4,B1/bits>>, Acc,
    Context = #hpack_context{dynamic_table=T}) ->
    {Index, Rem} = hpack_integer:decode(B1,4),
    {Str, B2} = hpack_string:decode(Rem),
    {Name,_}= hpack_index:lookup(Index, T),
    decode(B2, Acc ++ [{Name, Str}], Context).

decode_literal_header_never_indexed(<<2#0001:4,2#0000:4,B1/bits>>, Acc,
    Context) ->
    {Str, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    decode(B3, Acc ++ [{Str, Value}], Context);
decode_literal_header_never_indexed(<<2#0001:4,B1/bits>>, Acc,
    Context = #hpack_context{dynamic_table=T}) ->
    {Index, Rem} = hpack_integer:decode(B1,4),
    {Str, B2} = hpack_string:decode(Rem),
    {Name,_}= hpack_index:lookup(Index, T),
    decode(B2, Acc ++ [{Name, Str}], Context).

decode_dynamic_table_size_update(
  <<2#001:3,Bin/bits>>, []=Acc,
  #hpack_context{
     dynamic_table=T,
     connection_max_table_size=ConnMaxSize
    }=Context
 ) ->
    {NewSize, Rem} = hpack_integer:decode(Bin,5),
    case ConnMaxSize >= NewSize of
        true ->
            decode(Rem,
                   Acc,
                   Context#hpack_context{
                     dynamic_table=hpack_index:resize(NewSize, T)
                    });
        _ ->
            {error, compression_error}
    end.

-spec encode(headers(),
             binary(),
             context()) ->
                    {ok, {binary(), context()}}
                        | {error, term()}.
encode([], Acc, Context) ->
    {ok, {Acc, Context}};
encode([{HeaderName, HeaderValue}|Tail], B, Context = #hpack_context{dynamic_table=T}) ->
    {BinToAdd, NewContext} = case hpack_index:match({HeaderName, HeaderValue}, T) of
        {indexed, I} ->
            {encode_indexed(I), Context};
        {literal_with_indexing, I} ->
            {encode_literal_indexed(I, HeaderValue),
             Context#hpack_context{dynamic_table=hpack_index:add(HeaderName, HeaderValue, T)}};
        {literal_wo_indexing, _X} ->
            {encode_literal_wo_index(HeaderName, HeaderValue),
             Context#hpack_context{dynamic_table=hpack_index:add(HeaderName, HeaderValue, T)}}
    end,
    NewB = <<B/binary,BinToAdd/binary>>,
    encode(Tail, NewB, NewContext).

encode_indexed(I) when I < 63 ->
    <<2#1:1,I:7>>;
encode_indexed(I) ->
    Encoded = hpack_integer:encode(I, 7),
    <<2#1:1, Encoded/bits>>.

encode_literal_indexed(I, Value) when I < 63 ->
    BinToAdd = encode_literal(Value),
    <<2#01:2,I:6,BinToAdd/binary>>;
encode_literal_indexed(I, Value) ->
    Index = hpack_integer:encode(I, 6),
    BinToAdd = encode_literal(Value),
    <<2#01:2,Index/bits,BinToAdd/binary>>.

encode_literal(Value) ->
    L = hpack_integer:encode(size(Value),7),
    <<2#0:1,L/bits,Value/binary>>.

encode_literal_wo_index(Name, Value) ->
    EncName = encode_literal(Name),
    EncValue = encode_literal(Value),
    <<2#01000000,EncName/binary,EncValue/binary>>.

all_fields_indexed(Headers, #hpack_context{dynamic_table=T}) ->
    lists:all(fun(Header) ->
                      case hpack_index:match(Header, T) of
                          {indexed, _} ->
                              true;
                          _ ->
                              false
                      end
              end, Headers).
 
