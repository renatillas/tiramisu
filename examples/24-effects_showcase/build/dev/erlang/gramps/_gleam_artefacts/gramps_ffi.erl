-module(gramps_ffi).

-export([decode_packet/3]).

decode_packet(Type, Bin, Opts) ->
  case erlang:decode_packet(Type, Bin, Opts) of
    {ok, {http_request, Method, Uri, Version}, Rest} when is_atom(Method) ->
      {ok, {{http_request, atom_to_binary(Method), Uri, Version}, Rest}};
    {ok, Packet, Rest} ->
      {ok, {Packet, Rest}};
    {more, Length} ->
      {error, {more, Length}};
    {error, Reason} ->
      {error, {http_error, Reason}}
  end.
