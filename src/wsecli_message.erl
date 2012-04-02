-module(wsecli_message).
-include("wsecli.hrl").

-export([encode/2]).

-define(FRAGMENT_SIZE, 4096).

-spec encode(Data::string() | binary(), Type::atom()) -> binary().
encode(Data, Type) when is_list(Data)->
  encode(list_to_binary(Data), Type);

encode(Data, Type)->
  lists:reverse(encode(Data, Type, [])).

-spec encode(Data::binary(), Type :: atom(), Acc ::list()) -> list().
encode(<<Data:?FRAGMENT_SIZE/binary>>, Type, Acc) ->
  [frame(Data, [fin, {opcode, Type}]) | Acc];

encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, []) ->
  encode(Rest, continuation, [frame(Data, [{opcode, Type}]) | []]);

encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, Acc) ->
  encode(Rest, Type, [frame(Data, [{opcode, Type}]) | Acc]);

encode(<<>>, _Type, Acc) ->
  Acc;

encode(<<Data/binary>>, Type, Acc) ->
  [frame(Data, [fin, {opcode, Type}]) | Acc].

-spec frame(Data::binary(), Options::list()) -> binary().
frame(Data, Options) ->
  Frame = wsecli_framing:frame(Data, Options),
  wsecli_framing:to_binary(Frame).

