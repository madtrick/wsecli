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
  Frame = wsecli_framing:frame(Data, [fin, {opcode, Type}]),
  BinFrame = wsecli_framing:to_binary(Frame),
  [BinFrame | Acc];

encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, []) ->
  Frame = wsecli_framing:frame(Data, [{opcode, Type}]),
  BinFrame = wsecli_framing:to_binary(Frame),
  encode(Rest, continuation, [BinFrame | []]);

encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, Acc) ->
  Frame = wsecli_framing:frame(Data, [{opcode, Type}]),
  BinFrame = wsecli_framing:to_binary(Frame),
  encode(Rest, Type, [BinFrame | Acc]);

encode(<<>>, _Type, Acc) ->
  Acc;

encode(<<Data/binary>>, Type, Acc) ->
  Frame = wsecli_framing:frame(Data, [fin, {opcode, Type}]),
  BinFrame = wsecli_framing:to_binary(Frame),
  [BinFrame | Acc].

