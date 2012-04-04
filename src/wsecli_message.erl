-module(wsecli_message).
-include("wsecli.hrl").

-export([encode/2, decode/1, decode/2]).

-define(FRAGMENT_SIZE, 4096).
-type message_type() :: begin_message | continue_message.

-spec encode(Data::string() | binary(), Type::atom()) -> binary().
encode(Data, Type) when is_list(Data)->
  encode(list_to_binary(Data), Type);

encode(Data, Type)->
  lists:reverse(encode(Data, Type, [])).

-spec decode(Data::binary()) -> list(#message{}).
decode(Data) ->
  decode(Data, begin_message, #message{}).

-spec decode(Data::binary(), Message::#message{}) -> list(#message{}).
decode(Data, Message) ->
  decode(Data, continue_message, Message).

-spec decode(Data::binary(), Type :: message_type(), Message::#message{}) -> list(#message{}).
decode(Data, begin_message, _Message) ->
  Frames = wsecli_framing:from_binary(Data),
  lists:reverse(process_frames(begin_message, Frames, []));

decode(Data, continue_message, Message) ->
  Frames = wsecli_framing:from_binary(Data),
  lists:reverse(process_frames(continue_message, Frames, [Message | []])).

-spec process_frames(Type:: message_type(), Frames :: list(#frame{}), Messages :: list(#message{})) -> list(#message{}).
process_frames(begin_message, [Frame | Frames], Acc) ->
  case process_frame(Frame, begin_message, #message{}) of
    {fragmented, Message} ->
      process_frames(continue_message, Frames, [Message#message{type = fragmented} | Acc]);
    {completed, Message} ->
      process_frames(begin_message, Frames, [Message | Acc])
  end;

process_frames(continue_message, [Frame | Frames], [FramgmentedMessage | Acc]) ->
  case process_frame(Frame, continue_message, FramgmentedMessage) of
    {fragmented, Message} ->
      process_frames(continue_message, Frames, [Message#message{type = fragmented} | Acc]);
    {completed, Message} ->
      process_frames(begin_message, Frames, [Message | Acc])
  end;

process_frames(_, [], Acc) ->
  Acc.

-spec process_frame(Frame :: #frame{}, MessageType :: message_type(), Message :: #message{})-> {fragmented | completed, #message{}}.
process_frame(Frame, begin_message, Message) ->
  case contextualize_frame(Frame) of
    open_close ->
      BuiltMessage = build_message(Message, [Frame]),
      {completed, BuiltMessage};
    open_continue ->
      Frames = Message#message.frames,
      {fragmented, Message#message{frames = [Frame | Frames]}}
  end;

process_frame(Frame, continue_message, Message) ->
  case contextualize_frame(Frame) of
    continue ->
      Frames = Message#message.frames,
      {fragmented, Message#message{frames = [Frame | Frames]}};
    continue_close ->
      BuiltMessage = build_message(Message, lists:reverse([Frame | Message#message.frames])),
      {completed, BuiltMessage}
  end.

-spec contextualize_frame(Frame :: #frame{}) -> continue_close | open_continue | continue | open_close.
contextualize_frame(Frame) ->
  case {Frame#frame.fin, Frame#frame.opcode} of
    {1, 0} -> continue_close;
    {0, 0} -> continue;
    {1, _} -> open_close;
    {0, _} -> open_continue
  end.

build_message(Message, Frames) ->
  [HeadFrame | _] = Frames,

  case HeadFrame#frame.opcode of
    1 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{type = text, payload = Payload};
    2 ->
      Payload = build_payload_from_frames(binary, Frames),
      Message#message{type = binary, payload = Payload}
  end.

build_payload_from_frames(binary, Frames) ->
  contatenate_payload_from_frames(Frames);

build_payload_from_frames(text, Frames) ->
  Payload = contatenate_payload_from_frames(Frames),
  binary_to_list(Payload).

contatenate_payload_from_frames(Frames) ->
  contatenate_payload_from_frames(Frames, <<>>).

contatenate_payload_from_frames([Frame | Rest], Acc) ->
  contatenate_payload_from_frames(Rest, <<Acc/binary, (Frame#frame.payload)/binary>>);

contatenate_payload_from_frames([], Acc) ->
  Acc.

-spec process(Frames ::list(#frame{}), Messages::list(#message{})) -> list(#message{}).
process([Frame | Tail], Acc) ->
  Message = case Frame#frame.opcode of
    1 ->
      #message{type = text, payload = binary_to_list(Frame#frame.payload)};
    2 ->
      #message{type = binary, payload = Frame#frame.payload}
  end,

  process(Tail, [Message | Acc]);

process([], Acc) ->
  Acc.


%-spec process(Frames::list(#frame{}), IncompleteMessage::#message{}) -> {text | binary | control | fragmented, #message{}}.
%process(Frames, IncompleteMessage) ->
%  [Head | Tail] = Frames,
  
%
% Internal
%
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

