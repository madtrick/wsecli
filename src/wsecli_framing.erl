-module(wsecli_framing).
-include("wsecli.hrl").

-export([to_binary/1, frame/1, frame/2, control_frame/1]).

-define(OP_CODE_CONT, 0).
-define(OP_CODE_TEXT, 1).
-define(OP_CODE_BIN, 2).
-define(OP_CODE_CLOSE, 8).
-define(OP_CODE_PING, 9).
-define(OP_CODE_PONG, 10).

-spec to_binary(Frame::#frame{}) -> binary().
to_binary(Frame) ->
  <<
    (Frame#frame.fin):1,
    (Frame#frame.rsv1):1, (Frame#frame.rsv2):1, (Frame#frame.rsv3):1,
    (Frame#frame.opcode):4,
    (Frame#frame.mask):1,
    (Frame#frame.payload_len):7,
    (Frame#frame.extended_payload_len):(extended_payload_len_bit_width(Frame#frame.extended_payload_len, 16)),
    (Frame#frame.extended_payload_len_cont):(extended_payload_len_bit_width(Frame#frame.extended_payload_len_cont, 64)),
    (Frame#frame.masking_key):32,
    (Frame#frame.payload)/binary
  >>.

extended_payload_len_bit_width(PayloadLen, Max) ->
  case PayloadLen of
    0 -> 0;
    _ -> Max
  end.

-spec frame(Data::binary() | string()) -> #frame{}.
frame(Data) when is_binary(Data) ->
  frame(Data, [{opcode, binary}]);

frame(Data) when is_list(Data)->
  frame(list_to_binary(Data), [{opcode, text}]).

-spec frame(Data::string() | binary(), Options::list()) -> #frame{}.
frame(Data, Options) when is_list(Data) ->
  frame(list_to_binary(Data), Options);

frame(Data, Options) ->
  Frame = #frame{},
  Frame2 = length(Frame, Data),
  Frame3 = mask(Frame2, Data),
  apply_options(Frame3, Options).

-spec apply_options(Frame::#frame{}, Options::list()) -> #frame{}.
apply_options(Frame, [fin | Tail]) ->
  T = Frame#frame{fin = 1},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, continuation} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_CONT},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, text} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_TEXT},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, binary} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_BIN},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, close} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_CLOSE},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, ping} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_PING},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, pong} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_PONG},
  apply_options(T, Tail);

apply_options(Frame, []) ->
  Frame.

-spec control_frame({close, Code::integer(), Reason::string()}) -> #frame{};
                  ({ping, Reason::string()}) -> #frame{};
                  ({pong, Reason::string()}) -> #frame{}.
control_frame({close, Code, Reason}) ->
  %%TODO: enforce that Reason + Code is <= than 125 bytes
  BinReason = list_to_binary(Reason),
  Data = <<Code:16, BinReason/binary>>,
  frame(Data, [fin, {opcode, close}]);

control_frame({pong, Reason}) ->
  frame(Reason, [fin, {opcode, pong}]);

control_frame({ping, Reason}) ->
  frame(Reason, [fin, {opcode, ping}]).

-spec op_code( Data ::binary() | string()) -> pos_integer().
op_code(Data) when is_binary(Data) ->
  ?OP_CODE_BIN;

op_code(Data) ->
  ?OP_CODE_TEXT.

-spec length(Frame::#frame{}, Data :: binary()) -> #frame{}.
length(Frame, Data) ->
  %Len = string:len(Data),
  Len = byte_size(Data),
  if
    Len =< 125 ->
      Frame#frame{
        payload_len = Len,
        extended_payload_len = 0,
        extended_payload_len_cont = 0
      };
    (Len > 125) and (Len =< 65536) ->
      Frame#frame{
        payload_len = 126,
        extended_payload_len = Len,
        extended_payload_len_cont = 0
      };
    Len > 65536 ->
      Frame#frame{
        payload_len = 127,
        extended_payload_len = 0,
        extended_payload_len_cont = Len
      }
  end.

-spec mask(Frame::#frame{}, Data::binary()) -> #frame{}.
mask(Frame, Data) ->
  <<MaskKey:32>> = crypto:rand_bytes(4),
  %BinData = list_to_binary(Data),

  Frame#frame{
    mask = 1,
    masking_key = MaskKey,
    payload = mask(Data, MaskKey, <<>>)
  }.


%
% Masking code got at Cowboy source code
%
-spec mask(Data::binary(), MaskKey::integer(), Acc::binary()) -> binary().
mask(<<Data:32, Rest/bits>>, MaskKey, Acc) ->
  T = Data bxor MaskKey,
  mask(Rest, MaskKey, <<Acc/binary, T:32>>);

mask(<<Data:24>>, MaskKey, Acc) ->
  <<MaskKey2:24, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:24>>;

mask(<<Data:16>>, MaskKey, Acc) ->
  <<MaskKey2:16, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:16>>;

mask(<<Data:8>>, MaskKey, Acc) ->
  <<MaskKey2:8, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:8>>;

mask(<<>>, _, Acc) ->
  Acc.
