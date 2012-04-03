-module(wsecli_message_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("wsecli.hrl").

-define(FRAGMENT_SIZE, 4096).

spec() ->
  describe("encode", fun() ->
        before_all(fun()->
              meck:new(wsecli_framing, [passthrough])
          end),

        after_all(fun()->
              meck:unload(wsecli_framing)
          end),

        it("should set opcode to 'text' if type is text", fun() ->
              wsecli_message:encode("asadsd", text),
              assert_that(meck:called(wsecli_framing, frame, ['_', [fin, {opcode, text}]]), is(true))
          end),
        it("should set opcode to 'binary' if type is binary", fun() ->
              wsecli_message:encode(<<"asdasd">>, binary),
              assert_that(meck:called(wsecli_framing, frame, ['_', [fin, {opcode, binary}]]), is(true))
          end),
        describe("when payload size is <= fragment size", fun()->
              it("should return a list with only one binary fragment", fun()->
                    Data = "Foo bar",
                    [BinFrame | []] = wsecli_message:encode(Data, text),
                    assert_that(byte_size(list_to_binary(Data)),is(less_than(?FRAGMENT_SIZE))),
                    assert_that(is_binary(BinFrame), is(true)),
                    assert_that(meck:called(wsecli_framing, to_binary, '_'), is(true)),
                    assert_that(meck:called(wsecli_framing, frame, '_'), is(true))
                end),
              it("should set opcode to 'type'", fun() ->
                    Data = "Foo bar",
                    [Frame] = wsecli_message:encode(Data, text),

                    <<_:4, Opcode:4, _/binary>> = Frame,

                    assert_that(Opcode, is(1))
                end),
              it("should set fin", fun()->
                    Data = "Foo bar",
                    [Frame] = wsecli_message:encode(Data, text),

                    <<Fin:1, _/bits>> = Frame,

                    assert_that(Fin, is(1))
                end)
          end),
        describe("when payload size is > fragment size", fun() ->
              it("should return a list of binary fragments", fun()->
                    Data = crypto:rand_bytes(5000),
                    Frames = wsecli_message:encode(Data, binary),
                    assert_that(meck:called(wsecli_framing, to_binary, '_'), is(true)),
                    assert_that(meck:called(wsecli_framing, frame, '_'), is(true)),
                    assert_that(length(Frames), is(2))
                end),
              it("should set a payload of 4096 bytes or less on each fragment", fun() ->
                    Data = crypto:rand_bytes(12288),
                    Frames = wsecli_message:encode(Data, binary),

                    [Frame1, Frame2, Frame3] = Frames,

                    <<_:64, Payload1/binary>> = Frame1,
                    <<_:64, Payload2/binary>> = Frame2,
                    <<_:64, Payload3/binary>> = Frame3,

                    assert_that(byte_size(Payload1), is(4096)),
                    assert_that(byte_size(Payload2), is(4096)),
                    assert_that(byte_size(Payload3), is(4096))
                end),
              it("should set opcode to 'type' on the first fragment", fun()->
                    Data = crypto:rand_bytes(5000),
                    Frames = wsecli_message:encode(Data, binary),

                    [FirstFragment | _ ] = Frames,

                    <<_:4, Opcode:4, _/binary>> = FirstFragment,

                    assert_that(Opcode, is(2))
                end),
              it("should unset fin on all fragments but last", fun() ->
                    Data = crypto:rand_bytes(12288), %4096 * 3
                    Frames = wsecli_message:encode(Data, binary),

                    [Frame1, Frame2, Frame3] = Frames,

                    <<Fin1:1, _/bits>> = Frame1,
                    <<Fin2:1, _/bits>> = Frame2,
                    <<Fin3:1, _/bits>> = Frame3,

                    assert_that(Fin1, is(0)),
                    assert_that(Fin2, is(0)),
                    assert_that(Fin3, is(1))
                end),
              it("should set opcode to 'continuation' on all fragments but first", fun() ->
                    Data = crypto:rand_bytes(12288), %4096 * 3
                    Frames = wsecli_message:encode(Data, binary),

                    [Frame1, Frame2, Frame3] = Frames,

                    <<_:4, Opcode1:4, _/binary>> = Frame1,
                    <<_:4, Opcode2:4, _/binary>> = Frame2,
                    <<_:4, Opcode3:4, _/binary>> = Frame3,

                    assert_that(Opcode1, is(2)),
                    assert_that(Opcode2, is(0)),
                    assert_that(Opcode3, is(0))
                end)
          end)
    end),
  it("wsecli_message:control"),
  describe("decode", fun()->
        describe("unfragmented messages", fun()->
              it("shit")
              %it("decodes a text message", fun() ->
              %      Payload = "Iepa yei!",

              %      Fin = 1,
              %      Rsv = 0,
              %      Opcode = 1, %Text
              %      Mask = 0,
              %      PayloadLength = length(Payload),
              %      PayloadData = list_to_binary(Payload),

              %      FakeMessage =
              %        <<Fin:1, Rsv:3, Opcode:4, Mask:1, PayloadLength:7, PayloadData/bits>>,

              %      assert_that(wsecli_message:decode(FakeMessage), is({text, Payload}))
              %  end)
          end)
      end).
