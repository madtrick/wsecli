-module(wsecli_key_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

spec() ->
  describe("wsecli_key", fun() ->
        it("should return a valid Sec-WebSocket-Key", fun() ->
              %Meck crashes if we try to mock crypto module
              %meck:new(crypto, [passthrough]),
              meck:new(base64, [unstick, passthrough]),

              <<_Key:16>> = wsecli_key:generate(),

              %assert_that(meck:called(crypto, rand_bytes, 16), is(true)),
              assert_that(meck:called(base64, encode, '_'), is(true)),
              meck:unload(base64)
          end)
    end).
