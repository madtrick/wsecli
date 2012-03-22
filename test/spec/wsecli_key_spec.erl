-module(wsecli_key_spec).
-include_lib("espec/include/espec.hrl").

spec() ->
  describe("wsecli_key", fun() ->
        it("should return a valid Sec-WebSocket-Key")
    end).
