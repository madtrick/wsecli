-module(wsecli_spec).
-include_lib("espec/include/espec.hrl").

spec() ->
  describe("wsecli", fun() ->
        it("should do a proper WebSocket handshake")
    end).
