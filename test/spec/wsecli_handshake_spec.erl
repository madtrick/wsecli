-module(wsecli_handshake).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

-include("wsecli.hrl").

spec() ->
  describe("wsecli_handshake", fun() ->
        it("should return a valid handshake request", fun() ->
              Resource  = "/",
              Host     = "localhost",
              Port      = 8080,

              HandShake = wsecli_handshake:build(Resource, Host, Port),
              assert_that(HandShake#handshake.version, is(13)),

              RequestLine = HandShake#handshake.request_line,
              assert_that(proplists:get_value("method", RequestLine), is("GET")),
              assert_that(proplists:get_value("version", RequestLine), is("1.1")),
              assert_that(proplists:get_value("resource", RequestLine), is(Resource)),

              Headers = HandShake#handshake.headers,
              assert_that(proplists:get_value("host", Headers), is(Host ++ ":" ++ integer_to_list(Port))),
              assert_that(proplists:get_value("upgrade", Headers), is("websocket")),
              assert_that(proplists:get_value("connection", Headers), is("upgrade")),
              assert_that(proplists:get_value("sec-websocket-key", Headers), is_not(undefined)),
              assert_that(proplists:get_value("sec-websocket-version", Headers), is("13"))
          end)
    end).
