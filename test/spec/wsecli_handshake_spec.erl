-module(wsecli_handshake_spec).
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

              HttpMessage = HandShake#handshake.message,
              assert_that(wsecli_http:get_start_line_value(method, HttpMessage), is("GET")),
              assert_that(wsecli_http:get_start_line_value(version, HttpMessage), is("1.1")),
              assert_that(wsecli_http:get_start_line_value(resource, HttpMessage), is("/")),

              assert_that(wsecli_http:get_header_value("Host", HttpMessage), is(Host ++ ":" ++ integer_to_list(Port))),
              assert_that(wsecli_http:get_header_value("Upgrade", HttpMessage), is("websocket")),
              assert_that(wsecli_http:get_header_value("Connection", HttpMessage), is("upgrade")),
              assert_that(wsecli_http:get_header_value("Sec-Websocket-Key", HttpMessage), is_not(undefined)),
              assert_that(wsecli_http:get_header_value("Sec-Websocket-Version", HttpMessage), is("13"))
          end)
    end).
