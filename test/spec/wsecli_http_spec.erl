-module(wsecli_http_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("wsecli.hrl").

spec() ->
  describe("wsecli_http", fun() ->
        it("should build proper HTTP request strings", fun() ->
          RequestLine = [
            {method, "GET"},
            {version, "1.1"},
            {resource, "/"}
          ],

          Headers = [
            {'Header-A', "A"},
            {'Header-B', "B"}
          ],

          Request = wsecli_http:request(RequestLine, Headers),

          assert_that(Request, is([
                "GET / HTTP/1.1\r\n",
                "Header-A: A\r\n",
                "Header-B: B\r\n",
                "\r\n"
              ]))
      end),
    it("should build a proper HTTP response from binary message", fun() ->
          Data = <<"HTTP/1.1 205 Reset Content\r\n
          Header-A: A\r\n
          Header-C: dGhlIHNhbXBsZSBub25jZQ==\r\n
          Header-D: D\r\n\r\n">>,

          StatusLine = [
            {version, "1.1"},
            {status, "205"},
            {reason, "Reset Content"}
          ],

          Headers = [
            {'Header-A', "A"},
            {'Header-C', "dGhlIHNhbXBsZSBub25jZQ=="},
            {'Header-D', "D"}
          ],

          ExpectedResponse = #http_message{type = response, start_line = StatusLine, headers = Headers},
          Response = wsecli_http:to_response(Data),

          assert_that(Response, is(ExpectedResponse))
      end)
    end).
