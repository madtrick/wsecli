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
      end)
    end).
