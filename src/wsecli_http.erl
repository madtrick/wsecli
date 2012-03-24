-module(wsecli_http).

-export([request/2]).

-spec request(RequestLine::list({atom(), string()}), Headers::list({atom(), string()})) -> list(string()).
request(RequestLine, Headers) ->
  build_request_line(
    RequestLine,
    build_headers(Headers, ["\r\n"])
  ).

-spec build_headers(list({atom(), string()}), list(string())) -> list(string()).
build_headers(Headers, Acc) ->
  lists:foldr(fun({Key, Value}, AccIn) ->
        [ atom_to_list(Key) ++ ": " ++ Value ++ "\r\n" | AccIn]
    end, Acc, Headers).

-spec build_request_line(list({atom(), string()}), list(string())) -> list(string()).
build_request_line(RequestLine, Acc) ->
  Method   = proplists:get_value(method, RequestLine),
  Version  = proplists:get_value(version, RequestLine),
  Resource = proplists:get_value(resource, RequestLine),

  [Method ++ " " ++ Resource ++ " " ++ "HTTP/" ++ Version ++ "\r\n" | Acc].
