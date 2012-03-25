-module(wsecli_http).
-include("wsecli.hrl").

-export([request/2, to_response/1, get_start_line_value/2, get_header_value/2]).

-define(CTRL, "\r\n").

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

-spec to_response(Data::binary()) -> #http_message{}.
to_response(Data) ->
  [StatusLine | Headers] = binary:split(Data, <<?CTRL>>, [trim, global]),
  {match, [_, Version, Status, Reason]} = re:run(StatusLine, "HTTP/([0-9]\.[0-9])\s([0-9]{3,3})\s([a-zA-z0-9 ]+)", [{capture, all, list}]),

  StatusLineList = [{version, Version}, {status, Status}, {reason, Reason}],

  HeadersList = lists:foldr(fun(Element, Acc) ->
        {match, [_Match, HeaderName, HeaderValue]} = re:run(Element, "(\.+):\s+(\.+)", [{capture, all, list}]),
        [{list_to_atom(string:to_lower(string:strip(HeaderName))), HeaderValue} | Acc]
    end, [], Headers),


  #http_message{ type = response , start_line = StatusLineList, headers = HeadersList}.

-spec get_start_line_value(Key::atom(), Message::#http_message{}) -> string().
get_start_line_value(Key, Message) ->
  proplists:get_value(Key, Message#http_message.start_line).

-spec get_header_value(Key::atom(), Message::#http_message{}) -> string().
get_header_value(Key, Message) ->
  proplists:get_value(Key, Message#http_message.headers).
