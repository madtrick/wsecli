-module(wsecli_http).
-include("wsecli.hrl").

-export([build/3, request/2, from_response/1, get_start_line_value/2, get_header_value/2]).

-define(CTRL, "\r\n").

-spec build(Type::atom(), StartLine::list({atom(), string()}), Headers::list({string(), string()})) -> list(string()).
build(Type, StartLine, Headers) ->
  #http_message{type = Type, start_line = StartLine, headers = Headers}.

-spec request(RequestLine::list({atom(), string()}), Headers::list({string(), string()})) -> list(string()).
request(RequestLine, Headers) ->
  build_request_line(
    RequestLine,
    build_headers(Headers, ["\r\n"])
  ).

-spec build_headers(list({HeaderName::string(), HeaderValue::string()}), list(string())) -> list(string()).
build_headers(Headers, Acc) ->
  lists:foldr(fun({Key, Value}, AccIn) ->
        [ Key ++ ": " ++ Value ++ "\r\n" | AccIn]
    end, Acc, Headers).

-spec build_request_line(list({Name::atom(), Value::string()}), list(string())) -> list(string()).
build_request_line(RequestLine, Acc) ->
  Method   = proplists:get_value(method, RequestLine),
  Version  = proplists:get_value(version, RequestLine),
  Resource = proplists:get_value(resource, RequestLine),

  [Method ++ " " ++ Resource ++ " " ++ "HTTP/" ++ Version ++ "\r\n" | Acc].

-spec from_response(Data::binary()) -> #http_message{}.
from_response(Data) ->
  [StatusLine | Headers] = binary:split(Data, <<?CTRL>>, [trim, global]),
  {match, [_, Version, Status, Reason]} = re:run(StatusLine, "HTTP/([0-9]\.[0-9])\s([0-9]{3,3})\s([a-zA-z0-9 ]+)", [{capture, all, list}]),

  StatusLineList = [{version, Version}, {status, Status}, {reason, Reason}],

  HeadersList = lists:foldr(fun(Element, Acc) ->
        {match, [_Match, HeaderName, HeaderValue]} = re:run(Element, "(\.+):\s+(\.+)", [{capture, all, list}]),
        [{string:strip(HeaderName), HeaderValue} | Acc]
    end, [], Headers),


  #http_message{ type = response , start_line = StatusLineList, headers = HeadersList}.

-spec get_start_line_value(Key::atom(), Message::#http_message{}) -> string().
get_start_line_value(Key, Message) ->
  proplists:get_value(Key, Message#http_message.start_line).

-spec get_header_value(Key::string(), Message::#http_message{}) -> string().
get_header_value(Key, Message) ->
  LowerCasedKey = string:to_lower(Key),
  get_header_value_case_insensitive(LowerCasedKey, Message#http_message.headers).

-spec get_header_value_case_insensitive(Key::string(), list()) ->  undefined;
                                        (Key::string(), list()) -> string().
get_header_value_case_insensitive(_, []) ->
  undefined;

get_header_value_case_insensitive(Key, [{Name, Value} | Tail]) ->
  LowerCaseName = string:to_lower(Name),
  case Key == LowerCaseName of
    true ->
      Value;
    false ->
      get_header_value_case_insensitive(Key, Tail)
  end.
