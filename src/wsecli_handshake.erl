%% @hidden

-module(wsecli_handshake).

-include("wsecli.hrl").
-export([build/3, validate/2]).

-define(VERSION, 13).
-define(GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

-spec build(Resource ::string(), Host ::string(), Port::integer()) -> #handshake{}.
build(Resource, Host, Port) ->
  RequestLine = [
    {method, "GET"},
    {version, "1.1"},
    {resource, Resource}
  ],

  Headers =[
    {"Host", Host ++ ":" ++ integer_to_list(Port)},
    {"Upgrade", "websocket"},
    {"Connection", "upgrade"},
    {"Sec-Websocket-Key", wsecli_key:generate()},
    {"Sec-Websocket-Version", integer_to_list(?VERSION)}
  ],

  Message = wsecli_http:build(request, RequestLine, Headers),
  #handshake{ version = ?VERSION, message = Message}.

-spec validate(Response::#http_message{}, Handshake::#handshake{}) -> boolean().
validate(Response, Handshake) ->
  validate_http_status(Response)
  and
  validate_upgrade_header(Response)
  and
  validate_connection_header(Response)
  and
  validate_sec_websocket_accept_header(Response, Handshake).


-spec validate_http_status(Response::#http_message{}) -> boolean().
validate_http_status(Response) ->
  "101" == wsecli_http:get_start_line_value(status, Response).

-spec validate_upgrade_header(Response ::#http_message{}) -> boolean().
validate_upgrade_header(Response) ->
  "websocket" == string:to_lower(wsecli_http:get_header_value("upgrade", Response)).

-spec validate_connection_header(Response ::#http_message{}) -> boolean().
validate_connection_header(Response) ->
  "upgrade" == string:to_lower(wsecli_http:get_header_value("connection", Response)).

-spec validate_sec_websocket_accept_header(Response::#http_message{}, Handshake::#handshake{}) -> boolean().
validate_sec_websocket_accept_header(Response, Handshake) ->
  ClientKey         = wsecli_http:get_header_value("sec-websocket-key", Handshake#handshake.message),
  BinaryClientKey   = list_to_binary(ClientKey),
  ExpectedHashedKey = base64:encode_to_string(crypto:sha(<<BinaryClientKey/binary, ?GUID>>)),
  HashedKey         = wsecli_http:get_header_value("sec-websocket-accept", Response),

  ExpectedHashedKey == HashedKey.
