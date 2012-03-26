-module(wsecli_handshake).

-include("wsecli.hrl").
-export([build/3]).

-define(VERSION, 13).

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
