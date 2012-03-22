-module(wsecli_handshake).

-include("wsecli.hrl").
-export([build/3]).

-define(VERSION, 13).

-spec build(Resource ::string(), Host ::string(), Port::integer()) -> #handshake{}.
build(Resource, Host, Port) ->
  RequestLine = [
    {"method", "GET"},
    {"version", "1.1"},
    {"resource", Resource}
  ],

  Headers =[
    {"host", Host ++ ":" ++ integer_to_list(Port)},
    {"upgrade", "websocket"},
    {"connection", "upgrade"},
    {"sec-websocket-key", wsecli_key:generate()},
    {"sec-websocket-version", integer_to_list(?VERSION)}
  ],

  #handshake{
    version = ?VERSION,
    request_line = RequestLine,
    headers = Headers
  }.
