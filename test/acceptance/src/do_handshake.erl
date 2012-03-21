-module(do_handshake).
-export([setup/0, teardown/1, given/3, then/3]).

setup()->
  start_http_server(8080),
  ok.

teardown(_State) ->
  ok.

given([that, i, want, to, connect, to, a, websocket, server], _State, _) ->
  {ok, _State}.

then([i, the, connection, with, a, websocket, handshake], _State, _) ->
  false.

%
% Mock a http server
%

start_http_server(Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [{reuseaddr, true}, {packet, raw}, binary]),
  {ok, Socket} = gen_tcp:accept(Listen),
  handshake(Socket).

handshake(Socket) ->
  receive
    {tcp, Socket, Data } ->
      {ok, {http_request, 'GET', _Uri, _Version}, Rest} = erlang:decode_packet(http, Data, []),

      SecWebSocketKey = 'Sec-WebSocket-Key'(Rest),
      HandShake = [
        "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
        "Upgrade: WebSocket\r\n",
        "Connection: Upgrade\r\n",
        "Sec-WebSocket-Accept: Hash\r\n\r\n"
       ],
       gen_tcp:send(Socket, HandShake),
      wait(Socket)
  end.

'Sec-WebSocket-Key'(Packet) ->
  {ok, {http_header, _, Key, _, Value}, Rest} = erlang:decode_packet(httph, Packet, []),

  F = fun(S) when is_atom(S)-> atom_to_list(S);
        (S)-> S
      end,

  case string:to_lower(F(Key)) of
    "sec-websocket-key" ->
      Value;
    _ ->
      'Sec-WebSocket-Key'(Rest)
  end.

wait(_Socket)->
  false.
