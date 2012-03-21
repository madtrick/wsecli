-module(do_handshake).

%-export([setup/0, teardown/1, given/3, then/3]).
-compile([export_all]).
-include_lib("hamcrest/include/hamcrest.hrl").

setup()->
  start_http_server(self(), 8080),
  ok.

teardown(_State) ->
  ok.

given([that, i, want, to, connect, to, a, websocket, server], _State, _) ->
  wsecli:start("localhost", 8080, "/"),
  {ok, _State}.

then([i, upgrade, the, connection, with, a, websocket, handshake], _State, _) ->
  receive
    {client_headers, Headers} ->
      ok
  end,
  assert_that("Upgrade", is(get_header("connection", Headers))).

%
% Mock a http server
%

start_http_server(Tester, Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [{reuseaddr, true}, {packet, raw}, binary]),
  spawn_link(fun() -> accept(Tester, Listen) end).

accept(Tester, Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
 handshake(Tester, Socket).

handshake(Tester, Socket) ->
  receive
    {tcp, Socket, Data } ->
      {ok, {http_request, 'GET', _Uri, _Version}, Rest} = erlang:decode_packet(http, Data, []),

      Headers = headers(Rest, []),
      %io:format("Hewders ~w ~n", [_Headers]),
      Tester ! {client_headers, Headers},
      HandShake = [
        "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
        "Upgrade: WebSocket\r\n",
        "Connection: Upgrade\r\n",
        "Sec-WebSocket-Accept: Hash\r\n\r\n"
       ],
       gen_tcp:send(Socket, HandShake),
      wait(Socket)
  end.

headers(Packet, Acc) ->
  F = fun(S) when is_atom(S)-> atom_to_list(S);
        (S)-> S
      end,
  case erlang:decode_packet(httph, Packet, [])of
    {ok, {http_header, _, Key, _, Value}, Rest} ->
      headers(Rest, [{string:to_lower(F(Key)), Value} | Acc]);
    {ok, http_eoh, _} ->
      Acc
  end.

wait(_Socket)->
  false.

get_header(Key, Headers) ->
  proplists:get_value(Key, Headers).
