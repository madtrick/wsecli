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
    {request, Request, headers, Headers} ->
      ok
  end,

  assert_that('GET'           ,is(get_request_value(method                ,Request))) ,
  assert_that("/"             ,is(get_request_resource_uri(Request)))     ,
  assert_that("1.1"           ,is(get_request_version(Request)))          ,
  assert_that("localhost:8080",is(get_header_value("host"                 ,Headers))) ,
  assert_that("websocket"     ,is(get_header_value("upgrade"              ,Headers))) ,
  assert_that("Upgrade"       ,is(get_header_value("connection"           ,Headers))) ,
  assert_that("13"            ,is(get_header_value("sec-websocket-version",Headers))) ,
  assert_that(undefined       ,is_not(get_header_value("sec-websocket-key",Headers))).
  %checar el header origin

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
      {ok, {http_request, Method, Uri, Version}, Rest} = erlang:decode_packet(http, Data, []),

      Headers = headers(Rest, []),
      Request = {request, [{method, Method}, {uri, Uri}, {version, Version}], headers, Headers},

      Tester ! Request,
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

get_header_value(Key, Headers) ->
  proplists:get_value(Key, Headers).

get_request_resource_uri(Request) ->
  {abs_path, Path} = get_request_value(uri, Request),
  Path.

get_request_version(Request) ->
  {X, Y} = get_request_value(version, Request),
  integer_to_list(X) ++ "." ++ integer_to_list(Y).

get_request_value(Key, Request) ->
  proplists:get_value(Key, Request).

