-module(mock_http_server).

-export([start/2, stop/0]).

-spec start(Tester::pid(), Port::integer()) -> true.
start(Tester, Port) ->
  Starter = self(),
  register(mock_http_server, spawn(fun() -> start_link(Starter,Tester, Port) end)),
  receive
    started ->
      ok
  end.

start_link(Starter, Tester, Port) ->
  case gen_tcp:listen(Port, [{reuseaddr, true}, {packet, raw}, binary]) of
    {ok, Listen} ->
  Starter ! started,
      accept(Tester, Listen);
    {error, eaddrinuse} ->

      timer:sleep(1000),
      start_link(Starter, Tester, Port)
  end.

-spec stop() -> ok.
stop() ->
  mock_http_server ! {stop, self()},
  receive
    stopped ->
      ok
  end.

accept(Tester, Listen) ->
  receive
    {stop, From} ->
      stop(Listen, From)
    after 100 -> true
  end,

  case gen_tcp:accept(Listen, 500) of
    {ok, Socket} ->
      loop(Tester, Socket);
    {error, timeout} ->
      accept(Tester, Listen)
  end.

loop(Tester, Socket) ->
  receive
    {stop, From} ->
      stop(Socket, From);
    {tcp, Socket, Data} ->
      handshake(Tester, Socket, Data),
      loop(Tester, Socket)
  end.

handshake(Tester, Socket, Data) ->
  {ok, {http_request, Method, Uri, Version}, Rest} = erlang:decode_packet(http, Data, []),

  Headers = headers(Rest, []),
  Request = {request, [{method, Method}, {uri, Uri}, {version, Version}], headers, Headers},
  BinaryClientKey = list_to_binary(get_header_value("sec-websocket-key", Headers)),

  Tester ! Request,
  HandShake = [
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
    "Upgrade: WebSocket\r\n",
    "Connection: Upgrade\r\n",
    "Sec-WebSocket-Accept: ",
    base64:encode_to_string(crypto:sha(<<BinaryClientKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
    "\r\n\r\n"
  ],
  gen_tcp:send(Socket, HandShake).

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

stop(Socket, From)->
  gen_tcp:close(Socket),
  From ! stopped,
  exit(normal).

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
