-module(mock_http_server).
-behaviour(gen_server).

%-export([start/2, stop/0]).
-export([start/2, stop/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([accept_loop/1]).

-record(state, {
    socket,
    test_process,
    handshaked
  }).

start(TestProcess, Port)->
  gen_server:start({local, mock_http_server}, ?MODULE, [TestProcess, Port], []).

stop()->
  gen_server:call(mock_http_server, stop).

init([TestProcess, Port]) ->
  {ok, Socket} = gen_tcp:listen(Port, [{reuseaddr, true}, {packet, raw}, binary]),
  accept(Socket),
  NewState = #state{socket = Socket, test_process = TestProcess, handshaked = false},
  {ok, NewState}.


accept(Socket) ->
  spawn(?MODULE, accept_loop, [Socket]).

accept_loop(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  gen_tcp:controlling_process(Socket, whereis(mock_http_server)),
  gen_server:cast(mock_http_server, {accepted, Socket}).

handle_cast({accepted, Socket}, State) ->
  NewState = State#state{socket = Socket},
  {noreply, NewState}.

handle_info({tcp, _Socket, Data}, State) ->
  case State#state.handshaked  of
    true ->
      receive_data(Data),
      {noreply, State};
    false ->
      handshake(Data, State),
      NewState = State#state{handshaked = true},
      {noreply, NewState}
  end;


handle_info(_Msg, Library) -> {noreply, Library}.

handle_call(stop, _, State) ->
  {stop, normal, exit, State};

handle_call(_Msg, _Caller, State) -> {noreply, State}.

terminate(_, State) ->
  gen_tcp:close(State#state.socket).

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

%
% Internal
%
receive_data(_Data) ->
  ok.
handshake(Data, State) ->

  {ok, {http_request, Method, Uri, Version}, Rest} = erlang:decode_packet(http, Data, []),

  Headers = headers(Rest, []),
  Request = {request, [{method, Method}, {uri, Uri}, {version, Version}], headers, Headers},
  BinaryClientKey = list_to_binary(get_header_value("sec-websocket-key", Headers)),

  %State#state.test_process ! Request,
  HandShake = [
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
    "Upgrade: WebSocket\r\n",
    "Connection: Upgrade\r\n",
    "Sec-WebSocket-Accept: ",
    base64:encode_to_string(crypto:sha(<<BinaryClientKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
    "\r\n\r\n"
  ],
  gen_tcp:send(State#state.socket, HandShake).

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
