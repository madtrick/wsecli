-module(wsecli_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
%-compile([export_all]).

spec() ->
  describe("wsecli", fun() ->
        before_all(fun() ->
              spec_set(start_http_server(self(), 8080), "http_server")
          end),

        after_all(fun() ->
              stop_http_server(spec_get("http_server"))
          end),

        it("should open a tcp connection to the desired Host", fun() ->
              meck:new(gen_tcp, [unstick, passthrough]),

              Host = "localhost",
              Port = 8080,
              Resource = "/",

              wsecli:start(Host, Port, Resource),

              assert_that(meck:called(gen_tcp, connect, '_'), is(true)),
              meck:unload(gen_tcp),
              wsecli:stop()
          end),
        it("should send an opening handshake when connected", fun() ->
              meck:new(gen_tcp, [unstick, passthrough]),

              Host = "localhost",
              Port = 8080,
              Resource = "/",

              wsecli:start(Host, Port, Resource),

              assert_that(meck:called(gen_tcp, send, '_'), is(true)),
              meck:unload(gen_tcp),
              wsecli:stop()
          end)
    end).

% Mock a http server
%

start_http_server(Tester, Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [{reuseaddr, true}, {packet, raw}, binary]),
  spawn_link(fun() -> accept(Tester, Listen) end).

stop_http_server(Server) ->
  Server ! {stop, self()},
  receive
    {stopped, Server} ->
      ok
  end.

accept(Tester, Listen) ->
  receive
    {stop, From} ->
      stop(Listen, From)
    after 100 -> true
  end,

  case gen_tcp:accept(Listen, 100) of
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

  Tester ! Request,
  HandShake = [
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
    "Upgrade: WebSocket\r\n",
    "Connection: Upgrade\r\n",
    "Sec-WebSocket-Accept: Hash\r\n\r\n"
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
  From ! {stoppend, self()},
  exit(normal).

%get_header_value(Key, Headers) ->
  %proplists:get_value(Key, Headers).

%get_request_resource_uri(Request) ->
  %{abs_path, Path} = get_request_value(uri, Request),
  %Path.

%get_request_version(Request) ->
  %{X, Y} = get_request_value(version, Request),
  %integer_to_list(X) ++ "." ++ integer_to_list(Y).

%get_request_value(Key, Request) ->
  %proplists:get_value(Key, Request).
