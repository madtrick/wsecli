-module(wsecli_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
%-compile([export_all]).

spec() ->
  describe("wsecli", fun() ->
        it("should open a tcp connection to the desired Host", fun() ->
              start_http_server(self(), 8080),
              meck:new(gen_tcp, [unstick, passthrough]),

              Host = "localhost",
              Port = 8080,
              Resource = "/",

              wsecli:start(Host, Port, Resource),

              assert_that(meck:called(gen_tcp, connect, '_'), is(true)),
              meck:unload(gen_tcp)
          end)
    end).

% Mock a http server
%

start_http_server(Tester, Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [{reuseaddr, true}, {packet, raw}, binary]),
  spawn_link(fun() -> accept(Tester, Listen) end).

accept(Tester, Listen) ->
  error_logger:error_msg("Hey ~n"),
  {ok, Socket} = gen_tcp:accept(Listen),
 handshake(Tester, Socket).

handshake(Tester, Socket) ->
  error_logger:error_msg("HOU ~n"),
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
