%Copyright [2012] [Farruco Sanjurjo Arcay]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

%% @hidden

-module(mock_http_server).
-behaviour(gen_server).

%-export([start/2, stop/0]).
-export([start/2, stop/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([accept_loop/1]).

-record(state, { socket,
                 test_process,
                 handshaked }).

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
    ok = gen_tcp:controlling_process(Socket, whereis(mock_http_server)),
    gen_server:cast(mock_http_server, {accepted, Socket}).

handle_cast({accepted, Socket}, State) ->
    NewState = State#state{socket = Socket},
    {noreply, NewState}.

handle_info({tcp, _Socket, Data}, State) ->
    case State#state.handshaked  of
        true ->
            receive_data(State#state.test_process, Data, State),
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
receive_data(TestProcess, Data, State) ->
    TestProcess ! {mock_http_server, received_data},
    <<_:4, Opcode:4, _:1, Length:7, _/binary>> = Data,

    case Opcode of
        8 ->
            CloseMessage = <<1:1, 0:3, 8:4, 0:1, 0:7>>,
            gen_tcp:send(State#state.socket, CloseMessage);
        _ ->
            case Length of
                0 ->
                    ok;
                _ ->
                    <<_:16, MaskKey:32, Payload/binary>> = Data,
                    UnmaskedPayload = mask(Payload, MaskKey, <<>>),

                    Message = <<1:1, 0:3, Opcode:4, 0:1, Length:7, UnmaskedPayload/binary>>,
                    gen_tcp:send(State#state.socket, Message)
            end
    end.

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


mask(<<Data:32, Rest/bits>>, MaskKey, Acc) ->
    T = Data bxor MaskKey,
    mask(Rest, MaskKey, <<Acc/binary, T:32>>);

mask(<<Data:24>>, MaskKey, Acc) ->
    <<MaskKey2:24, _/bits>> = <<MaskKey:32>>,
    T = Data bxor MaskKey2,
    <<Acc/binary, T:24>>;

mask(<<Data:16>>, MaskKey, Acc) ->
    <<MaskKey2:16, _/bits>> = <<MaskKey:32>>,
    T = Data bxor MaskKey2,
    <<Acc/binary, T:16>>;

mask(<<Data:8>>, MaskKey, Acc) ->
    <<MaskKey2:8, _/bits>> = <<MaskKey:32>>,
    T = Data bxor MaskKey2,
    <<Acc/binary, T:8>>;

mask(<<>>, _, Acc) ->
    Acc.
