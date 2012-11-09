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

%% @author Farruco Sanjurjo <madtrick@gmail.com>
%% @copyright 2012, Farruco Sanjurjo
%% @doc Websocket Client

-module(wsecli).
-behaviour(gen_fsm).

-include_lib("wsock/include/wsock.hrl").

-export([start/3, start/4, stop/0, stop/1, send/1, send/2]).
-export([on_open/1, on_open/2, on_error/1, on_error/2, on_message/1, on_message/2, on_close/1, on_close/2]).
-export([init/1, connecting/2, open/2, closing/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(callbacks, {
    on_open = fun()-> undefined end,
    on_error = fun(_Reason)-> undefined end,
    on_message = fun(_Type, _Message) -> undefined end,
    on_close = fun(_Reason) -> undefined end
  }).
-record(data, {
    socket :: gen_tcp:socket(),
    handshake :: #handshake{},
    cb  = #callbacks{},
    fragmented_message = undefined :: #message{}
  }).

-define(CLOSE_HANDSHAKE_TIMEOUT, 2000).
-define(TCP_CLOSE_TIMEOUT, 500).
-define(DEFAULT_REG_NAME, ?MODULE).

%%%%%%%%%%%%%%%%%%%%%
%
% PUBLIC API
%
%%%%%%%%%%%%%%%%%%%%%

%% @doc This function will start the websocket client
%%
%% This function will open a connection with the specified remote endpoint. Parameters
%% <ul>
%% <li>Host, a string. The URL of the remote endpoint</li>
%% <li>Port, an integer. The port where the remote endpoint is listening</li>
%% <li>Resouce, an string. The resource path where the websockets live</li>
%% </ul>
-spec start(Host::string(), Port::integer(), Resource::string()) -> pid().
start(Host, Port, Path) ->
    start(Host, Port, Path, {local, ?DEFAULT_REG_NAME}).

%% @doc This function will start the websocket client
%%
%% This function will open a connection with the specified remote endpoint. Parameters
%% <ul>
%% <li>Host, a string. The URL of the remote endpoint</li>
%% <li>Port, an integer. The port where the remote endpoint is listening</li>
%% <li>Resouce, an string. The resource path where the websockets live</li>
%% <li>FsmName, a tuple or 'anon'. 'anon' will skip registering process. See also <a href="http://www.erlang.org/doc/man/gen_fsm.html#start_link-3">here</a></li>
%% </ul>
-spec start(Host::string(), Port::integer(), Resource::string(), FsmName::tuple()|atom()) -> pid().
start(Host, Port, Path, anon) ->
  {ok, Pid} = gen_fsm:start_link(?MODULE, {Host, Port, Path}, [{timeout, 5000}]),
  Pid;
start(Host, Port, Path, FsmName) ->
  {ok, Pid} = gen_fsm:start_link(FsmName, ?MODULE, {Host, Port, Path}, [{timeout, 5000}]),
  Pid.

%% @doc This function will stop the websocket client
%%
%% Calling this function doesn't mean that the connection will be inmediatelly closed. A
%%closing handshake is tryed and only after that the connection is closed.
-spec stop() -> ok.
stop() ->
  stop(?DEFAULT_REG_NAME).

%% @doc This function will stop the websocket client
%%
%% Calling this function doesn't mean that the connection will be inmediatelly closed. A
%% closing handshake is tryed and only after that the connection is closed. Parameters
%% <ul>
%% <li>Socket, a pid or atom. Handle to socket that is to be closed.</li>
%% </ul>
-spec stop(Socket::pid()|atom()) -> ok.
stop(Socket) ->
  gen_fsm:sync_send_all_state_event(Socket, stop).

%% @doc Send data to a remote endpoint
%%
%% Websockets only support 2 types of data in the payload of a message: binary or text, so the type to apply
%%to the data given as parameter to this will depend on the result of the BIFs is_binary and is_list
-spec send(Data::string()) -> ok;
          (Data::binary()) -> ok.
send(Data) ->
  send(?DEFAULT_REG_NAME, Data).

%% @doc Send data to a remote endpoint
%%
%% Websockets only support 2 types of data in the payload of a message: binary or text, so the type to apply
%% to the data given as parameter to this will depend on the result of the BIFs is_binary and is_list. Parameters
%% <ul>
%% <li>Socket, a pid or atom. Handle to socket that is to be closed.</li>
%% <li>Data, a string or binary. Data to be sent.</li>
%% </ul>
-spec send(Socket::pid()|atom(), Data::binary()|string()) -> ok.
send(Socket, Data) ->
  gen_fsm:send_event(Socket, {send, Data}).

%% @doc Add a callback to be called when a connection is opened
%%
%% This callback is called when the connection is opened (after a successful websocket handshake)
%%or inmediatelly if called while in the open state. Adding it not it connecting or open state will
%%raise an error.
%%
%% The callback function must be a function which takes no parameters
-spec on_open(Callback::fun()) -> any().
on_open(Callback) ->
  on_open(?DEFAULT_REG_NAME, Callback).

%% @doc Add a callback to be called when a connection is opened
%%
%% This callback is called when the connection is opened (after a successful websocket handshake)
%%or inmediatelly if called while in the open state. Adding it not it connecting or open state will
%%raise an error.
%%
%% The callback function must be a function which takes no parameters
-spec on_open(Socket::pid()|atom(), Callback::fun()) -> any().
on_open(Socket, Callback) ->
  gen_fsm:send_event(Socket, {on_open, Callback}).

%% @doc Add a callback to be called when an error occurs
%%
%% The callback function must be a function which takes one parameter, the reason for
%%the error
-spec on_error(Callback::fun()) -> any().
on_error(Callback) ->
  on_error(?DEFAULT_REG_NAME, Callback).

%% @doc Add a callback to be called when an error occurs
%%
%% The callback function must be a function which takes one parameter, the reason for
%%the error
-spec on_error(Socket::pid()|atom(), Callback::fun()) -> any().
on_error(Socket, Callback) ->
  gen_fsm:send_all_state_event(Socket, {on_error, Callback}).

%% @doc Add a callback to be called when a message arrives
%%
%% The callback function must be a function which takes two parameters:
%% <ul>
%% <li> The first is an atom with values binary or text depending on the type of the websocket message</li>
%% <li> The second parameter is the message payload</li>
%% </ul>
-spec on_message(Callback::fun()) -> any().
on_message(Callback) ->
  on_message(?DEFAULT_REG_NAME, Callback).

%% @doc Add a callback to be called when a message arrives
%%
%% The callback function must be a function which takes two parameters:
%% <ul>
%% <li> The first is an atom with values binary or text depending on the type of the websocket message</li>
%% <li> The second parameter is the message payload</li>
%% </ul>
-spec on_message(Socket::atom()|pid(), Callback::fun()) -> any().
on_message(Socket, Callback) ->
  gen_fsm:send_all_state_event(Socket, {on_message, Callback}).

%% @doc Add a callback to be called when then connection was closed
%%
%% This callback will be called when the connection is successfully closed, i.e. after performing the
%%websocket closing handshake.
%%
%% The callback function must be a function which takes one parameter, the payload
%%of the close message received from the remote endpoint (if any)
-spec on_close(Callback::fun()) -> any().
on_close(Callback) ->
  on_close(?DEFAULT_REG_NAME, Callback).

%% @doc Add a callback to be called when then connection was closed
%%
%% This callback will be called when the connection is successfully closed, i.e. after performing the
%%websocket closing handshake.
%%
%% The callback function must be a function which takes one parameter, the payload
%%of the close message received from the remote endpoint (if any)
-spec on_close(Socket::pid()|atom(), Callback::fun()) -> any().
on_close(Socket, Callback) ->
  gen_fsm:send_all_state_event(Socket, {on_close, Callback}).

%%%%%%%%%%%%%%%%%%%%%
%
% GEN FSM STATENAME FUNCTIONS
%
%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec init({Host::string(), Port::integer(), Resource::string()}) -> {ok, connecting, #data{}}.
init({Host, Port, Resource}) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {reuseaddr, true}, {packet, raw}] ),

  Handshake = wsock_handshake:open(Resource, Host, Port),
  Request = wsock_http:encode(Handshake#handshake.message),

  ok = gen_tcp:send(Socket, Request),
  {ok, connecting, #data{ socket = Socket, handshake = Handshake}}.

%% @hidden
-spec connecting({on_open, Callback::fun()}, StateData::#data{}) -> term();
                ({send, Data::binary()}, StateData::#data{}) -> term().
connecting({on_open, Callback}, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_open = Callback},
  {next_state, connecting, StateData#data{cb = Callbacks}};

connecting({send, _Data}, StateData) ->
  (StateData#data.cb#callbacks.on_error)("Can't send data while in connecting state"),
  {next_state, connecting, StateData}.

%% @hidden
-spec open(Event::term(), StateData::#data{}) -> term().
open({on_open, Callback}, StateData) ->
  spawn(Callback),
  {next_state, open, StateData};

open({send, Data}, StateData) ->
  Message = wsock_message:encode(Data, [mask, text]),
  case gen_tcp:send(StateData#data.socket, Message) of
    ok ->
      ok;
    {error, Reason} ->
      (StateData#data.cb#callbacks.on_error)(Reason)
  end,
  {next_state, open, StateData}.


%% @hidden
-spec closing(Event::term(), StateData::#data{}) -> term().
closing({send, _Data}, StateData) ->
  (StateData#data.cb#callbacks.on_error)("Can't send data while in closing state"),
  {next_state, closing, StateData};

%% @hidden
closing({timeout, _Ref, waiting_tcp_close}, StateData) ->
  %The tcp connection hasn't been close so, kill them all
  {stop, normal, StateData};

%% @hidden
closing({timeout, _Ref, waiting_close_reply}, StateData) ->
  %The websocket close handshake hasn't been properly done, kill them all
  {stop, normal, StateData}.

%%%%%%%%%%%%%%%%%%%%%
%
% GEN FSM CALLBACK FUNCTIONS
%
%%%%%%%%%%%%%%%%%%%%%
%% @hidden
handle_event({on_error, Callback}, StateName, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_error = Callback},
  {next_state, StateName, StateData#data{cb = Callbacks} };

handle_event({on_message, Callback}, StateName, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_message = Callback},
  {next_state, StateName, StateData#data{cb = Callbacks}};

handle_event({on_close, Callback}, StateName, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_close = Callback},
  {next_state, StateName, StateData#data{cb = Callbacks}}.

%% @hidden
-spec handle_sync_event(stop, pid(), atom(), #data{}) -> {stop, stop, term(), #data{}}.
handle_sync_event(stop, _From, closing, StateData) ->
  {reply, {ok, closing}, closing, StateData};

handle_sync_event(stop, _From, connecting, StateData) ->
  {stop, normal, {ok, closing}, StateData};

handle_sync_event(stop, _From, open, StateData) ->
  Message = wsock_message:encode([], [mask, close]),
  case gen_tcp:send(StateData#data.socket, Message) of
    ok ->
      gen_fsm:start_timer(?CLOSE_HANDSHAKE_TIMEOUT, waiting_close_reply),
      {reply, {ok, closing}, closing, StateData};
    {error, Reason} ->
      {stop, socket_error, {error, socket_error}, StateData }
  end.

%% @hidden
-spec handle_info({tcp, Socket::gen_tcp:socket(), Data::binary()}, connecting, #data{}) -> {next_state, atom(), #data{}}.
handle_info({tcp, Socket, Data}, connecting, StateData) ->
  {ok, Response} = wsock_http:decode(Data, response),
  case wsock_handshake:handle_response(Response, StateData#data.handshake) of
    {ok, _Handshake} ->
      spawn(StateData#data.cb#callbacks.on_open),
      {next_state, open, StateData};
    {error, _Error} ->
      {stop, failed_handshake, StateData}
  end;

handle_info({tcp, Socket, Data}, open, StateData) ->
  {Messages, State} = case StateData#data.fragmented_message of
    undefined ->
      {wsock_message:decode(Data, []), StateData};
    Message ->
      {wsock_message:decode(Data, Message, []), StateData#data{fragmented_message = undefined}}
  end,
  NewStateData = process_messages(Messages, State),
  {next_state, open, NewStateData};

handle_info({tcp, Socket, Data}, closing, StateData) ->
  [Message] = wsock_message:decode(Data, []),
  case Message#message.type of
    close ->
      % if we don't receive a tcp_closed message, move to closed state anyway
      gen_fsm:start_timer(?TCP_CLOSE_TIMEOUT, waiting_tcp_close),
      {next_state, closing, StateData};
    _ ->
      {next_state, closing, StateData}
  end;


handle_info({tcp_closed, _}, _StateName, StateData) ->
 {stop, normal, StateData}.

%% @hidden
-spec terminate(Reason::atom(), StateName::atom(), #data{}) -> [].
terminate(_Reason, _StateName, StateData) ->
  gen_tcp:close(StateData#data.socket),
  spawn(fun() -> (StateData#data.cb#callbacks.on_close)(undefined) end).

%% @hidden
code_change(OldVsn, StateName, StateData, Extra) ->
  {ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%
%
% GEN FSM INTERNAL
%
%%%%%%%%%%%%%%%%%%%%%
-spec process_messages(Messages :: list(#message{}), StateData :: #data{}) -> #data{}.
process_messages([], StateData) ->
  StateData;

process_messages([Message | Messages], StateData) ->
  case Message#message.type of
    text ->
      spawn(fun() -> (StateData#data.cb#callbacks.on_message)(text, Message#message.payload) end),
      process_messages(Messages, StateData);
    binary ->
      spawn(fun() -> (StateData#data.cb#callbacks.on_message)(binary, Message#message.payload) end),
      process_messages(Messages, StateData);
    fragmented ->
      NewStateData = StateData#data{fragmented_message = Message},
      process_messages(Messages, NewStateData)
  end.
