%% @author Farruco Sanjurjo <madtrick@gmail.com>
%% @copyright 2012, Farruco Sanjurjo
%% @doc Websocket Client

-module(wsecli).
-behaviour(gen_fsm).

-include("wsecli.hrl").

-export([start/3, stop/0, send/1]).
-export([on_open/1, on_error/1, on_message/1, on_close/1]).
-export([init/1, connecting/2, open/2, closing/2, closed/2]).
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
start(Host, Port, Path)->
  {ok, Pid} = gen_fsm:start_link({local, wsecli}, ?MODULE, {Host, Port, Path}, [{timeout, 5000}]),
  Pid.

%% @doc This function will stop the websocket client
-spec stop() -> ok.
stop() ->
  %error_logger:info_msg("Calling stop on wsecli\n"),
  gen_fsm:sync_send_all_state_event(wsecli, stop).

%% @doc Send data to a remote endpoint
-spec send(Data::string()) -> ok;
          (Data::binary()) -> ok.
send(Data) ->
  gen_fsm:send_event(wsecli, {send, Data}).

%% @doc Add a callback to be called when a connection is opened
%%
%% The callback function must be a function which takes no parameters
-spec on_open(Callback::fun()) -> any().
on_open(Callback) ->
  gen_fsm:send_event(wsecli, {on_open, Callback}).

%% @doc Add a callback to be called when an error occurs
%%
%% The callback function must be a function which takes one parameter, the reason for
%%the error
-spec on_error(Callback::fun()) -> any().
on_error(Callback) ->
  gen_fsm:send_all_state_event(wsecli, {on_error, Callback}).

%% @doc Add a callback to be called when a message arrives
%%
%% The callback function must be a function which takes two parameters:
%% <ul>
%% <li> The first is an atom with values binary or text </li>
%% <li> The second parameter is the message payload</li>
%% </ul>
-spec on_message(Callback::fun()) -> any().
on_message(Callback) ->
  gen_fsm:send_all_state_event(wsecli, {on_message, Callback}).

%% @doc Add a callback to be called when then connection was closed
%%
%% The callback function must be a function which takes one parameter, the payload
%%of the close message received from the remote endpoint (if any)
-spec on_close(Callback::fun()) -> any().
on_close(Callback) ->
  gen_fsm:send_all_state_event(wsecli, {on_close, Callback}).

%%%%%%%%%%%%%%%%%%%%%
%
% GEN FSM STATENAME FUNCTIONS
%
%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec init({Host::string(), Port::integer(), Resource::string()}) -> {ok, connecting, #data{}}.
init({Host, Port, Resource}) ->
  %error_logger:info_msg("Start wsecli \n"),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {reuseaddr, true}, {packet, raw}] ),

  Handshake = wsecli_handshake:build(Resource, Host, Port),
  Request = wsecli_http:to_request(Handshake#handshake.message),

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
  Message = wsecli_message:encode(Data, text),
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
  {next_state, closing, StateData}.

%% @hidden
-spec closed(Event::term(), StateData::#data{}) -> term().
closed(Event, StateData) ->
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
  {reply, {ok, already_closing}, closing, StateData};

handle_sync_event(stop, _From, closed, StateData) ->
  {reply, {ok, closed}, closed, StateData, 1};

handle_sync_event(stop, _From, connecting, StateData) ->
  {reply, {ok, closing}, closed, StateData, 1};

handle_sync_event(stop, _From, open, StateData) ->
  Message = wsecli_message:encode([], close),
  case gen_tcp:send(StateData#data.socket, Message) of
    ok ->
      {reply, {ok, closing}, closing, StateData};
    {error, Reason} ->
      {stop, socket_error, {error, socket_error}, StateData }
  end.

%handle_sync_event(Event, From, StateName, StateData) ->
%  error_logger:info_msg("Event ~w \n", [Event]),
%  handle_sync_event.

%% @hidden
-spec handle_info({tcp, Socket::gen_tcp:socket(), Data::binary()}, connecting, #data{}) -> {next_state, atom(), #data{}}.
handle_info({tcp, Socket, Data}, connecting, StateData) ->
  Response = wsecli_http:from_response(Data),
  case wsecli_handshake:validate(Response, StateData#data.handshake) of
    true ->
      spawn(StateData#data.cb#callbacks.on_open),
      {next_state, open, StateData};
    false ->
      {stop, failed_handshake, StateData}
  end;

handle_info({tcp, Socket, Data}, open, StateData) ->
  {Messages, State} = case StateData#data.fragmented_message of
    undefined ->
      {wsecli_message:decode(Data), StateData};
    Message ->
      {wsecli_message:decode(Data, Message), StateData#data{fragmented_message = undefined}}
  end,
  NewStateData = process_messages(Messages, State),
  {next_state, open, NewStateData};

handle_info({tcp, Socket, Data}, closing, StateData) ->
  %error_logger:info_msg("Received msg in closing state \n"),
  [Message] = wsecli_message:decode(Data),
  case Message#message.type of
    close ->
      % if we don't receive a tcp_closed message, move to closed state anyway
      {next_state, closed, StateData, 500};
    _ ->
      %Discard everything
      %TODO: may we stay here in a starvation state if no
      % close message arruvis????????????????????????
      {next_state, closing, StateData}
  end;


handle_info({tcp_closed, _}, _StateName, StateData) ->
 {next_state, closed, StateData, 1};

handle_info(_, closed, StateData) ->
  {stop, normal, StateData}.

%% @hidden
-spec terminate(Reason::atom(), StateName::atom(), #data{}) -> [].
terminate(_Reason, _StateName, StateData) ->
  gen_tcp:close(StateData#data.socket),
  spawn(fun() -> (StateData#data.cb#callbacks.on_close)(undefined) end).

%% @hidden
code_change(OldVsn, StateName, StateData, Extra) ->
  code_change.

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
