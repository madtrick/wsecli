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
    fragmented_message :: #message{}
  }).

-spec start(Host::string(), Port::integer(), Resource::string()) -> pid().
start(Host, Port, Path)->
  {ok, Pid} = gen_fsm:start_link({local, wsecli}, ?MODULE, {Host, Port, Path}, [{timeout, 5000}]),
  Pid.

-spec stop() -> ok.
stop() ->
  %error_logger:info_msg("Calling stop on wsecli\n"),
  gen_fsm:sync_send_all_state_event(wsecli, stop).

-spec send(Data::string()) -> ok;
          (Data::binary()) -> ok.
send(Data) ->
  gen_fsm:send_event(wsecli, {send, Data}).

-spec on_open(Callback::fun()) -> any().
on_open(Callback) ->
  gen_fsm:send_event(wsecli, {on_open, Callback}).

-spec on_error(Callback::fun()) -> any().
on_error(Callback) ->
  gen_fsm:send_all_state_event(wsecli, {on_error, Callback}).

-spec on_message(Callback::fun()) -> any().
on_message(Callback) ->
  gen_fsm:send_all_state_event(wsecli, {on_message, Callback}).

-spec on_close(Callback::fun()) -> any().
on_close(Callback) ->
  gen_fsm:send_all_state_event(wsecli, {on_close, Callback}).

%
% GEN_FSM behaviour functions
%
-spec init({Host::string(), Port::integer(), Resource::string()}) -> {ok, connecting, #data{}}.
init({Host, Port, Resource}) ->
  %error_logger:info_msg("Start wsecli \n"),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {reuseaddr, true}, {packet, raw}] ),

  Handshake = wsecli_handshake:build(Resource, Host, Port),
  Request = wsecli_http:to_request(Handshake#handshake.message),

  ok = gen_tcp:send(Socket, Request),
  {ok, connecting, #data{ socket = Socket, handshake = Handshake}}.

-spec connecting({on_open, Callback::fun()}, StateData::#data{}) -> term();
                ({send, Data::binary()}, StateData::#data{}) -> term().
connecting({on_open, Callback}, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_open = Callback},
  {next_state, connecting, StateData#data{cb = Callbacks}};

connecting({send, _Data}, StateData) ->
  (StateData#data.cb#callbacks.on_error)("Can't send data while in connecting state"),
  {next_state, connecting, StateData}.

-spec open(Event::term(), StateData::#data{}) -> term().
open({send, Data}, StateData) ->
  Message = wsecli_message:encode(Data, text),
  case gen_tcp:send(StateData#data.socket, Message) of
    ok ->
      ok;
    {error, Reason} ->
      (StateData#data.cb#callbacks.on_error)(Reason)
  end,
  {next_state, open, StateData}.


-spec closing(Event::term(), StateData::#data{}) -> term().
closing(Event, StateData) ->
  %TODO: handle {send,Data in this state and notify it as an error
  closing.

-spec closed(Event::term(), StateData::#data{}) -> term().
closed(Event, StateData) ->
  {stop, normal, StateData}.

%
% GEN_FSM behaviour callbacks
%
handle_event({on_error, Callback}, StateName, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_error = Callback},
  {next_state, StateName, StateData#data{cb = Callbacks} };

handle_event({on_message, Callback}, StateName, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_message = Callback},
  {next_state, StateName, StateData#data{cb = Callbacks}};

handle_event({on_close, Callback}, StateName, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_close = Callback},
  {next_state, StateName, StateData#data{cb = Callbacks}};

handle_event(Event, StateName, StateData) ->
  handl_event.

-spec handle_sync_event(stop, pid(), atom(), #data{}) -> {stop, stop, term(), #data{}}.
handle_sync_event(stop, _From, StateName, StateData) when StateName =:= closing ->
  %TODO: raise an error
  ok;

handle_sync_event(stop, _From, connecting, StateData) ->
  {reply, {ok, closing}, closed, StateData, 1};

handle_sync_event(stop, _From, open, StateData) ->
  Message = wsecli_message:encode([], close),
  case gen_tcp:send(StateData#data.socket, Message) of
    ok ->
      {reply, {ok, closing}, closing, StateData};
    {error, Reason} ->
      {stop, socket_error, {error, socket_error}, StateData }
  end;

handle_sync_event(Event, From, StateName, StateData) ->
  handle_sync_event.

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
  %TODO: append previous fragmented message
  %error_logger:info_msg("Receiving in open state \n"),
  Messages = wsecli_message:decode(Data),
  NewStateData = process_messages(Messages, StateData),
  {next_state, open, NewStateData};

handle_info({tcp, Socket, Data}, closing, StateData) ->
  %error_logger:info_msg("Received msg in closing state \n"),
  [Message] = wsecli_message:decode(Data),
  case Message#message.type of
    close ->
      {next_state, closed, StateData, 500};
    _ ->
      %Discard everything
      {next_state, closing, StateData}
  end;

handle_info({tcp_closed, _}, closing, StateData) ->
  {next_state, closed, StateData, 1};

handle_info(_, closed, StateData) ->
  {stop, normal, StateData}.

-spec terminate(Reason::atom(), StateName::atom(), #data{}) -> [].
terminate(_Reason, _StateName, StateData) ->
  %error_logger:error_msg("Closing wsecli socket \n"),
  gen_tcp:close(StateData#data.socket),
  spawn(fun() -> (StateData#data.cb#callbacks.on_close)(undefined) end).

code_change(OldVsn, StateName, StateData, Extra) ->
  code_change.

%
% Internal
%
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
