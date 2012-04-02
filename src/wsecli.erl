-module(wsecli).
-behaviour(gen_fsm).

-include("wsecli.hrl").

-export([start/3, stop/0, send/1]).
-export([on_open/1, on_error/1]).
-export([init/1, connecting/2, open/2, closing/2, closed/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(callbacks, {
    on_open = fun()-> undefined end,
    on_error = fun(_Reason)-> undefined end
  }).
-record(data, {
    socket :: gen_tcp:socket(),
    handshake :: #handshake{},
    cb  = #callbacks{}
  }).

-spec start(Host::string(), Port::integer(), Resource::string()) -> pid().
start(Host, Port, Path)->
  {ok, Pid} = gen_fsm:start_link({local, wsecli}, ?MODULE, {Host, Port, Path}, [{timeout, 5000}]),
  Pid.

-spec stop() -> ok.
stop() ->
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

%
% GEN_FSM behaviour functions
%
-spec init({Host::string(), Port::integer(), Resource::string()}) -> {ok, connecting, #data{}}.
init({Host, Port, Resource}) ->
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
  closing.

-spec closed(Event::term(), StateData::#data{}) -> term().
closed(Event, StateData) ->
  closed.

%
% GEN_FSM behaviour callbacks
%
handle_event({on_error, Callback}, StateName, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_error = Callback},
  {next_state, StateName, StateData#data{cb = Callbacks} };

handle_event(Event, StateName, StateData) ->
  handl_event.

-spec handle_sync_event(stop, pid(), atom(), #data{}) -> {stop, stop, term(), #data{}}.
handle_sync_event(stop, _From, _StateName, StateData) ->
  {stop, normal, stopping, StateData};

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

handle_info(Info, StateName, StateData) ->
  handle_info.

-spec terminate(Reason::atom(), StateName::atom(), #data{}) -> [].
terminate(_Reason, _StateName, StateData) ->
  gen_tcp:close(StateData#data.socket).

code_change(OldVsn, StateName, StateData, Extra) ->
  code_change.
