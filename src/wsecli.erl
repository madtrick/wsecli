-module(wsecli).
-behaviour(gen_fsm).

-export([start/3, stop/0]).
-export([init/1, connecting/2, open/2, closing/2, closed/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(data, {
    socket :: gen_tcp:socket()
  }).

-spec start(Host::string(), Port::integer(), Resource::string()) -> pid().
start(Host, Port, Path)->
  {ok, Pid} = gen_fsm:start_link({local, wsecli}, ?MODULE, {Host, Port, Path}, [{timeout, 5000}]),
  Pid.

-spec stop() -> ok.
stop() ->
  gen_fsm:sync_send_all_state_event(wsecli, stop).


%
% GEN_FSM behaviour functions
%
-spec init({Host::string(), Port::integer(), Resource::string()}) -> {ok, connecting, #data{}}.
init({Host, Port, Resource}) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {reuseaddr, true}, {packet, raw}] ),
  {ok, connecting, #data{ socket = Socket}}.

-spec connecting(Event::term(), StateData::#data{}) -> term().
connecting(Event, StateData) ->
  ok.

-spec open(Event::term(), StateData::#data{}) -> term().
open(Event, StateData) ->
  ok.

-spec closing(Event::term(), StateData::#data{}) -> term().
closing(Event, StateData) ->
  ok.

-spec closed(Event::term(), StateData::#data{}) -> term().
closed(Event, StateData) ->
  ok.

%
% GEN_FSM behaviour callbacks
%
handle_event(Event, StateName, StateData) ->
  ok.

-spec handle_sync_event(stop, pid(), atom(), #data{}) -> {stop, stop, term(), #data{}}.
handle_sync_event(stop, _From, _StateName, StateData) ->
  {stop, normal, stopping, StateData};

handle_sync_event(Event, From, StateName, StateData) ->
  ok.

handle_info(Info, StateName, StateData) ->
  ok.

-spec terminate(stop, atom(), #data{}) -> [].
terminate(normal, _StateName, StateData) ->
  gen_tcp:close(StateData#data.socket).

code_change(OldVsn, StateName, StateData, Extra) ->
  ok.
