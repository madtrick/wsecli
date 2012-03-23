-module(wsecli).
-behaviour(gen_fsm).

-export([start/3]).
-export([init/1, connecting/2, open/2, closing/2, closed/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(data, {
    socket :: gen_tcp:socket()
  }).

-spec start(Host::string(), Port::integer(), Resource::string()) -> pid().
start(Host, Port, Path)->
  {ok, Pid} = gen_fsm:start_link({local, wsecli}, ?MODULE, {Host, Port, Path}, [{timeout, 5000}]),
  Pid.


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

handle_sync_event(Event, From, StateName, StateData) ->
  ok.

handle_info(Info, StateName, StateData) ->
  ok.
terminate(Reason, StateName, StateData) ->
  ok.

code_change(OldVsn, StateName, StateData, Extra) ->
  ok.
