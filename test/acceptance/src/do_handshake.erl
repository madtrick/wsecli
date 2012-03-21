-module(do_handshake).
-export([setup/0, teardown/1, given/3, then/3]).

setup()->
  ok.

teardown(_State) ->
  ok.

given([that, i, want, to, connect, to, a, websocket, server], _State, _) ->
  {ok, _State}.

then([i, the, connection, with, a, websocket, handshake], _State, _) ->
  false.

