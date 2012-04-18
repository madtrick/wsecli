-module(send_data).

-compile([export_all]).

setup() ->
  % Use a differente port, to avoid eaddrinuser problems
  %with previous tests
  mock_http_server:start(self(), 8081).

teardown(_) ->
  mock_http_server:stop().


given([that, i, want, to, send, data, to, a, server], _State, _) ->
  wsecli:start("localhost", 8081, "/"),
  {ok, _State}.

then([i, encapsulate, data, according, to, the, rfc], _State, _) ->
  {ok, _State};

then([i, send, it, to, the, server], _State, _) ->
  wsecli:send("La casa de la pradera"),
  receive
    {sent, Data} ->
      Data
  end,

  %assertions for wsecli protocol framing
  false.
