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
