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

-module(do_handshake).

%-export([setup/0, teardown/1, given/3, then/3]).
-compile([export_all]).
-include_lib("hamcrest/include/hamcrest.hrl").

setup()->
  mock_http_server:start(self(), 8080),
  ok.

teardown(_State) ->
  mock_http_server:stop(),
  ok.

given([that, i, want, to, connect, to, a, websocket, server], _State, _) ->
  wsecli:start("localhost", 8080, "/"),
  {ok, _State}.

then([i, upgrade, the, connection, with, a, websocket, handshake], _State, _) ->
  %
  % Message sebt by the mock server
  %
  receive
    {request, Request, headers, Headers} ->
      ok
  end,

  wsecli:stop(),

  assert_that('GET'           ,is(get_request_value(method                ,Request))) ,
  assert_that("/"             ,is(get_request_resource_uri(Request)))     ,
  assert_that("1.1"           ,is(get_request_version(Request)))          ,
  assert_that("localhost:8080",is(get_header_value("host"                 ,Headers))) ,
  assert_that("websocket"     ,is(get_header_value("upgrade"              ,Headers))) ,
  assert_that("upgrade"       ,is(get_header_value("connection"           ,Headers))) ,
  assert_that("13"            ,is(get_header_value("sec-websocket-version",Headers))) ,
  assert_that(undefined       ,is_not(get_header_value("sec-websocket-key",Headers))) .
  %checar el header origin


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

