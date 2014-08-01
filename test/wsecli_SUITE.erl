-module(wsecli_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

all() ->
    [do_handshake].

suite() ->
    [].

init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, Config) ->
    Config.

%%
%% Tests
%%

do_handshake(_) ->
    Server = given_running_websocket_server(),
    when_i_want_to_connect_to_a_websocket_server(Server),
    then_i_upgrade_the_connection_with_a_websocket_handshake().

%%
%% Helpers
%%

given_running_websocket_server() ->
    Port = 8183,
    mock_http_server:start(self(), Port),
    {"localhost", Port, "/"}.

when_i_want_to_connect_to_a_websocket_server({Host, Port, Path}) ->
    wsecli:start(Host, Port, Path, []).

then_i_upgrade_the_connection_with_a_websocket_handshake() ->
    %% Message sent by the mock server
    {Request, Headers} = receive
                             {request, R, headers, H} -> {R, H}
                         end,
    %% TODO: checar el header origin
    assert_that('GET', is(get_request_value(method, Request))),
    assert_that("/", is(get_request_resource_uri(Request))),
    assert_that("1.1", is(get_request_version(Request))),
    assert_that("localhost:8183", is(get_header_value("host", Headers))),
    assert_that("websocket", is(get_header_value("upgrade", Headers))),
    assert_that("upgrade", is(get_header_value("connection", Headers))),
    assert_that("13", is(get_header_value("sec-websocket-version", Headers))),
    assert_that(undefined, is_not(get_header_value("sec-websocket-key", Headers))).

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
