-module(wsecli_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

all() ->
    [
     do_handshake
     %% send_data is unfinished and fails.
     %send_data
    ].

suite() ->
    [].

init_per_testcase(_CaseName, Config) ->
    Config.

end_per_testcase(_CaseName, Config) ->
    mock_http_server:stop(),
    Config.

%%
%% Tests
%%

do_handshake(_) ->
    Server = given_running_websocket_server(),
    WebSocket = when_i_want_to_connect_to_a_websocket_server(Server),
    then_i_upgrade_the_connection_with_a_websocket_handshake(),
    wsecli:stop(WebSocket).

send_data(_) ->
    Server = given_running_websocket_server(),
    WebSocket = when_i_send_data_to_server(Server),
    Encoded = then_i_encapsulate_data_according_to_the_rfc([]),
    then_i_send_it_to_the_server(WebSocket, Encoded),
    wsecli:stop(WebSocket).

%%
%% Helpers
%%

given_running_websocket_server() ->
    Port = 8183,
    mock_http_server:start(self(), Port),
    {"localhost", Port, "/"}.

when_i_want_to_connect_to_a_websocket_server({Host, Port, Path}) ->
    websocket_connect(Host, Port, Path).

websocket_connect(Host, Port, Path) ->
    case wsecli:start(Host, Port, Path, []) of
        {ok, WebSocket} -> WebSocket;
        _ -> error(wsecli_start_error, [Host, Port, Path])
    end.

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

when_i_send_data_to_server({Host, Port, Path}) ->
    websocket_connect(Host, Port, Path).

then_i_encapsulate_data_according_to_the_rfc(_Data) ->
    %% TODO: what do we do here?
    ok.

then_i_send_it_to_the_server(WebSocket, _Encoded) ->
    wsecli:send(WebSocket, "La casa de la pradera"),
    receive
        {sent, Data} ->
            Data
    end,
    %% Assertions for wsecli protocol framing.
    ct:fail(unfinished).
