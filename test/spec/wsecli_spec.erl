-module(wsecli_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
%-compile([export_all]).

spec() ->
  describe("wsecli", fun() ->
        before_each(fun() ->
              meck:new(gen_tcp, [unstick, passthrough]),
              mock_http_server:start(self(), 8080)
          end),

        after_each(fun() ->
              mock_http_server:stop(),
              meck:unload(gen_tcp)
          end),

        describe("on_close", fun()->
              it("should be called after the websocket connection has been closed", fun()->
                    Pid = self(),
                    wsecli:start("localhost", 8080, "/"),
                    wsecli:on_close(fun(_Reason) -> Pid ! {Pid, on_close} end),
                    wsecli:stop(),

                    assert_that((fun() ->
                            receive
                              {Pid, on_close} ->
                                true
                            after 500 ->
                                false
                            end
                        end)(), is(true))
                end)
          end),
        describe("start", fun() ->
          it("should open a tcp connection to the desired Host", fun() ->
                Host = "localhost",
                Port = 8080,
                Resource = "/",

                wsecli:start(Host, Port, Resource),

                assert_that(meck:called(gen_tcp, connect, '_'), is(true)),
                cleanly_stop_wsecli(true)
            end),
          it("should send an opening handshake when connected", fun() ->
                Host = "localhost",
                Port = 8080,
                Resource = "/",

                wsecli:start(Host, Port, Resource),

                assert_that(meck:called(gen_tcp, send, '_'), is(true)),
                cleanly_stop_wsecli(true)
            end),
          describe("on successful handshake", fun() ->
                it("should invoke on_open callback", fun() ->
                      Host = "localhost",
                      Port = 8080,
                      Resource = "/",

                      Pid = self(),
                      wsecli:start(Host, Port, Resource),
                      wsecli:on_open(fun()-> Pid ! {Pid, on_open} end),

                      assert_that((fun() ->
                            receive
                              {Pid, on_open} ->
                                true
                            after 500 ->
                                false
                            end
                        end)(), is(true)),
                    cleanly_stop_wsecli(true)
                  end)
            end)
      end),
    describe("send", fun() ->
          it("should send data as messages", fun() ->
                meck:new(wsecli_message, [passthrough]),

                wsecli:start("localhost", 8080, "/"),
                wsecli:on_open(fun() ->
                      wsecli:send("la casa de la pradera")
                  end),

                receive {mock_http_server, received_data} -> ok end,

                assert_that(meck:called(gen_tcp, send, '_'), is(true)),
                %assert_that(meck:called(wsecli_message, encode, '_'), is(true)),
                cleanly_stop_wsecli(true),
                meck:unload(wsecli_message)
            end),
          it("should invoke on_error callback if not on open state", fun() ->
                Pid = self(),
                wsecli:start("localhost", 8080, "/"),
                wsecli:on_error(fun(_Reason) -> Pid ! {Pid, on_error} end),
                wsecli:send("La casa de la pradera"),

                assert_that((fun() ->
                        receive
                          {Pid, on_error} ->
                            true
                        after 500 ->
                            false
                        end
                    end)(), is(true)),
                cleanly_stop_wsecli(true)
            end),
          it("should invoke on_error if not connected")
      end),
      describe("on_message", fun() ->
            it("should be called when a data message is received", fun() ->
                  Pid = self(),
                  wsecli:start("localhost", 8080, "/"),
                  wsecli:on_message(fun(_Type,_Messae)-> Pid ! {Pid, on_message} end),
                  wsecli:on_open(fun() -> wsecli:send("Hello") end),

                  assert_that((fun() ->
                          receive
                            {Pid, on_message} ->
                              true
                          after 500 ->
                              false
                          end
                      end)(), is(true)),
                  cleanly_stop_wsecli(true)
              end),
            it("should be called with the data in the message as parameter", fun() ->
                  Pid = self(),
                  wsecli:start("localhost", 8080, "/"),
                  wsecli:on_message(fun(text, Message)-> Pid ! {Pid, on_message, Message} end),
                  wsecli:on_open(fun() -> wsecli:send("Hello") end),

                  Message = receive
                    {Pid, on_message, Data} ->
                      Data
                  after 500 ->
                      false
                  end,
                  assert_that(Message, is("Hello")),
                  cleanly_stop_wsecli(true)
              end)
        end),
      describe("stop", fun() ->
            it("should not send websocket close handshake if not connected", fun() ->
                  meck:new(wsecli_message, [passthrough, unstick]),
                  wsecli:start("localhost", 8080, "/"),
                  cleanly_stop_wsecli(true),

                  assert_that(meck:called(wsecli_message, encode, ['_', close]), is(false)),
                  meck:unload(wsecli_message)
              end),
            it("should send websocket close handshake if connected", fun() ->
                  meck:new(wsecli_message, [passthrough, unstick]),
                  wsecli:start("localhost", 8080, "/"),
                  Pid = self(),
                  wsecli:on_open(fun() ->
                        cleanly_stop_wsecli(true),
                        Pid ! {Pid, closed}
                    end),

                  receive {Pid, closed} -> ok  end,

                  assert_that(meck:called(wsecli_message, encode, ['_', close]), is(true)),
                  meck:unload(wsecli_message)
              end),
            it("should close the socket", fun() ->
                  wsecli:start("localhost", 8080, "/"),
                  cleanly_stop_wsecli(true),

                  assert_that(meck:called(gen_tcp, close, '_') , is(true))
              end)
        end)
  end).

cleanly_stop_wsecli(_) ->
  Pid = self(),
  wsecli:on_close(fun(_) -> Pid ! {Pid, on_close} end),
  wsecli:stop(),
  receive {Pid, on_close} -> true end.
