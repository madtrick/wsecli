-module(wsecli_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
%-compile([export_all]).

spec() ->
  describe("wsecli", fun() ->
        before_each(fun() ->
              mock_http_server:start(self(), 8080)
          end),

        after_each(fun() ->
              mock_http_server:stop()
          end),

        describe("start", fun() ->
          it("should open a tcp connection to the desired Host", fun() ->
                meck:new(gen_tcp, [unstick, passthrough]),

                Host = "localhost",
                Port = 8080,
                Resource = "/",

                wsecli:start(Host, Port, Resource),

                assert_that(meck:called(gen_tcp, connect, '_'), is(true)),
                meck:unload(gen_tcp),
                wsecli:stop()
            end),
          it("should send an opening handshake when connected", fun() ->
                meck:new(gen_tcp, [unstick, passthrough]),

                Host = "localhost",
                Port = 8080,
                Resource = "/",

                wsecli:start(Host, Port, Resource),

                assert_that(meck:called(gen_tcp, send, '_'), is(true)),
                meck:unload(gen_tcp),
                wsecli:stop()
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
                      wsecli:stop()
                  end)
            end)
      end),
    describe("send", fun() ->
          it("should send data as messages", fun() ->
                meck:new(wsecli_message, [passthrough]),
                meck:new(gen_tcp, [passthrough, unstick]),

                wsecli:start("localhost", 8080, "/"),
                wsecli:on_open(fun() ->
                      wsecli:send("la casa de la pradera")
                  end),

                receive {mock_http_server, received_data} -> ok end,

                assert_that(meck:called(gen_tcp, send, '_'), is(true)),
                assert_that(meck:called(wsecli_message, encode, '_'), is(true)),
                wsecli:stop(),
                meck:unload([gen_tcp, wsecli_message])
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
                wsecli:stop()
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
                  wsecli:stop()

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
                  wsecli:stop()

              end)
        end)
    end).
