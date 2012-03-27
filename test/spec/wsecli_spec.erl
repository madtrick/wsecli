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
          it("should frame data", fun() ->
                meck:new(wsecli_framing, [passthrough]),

                Pid = self(),
                wsecli:start("localhost", 8080, "/"),
                wsecli:on_open(fun() ->
                      wsecli:send("la casa de la pradera"),
                      Pid ! message_sent
                  end),

                receive message_sent -> ok end,

                assert_that(meck:called(wsecli_framing, frame, '_'), is(true)),
                wsecli:stop(),
                meck:unload(wsecli_framing)
            end),
          it("should buffer data while not connected"),
          it("should invoke on_error if not connected")
      end)
    end).
