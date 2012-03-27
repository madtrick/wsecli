-module(test_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
%-compile([export_all]).

spec() ->
  describe("something", fun() ->
        it("A pending test", fun()->
              ok
              %function()
          end)
    end).

%function()->
  %ok.
