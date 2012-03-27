-module(wsecli_framing).
-include("wsecli.hrl").

-export([frame/1]).

-spec frame(Data::string()|binary()) -> {ok, #frame{}}.
frame(Data) ->
  {ok, #frame{}}.
