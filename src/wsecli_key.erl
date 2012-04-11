%% @hidden

-module(wsecli_key).

-export([generate/0]).

-spec generate() -> string().
generate() ->
  binary_to_list(base64:encode(crypto:rand_bytes(16))).
