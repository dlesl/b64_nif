-module(b64_nif).

-export([encode/1, decode/1]).
-on_load(init/0).

encode(_Data) ->
  exit(nif_not_loaded).

decode(_Data) ->
  exit(nif_not_loaded).

init() ->
  Nif = filename:join([code:priv_dir(b64_nif), "b64_nif"]),
  ok = erlang:load_nif(Nif, 0).
