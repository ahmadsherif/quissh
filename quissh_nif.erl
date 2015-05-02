-module(quissh_nif).

-export([exec/2]).

-on_load(init/0).

init() ->
  ok = erlang:load_nif("./quissh_nif", 0).

exec(_Path, _Args) ->
  exit(nif_library_not_loaded).
