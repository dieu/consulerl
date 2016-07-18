%% @hidden
-module(consulerl_app).

-behavior(application).

-export([
  start/2,
  stop/1
]).

-spec start(term(), term()) -> {error, _} | {ok, pid()} | {ok, pid(), _}.
start(_StartType, _StartArgs) ->
  consulerl_api_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
  ok.
