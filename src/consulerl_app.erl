%% @hidden
-module(consulerl_app).

-behavior(application).

-export([
  start/2,
  stop/1
]).

-spec start(term(), term()) -> {error, _} | {ok, pid()} | {ok, pid(), _}.
start(_StartType, _StartArgs) ->
  ok = httpc:set_options([
    {max_keep_alive_length, application:get_env(consulerl, max_keep_alive, 0)}
  ]),
  consulerl_api_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
  ok.
