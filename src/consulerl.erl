-module(consulerl).

-include("consulerl.hrl").

-export([
  start/0,
  stop/0
]).

-export([

]).

%% @doc Start the application
-spec start() -> {ok, [atom()]}.
start() ->
  {ok, _} = application:ensure_all_started(consulerl).

%% @doc Stop the application
-spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(consulerl).
