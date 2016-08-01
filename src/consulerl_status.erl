-module(consulerl_status).

-include("consulerl.hrl").

%% API
-export([
  leader/0,
  peers/0
]).

-spec leader() -> return().
leader() ->
  consulerl_api:get([status, leader]).

-spec peers() -> return().
peers() ->
  consulerl_api:get([status, peers]).