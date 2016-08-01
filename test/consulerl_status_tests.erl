-module(consulerl_status_tests).

-include("consulerl.hrl").
-include("consulerl_eunit.hrl").

-include_lib("eunit/include/eunit.hrl").

-import(consulerl_eunit, [command/4]).

setup_200(Response) -> fun() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(Response)
end.

leader_test_() ->
  ?setup(setup_200(?LEADER_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_status, leader, [], {ok, ?LEADER_RESPONSE_MAP})
  end).

peers_test_() ->
  ?setup(setup_200(?PEERS_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_status, peers, [], {ok, [?PEERS_RESPONSE_MAP]})
  end).