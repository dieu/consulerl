-module(consulerl_events_tests).

-include("consulerl.hrl").
-include("consulerl_eunit.hrl").

-include_lib("eunit/include/eunit.hrl").

-import(consulerl_eunit, [command/4]).

setup_200(Response) -> fun() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(Response)
end.

fire_null_test_() ->
  ?setup(setup_200(?FIRE_RESPONSE_NULL), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_events, fire, ["test"], {ok, ?FIRE_RESPONSE_NULL_MAP})
  end).

fire_test_() ->
  ?setup(setup_200(?FIRE_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_events, fire, ["test", "test"], {ok, ?FIRE_RESPONSE_MAP}),
    command(consulerl_events, fire, ["test", "test", "test", "test", "test", "test"], {ok, ?FIRE_RESPONSE_MAP})
  end).

list_test_() ->
  ?setup(setup_200(?LIST_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_events, list, [], {ok, ?LIST_RESPONSE_MAP}),
    command(consulerl_events, list, ["test"], {ok, ?LIST_RESPONSE_MAP})
  end).