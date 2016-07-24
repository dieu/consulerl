-module(consulerl_api_worker_tests).

-include("consulerl.hrl").

-include_lib("eunit/include/eunit.hrl").

setup_worker() ->
  consulerl_api_worker:start_link("localhost", 8500, "").

setup_get() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?GET_RESPONSE),
  setup_worker().

setup_put() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?PUT_RESPONSE),
  setup_worker().

setup_delete() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?DELETE_RESPONSE),
  setup_worker().

get_api({ok, Pid}) ->
  ok = consulerl_api_worker:get(Pid, self(), [test], []),
  Response = consulerl_util:receive_response(),
  ?_assertEqual({ok, ?GET_RESPONSE_JSON}, Response).

put_api({ok, Pid}) ->
  ok = consulerl_api_worker:put(Pid, self(), [test], test, []),
  Response = consulerl_util:receive_response(),
  ?_assertEqual({ok, ?PUT_RESPONSE_JSON}, Response).

delete_api({ok, Pid}) ->
  ok = consulerl_api_worker:delete(Pid, self(), [test], []),
  Response = consulerl_util:receive_response(),
  ?_assertEqual({ok, ?DELETE_RESPONSE_JSON}, Response).

get_test_() ->
  ?setup(fun setup_get/0, fun consulerl_eunit:stop/1, fun get_api/1).

put_test_() ->
  ?setup(fun setup_put/0, fun consulerl_eunit:stop/1, fun put_api/1).

delete_test_() ->
  ?setup(fun setup_delete/0, fun consulerl_eunit:stop/1, fun delete_api/1).