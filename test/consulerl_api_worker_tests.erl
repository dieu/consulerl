-module(consulerl_api_worker_tests).

-include("consulerl.hrl").
-include("consulerl_eunit.hrl").

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

setup_error() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_error(),
  setup_worker().

get_test_() ->
  ?setup(fun setup_get/0, fun consulerl_eunit:stop/1, fun({ok, Pid}) ->
    consulerl_eunit:command_async(consulerl_api_worker, get, [Pid, self(), [test], [], true], {ok, ?GET_RESPONSE_JSON})
  end).

put_test_() ->
  ?setup(fun setup_put/0, fun consulerl_eunit:stop/1,  fun({ok, Pid}) ->
    consulerl_eunit:command_async(consulerl_api_worker, put, [Pid, self(), [test], test, []], {ok, ?PUT_RESPONSE_JSON})
  end).

delete_test_() ->
  ?setup(fun setup_delete/0, fun consulerl_eunit:stop/1,  fun({ok, Pid}) ->
    consulerl_eunit:command_async(consulerl_api_worker, delete, [Pid, self(), [test], []], {ok, ?DELETE_RESPONSE_JSON})
  end).

error_test_() ->
  ?setup(fun setup_error/0, fun consulerl_eunit:stop/1, fun({ok, Pid}) ->
    consulerl_eunit:command_async(consulerl_api_worker, get, [Pid, self(), [test], [], true], {error, error})
  end).