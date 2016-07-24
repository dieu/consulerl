-module(consulerl_kv_tests).

-include("consulerl.hrl").

-include_lib("eunit/include/eunit.hrl").

setup_get() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?GET_RESPONSE).

setup_not_found() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_404().

get_sync(ExpectedResponse) ->
  Response = consulerl_kv:get("test"),
  ?_assertEqual(ExpectedResponse, Response).

get_sync_second(ExpectedResponse) ->
  Response = consulerl_kv:get("test", []),
  ?_assertEqual(ExpectedResponse, Response).

get_async(ExpectedResponse) ->
  Pid = consulerl_kv:get(self(), "test", []),
  Response = consulerl_util:receive_response(),
  ok = consulerl_api:terminate(Pid),
  ?_assertEqual(ExpectedResponse, Response).

get_test_() ->
  ?setup(fun setup_get/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      get_sync({ok, ?GET_RESPONSE_MAP}),
      get_sync_second({ok, ?GET_RESPONSE_MAP}),
      get_async({ok, ?GET_RESPONSE_MAP})
    ]
  } end).

get_not_found_test_() ->
  ?setup(fun setup_not_found/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      get_sync({error, "Not Found"}),
      get_sync_second({error, "Not Found"}),
      get_async({error, "Not Found"})
    ]
  } end).