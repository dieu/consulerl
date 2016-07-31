-module(consulerl_api_tests).

-include("consulerl.hrl").
-include("consulerl_eunit.hrl").

-include_lib("eunit/include/eunit.hrl").


-import(consulerl_eunit, [
  command/4,
  command_async/4,
  command_async_twice/4
]).

setup_get() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?GET_RESPONSE).

setup_put() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?PUT_RESPONSE).

setup_delete() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?DELETE_RESPONSE).

get_test_() ->
  ?setup(fun setup_get/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_api, get, [["test"]], {ok, [?GET_RESPONSE_MAP]}),
      command(consulerl_api, get, [["test"], []], {ok, [?GET_RESPONSE_MAP]}),
      command_async(consulerl_api, get, [self(), ["test"], []], {ok, [?GET_RESPONSE_MAP]}),
      command_async_twice(consulerl_api, get, [self(), ["test"], []], {ok, [?GET_RESPONSE_MAP]})
    ]
  } end).

put_test_() ->
  ?setup(fun setup_put/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_api, put, [["test"], "test"],  {ok, ?PUT_RESPONSE_JSON}),
      command(consulerl_api, put, [["test"], "test", []], {ok, ?PUT_RESPONSE_JSON}),
      command_async(consulerl_api, put, [self(), ["test"], "test", []], {ok, ?PUT_RESPONSE_JSON}),
      command_async_twice(consulerl_api, put, [self(), ["test"], "test", []], {ok, ?PUT_RESPONSE_JSON})
    ]
  } end).

delete_test_() ->
  ?setup(fun setup_delete/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_api, delete, [["test"]], {ok, ?DELETE_RESPONSE_JSON}),
      command(consulerl_api, delete, [["test"], []], {ok, ?DELETE_RESPONSE_JSON}),
      command_async(consulerl_api, delete, [self(), ["test"], []], {ok, ?DELETE_RESPONSE_JSON}),
      command_async_twice(consulerl_api, delete, [self(), ["test"], []], {ok, ?DELETE_RESPONSE_JSON})
    ]
  } end).