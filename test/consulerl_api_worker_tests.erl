-module(consulerl_api_worker_tests).

-include("consulerl.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(setup(Start, Stop, F), {setup, Start, Stop, F}).

-define(GET_RESPONSE, <<"[{\"LockIndex\":0,\"Key\":\"test\",\"Flags\":0,\"Value\":\"czIz\",\"CreateIndex\":141,\"ModifyIndex\":142}]">>).
-define(GET_RESPONSE_JSON, [[
  {<<"LockIndex">>,0},
  {<<"Key">>,<<"test">>},
  {<<"Flags">>,0},
  {<<"Value">>,<<"czIz">>},
  {<<"CreateIndex">>,141},
  {<<"ModifyIndex">>,142}
]]).
-define(PUT_RESPONSE, <<"true">>).
-define(PUT_RESPONSE_JSON, true).
-define(DELETE_RESPONSE, <<"true">>).
-define(DELETE_RESPONSE_JSON, true).

setup_app() ->
  {ok, _} = application:ensure_all_started(consulerl),
  ok.

setup_httpc(Response) ->
  ok = meck:expect(httpc, request, fun(_, _, _, [{sync, false}, {receiver, {M, F, Args}}]) ->
    apply(M, F, [{self(), {{test, 200, test}, [{"content-type", "application/json"}], Response}} | Args]),
    {ok, self()}
  end).

setup_worker() ->
  consulerl_api_worker:start_link("localhost", 8500, "").

setup_get() ->
  ok = setup_app(),
  ok = setup_httpc(?GET_RESPONSE),
  setup_worker().

setup_put() ->
  ok = setup_app(),
  ok = setup_httpc(?PUT_RESPONSE),
  setup_worker().

setup_delete() ->
  ok = setup_app(),
  ok = setup_httpc(?DELETE_RESPONSE),
  setup_worker().

stop_app() ->
  application:stop(consulerl).

stop_httpc() ->
  ok = meck:unload(httpc).

stop(_) ->
  ok = stop_httpc(),
  ok = stop_app().

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
  ?setup(fun setup_get/0, fun stop/1, fun get_api/1).

put_test_() ->
  ?setup(fun setup_put/0, fun stop/1, fun put_api/1).

delete_test_() ->
  ?setup(fun setup_delete/0, fun stop/1, fun delete_api/1).