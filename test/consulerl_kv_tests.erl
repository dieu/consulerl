-module(consulerl_kv_tests).

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

setup_keys() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?KEYS_RESPONSE).

setup_put() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?PUT_RESPONSE).

setup_delete() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?DELETE_RESPONSE).

setup_txn() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200(?TXN_RESPONSE).

setup_not_found() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_404().

setup_conflict() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_409(?TXN_ERROR_RESPONSE).

setup_timeout() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_timeout().

get_test_() ->
  ?setup(fun setup_get/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, get, ["test"], {ok, ?GET_RESPONSE_MAP}),
      command(consulerl_kv, get, ["test", []], {ok, ?GET_RESPONSE_MAP}),
      command_async(consulerl_kv, get, [self(), "test", []], {ok, ?GET_RESPONSE_MAP}),
      command_async_twice(consulerl_kv, get, [self(), "test", []], {ok, ?GET_RESPONSE_MAP})
    ]
  } end).

get_not_found_test_() ->
  ?setup(fun setup_not_found/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, get, ["test"], {error, "Not Found"}),
      command(consulerl_kv, get, ["test", []], {error, "Not Found"}),
      command_async(consulerl_kv, get, [self(), "test", []], {error, "Not Found"}),
      command_async_twice(consulerl_kv, get, [self(), "test", []], {error, "Not Found"})
    ]
  } end).

get_timeout_test_() ->
  ?setup(fun setup_timeout/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, get, ["test"], {error, timeout}),
      command(consulerl_kv, get, ["test", []], {error, timeout}),
      command_async(consulerl_kv, get, [self(), "test", []], {error, timeout}),
      command_async_twice(consulerl_kv, get, [self(), "test", []], {error, timeout})
    ]
  } end).

get_all_test_() ->
  ?setup(fun setup_get/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, get_all, ["test"], {ok, [?GET_RESPONSE_MAP]}),
      command(consulerl_kv, get_all, ["test", []], {ok, [?GET_RESPONSE_MAP]}),
      command_async(consulerl_kv, get_all, [self(), "test", []], {ok, [?GET_RESPONSE_MAP]}),
      command_async_twice(consulerl_kv, get_all, [self(), "test", []], {ok, [?GET_RESPONSE_MAP]})
    ]
  } end).

get_all_not_found_test_() ->
  ?setup(fun setup_not_found/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, get_all, ["test"], {error, "Not Found"}),
      command(consulerl_kv, get_all, ["test", []], {error, "Not Found"}),
      command_async(consulerl_kv, get_all, [self(), "test", []], {error, "Not Found"}),
      command_async_twice(consulerl_kv, get_all, [self(), "test", []], {error, "Not Found"})
    ]
  } end).

get_all_timeout_test_() ->
  ?setup(fun setup_timeout/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, get_all, ["test"], {error, timeout}),
      command(consulerl_kv, get_all, ["test", []], {error, timeout}),
      command_async(consulerl_kv, get_all, [self(), "test", []], {error, timeout}),
      command_async_twice(consulerl_kv, get_all, [self(), "test", []], {error, timeout})
    ]
  } end).

keys_test_() ->
  ?setup(fun setup_keys/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, keys, [], {ok, ?KEYS_RESPONSE_BIN}),
      command(consulerl_kv, keys, ["test"], {ok, ?KEYS_RESPONSE_BIN}),
      command(consulerl_kv, keys, ["test", []], {ok, ?KEYS_RESPONSE_BIN}),
      command_async(consulerl_kv, keys, [self(), "test", []], {ok, ?KEYS_RESPONSE_BIN}),
      command_async_twice(consulerl_kv, keys, [self(), "test", []], {ok, ?KEYS_RESPONSE_BIN})
    ]
  } end).

keys_not_found_test_() ->
  ?setup(fun setup_not_found/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, keys, [], {error, "Not Found"}),
      command(consulerl_kv, keys, ["test"], {error, "Not Found"}),
      command(consulerl_kv, keys, ["test", []], {error, "Not Found"}),
      command_async(consulerl_kv, keys, [self(), "test", []], {error, "Not Found"}),
      command_async_twice(consulerl_kv, keys, [self(), "test", []], {error, "Not Found"})
    ]
  } end).

keys_timeout_test_() ->
  ?setup(fun setup_timeout/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, keys, [], {error, timeout}),
      command(consulerl_kv, keys, ["test"], {error, timeout}),
      command(consulerl_kv, keys, ["test", []], {error, timeout}),
      command_async(consulerl_kv, keys, [self(), "test", []], {error, timeout}),
      command_async_twice(consulerl_kv, keys, [self(), "test", []], {error, timeout})
    ]
  } end).

put_test_() ->
  ?setup(fun setup_put/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, put, ["test", "test"],  {ok, ?PUT_RESPONSE_JSON}),
      command(consulerl_kv, put, ["test", "test", 0], {ok, ?PUT_RESPONSE_JSON}),
      command(consulerl_kv, put, ["test", "test", 0, none], {ok, ?PUT_RESPONSE_JSON}),
      command_async(consulerl_kv, put, [self(), "test", "test", 0, none], {ok, ?PUT_RESPONSE_JSON}),
      command_async_twice(consulerl_kv, put, [self(), "test", "test", 0, none], {ok, ?PUT_RESPONSE_JSON})
    ]
  } end).

put_not_found_test_() ->
  ?setup(fun setup_not_found/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, put, ["test", "test"],  {error, "Not Found"}),
      command(consulerl_kv, put, ["test", "test", 0],  {error, "Not Found"}),
      command(consulerl_kv, put, ["test", "test", 0, none],  {error, "Not Found"}),
      command_async(consulerl_kv, put, [self(), "test", "test", 0, none], {error, "Not Found"}),
      command_async_twice(consulerl_kv, put, [self(), "test", "test", 0, none], {error, "Not Found"})
    ]
  } end).

put_timeout_test_() ->
  ?setup(fun setup_timeout/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, put, ["test", "test"],  {error, timeout}),
      command(consulerl_kv, put, ["test", "test", 0],  {error, timeout}),
      command(consulerl_kv, put, ["test", "test", 0, none],  {error, timeout}),
      command_async(consulerl_kv, put, [self(), "test", "test", 0, none], {error, timeout}),
      command_async_twice(consulerl_kv, put, [self(), "test", "test", 0, none], {error, timeout})
    ]
  } end).

delete_test_() ->
  ?setup(fun setup_delete/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, delete, ["test"], {ok, ?DELETE_RESPONSE_JSON}),
      command(consulerl_kv, delete, ["test", false], {ok, ?DELETE_RESPONSE_JSON}),
      command(consulerl_kv, delete, ["test", false, none], {ok, ?DELETE_RESPONSE_JSON}),
      command_async(consulerl_kv, delete, [self(), "test", false, none], {ok, ?DELETE_RESPONSE_JSON}),
      command_async_twice(consulerl_kv, delete, [self(), "test", false, none], {ok, ?DELETE_RESPONSE_JSON})
    ]
  } end).

delete_not_found_test_() ->
  ?setup(fun setup_not_found/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, delete, ["test"], {error, "Not Found"}),
      command(consulerl_kv, delete, ["test", true], {error, "Not Found"}),
      command(consulerl_kv, delete, ["test", false], {error, "Not Found"}),
      command(consulerl_kv, delete, ["test", true, 0], {error, "Not Found"}),
      command(consulerl_kv, delete, ["test", false, 0], {error, "Not Found"}),
      command(consulerl_kv, delete, ["test", true, none], {error, "Not Found"}),
      command(consulerl_kv, delete, ["test", false, none], {error, "Not Found"}),
      command_async(consulerl_kv, delete, [self(), "test", true, 0], {error, "Not Found"}),
      command_async(consulerl_kv, delete, [self(), "test", false, 0], {error, "Not Found"}),
      command_async(consulerl_kv, delete, [self(), "test", true, none], {error, "Not Found"}),
      command_async(consulerl_kv, delete, [self(), "test", false, none], {error, "Not Found"}),
      command_async_twice(consulerl_kv, delete, [self(), "test", true, 0], {error, "Not Found"}),
      command_async_twice(consulerl_kv, delete, [self(), "test", false, 0], {error, "Not Found"}),
      command_async_twice(consulerl_kv, delete, [self(), "test", true, none], {error, "Not Found"}),
      command_async_twice(consulerl_kv, delete, [self(), "test", false, none], {error, "Not Found"})
    ]
  } end).

delete_timeout_test_() ->
  ?setup(fun setup_timeout/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, delete, ["test"], {error, timeout}),
      command(consulerl_kv, delete, ["test", false], {error, timeout}),
      command(consulerl_kv, delete, ["test", false, none], {error, timeout}),
      command_async(consulerl_kv, delete, [self(), "test", false, none], {error, timeout}),
      command_async_twice(consulerl_kv, delete, [self(), "test", false, none], {error, timeout})
    ]
  } end).

txn_test_() ->
  ?setup(fun setup_txn/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, txn, [[{get, "test"}], []], {ok, ?TXN_RESPONSE_MAP}),
      command_async(consulerl_kv, txn, [self(), [{get, "test"}], []], {ok, ?TXN_RESPONSE_MAP}),
      command_async(consulerl_kv, txn, [self(), [{set, "test", "test", []}], []], {ok, ?TXN_RESPONSE_MAP}),
      command_async_twice(consulerl_kv, txn, [self(), [{get, "test"}], []], {ok, ?TXN_RESPONSE_MAP}),
      command_async_twice(consulerl_kv, txn, [self(), [{set, "test", "test", []}], []], {ok, ?TXN_RESPONSE_MAP})
    ]
  } end).

txn_conflict_test_() ->
  ?setup(fun setup_conflict/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, txn, [[{get, "test"}], []], {error, "Conflict", ?TXN_ERROR_RESPONSE_MAP}),
      command_async(consulerl_kv, txn, [self(), [{get, "test"}], []], {error, "Conflict", ?TXN_ERROR_RESPONSE_MAP}),
      command_async_twice(consulerl_kv, txn, [self(), [{get, "test"}], []], {error, "Conflict", ?TXN_ERROR_RESPONSE_MAP})
    ]
  } end).

txn_not_found_test_() ->
  ?setup(fun setup_not_found/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, txn, [[{get, "test"}], []], {error, "Not Found"}),
      command_async(consulerl_kv, txn, [self(), [{get, "test"}], []], {error, "Not Found"}),
      command_async_twice(consulerl_kv, txn, [self(), [{get, "test"}], []], {error, "Not Found"})
    ]
  } end).

txn_timeout_test_() ->
  ?setup(fun setup_timeout/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, txn, [[{get, "test"}], []], {error, timeout}),
      command_async(consulerl_kv, txn, [self(), [{get, "test"}], []], {error, timeout}),
      command_async_twice(consulerl_kv, txn, [self(), [{get, "test"}], []], {error, timeout})
    ]
  } end).

watch_test_() ->
  ?setup(fun setup_get/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, watch, ["test"], {ok, ?GET_RESPONSE_MAP}),
      command_async(consulerl_kv, watch, ["test", self()], {ok, ?GET_RESPONSE_MAP}),
      command_async(consulerl_kv, watch, ["test", [], self()], {ok, ?GET_RESPONSE_MAP}),
      command_async(consulerl_kv, watch, ["test", [], self(), 1], {ok, ?GET_RESPONSE_MAP}),
      command_async(consulerl_kv, watch, ["test", [], self(), 2], {ok, ?GET_RESPONSE_MAP}),
      command(consulerl_kv, watch, ["test", [], self(), 0], ok)
    ]
  } end).

watch_not_found_test_() ->
  ?setup(fun setup_not_found/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, watch, ["test"], {error, "Not Found"}),
      command_async(consulerl_kv, watch, ["test", self()], {error, "Not Found"}),
      command_async(consulerl_kv, watch, ["test", [], self()], {error, "Not Found"}),
      command_async(consulerl_kv, watch, ["test", [], self(), 1], {error, "Not Found"})
    ]
  } end).

watch_timeout_test_() ->
  ?setup(fun setup_timeout/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_kv, watch, ["test"], {error, timeout}),
      command_async(consulerl_kv, watch, ["test", self()], {error, timeout}),
      command_async(consulerl_kv, watch, ["test", [], self()], {error, timeout}),
      command_async(consulerl_kv, watch, ["test", [], self(), 1], {error, timeout})
    ]
  } end).