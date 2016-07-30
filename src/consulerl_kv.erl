-module(consulerl_kv).

-include("consulerl.hrl").

-export([
  get/1,
  get/2,
  get/3,
  get/4
]).

-export([
  get_all/1,
  get_all/2,
  get_all/3,
  get_all/4
]).

-export([
  keys/0,
  keys/1,
  keys/2,
  keys/3,
  keys/4
]).

-export([
  put/2,
  put/3,
  put/4,
  put/5,
  put/6
]).

-export([
  watch/1,
  watch/2,
  watch/3,
  watch/4
]).

-export([
  delete/1,
  delete/2,
  delete/3,
  delete/4,
  delete/5
]).

-export([
  txn/2,
  txn/3,
  txn/4
]).

-spec get(string()) -> return().
get(Key) ->
  get(Key, []).

-spec get(string(), list()) -> return().
get(Key, QArgs) ->
  case consulerl_api:get([kv, Key], QArgs) of
    {ok, [Payload]} -> {ok, response(Payload)};
    {error, Reason} -> {error, Reason};
    {error, Reason, _} -> {error, Reason}
  end.

-spec get(ref(), string(), list()) -> pid().
get(From, Key, QArgs) ->
  consulerl_api:get(response_fun(From, fun response/1), [kv, Key], QArgs).

-spec get(pid(), ref(), string(), list()) -> ok.
get(Pid, From, Key, QArgs) ->
  consulerl_api:get(Pid, response_fun(From, fun response/1), [kv, Key], QArgs).

-spec get_all(string()) -> return().
get_all(Prefix) ->
  get_all(Prefix, []).


-spec get_all(string(), list()) -> return().
get_all(Prefix, QArgs) ->
  case consulerl_api:get([kv, Prefix], [rescure | QArgs]) of
    {ok, Response} -> {ok, responses(Response)};
    {error, Reason} -> {error, Reason};
    {error, Reason, _} -> {error, Reason}
  end.

-spec get_all(ref(), string(), list()) -> pid().
get_all(From, Prefix, QArgs) ->
  consulerl_api:get(response_fun(From, fun responses/1), [kv, Prefix], [rescure | QArgs]).

-spec get_all(pid(), ref(), string(), list()) -> ok.
get_all(Pid, From, Prefix, QArgs) ->
  consulerl_api:get(Pid, response_fun(From, fun responses/1), [kv, Prefix], [rescure | QArgs]).

-spec keys() -> return().
keys() ->
  keys("").

-spec keys(string()) -> return().
keys(Prefix) ->
  keys(Prefix, []).

-spec keys(string(), list()) -> return().
keys(Prefix, QArgs) ->
  case consulerl_api:get([kv, Prefix], [keys | QArgs]) of
    {ok, Response} -> {ok, Response};
    {error, Reason} -> {error, Reason};
    {error, Reason, _} -> {error, Reason}
  end.

-spec keys(ref(), string(), list()) -> pid().
keys(From, Prefix, QArgs) ->
  consulerl_api:get(From, [kv, Prefix], [keys | QArgs]).

-spec keys(pid(), ref(), string(), list()) -> ok.
keys(Pid, From, Prefix, QArgs) ->
  consulerl_api:get(Pid, From, [kv, Prefix], [keys | QArgs]).

-spec put(string(), term()) -> return().
put(Key, Value) ->
  put(Key, Value, 0, none).

-spec put(string(), term(), non_neg_integer()) -> return().
put(Key, Value, Flags) ->
  put(Key, Value, Flags, none).

-spec put(string(), term(), non_neg_integer(), none | non_neg_integer()) -> return().
put(Key, Value, Flags, CAS) ->
  case consulerl_api:put([kv, Key], Value, [{flags, Flags}] ++ cas(CAS)) of
    {ok, Response} -> {ok, Response};
    {error, Reason} -> {error, Reason};
    {error, Reason, _} -> {error, Reason}
  end.

-spec put(ref(), string(), term(), list(), atom()) -> pid().
put(From, Key, Value, Flags, CAS) ->
  consulerl_api:put(From, [kv, Key], Value, [{flags, Flags} | cas(CAS)]).

-spec put(pid(), ref(), string(), term(), list(), atom()) -> ok.
put(Pid, From, Key, Value, Flags, CAS) ->
  consulerl_api:put(Pid, From, [kv, Key], Value, [{flags, Flags} | cas(CAS)]).

-spec watch(string()) -> return().
watch(Key) ->
  _ = watch(Key, self()),
  consulerl_util:receive_response().

-spec watch(string(), ref()) -> pid().
watch(Key, Callback) ->
  watch(Key, [], Callback).

-spec watch(string(), list(), ref()) -> pid().
watch(Key, QArgs, Callback) ->
  watch(Key, QArgs, Callback, 1).

-spec watch(string(), list(), ref(), pos_integer() | infinity) -> pid() | ok.
watch(Path, QArgs, Callback, Retry) ->
  Args = lists:delete(raw, QArgs),
  case watch_fun([kv, Path], Args, Callback, Retry) of
    ok ->
      ok;
    Watch ->
      consulerl_api:get(Watch, [kv, Path], Args)
  end.

-spec delete(string()) -> return().
delete(Key) ->
  delete(Key, false, none).

-spec delete(string(), boolean()) -> return().
delete(Key, Recurse) ->
  delete(Key, Recurse, none).

-spec delete(string(), boolean(), integer() | none) -> return().
delete(Key, Recurse, CAS) ->
  case consulerl_api:delete([kv, Key], recurse(Recurse) ++ cas(CAS)) of
    {ok, Response} -> {ok, Response};
    {error, Reason} -> {error, Reason};
    {error, Reason, _} -> {error, Reason}
  end.

-spec delete(ref(), string(), boolean(), integer() | none) -> pid().
delete(From, Key, Recurse, CAS) ->
  consulerl_api:delete(From, [kv, Key], recurse(Recurse) ++ cas(CAS)).

-spec delete(pid(), ref(), string(), boolean(), integer() | none) -> ok.
delete(Pid, From, Key, Recurse, CAS) ->
  consulerl_api:delete(Pid, From, [kv, Key], recurse(Recurse) ++ cas(CAS)).

-spec txn(list(), list()) -> return().
txn(Operations, Args) ->
  OperationsTxn = lists:map(fun txn_operation/1, Operations),

  Transaction = lists:map(fun(Tx) -> #{
    "KV" => Tx
  } end, OperationsTxn),

  case consulerl_api:put([txn], consulerl_util:to_json(Transaction), Args) of
    ({ok, Payload}) -> {ok, txn_response(Payload)};
    ({error, _} = Error) -> Error;
    ({error, Reason, <<>>}) -> {error, Reason};
    ({error, Reason, Payload}) -> {error, Reason, txn_response(Payload)}
  end.

-spec txn(ref(), list(), list()) -> pid().
txn(From, Operations, Args) ->
  OperationsTxn = lists:map(fun txn_operation/1, Operations),

  Transaction = lists:map(fun(Tx) -> #{
    "KV" => Tx
  } end, OperationsTxn),

  consulerl_api:put(
    response_fun(From, fun txn_response/1, fun txn_response/1),
    [txn],
    consulerl_util:to_json(Transaction),
    Args
  ).

-spec txn(pid(), ref(), list(), list()) -> ok.
txn(Pid, From, Operations, Args) ->
  OperationsTxn = lists:map(fun txn_operation/1, Operations),

  Transaction = lists:map(fun(Tx) -> #{
    "KV" => Tx
  } end, OperationsTxn),

  consulerl_api:put(
    Pid,
    response_fun(From, fun txn_response/1, fun txn_response/1),
    [txn],
    consulerl_util:to_json(Transaction),
    Args
  ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec watch_fun(list(), list(), ref(), pos_integer() | infinity) -> fun((term()) -> ok) | ok.
watch_fun(Path, QArgs, Callback, Retry) when Retry > 0 orelse Retry =:= infinity ->
  fun
    ({ok, [Payload]}) ->
      consulerl_api:get(self(), fun
        ({ok, [Response]}) ->
          consulerl_util:do(Callback, {ok, response(Response)}),
          case watch_fun(Path, QArgs, Callback, dec(Retry)) of
            ok ->
              ok;
            Watch ->
              consulerl_api:get(self(), Watch, Path, QArgs)
          end;
        ({error, _} = Error) ->
          consulerl_util:do(Callback, Error)
      end, Path, [index(Payload) | QArgs]);
    ({error, _} = Error) ->
      consulerl_util:do(Callback, Error);
    ({error, _, _} = Error) ->
      consulerl_util:do(Callback, Error)
  end;

watch_fun(_, _, _, _) ->
  ok.

-spec txn_operation(tuple()) -> map().
txn_operation({set, Key, Value, Flags}) -> #{
  "Verb" => set,
  "Key" => Key,
  "Value" => consulerl_util:base64encode(Value),
  "Falgs" => Flags
};

txn_operation({set, Key, Value}) -> #{
  "Verb" => set,
  "Key" => Key,
  "Value" => consulerl_util:base64encode(Value)
};

txn_operation({cas, Key, Value, Index}) -> #{
  "Verb" => set,
  "Key" => Key,
  "Value" => consulerl_util:base64encode(Value),
  "Index" => Index
};

txn_operation({cas, Key, Value, Flags, Index}) -> #{
  "Verb" => set,
  "Key" => Key,
  "Value" => consulerl_util:base64encode(Value),
  "Index" => Index,
  "Flags" => Flags
};

txn_operation({Verb, Key, Value, Session}) when Verb =:= lock orelse Verb =:= unlock -> #{
  "Verb" => Verb,
  "Key" => Key,
  "Value" => consulerl_util:base64encode(Value),
  "Session" => Session
};

txn_operation({Verb, Key, Value, Flags, Session}) when Verb =:= lock orelse Verb =:= unlock -> #{
  "Verb" => Verb,
  "Key" => Key,
  "Value" => consulerl_util:base64encode(Value),
  "Session" => Session,
  "Flags" => Flags
};

txn_operation({Verb, Key}) when Verb =:= get orelse Verb =:= get_tree -> #{
  "Verb" => Verb,
  "Key" => Key
};

txn_operation({Verb, Key}) when Verb =:= delete orelse Verb =:= delete_tree -> #{
  "Verb" => Verb,
  "Key" => Key
};

txn_operation({Verb, Key, Index}) when Verb =:= check_index orelse Verb =:= delete_cas -> #{
  "Verb" => Verb,
  "Key" => Key,
  "Index" => Index
};

txn_operation({Verb, Key, Session}) when Verb =:= check_session -> #{
  "Verb" => Verb,
  "Key" => Key,
  "Session" => Session
}.

-spec response_fun(ref(), fun((term()) -> term())) -> fun((term()) -> ok).
response_fun(From, Decode) ->
  fun
    ({ok, Payload}) ->
      consulerl_util:do(From, {ok, Decode(Payload)});
    ({error, _} = Error) ->
      consulerl_util:do(From, Error);
    ({error, Reason, _}) ->
      consulerl_util:do(From, {error, Reason})
  end.

-spec response_fun(ref(), fun((term()) -> term()), fun((term()) -> term())) -> fun((term()) -> ok).
response_fun(From, Decode, ErrorDecode) ->
  fun
    ({ok, Payload}) ->
      consulerl_util:do(From, {ok, Decode(Payload)});
    ({error, _} = Error) ->
      consulerl_util:do(From, Error);
    ({error, Reason, Payload}) ->
      consulerl_util:do(From, {error, Reason, ErrorDecode(Payload)})
  end.

-spec responses([proplists:proplist()]) -> [map()].
responses(Payloads) ->
  lists:map(fun response/1, Payloads).

-spec response(proplists:proplist() | [proplists:proplist()]) -> map().
response([Payload]) ->
  response(Payload);

response(Payload) -> #{
  create_index => proplists:get_value(<<"CreateIndex">>, Payload),
  modify_index => proplists:get_value(<<"ModifyIndex">>, Payload),
  lock_index => proplists:get_value(<<"LockIndex">>, Payload),
  key => binary_to_list(proplists:get_value(<<"Key">>, Payload)),
  flags => proplists:get_value(<<"Flags">>, Payload),
  value => base64:decode_to_string(proplists:get_value(<<"Value">>, Payload))
}.

-spec error_response(proplists:proplist()) -> map().
error_response(Payload) -> #{
  op_index => proplists:get_value(<<"OpIndex">>, Payload),
  waht => proplists:get_value(<<"What">>, Payload)
}.

-spec txn_response(proplists:proplist() | binary()) -> map() | binary().
txn_response(Payload) when is_list(Payload) -> #{
  results => case proplists:get_value(<<"Results">>, Payload, null) of
    null -> null;
    Value -> lists:map(fun({_, Response}) -> response(Response) end, hd(Value))
  end,
  errors => case proplists:get_value(<<"Errors">>, Payload, null) of
    null -> null;
    Value -> lists:map(fun(Error) -> error_response(Error) end, Value)
  end,
  index => proplists:get_value(<<"Index">>, Payload),
  last_contact => proplists:get_value(<<"LastContact">>, Payload),
  know_leader => proplists:get_value(<<"KnownLeader">>, Payload)
};

txn_response(Value) ->
  Value.


-spec dec(integer() | infinity) -> integer() | infinity.
dec(Retry) when is_integer(Retry) ->
  Retry - 1;

dec(infinity) ->
  infinity.

-spec index(proplists:proplist()) -> {index, integer()}.
index(Payload) ->
  {index, proplists:get_value(<<"ModifyIndex">>, Payload)}.

-spec recurse(boolean()) -> list().
recurse(true) ->
  [recurse];

recurse(false) ->
  [].

-spec cas(none | non_neg_integer()) -> list().
cas(none) ->
  [];

cas(CAS) ->
  [{cas, CAS}].