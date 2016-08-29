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
    {ok, [Payload]} -> {ok, get_response(Payload)};
    Error -> Error
  end.

-spec get(ref(), string(), list()) -> pid().
get(From, Key, QArgs) ->
  consulerl_api:get(get_response_fun(From), [kv, Key], QArgs).

-spec get(pid(), ref(), string(), list()) -> ok.
get(Pid, From, Key, QArgs) ->
  consulerl_api:get(Pid, get_response_fun(From), [kv, Key], QArgs).

-spec get_all(string()) -> return().
get_all(Prefix) ->
  get_all(Prefix, []).

-spec get_all(string(), list()) -> return().
get_all(Prefix, QArgs) ->
  get_response(consulerl_api:get([kv, Prefix], [recurse | QArgs])).

-spec get_all(ref(), string(), list()) -> pid().
get_all(From, Prefix, QArgs) ->
  consulerl_api:get(get_all_response_fun(From), [kv, Prefix], [recurse | QArgs]).

-spec get_all(pid(), ref(), string(), list()) -> ok.
get_all(Pid, From, Prefix, QArgs) ->
  consulerl_api:get(Pid, get_all_response_fun(From), [kv, Prefix], [recurse | QArgs]).

-spec keys() -> return().
keys() ->
  keys("").

-spec keys(string()) -> return().
keys(Prefix) ->
  keys(Prefix, []).

-spec keys(string(), list()) -> return().
keys(Prefix, QArgs) ->
  consulerl_api:get([kv, Prefix], [keys | QArgs]).

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
  consulerl_api:put([kv, Key], Value, [{flags, Flags}] ++ cas(CAS)).

-spec put(ref(), string(), term(), list(), atom()) -> pid().
put(From, Key, Value, Flags, CAS) ->
  consulerl_api:put(From, [kv, Key], Value, [{flags, Flags} | cas(CAS)]).

-spec put(pid(), ref(), string(), term(), list(), atom()) -> ok.
put(Pid, From, Key, Value, Flags, CAS) ->
  consulerl_api:put(Pid, From, [kv, Key], Value, [{flags, Flags} | cas(CAS)]).

-spec watch(string()) -> return().
watch(Key) ->
  From = {_, Ref} = {self(), make_ref()},
  _ = watch(Key, From),
  consulerl_util:receive_response(Ref).

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
  consulerl_api:delete([kv, Key], recurse(Recurse) ++ cas(CAS)).

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
    <<"KV">> => Tx
  } end, OperationsTxn),

  case consulerl_api:put([txn], consulerl_json:encode(?JSON, Transaction), Args) of
    {ok, Payload} -> {ok, txn_response(Payload)};
    Error -> Error
  end.

-spec txn(ref(), list(), list()) -> pid().
txn(From, Operations, Args) ->
  OperationsTxn = lists:map(fun txn_operation/1, Operations),

  Transaction = lists:map(fun(Tx) -> #{
    <<"KV">> => Tx
  } end, OperationsTxn),

  consulerl_api:put(
    txn_response_fun(From),
    [txn],
    consulerl_json:encode(?JSON, Transaction),
    Args
  ).

-spec txn(pid(), ref(), list(), list()) -> ok.
txn(Pid, From, Operations, Args) ->
  OperationsTxn = lists:map(fun txn_operation/1, Operations),

  Transaction = lists:map(fun(Tx) -> #{
    <<"KV">> => Tx
  } end, OperationsTxn),

  consulerl_api:put(
    Pid,
    txn_response_fun(From),
    [txn],
    consulerl_json:encode(?JSON, Transaction),
    Args
  ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec watch_fun(list(), list(), ref(), pos_integer() | infinity) -> fun((term()) -> ok) | ok.
watch_fun(Path, QArgs, Callback, Retry) when Retry > 0 orelse Retry =:= infinity ->
  fun
    ({ok, [#{modify_index := ModifyIndex}]}) ->
      consulerl_api:get(self(), fun
        ({ok, [Response]}) ->
          consulerl_util:do(Callback, {ok, get_response(Response)}),
          case watch_fun(Path, QArgs, Callback, consulerl_util:dec(Retry)) of
            ok ->
              ok;
            Watch ->
              consulerl_api:get(self(), Watch, Path, QArgs, true)
          end;
        (Error) ->
          consulerl_util:do(Callback, Error)
      end, Path, [{index, ModifyIndex} | QArgs], true);
    (Error) ->
      consulerl_util:do(Callback, Error)
  end;

watch_fun(_, _, _, _) ->
  ok.

-spec txn_operation(tuple()) -> map().
txn_operation({set, Key, Value, Flags}) -> #{
  <<"Verb">> => set,
  <<"Key">> => Key,
  <<"Value">> => consulerl_util:base64encode(Value),
  <<"Falgs">> => Flags
};

txn_operation({set, Key, Value}) -> #{
  <<"Verb">> => set,
  <<"Key">> => Key,
  <<"Value">> => consulerl_util:base64encode(Value)
};

txn_operation({cas, Key, Value, Index}) -> #{
  <<"Verb">> => set,
  <<"Key">> => Key,
  <<"Value">> => consulerl_util:base64encode(Value),
  <<"Index">> => Index
};

txn_operation({cas, Key, Value, Flags, Index}) -> #{
  <<"Verb">> => set,
  <<"Key">> => Key,
  <<"Value">> => consulerl_util:base64encode(Value),
  <<"Index">> => Index,
  <<"Flags">> => Flags
};

txn_operation({Verb, Key, Value, Session}) when Verb =:= lock orelse Verb =:= unlock -> #{
  <<"Verb">> => Verb,
  <<"Key">> => Key,
  <<"Value">> => consulerl_util:base64encode(Value),
  <<"Session">> => Session
};

txn_operation({Verb, Key, Value, Flags, Session}) when Verb =:= lock orelse Verb =:= unlock -> #{
  <<"Verb">> => Verb,
  <<"Key">> => Key,
  <<"Value">> => consulerl_util:base64encode(Value),
  <<"Session">> => Session,
  <<"Flags">> => Flags
};

txn_operation({Verb, Key}) when Verb =:= get orelse Verb =:= get_tree -> #{
  <<"Verb">> => Verb,
  <<"Key">> => Key
};

txn_operation({Verb, Key}) when Verb =:= delete orelse Verb =:= delete_tree -> #{
  <<"Verb">> => Verb,
  <<"Key">> => Key
};

txn_operation({Verb, Key, Index}) when Verb =:= check_index orelse Verb =:= delete_cas -> #{
  <<"Verb">> => Verb,
  <<"Key">> => Key,
  <<"Index">> => Index
};

txn_operation({Verb, Key, Session}) when Verb =:= check_session -> #{
  <<"Verb">> => Verb,
  <<"Key">> => Key,
  <<"Session">> => Session
}.

-spec get_response_fun(ref()) -> fun((term()) -> ok).
get_response_fun(From) ->
  fun
    ({ok, [Payload]}) ->
      consulerl_util:do(From, {ok, get_response(Payload)});
    ({error, _} = Error) ->
      consulerl_util:do(From, Error);
    ({error, Reason, _}) ->
      consulerl_util:do(From, {error, Reason})
  end.

-spec get_all_response_fun(ref()) -> fun((term()) -> ok).
get_all_response_fun(From) ->
  fun
    ({ok, Payload}) ->
      consulerl_util:do(From, {ok, get_response(Payload)});
    ({error, _} = Error) ->
      consulerl_util:do(From, Error);
    ({error, Reason, _}) ->
      consulerl_util:do(From, {error, Reason})
  end.

-spec get_response(return() | map() | list()) -> return() | map() | list().
get_response({ok, Responses}) when is_list(Responses) ->
  {ok, lists:map(fun get_response/1, Responses)};

get_response(Responses) when is_list(Responses) ->
  lists:map(fun get_response/1, Responses);

get_response(#{value := Value} = Response) ->
  maps:update(value, consulerl_util:base64decode(Value), Response);

get_response(Response) ->
  Response.

-spec txn_response_fun(ref()) -> fun((term()) -> ok).
txn_response_fun(From) ->
  fun
    ({ok, Payload}) ->
      consulerl_util:do(From, {ok, txn_response(Payload)});
    (Error) ->
      consulerl_util:do(From, Error)
  end.

-spec txn_response(map() | term()) -> map() | term().
txn_response(#{results := Results} = Payload) ->
  NewResults = lists:map(fun
    (#{kv := #{value := Value} = Response}) -> #{
      kv =>
        maps:update(value, consulerl_util:base64decode(Value), Response)
      };
    (Response) -> Response
  end, Results),
  maps:update(results, NewResults, Payload);

txn_response(Payload) ->
  Payload.

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