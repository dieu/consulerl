-module(consulerl_api).

-include("consulerl.hrl").

%% API
-export([
  get/1,
  get/2,
  get/3,
  get/4
]).

-export([
  put/2,
  put/3,
  put/4,
  put/5
]).

-export([
  delete/1,
  delete/2,
  delete/3,
  delete/4
]).

-export([
  terminate/1,
  ensure_stopped/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec get(list()) -> return().
get(Path) ->
  get(Path, []).

-spec get(list(), list()) -> return().
get(Path, QArgs) ->
  consulerl_api_sup:execute_once(fun(Pid) ->
    ok = get(Pid, self(), Path, QArgs),
    consulerl_util:receive_response()
  end).

-spec get(ref(), list(), list()) -> pid().
get(From, Path, QArgs) ->
  consulerl_api_sup:execute(fun(Pid) ->
    ok = get(Pid, From, Path, QArgs),
    Pid
  end).

-spec get(pid(), ref(), list(), list()) -> ok.
get(Pid, From, Path, QArgs) ->
  consulerl_api_worker:get(Pid, From, Path, QArgs).

-spec put(list(), term()) -> return().
put(Path, Value) ->
  put(Path, Value, []).

-spec put(list(), term(), list()) -> return().
put(Path, Value, QArgs) ->
  consulerl_api_sup:execute_once(fun(Pid) ->
    ok = put(Pid, self(), Path, Value, QArgs),
    consulerl_util:receive_response()
  end).

-spec put(ref(), list(), term(), list()) -> pid().
put(From, Path, Value, QArgs) ->
  consulerl_api_sup:execute(fun(Pid) ->
    ok = put(Pid, From, Path, Value, QArgs),
    Pid
  end).

-spec put(pid(), ref(), list(), term(), list()) -> ok.
put(Pid, From, Path, Value, QArgs) ->
  consulerl_api_worker:put(Pid, From, Path, Value, QArgs).

-spec delete(list()) -> return().
delete(Path) ->
  delete(Path, []).

-spec delete(list(), list()) -> return().
delete(Path, QArgs) ->
  consulerl_api_sup:execute_once(fun(Pid) ->
    ok = delete(Pid, self(), Path, QArgs),
    consulerl_util:receive_response()
  end).

-spec delete(ref(), list(), list()) -> pid().
delete(From, Path, QArgs) ->
  consulerl_api_sup:execute(fun(Pid) ->
    ok = delete(Pid, From, Path, QArgs),
    Pid
  end).

-spec delete(pid(), ref(), list(), list()) -> ok.
delete(Pid, From, Path, QArgs) ->
  consulerl_api_worker:delete(Pid, From, Path, QArgs).

-spec terminate(pid() | term()) -> ok | {error, term()}.
terminate(Pid) when is_pid(Pid) ->
  consulerl_api_sup:stop(Pid);

terminate(_) ->
  ok.

-spec ensure_stopped(pid() | term()) -> ok.
ensure_stopped(Pid) when is_pid(Pid) ->
  consulerl_api_sup:ensure_stopped(Pid);

ensure_stopped(_) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
