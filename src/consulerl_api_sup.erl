-module(consulerl_api_sup).

-include("consulerl.hrl").

-behaviour(supervisor).

-export([
  start_link/0,
  execute/1,
  execute_once/1,
  stop/1,
  ensure_stopped/1
]).

-export([
  init/1
]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec execute_once(fun((pid()) -> return())) -> return().
execute_once(Fun) ->
  case supervisor:start_child(?SERVER, []) of
    {ok, Pid} when is_pid(Pid) ->
      Response = Fun(Pid),
      ok = stop(Pid),
      Response;
    {error, Reason} ->
      {error, Reason}
  end.

-spec execute(fun((pid()) -> pid())) -> pid() | error().
execute(Fun) ->
  case supervisor:start_child(?SERVER, []) of
    {ok, Pid} when is_pid(Pid) ->
      Fun(Pid);
    {error, Reason} ->
      {error, Reason}
  end.

-spec stop(pid()) -> ok | {error, term()}.
stop(Pid) ->
  supervisor:terminate_child(?SERVER, Pid).

-spec ensure_stopped(pid()) -> ok.
ensure_stopped(Pid) ->
  ensure_stopped(Pid, erlang:is_process_alive(Pid)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init(Args :: term()) ->
  {ok,
    {SupFlags :: {RestartStrategy :: supervisor:strategy(), MaxR :: non_neg_integer(), MaxT :: pos_integer()},
      [ChildSpec :: supervisor:child_spec()]
    }} | ignore.
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Worker = worker,

  {ok, Host} = application:get_env(consulerl, hostname),
  {ok, Port} = application:get_env(consulerl, port),
  {ok, ACL} = application:get_env(consulerl, acl),
  
  AclSettings = parse_acl_config(ACL),
  ClientWorker = {
    consulerl_api_worker,
    {consulerl_api_worker, start_link, [Host, Port, AclSettings]},
    Restart, Shutdown, Worker, [consulerl_api_worker]
  },

  {ok, {SupFlags, [ClientWorker]}}.

-spec ensure_stopped(pid(), boolean()) -> ok.
ensure_stopped(Pid, true) ->
  ensure_stopped(Pid, erlang:is_process_alive(Pid));

ensure_stopped(_Pid, false) ->
  ok.

-spec parse_acl_config(Acl :: {string() | atom(), string()} | string()) -> ok.
parse_acl_config({AclKey,Acl}) ->
  {AclKey,Acl};
parse_acl_config(Acl) ->
    {acl, Acl}.
