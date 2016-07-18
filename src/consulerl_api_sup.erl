-module(consulerl_api_sup).

-include("consulerl.hrl").

-behaviour(supervisor).

-export([
  start_link/0,
  execute/1,
  stop/1
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


-spec execute(fun((pid()) -> pid() | return())) -> pid() | return().
execute(Fun) ->
  case supervisor:start_child(?SERVER, []) of
    {ok, Pid} when is_pid(Pid) ->
      Fun(Pid);
    {ok, Pid, _} when is_pid(Pid) ->
      Fun(Pid);
    {error, Reason} ->
      {error, Reason}
  end.

-spec stop(pid()) -> ok | {error, term()}.
stop(Pid) ->
  supervisor:terminate_child(?SERVER, Pid).

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


  ClientWorker = {
    consulerl_api_worker,
    {consulerl_api_worker, start_link, [Host, Port, ACL]},
    Restart, Shutdown, Worker, [consulerl_api_worker]
  },

  {ok, {SupFlags, [ClientWorker]}}.
