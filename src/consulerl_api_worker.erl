-module(consulerl_api_worker).

-include("consulerl.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/3
]).

-export([
  get/4,
  put/5,
  delete/4
]).

%% internal callbacks
-export([
  response/3
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-record(client, {
  host :: string(),
  port :: pos_integer(),
  acl :: string(),
  requests = [] :: list()
}).

-type client_state() :: #client{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(string(), pos_integer(), string()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Host, Port, Acl) ->
  gen_server:start_link(?MODULE, [Host, Port, Acl], []).

-spec get(pid(), ref(), list(), list()) -> ok.
get(Pid, From, Path, QArgs) ->
  gen_server:cast(Pid, {get, From, Path, QArgs}).

-spec put(pid(), ref(), list(), term(), list()) -> ok.
put(Pid, From, Path, Value, QArgs) ->
  gen_server:cast(Pid, {put, From, Path, Value, QArgs}).

-spec delete(pid(), ref(), list(), list()) -> ok.
delete(Pid, From, Path, QArgs) ->
  gen_server:cast(Pid, {delete, From, Path, QArgs}).

-spec response(term(), pid(), ref()) -> ok.
response(Response, Pid, RequestFrom) ->
  gen_server:cast(Pid, {response, RequestFrom, Response}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: client_state()}.
init([Host, Port, Acl]) ->
  _ = process_flag(trap_exit, true),
  {ok, #client{
    host = Host,
    port = Port,
    acl = Acl
  }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: client_state()) ->
  {reply, Reply :: term(), NewState :: client_state()}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: client_state()) -> {noreply, NewState :: client_state()}.
handle_cast({get, From, Path, QArgs}, #client{host = Host, port = Port, acl = Acl, requests = Queue} = State) ->
  URL = consulerl_util:build_url(Host, Port, Path, lists:merge(QArgs, [{acl, Acl}])),

  {ok, RequestId} = make_request(get, {URL, []}, From),

  {noreply, State#client{
    requests = [RequestId | Queue]
  }};

handle_cast({put, From, Path, Value, QArgs}, #client{host = Host, port = Port, acl = Acl, requests = Queue} = State) ->
  URL = consulerl_util:build_url(Host, Port, Path, lists:merge(QArgs, [{acl, Acl}])),

  BinValue = consulerl_util:to_binary(Value),

  {ok, RequestId} = make_request(put, {URL, [], ?MIME_FORM, BinValue}, From),

  {noreply, State#client{
    requests = [RequestId | Queue]
  }};

handle_cast({delete, From, Path, QArgs}, #client{host = Host, port = Port, acl = Acl, requests = Queue} = State) ->
  URL = consulerl_util:build_url(Host, Port, Path, lists:merge(QArgs, [{acl, Acl}])),

  {ok, RequestId} = make_request(delete, {URL, []}, From),

  {noreply, State#client{
    requests = [RequestId | Queue]
  }};

handle_cast({response, RequestFrom, Response}, #client{requests = Queue} = State) ->
  NewQueue = case consulerl_util:response(Response) of
    {RequestId, Res} when is_pid(RequestId) orelse is_reference(RequestId) ->
      consulerl_util:do(RequestFrom, Res),
      httpc:cancel_request(RequestId),
      lists:delete(RequestId, Queue);
    Res ->
      consulerl_util:do(RequestFrom, Res),
      Queue
  end,
  {noreply, State#client{
    requests = NewQueue
  }};

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: client_state()) ->
  {noreply, NewState :: client_state()}.
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: client_state()) -> term().
terminate(_Reason, #client{requests = Requests}) ->
  lists:foreach(fun(Request) ->
    ok = httpc:cancel_request(Request)
  end, Requests),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: client_state(), Extra :: term()) ->
  {ok, NewState :: client_state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec make_request(get | put | delete, tuple(), pid()) -> {ok, term()} | {error, term()}.
make_request(Method, Body, From) ->
  httpc:request(Method, Body, [],
    [{sync, false}, {receiver, {?MODULE, response, [self(), From]}}]
  ).