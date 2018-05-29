-module(consulerl_api_worker).

-include("consulerl.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/3
]).

-export([
  sync/3,
  get/5,
  put/5,
  delete/4
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
  host                    :: string(),
  port                    :: pos_integer(),
  acl                     :: {Key :: string(), Acl :: string()},
  requests = #{}          :: map()
}).

-record(response, {
  status                  :: undefined | pos_integer(),
  reason                  :: term(),
  headers = []            :: list(),
  body    = <<>>          :: binary()
}).

-record(request, {
  ref                     :: term(),
  from                    :: ref(),
  trim_header             :: boolean(),
  response = #response{}  :: response()
}).

-type response()          :: #response{}.
-type client_state()      :: #client{}.

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

-spec sync(pid(), atom(), list()) -> return().
sync(Pid, Method, Args) ->
  case catch gen:call(Pid, '$gen_call', {sync, Method, Args}, ?TIMEOUT) of
    {ok, Res} ->
      Res;
    {'EXIT', _} ->
      {error, timeout}
  end.

-spec get(pid(), ref(), list(), list(), boolean()) -> ok.
get(Pid, From, Path, QArgs, TrimHeader) ->
  gen_server:cast(Pid, {get, From, Path, QArgs, TrimHeader}).

-spec put(pid(), ref(), list(), term(), list()) -> ok.
put(Pid, From, Path, Value, QArgs) ->
  gen_server:cast(Pid, {put, From, Path, Value, QArgs}).

-spec delete(pid(), ref(), list(), list()) -> ok.
delete(Pid, From, Path, QArgs) ->
  gen_server:cast(Pid, {delete, From, Path, QArgs}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the serveracl_key
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: client_state()}.
init([Host, Port, {AclKey, Acl}]) ->
  _ = process_flag(trap_exit, true),
  {ok, #client{
    host = Host,
    port = Port,
    acl = {AclKey, Acl}
  }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: client_state()) ->
  {noreply, NewState :: client_state()} |
  {reply, Reply :: term(), NewState :: client_state()}.
handle_call({sync, Method, Args}, From, State) ->
  ok = apply(?SERVER, Method, [self(), From | Args]),
  {noreply, State};

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
handle_cast({get, From, Path, QArgs, Trim}, #client{host = Host, port = Port, acl = {AclKey, Acl}} = State) ->
  URL = consulerl_util:build_url(Host, Port, Path, lists:merge(QArgs, [{AclKey, Acl}])),

  reply(make_request(get, {URL, [], <<>>}, From), From, Trim, State);

handle_cast({put, From, Path, Value, QArgs}, #client{host = Host, port = Port, acl = {AclKey, Acl}} = State) ->
  URL = consulerl_util:build_url(Host, Port, Path, lists:merge(QArgs, [{AclKey, Acl}])),

  BinValue = consulerl_util:to_binary(Value),

  reply(make_request(put, {URL, [?MIME_FORM], BinValue}, From), From, true, State);

handle_cast({delete, From, Path, QArgs}, #client{host = Host, port = Port, acl = {AclKey, Acl}} = State) ->
  URL = consulerl_util:build_url(Host, Port, Path, lists:merge(QArgs, [{AclKey, Acl}])),

  reply(make_request(delete, {URL, [], <<>>}, From), From, true, State);

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
handle_info({hackney_response, Ref, {status, Status, Reason}}, #client{requests = Reqs} = State) ->
  Request = #request{response = Response} = maps:get(Ref, Reqs),
  {noreply, State#client{requests = maps:update(Ref, Request#request{
    response = Response#response{status = Status, reason = Reason}
  }, Reqs)}};

handle_info({hackney_response, Ref, {headers, Headers}}, #client{requests = Reqs} = State) ->
  Request = #request{response = Response} = maps:get(Ref, Reqs),
  {noreply, State#client{requests = maps:update(Ref, Request#request{
    response = Response#response{headers = Headers}
  }, Reqs)}};

handle_info({hackney_response, Ref, done}, #client{requests = Reqs} = State) ->
  #request{response = Response, from = From, trim_header = Trim} = maps:get(Ref, Reqs),

  ok = lager:debug("response=~p", [Response]),

  Reply = response(Response, Trim),

  ok = consulerl_util:do(From, Reply),
  {noreply, State#client{requests = maps:remove(Ref, Reqs)}};

handle_info({hackney_response, Ref, {error, Reason}}, #client{requests = Reqs} = State) ->
  Request = #request{response = Response} = maps:get(Ref, Reqs),
  handle_info({hackney_response, Ref, done}, State#client{requests = maps:update(Ref, Request#request{
    response = Response#response{reason = Reason}
  }, Reqs)});

handle_info({hackney_response, Ref, Bin}, #client{requests = Reqs} = State) ->
  Request = #request{response = #response{body = Body} = Response} = maps:get(Ref, Reqs),
  {noreply, State#client{requests = maps:update(Ref, Request#request{
    response = Response#response{body = <<Body/binary, Bin/binary>>}
  }, Reqs)}};

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
  lists:foreach(fun({Request, _}) ->
    _ = hackney:cancel_request(Request)
  end, maps:to_list(Requests)),
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
make_request(Method, {URL, Header, Body}, From) ->
  ok = lager:debug("HTTP Request: method=~p; url=~p; header=~p; body=~p; from=~p; self=~p", [Method, URL, Header, Body, From, self()]),
  hackney:request(Method, URL, Header, Body, [
    async,
    {connect_timeout, infinity},
    {recv_timeout, infinity},
    {following_redirect, true}
  ]).

-spec response(response(), boolean()) -> {ok, string()} | {error, term()}.
response(#response{status = 200} = Response, true) ->
  {ok, decode(Response)};

response(#response{status = 200, headers = Headers} = Response, false) ->
  {ok, decode(Response), Headers};

response(#response{reason = Reason, body = <<>>}, _) ->
  {error, Reason};

response(#response{reason = Reason} = Response, _) ->
  {error, Reason, decode(Response)}.

-spec decode(response()) -> binary() | map().
decode(#response{headers = Headers, body = Body}) ->
  case proplists:get_value(<<"Content-Type">>, Headers) of
    <<"application/json">> -> consulerl_json:decode(?JSON, Body);
    _ -> Body
  end.

-spec reply(return(), ref(), boolean(), client_state()) -> {noreply, client_state()}.
reply({ok, RequestId}, From, Trim, #client{requests = Reqs} = State) ->
  {noreply, State#client{
    requests = maps:put(RequestId, #request{
      ref = RequestId,
      from = From,
      trim_header = Trim
    }, Reqs)
  }};

reply({error, Reason}, From, _, State) ->
  consulerl_util:do(From, {error, Reason}),
  {noreply, State}.
