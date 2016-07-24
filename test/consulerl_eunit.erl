-module(consulerl_eunit).

%% API
-export([
  setup_app/0,
  setup_httpc_200/1,
  setup_httpc_404/0
]).

-export([
  stop_app/0,
  stop_httpc/0,
  stop/1
]).

setup_app() ->
  {ok, _} = application:ensure_all_started(consulerl),
  ok.

setup_httpc_200(Response) ->
  ok = meck:expect(httpc, request, fun(_, _, _, [{sync, false}, {receiver, {M, F, Args}}]) ->
    apply(M, F, [{self(), {{test, 200, test}, [{"content-type", "application/json"}], Response}} | Args]),
    {ok, self()}
  end).

setup_httpc_404() ->
  ok = meck:expect(httpc, request, fun(_, _, _, [{sync, false}, {receiver, {M, F, Args}}]) ->
    apply(M, F, [{self(), {{test, 404, "Not Found"}, [{"content-type", "text/plain; charset=utf-8"}], <<>>}} | Args]),
    {ok, self()}
  end).


stop_app() ->
  application:stop(consulerl).

stop_httpc() ->
  ok = meck:unload(httpc).

stop(_) ->
  ok = stop_httpc(),
  ok = stop_app().
