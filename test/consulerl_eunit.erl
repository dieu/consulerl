-module(consulerl_eunit).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
  setup_app/0,
  setup_httpc_200/1,
  setup_httpc_200/2,
  setup_httpc_404/0,
  setup_httpc_409/1,
  setup_httpc_timeout/0,
  setup_error/0
]).

-export([
  stop_app/0,
  stop_httpc/0,
  stop/1
]).

-export([
  command/4,
  command_async/4
]).

setup_app() ->
  {ok, _} = application:ensure_all_started(consulerl),
  ok.

setup_httpc_200(Response) ->
  ok = meck:expect(httpc, request, fun(_, _, _, [{sync, false}, {receiver, {M, F, Args}}]) ->
    apply(M, F, [{self(), {{test, 200, test}, [{"content-type", "application/json"}], Response}} | Args]),
    {ok, self()}
  end).

setup_httpc_200(Response, Timeout) ->
  ok = meck:expect(httpc, request, fun(_, _, _, [{sync, false}, {receiver, {M, F, Args}}]) ->
    WArgs = [{self(), {{test, 200, test}, [{"content-type", "application/json"}], Response}} | Args],
    apply(M, F, WArgs),
    timer:apply_after(Timeout, M, F, WArgs),
    {ok, self()}
  end).

setup_httpc_404() ->
  ok = meck:expect(httpc, request, fun(_, _, _, [{sync, false}, {receiver, {M, F, Args}}]) ->
    apply(M, F, [{self(), {{test, 404, "Not Found"}, [{"content-type", "text/plain; charset=utf-8"}], <<>>}} | Args]),
    {ok, self()}
  end).

setup_httpc_409(Error) ->
  ok = meck:expect(httpc, request, fun(_, _, _, [{sync, false}, {receiver, {M, F, Args}}]) ->
    apply(M, F, [{self(), {{test, 409, "Conflict"}, [{"content-type", "application/json"}], Error}} | Args]),
    {ok, self()}
  end).

setup_httpc_timeout() ->
  ok = meck:expect(httpc, request, fun(_, _, _, [{sync, false}, {receiver, {_, _, _}}]) ->
    {ok, self()}
  end).

setup_error() ->
  ok = meck:expect(httpc, request, fun(_, _, _, [{sync, false}, {receiver, {M, F, Arg}}]) ->
    apply(M, F, [{error, error} | Arg]),
    {ok, self()}
  end).

stop_app() ->
  application:stop(consulerl).

stop_httpc() ->
  ok = meck:unload(httpc).

stop(_) ->
  ok = stop_httpc(),
  ok = stop_app().

command(Module, Method, Args, ExpectedResponse) ->
  Response = apply(Module, Method, Args),
  ?_assertEqual(ExpectedResponse, Response).

command_async(Module, Method, Args, ExpectedResponse) ->
  Pid = apply(Module, Method, Args),
  Response = consulerl_util:receive_response(),
  ok = consulerl_api:terminate(Pid),
  ?_assertEqual(ExpectedResponse, Response).