-module(consulerl_eunit).

-include("consulerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
  setup_app/0,
  setup_httpc_200/1,
  setup_httpc_200/2,
  setup_httpc_200_plain/1,
  setup_httpc_404/0,
  setup_httpc_405/0,
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
  command_async/4,
  command_async_twice/4
]).

setup_app() ->
  {ok, _} = application:ensure_all_started(consulerl),
  ok.

setup_httpc_200(Response) ->
  ok = meck:expect(hackney, request, fun(_, _, _, _, _) ->
    Ref = make_ref(),
    ok = exec(self(), Ref, 200, test, <<"application/json">>, Response),
    {ok, Ref}
  end).

setup_httpc_200(Response, Timeout) ->
  ok = meck:expect(hackney, request, fun(_, _, _, _, _) ->
    Ref = make_ref(),
    Self = self(),
    timer:apply_after(Timeout, ?MODULE, exec, [Self, Ref, 200, test, <<"application/json">>, Response]),
    {ok, Ref}
  end).

setup_httpc_200_plain(Response) ->
  ok = meck:expect(hackney, request, fun(_, _, _, _, _) ->
    Ref = make_ref(),
    ok = exec(self(), Ref, 200, test, <<"text/plain; charset=utf-8">>, Response),
    {ok, Ref}
  end).

setup_httpc_404() ->
  ok = meck:expect(hackney, request, fun(_, _, _, _, _) ->
    Ref = make_ref(),
    ok = exec(self(), Ref, 404, "Not Found", <<"text/plain; charset=utf-8">>, <<>>),
    {ok, Ref}
  end).

setup_httpc_405() ->
  ok = meck:expect(hackney, request, fun(_, _, _, _, _) ->
    Ref = make_ref(),
    ok = exec(self(), Ref, 405, "Method Not Allowed", <<"text/plain; charset=utf-8">>, <<>>),
    {ok, Ref}
  end).

setup_httpc_409(Error) ->
  ok = meck:expect(hackney, request, fun(_, _, _, _, _) ->
    Ref = make_ref(),
    ok = exec(self(), Ref, 409, "Conflict", <<"application/json">>, Error),
    {ok, Ref}
  end).

setup_httpc_timeout() ->
  ok = meck:expect(hackney, request, fun(_, _, _, _, _) ->
    Ref = make_ref(),
    {ok, Ref}
  end).

setup_error() ->
  ok = meck:expect(hackney, request, fun(_, _, _, _, _) ->
    {error, error}
  end).

stop_app() ->
  application:stop(consulerl).

stop_httpc() ->
  ok = meck:unload(hackney).

stop(_) ->
  ok = stop_httpc(),
  ok = stop_app().

command(Module, Method, Args, ExpectedResponse) ->
  Response = apply(Module, Method, Args),
  ?_assertEqual(ExpectedResponse, Response).

command_async(Module, Method, Args, ExpectedResponse) ->
  Pid = apply(Module, Method, Args),
  Response = consulerl_util:receive_response(?EVENT_RESPONSE),
  ok = consulerl_api:terminate(Pid),
  ok = consulerl_api:ensure_stopped(Pid),
  ?_assertEqual(ExpectedResponse, Response).

command_async_twice(Module, Method, Args, ExpectedResponse) ->
  Pid = apply(Module, Method, Args),
  Response = consulerl_util:receive_response(?EVENT_RESPONSE),

  ok = apply(Module, Method, [Pid | Args]),
  Response2 = consulerl_util:receive_response(?EVENT_RESPONSE),
  ok = consulerl_api:terminate(Pid),
  [
    ?_assertEqual(ExpectedResponse, Response),
    ?_assertEqual(ExpectedResponse, Response2)
  ].

exec(Dest, Ref, Status, Reason, Type, Response) ->
  Dest ! {hackney_response, Ref, {status, Status, Reason}},
  Dest ! {hackney_response, Ref, {headers, [{<<"Content-Type">>, Type}]}},
  Dest ! {hackney_response, Ref, Response},
  Dest ! {hackney_response, Ref, done},
  ok.