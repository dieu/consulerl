-module(consulerl_util).

-include("consulerl.hrl").

%% API
-export([
  do/2,
  receive_response/0,
  to_json/1,
  to_binary/1,
  base64encode/1
]).

-export([
  build_url/4,
  response/1
]).

-spec do(ref(), return()) -> ok.
do(Pid, Response) when is_pid(Pid) ->
  Pid ! {?EVENT_RESPONSE, Response},
  ok;

do(Fun, Response) when is_function(Fun) ->
  Fun(Response).


-spec receive_response() -> return().
receive_response() ->
  receive
    {?EVENT_RESPONSE, Response} ->
      Response
  after
    ?TIMEOUT ->
      {error, timeout}
  end.

-spec build_url(string(), pos_integer(), list(), list()) -> string().
build_url([$h, $t, $t, $p, $:, $/, $/ | Host], Port, Paths, QArgs) ->
  build_url(Host, Port, Paths, QArgs);

build_url(Host, Port, Paths, QArgs) ->
  lists:flatten(io_lib:format("http://~s:~p/~s/~s", [Host, Port, ?API_VERSION, build_full_path(Paths, QArgs)])).

-spec build_full_path(list(), list()) -> string().
build_full_path(Paths, QArgs) ->
  lists:flatten(io_lib:format("~s?~s", [build_path(Paths), build_args(QArgs)])).

-spec build_path(list()) -> string().
build_path(Paths) ->
  build_path(Paths, []).

-spec build_path(list(), list()) -> string().
build_path([H | T], Acc) when is_atom(H) ->
  build_path(T, [atom_to_list(H) | Acc]);

build_path([H | T], Acc) ->
  build_path(T, [H, Acc]);

build_path([], Acc) ->
  string:join(lists:reverse(Acc), "/").

-spec build_args(list()) -> string().
build_args(QArgs) ->
  build_args(QArgs, []).

-spec build_args(list(), list()) -> string().
build_args([{Key, Value} | T], Acc) ->
  build_args(T, [to_string(Key, Value) | Acc]);

build_args([Key | T], Acc) when is_atom(Key) ->
  build_args(T, [atom_to_list(Key) | Acc]);

build_args([Ignore | T], Acc) -> %% ignore what can't covert
  ok = lager:warning("Can't build argument to url; argument=~p", [Ignore]),
  build_args(T, Acc);

build_args([], Acc) ->
  string:join(lists:reverse(Acc), "&").

-spec to_string(term(), list()) -> string().
to_string(Key, []) ->
  io_lib:format("~p=", [Key]);

to_string(Key, Value) ->
  io_lib:format("~p=~p", [Key, Value]).

-spec response(tuple()) -> {ref(), ok()} | error().
response({Ref, {{_Vsn, 200, _Reason}, Headers, Body}}) ->
  ContentType = proplists:get_value("content-type", Headers),
  case ContentType of
    "application/json" -> {Ref, {ok, jsx:decode(Body)}};
    _ -> {Ref, Body}
  end;

response({ok, {{_Vsn, _, Reason}, Headers, Body}}) ->
  ContentType = proplists:get_value("content-type", Headers),
  case ContentType of
    "application/json" -> {error, Reason, jsx:decode(Body)};
    _ -> {error, Reason, Body}
  end;

response({Ref, {{_Vsn, _, Reason}, Headers, Body}}) ->
  ContentType = proplists:get_value("content-type", Headers),
  case ContentType of
    "application/json" -> {Ref, {error, Reason, jsx:decode(Body)}};
    _ -> {Ref, {error, Reason, Body}}
  end;

response({Ref, {error, Reason}}) -> {Ref, {error, Reason}};

response({error, Reason}) -> {error, Reason}.

-spec to_json(term()) -> string().
to_json([C | _] = String) when 32 =< C andalso C < 127 ->
  "\"" ++ String ++ "\"";

to_json(List) when is_list(List) ->
  "[" ++ string:join(
    lists:map(fun to_json/1, List),
    ","
  )++ "]";

to_json(Map) when is_map(Map) ->
  "{" ++ string:join(
    maps:fold(fun(K, V, Acc) ->
      [to_json(K) ++ ":" ++ to_json(V) | Acc]
    end, [], Map),
    ","
  ) ++ "}";

to_json(Value) when is_atom(Value) ->
  to_json(atom_to_list(Value));

to_json(Value) when is_integer(Value) ->
  integer_to_list(Value);

to_json(Value) ->
  Value.

-spec to_binary(term()) -> binary().
to_binary(Value) when is_binary(Value) ->
  Value;

to_binary(Value) when is_atom(Value) ->
  to_binary(atom_to_list(Value));

to_binary(Value) when is_list(Value) ->
  list_to_binary(Value);

to_binary(Value) ->
  to_binary(base64encode(Value)).

-spec base64encode(term()) -> string().
base64encode(Value) when is_binary(Value) ->
  base64:encode_to_string(Value);

base64encode(Value) when is_list(Value) ->
  base64:encode_to_string(Value);

base64encode(Value) ->
  base64:encode_to_string(term_to_binary(Value)).