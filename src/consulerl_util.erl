-module(consulerl_util).

-include("consulerl.hrl").

%% API
-export([
  do/2,
  receive_response/1,
  to_interval/1,
  to_interval/2,
  to_binary/1,
  list_of_strings_to_binary/1,
  base64encode/1,
  base64decode/1
]).

-export([
  build_url/4,
  dec/1
]).

-spec do(ref(), return()) -> ok.
do({Pid, _} = From, Response) when is_pid(Pid) ->
  gen_server:reply(From, Response),
  ok;

do(Pid, Response) when is_pid(Pid) ->
  Pid ! {?EVENT_RESPONSE, Response},
  ok;

do(Fun, Response) when is_function(Fun) ->
  Fun(Response).

-spec receive_response(reference()) -> return().
receive_response(Ref) ->
  receive
    {Ref, Response} ->
      Response
  after
    ?TIMEOUT ->
      ok = lager:debug("Don't received response; timeout=~p", [?TIMEOUT]),
      {error, timeout}
  end.

-spec build_url(string(), pos_integer(), list(), list()) -> string().

build_url([$h, $t, $t, $p, $:, $/, $/ | Host], Port, Paths, QArgs) ->
  build_url(Host, Port, Paths, QArgs);
build_url([$h, $t, $t, $p, $s, $:, $/, $/ | Host], Port, Paths, QArgs) ->
  build_url(Host, Port, Paths, QArgs);

build_url(Host, Port, Paths, QArgs) ->
  lists:flatten(io_lib:format("https://~s:~p/~s/~s", [Host, Port, ?API_VERSION, build_full_path(Paths, QArgs)])).

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
  build_path(T, [H | Acc]);

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
  io_lib:format("~s=", [Key]);

to_string(Key, Value) when is_integer(Value) ->
  io_lib:format("~s=~p", [Key, Value]);

to_string(Key, Value) ->
  io_lib:format("~s=~s", [Key, Value]).

-spec to_interval(binary() | string() | pos_integer() | term()) -> binary() | none.
to_interval(Binary) when is_binary(Binary) ->
  Binary;

to_interval(List) when is_list(List) ->
  to_binary(List);

to_interval(Int) when is_integer(Int) ->
  to_interval(Int, "s");

to_interval(_) ->
  none.

-spec to_interval(pos_integer(), string()) -> binary().
to_interval(Int, Metric) when is_integer(Int) andalso is_list(Metric) ->
  <<(list_to_binary(integer_to_list(Int)))/binary, (list_to_binary(Metric))/binary>>.

-spec to_binary(term()) -> binary().
to_binary(Value) when is_binary(Value) ->
  Value;

to_binary(Value) when is_atom(Value) ->
  to_binary(atom_to_list(Value));

to_binary(Value) when is_list(Value) ->
  list_to_binary(Value);

to_binary(Value) ->
  term_to_binary(Value).

-spec list_of_strings_to_binary(list() | none) -> list() | none.
list_of_strings_to_binary(List) when is_list(List) ->
  lists:map(fun to_binary/1, List);

list_of_strings_to_binary(none) ->
  none.

-spec base64encode(term()) -> string().
base64encode(Value) when is_binary(Value) ->
  base64:encode_to_string(Value);

base64encode(Value) when is_list(Value) ->
  base64:encode_to_string(Value);

base64encode(none) ->
  "";

base64encode(Value) ->
  base64:encode_to_string(term_to_binary(Value)).

-spec base64decode(binary()) -> binary().
base64decode(Value) when is_binary(Value) ->
  list_to_binary(base64:decode_to_string(Value));

base64decode(null) ->
  null.

-spec dec(integer() | infinity) -> integer() | infinity.
dec(Retry) when is_integer(Retry) ->
  Retry - 1;

dec(infinity) ->
  infinity.