-module(consulerl_events).

-include("consulerl.hrl").

%% API
-export([
  fire/1,
  fire/2,
  fire/6
]).

-export([
  list/0,
  list/1
]).

-export([
  watch/1,
  watch/2,
  watch/3
]).

-spec fire(string()) -> return().
fire(Name) ->
  fire(Name, <<>>, none, none, none, none).

-spec fire(string(), binary()) -> return().
fire(Name, Event) ->
  fire(Name, Event, none, none, none, none).

-spec fire(string(), binary(), string() | none, string() | none, string() | none, string() | none) -> return().
fire(Name, Event, DC, Node, Service, Tag) ->
  QArgs = lists:filtermap(fun
    ({_, none}) -> false;
    ({Key, Value}) -> {true, {Key, Value}}
  end, [{dc, DC}, {node, Node}, {service, Service}, {tag, Tag}]),

  case consulerl_api:put([event, fire, Name], Event, QArgs) of
    {ok, #{payload := Payload} = Response} ->
      {ok, maps:update(payload, consulerl_util:base64decode(Payload), Response)};
    Error ->
      Error
  end.

-spec list() -> return().
list() ->
  list_response(consulerl_api:get([event, list])).

-spec list(string()) -> return().
list(Name) ->
  list_response(consulerl_api:get([event, list], [{name, Name}])).

-spec watch(ref()) -> ok.
watch(Callback) ->
  watch(none, Callback, infinity).

-spec watch(ref(), pos_integer() | infinity) -> ok.
watch(Callback, Retry) ->
  watch(none, Callback, Retry).

-spec watch(string() | none, ref(), pos_integer() | infinity) -> ok.
watch(Name, Callback, Retry) ->
  QArgs = name(Name),
  case watch_fun([event, list], QArgs, Callback, Retry) of
    ok ->
      ok;
    Watch ->
      consulerl_api:get(Watch, [event, list], QArgs, false)
  end.

-spec watch_fun(list(), list(), ref(), pos_integer() | infinity) -> fun((term()) -> ok) | ok.
watch_fun(Path, QArgs, Callback, Retry) when Retry > 0 orelse Retry =:= infinity ->
  fun
    ({ok, _, Header}) ->
      consulerl_api:get(self(), fun
        ({ok, _} = Response) ->
          consulerl_util:do(Callback, list_response(Response)),
          case watch_fun(Path, QArgs, Callback, consulerl_util:dec(Retry)) of
            ok ->
              ok;
            Watch ->
              consulerl_api:get(self(), Watch, Path, QArgs, false)
          end;
        (Error) ->
          consulerl_util:do(Callback, Error)
      end, Path, index(Header) ++ QArgs, true);
    (Error) ->
      consulerl_util:do(Callback, Error)
  end;

watch_fun(_, _, _, _) ->
  ok.

-spec list_response(return() | map()) -> return() | map().
list_response({ok, Responses}) when is_list(Responses) ->
  {ok, lists:map(fun list_response/1, Responses)};

list_response(#{payload := Payload} = Response) ->
  maps:update(payload, consulerl_util:base64decode(Payload), Response);

list_response(Response) ->
  Response.

-spec name(string() | none) -> list().
name(none) ->
  [];

name(Value) ->
  [{name, Value}].

-spec index(proplists:proplist()) -> list().
index(Proplist) when is_list(Proplist) ->
  case proplists:get_value(<<"X-Consul-Index">>, Proplist, none) of
    none -> [];
    Index when is_binary(Index) -> [{index, binary_to_integer(Index)}];
    Index when is_list(Index) -> [{index, list_to_integer(Index)}];
    Index when is_integer(Index) -> [{index, Index}];
    _ -> []
  end;

index(_) ->
  [].