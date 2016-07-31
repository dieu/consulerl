-module(consulerl_json).

%% API
-export([
  decode/2,
  encode/2
]).

-spec decode(atom(), iodata()) -> term().
decode(jiffy, IoData) ->
  Decoded = jiffy:decode(IoData, [return_maps]),
  keys_to_atom(Decoded).

-spec encode(atom(), term()) -> iodata().
encode(jiffy, Term) ->
  jiffy:encode(Term, []).

-spec keys_to_atom(term()) -> term().
keys_to_atom(Map) when is_map(Map) ->
  maps:from_list([{to_atom(K), keys_to_atom(V)} || {K, V} <- maps:to_list(Map)]);

keys_to_atom(List) when is_list(List) ->
  lists:map(fun keys_to_atom/1, List);

keys_to_atom(Value) ->
  Value.

-spec to_atom(term()) -> atom().
to_atom(Binary) when is_binary(Binary) ->
  list_to_atom(normalize(binary_to_list(Binary))).

-spec normalize(list()) -> list().
normalize(String) when is_list(String) ->
  {NewString, _} = lists:foldl(fun(Char, {Acc, Last}) ->
    case {is_capital(Last), is_capital(Char)} of
      {_, space} ->
        {Acc, Last};
      {none, true} ->
        {[Char + 32 | Acc], Char};
      {true, true} ->
        {[Char + 32 | Acc], Char};
      {false, true} ->
        {[Char + 32, $_ | Acc], Char};
      {_, false} ->
        {[Char | Acc], Char}
    end
  end, {[], none}, String),

  lists:reverse(NewString).

-spec is_capital(integer()) -> boolean() | space | none .
is_capital(C) when is_integer(C), $A =< C, C =< $Z ->
  true;
is_capital(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 ->
  true;
is_capital(C) when is_integer(C), 16#D8 =< C, C =< 16#DE ->
  true;
is_capital(C) when is_integer(C), $\ =:= C ->
  space;
is_capital(none) ->
  none;
is_capital(_) ->
  false.
