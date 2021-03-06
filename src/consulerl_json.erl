-module(consulerl_json).

%% API
-export([
  decode/2,
  encode/2
]).

-spec decode(atom(), iodata()) -> term().
decode(jiffy, IoData) ->
  Decoded = jiffy:decode(IoData, [return_maps]),
  consulerl_atom:keys_to_atom(Decoded).

-spec encode(atom(), term()) -> iodata().
encode(jiffy, Term) ->
  jiffy:encode(Term, []).