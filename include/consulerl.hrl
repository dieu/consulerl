-define(API_VERSION, "v1").
-define(MIME_FORM, "application/x-www-form-urlencoded; charset=UTF-8").
-define(CONTENT_FORM, "application/x-www-form-urlencoded; charset=UTF-8").
-define(CONTENT_JSON, "application/json; charset=UTF-8").
-define(SCHEME, "http").

-record(state, {host, port, acl}).
-define(EVENT_RESPONSE, consulerl_response).

-type verb() ::
set |
cas |
lock |
unlock |
get |
get_tree |
check_index |
check_session |
delete |
delete_tree |
delete_case.

-type ok() :: {ok, term()}.
-type error() :: {error, term()} | {error, term(), term()}.
-type return() :: ok() | error().

-type ref() :: pid() | fun((term()) -> ok).

-export_type([
  ok/0,
  error/0,
  return/0,
  ref/0,
  verb/0
]).


-ifdef(TEST).

-define(TIMEOUT, 10).

-define(setup(Start, Stop, F), {setup, Start, Stop, F}).

-define(GET_RESPONSE, <<"[{\"LockIndex\":0,\"Key\":\"test\",\"Flags\":0,\"Value\":\"czIz\",\"CreateIndex\":141,\"ModifyIndex\":142}]">>).
-define(GET_RESPONSE_JSON, [[
  {<<"LockIndex">>, 0},
  {<<"Key">>, <<"test">>},
  {<<"Flags">>, 0},
  {<<"Value">>, <<"czIz">>},
  {<<"CreateIndex">>, 141},
  {<<"ModifyIndex">>, 142}
]]).
-define(GET_RESPONSE_MAP, #{
  create_index => 141,
  flags => 0,
  key => "test",
  lock_index => 0,
  modify_index => 142,
  value => "s23"
}).
-define(PUT_RESPONSE, <<"true">>).
-define(PUT_RESPONSE_JSON, true).
-define(DELETE_RESPONSE, <<"true">>).
-define(DELETE_RESPONSE_JSON, true).
-define(KEYS_RESPONSE, <<"[\"test\"]">>).
-define(KEYS_RESPONSE_BIN, [<<"test">>]).
-define(TXN_RESPONSE, <<"{\"Results\":[{\"KV\":{\"LockIndex\":0,\"Key\":\"test\",\"Flags\":0,\"Value\":\"dGVzdA==\",\"CreateIndex\":161211,\"ModifyIndex\":161707}}],\"Errors\":null,\"Index\":0,\"LastContact\":0,\"KnownLeader\":true}">>).
-define(TXN_ERROR_RESPONSE, <<"{\"Results\":null,\"Errors\":[{\"OpIndex\":0,\"What\":\"key \\\"test\\\" doesn't exist\"}],\"Index\":0,\"LastContact\":0,\"KnownLeader\":true}">>).
-define(TXN_RESPONSE_MAP, #{
  errors => null,
  index => 0,
  know_leader => true,
  last_contact => 0,
  results => [#{create_index => 161211,
    flags => 0,
    key => "test",
    lock_index => 0,
    modify_index => 161707,
    value => "test"
  }]
}).
-define(TXN_ERROR_RESPONSE_MAP, #{
  errors => [#{
    op_index => 0,
    waht => <<"key \"test\" doesn't exist">>
  }],
  index => 0,
  know_leader => true,
  last_contact => 0,
  results => null
}).

-else.

-define(TIMEOUT, timer:seconds(5)).

-endif.
