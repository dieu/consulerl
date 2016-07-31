-define(setup(Start, Stop, F), {setup, Start, Stop, F}).

-define(GET_RESPONSE, <<"[{\"LockIndex\":0,\"Key\":\"test\",\"Flags\":0,\"Value\":\"czIz\",\"CreateIndex\":141,\"ModifyIndex\":142}]">>).
-define(GET_RESPONSE_JSON, [#{
  create_index => 141,
  flags => 0,
  key => <<"test">>,
  lock_index => 0,
  modify_index => 142,
  value => <<"czIz">>
}]).
-define(GET_RESPONSE_MAP, #{
  create_index => 141,
  flags => 0,
  key => <<"test">>,
  lock_index => 0,
  modify_index => 142,
  value => <<"czIz">>
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
  known_leader => true,
  last_contact => 0,
  results => [#{kv => #{
    create_index => 161211,
    flags => 0,
    key => <<"test">>,
    lock_index => 0,
    modify_index => 161707,
    value => <<"test">>
  }}]
}).
-define(TXN_ERROR_RESPONSE_MAP, #{
  errors => [#{
    op_index => 0,
    what => <<"key \"test\" doesn't exist">>
  }],
  index => 0,
  known_leader => true,
  last_contact => 0,
  results => null
}).
