-define(API_VERSION, "v1").
-define(MIME_FORM, "application/x-www-form-urlencoded; charset=UTF-8").
-define(CONTENT_FORM, "application/x-www-form-urlencoded; charset=UTF-8").
-define(CONTENT_JSON, "application/json; charset=UTF-8").
-define(SCHEME, "http").
-define(JSON, jiffy).

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

-else.

-define(TIMEOUT, timer:seconds(5)).

-endif.
