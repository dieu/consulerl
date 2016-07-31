-module(consulerl_agent_tests).

-include("consulerl.hrl").
-include("consulerl_eunit.hrl").

-include_lib("eunit/include/eunit.hrl").

-import(consulerl_eunit, [
  command/4,
  command_async/4,
  command_async_twice/4
]).

setup_200(Response) -> fun() ->
    ok = consulerl_eunit:setup_app(),
    ok = consulerl_eunit:setup_httpc_200(Response)
  end.

setup_200_plain(Response) -> fun() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_200_plain(Response)
end.

setup_405() ->
  ok = consulerl_eunit:setup_app(),
  ok = consulerl_eunit:setup_httpc_405().

checks_test_() ->
  ?setup(setup_200(?CHECKS_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_agent, checks, [], {ok, ?CHECKS_RESPONSE_MAP})
  end).

checks_full_test_() ->
  ?setup(setup_200(?CHECKS_RESPONSE_FULL), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_agent, checks, [], {ok, ?CHECKS_RESPONSE_MAP_FULL})
  end).

services_test_() ->
  ?setup(setup_200(?SERVICES_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_agent, services, [], {ok, ?SERVICES_RESPONSE_MAP})
  end).

members_test_() ->
  ?setup(setup_200(?MEMBERS_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_agent, members, [], {ok, [?MEMBERS_RESPONSE_MAP]})
  end).

self_test_() ->
  ?setup(setup_200(?SELF_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_agent, self, [], {ok, ?SELF_RESPONSE_MAP})
  end).

maintenance_test_() ->
  ?setup(fun setup_405/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, maintenance, [true], {error, "Method Not Allowed"}),
      command(consulerl_agent, maintenance, [false], {error, "Method Not Allowed"}),
      command(consulerl_agent, maintenance, [true, none], {error, "Method Not Allowed"}),
      command(consulerl_agent, maintenance, [false, "reason"], {error, "Method Not Allowed"})
    ]
  } end).

join_test_() ->
  ?setup(setup_200_plain(?JOIN_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, join, ["127.0.0.1"], {ok, ?JOIN_RESPONSE_MAP}),
      command(consulerl_agent, join, ["127.0.0.1", 1], {ok, ?JOIN_RESPONSE_MAP}),
      command(consulerl_agent, join, ["127.0.0.1", 0], {ok, ?JOIN_RESPONSE_MAP}),
      command(consulerl_agent, join, ["127.0.0.1", none], {ok, ?JOIN_RESPONSE_MAP})
    ]
  } end).

force_leave_test_() ->
  ?setup(setup_200_plain(?FORCE_LEAVE_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, force_leave, ["127.0.0.1"], {ok, ?FORCE_LEAVE_RESPONSE_MAP})
    ]
  } end).

register_check_test_() ->
  ?setup(setup_200_plain(?REGISTER_CHECK_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_agent, register_check, ["google:test", "test", "test", "/bin/bash", "db8ae3e522df", "env", none, none, 15, 15, "redis", "pass"], {ok, ?REGISTER_DOCKER_CHECK_RESPONSE_MAP}),
    command(consulerl_agent, register_check, ["google:test", "test", "test", "/bin/bash", none, none, none, none, 15, 15, "redis", "pass"], {ok, ?REGISTER_SCRIPT_CHECK_RESPONSE_MAP}),
    command(consulerl_agent, register_check, ["google:test", "test", "test", none, none, none, "google.com", none, 15, 15, "redis", "pass"], {ok, ?REGISTER_HTTP_CHECK_RESPONSE_MAP}),
    command(consulerl_agent, register_check, ["google:test", "test", "test", none, none, none, none, "google.com", 15, 15, "redis", "pass"], {ok, ?REGISTER_TCP_CHECK_RESPONSE_MAP})
  end).

register_script_check_test_() ->
  ?setup(setup_200_plain(?REGISTER_SCRIPT_CHECK_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, register_script_check, ["google:test", "test", "test", "/bin/bash", 15, 15, "redis", "pass"], {ok, ?REGISTER_SCRIPT_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_script_check, ["google:test", "test", "test", "/bin/bash", 15, 15, "redis"], {ok, ?REGISTER_SCRIPT_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_script_check, ["google:test", "test", "test", "/bin/bash", 15, 15], {ok, ?REGISTER_SCRIPT_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_script_check, ["google:test", "test", "/bin/bash", 15, 15], {ok, ?REGISTER_SCRIPT_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_script_check, ["google:test", "test", "/bin/bash", 15], {ok, ?REGISTER_SCRIPT_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_script_check, ["test", "/bin/bash", 15], {ok, ?REGISTER_SCRIPT_CHECK_RESPONSE_MAP})
    ]
  } end).

register_docker_check_test_() ->
  ?setup(setup_200_plain(?REGISTER_DOCKER_CHECK_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, register_docker_check, ["google:test", "test", "test", "db8ae3e522df", "env", "/bin/bash", 15, 15, "redis", "pass"], {ok, ?REGISTER_DOCKER_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_docker_check, ["google:test", "test", "test", "db8ae3e522df", "env", "/bin/bash", 15, 15, "redis"], {ok, ?REGISTER_DOCKER_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_docker_check, ["google:test", "test", "test", "db8ae3e522df", "env", "/bin/bash", 15, 15], {ok, ?REGISTER_DOCKER_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_docker_check, ["google:test", "test", "db8ae3e522df", "env", "/bin/bash", 15, 15], {ok, ?REGISTER_DOCKER_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_docker_check, ["google:test", "test", "db8ae3e522df", "env", "/bin/bash", 15], {ok, ?REGISTER_DOCKER_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_docker_check, ["test", "db8ae3e522df", "env", "/bin/bash", 15], {ok, ?REGISTER_DOCKER_CHECK_RESPONSE_MAP})
    ]
  } end).

register_http_check_test_() ->
  ?setup(setup_200_plain(?REGISTER_HTTP_CHECK_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, register_http_check, ["google:test", "test", "test", "http://google.com", 15, 15, "redis", "pass"], {ok, ?REGISTER_HTTP_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_http_check, ["google:test", "test", "test", "http://google.com", 15, 15, "redis"], {ok, ?REGISTER_HTTP_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_http_check, ["google:test", "test", "test", "http://google.com", 15, 15], {ok, ?REGISTER_HTTP_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_http_check, ["google:test", "test", "http://google.com", 15, 15], {ok, ?REGISTER_HTTP_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_http_check, ["google:test", "test", "http://google.com", 15], {ok, ?REGISTER_HTTP_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_http_check, ["test", "http://google.com", 15], {ok, ?REGISTER_HTTP_CHECK_RESPONSE_MAP})
    ]
  } end).

register_tcp_check_test_() ->
  ?setup(setup_200_plain(?REGISTER_TCP_CHECK_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, register_tcp_check, ["google:test", "test", "test", "google.com", 15, 15, "redis", "pass"], {ok, ?REGISTER_TCP_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_tcp_check, ["google:test", "test", "test", "google.com", 15, 15, "redis"], {ok, ?REGISTER_TCP_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_tcp_check, ["google:test", "test", "test", "google.com", 15, 15], {ok, ?REGISTER_TCP_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_tcp_check, ["google:test", "test", "google.com", 15, 15], {ok, ?REGISTER_TCP_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_tcp_check, ["google:test", "test", "google.com", 15], {ok, ?REGISTER_TCP_CHECK_RESPONSE_MAP}),
      command(consulerl_agent, register_tcp_check, ["test", "google.com", 15], {ok, ?REGISTER_TCP_CHECK_RESPONSE_MAP})
    ]
  } end).

deregister_check_test_() ->
  ?setup(setup_200_plain(?DEREGISTER_CHECK_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_agent, deregister_check, ["google:test"], {ok, ?DEREGISTER_CHECK_RESPONSE})
  end).

pass_test_() ->
  ?setup(setup_200_plain(?PASS_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, pass, ["google:test"], {ok, ?PASS_RESPONSE_MAP}),
      command(consulerl_agent, pass, ["google:test", "reason"], {ok, ?PASS_RESPONSE_MAP})
    ]
  } end).

warn_test_() ->
  ?setup(setup_200_plain(?WARN_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, warn, ["google:test"], {ok, ?WARN_RESPONSE_MAP}),
      command(consulerl_agent, warn, ["google:test", "reason"], {ok, ?WARN_RESPONSE_MAP})
    ]
  } end).

fail_test_() ->
  ?setup(setup_200_plain(?FAIL_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, fail, ["google:test"], {ok, ?FAIL_RESPONSE_MAP}),
      command(consulerl_agent, fail, ["google:test", "reason"], {ok, ?FAIL_RESPONSE_MAP})
    ]
  } end).

update_test_() ->
  ?setup(setup_200_plain(?UPDATE_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, update, ["google:test", passing, "reson"], {ok, ?UPDATE_RESPONSE_MAP}),
      command(consulerl_agent, update, ["google:test", warning, "reson"], {ok, ?UPDATE_RESPONSE_MAP}),
      command(consulerl_agent, update, ["google:test", critical, "reson"], {ok, ?UPDATE_RESPONSE_MAP})
    ]
  } end).

register_service_test_() ->
  ?setup(setup_200_plain(?REGISTER_SERVICE_RESPONSE), fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80, consulerl_agent:http_check("google.com", 15, 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80, consulerl_agent:tcp_check("google.com", 15, 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80, consulerl_agent:script_check("/bin/shell", 15, 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80, consulerl_agent:http_check("google.com", 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80, consulerl_agent:tcp_check("google.com", 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80, consulerl_agent:script_check("/bin/shell", 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),


      command(consulerl_agent, register_service, ["google:test", "test", "example.com", 80, consulerl_agent:http_check("google.com", 15, 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", "example.com", 80, consulerl_agent:tcp_check("google.com", 15, 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", "example.com", 80, consulerl_agent:script_check("/bin/shell", 15, 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", "example.com", 80, consulerl_agent:http_check("google.com", 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", "example.com", 80, consulerl_agent:tcp_check("google.com", 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", "example.com", 80, consulerl_agent:script_check("/bin/shell", 15)], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),

      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com", 80], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),

      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"], "example.com"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),

      command(consulerl_agent, register_service, ["google:test", "test", ["tag"]], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"]], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"]], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"]], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"]], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test", ["tag"]], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),

      command(consulerl_agent, register_service, ["google:test", "test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test", "test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),

      command(consulerl_agent, register_service, ["google:test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP}),
      command(consulerl_agent, register_service, ["google:test"], {ok, ?REGISTER_SERVICE_RESPONSE_MAP})
    ]
  } end).

deregister_service_test_() ->
  ?setup(setup_200_plain(?DEREGISTER_SERVICE_RESPONSE), fun consulerl_eunit:stop/1, fun(_) ->
    command(consulerl_agent, deregister_service, ["google:test"], {ok, ?DEREGISTER_SERVICE_RESPONSE_MAP})
  end).

maintenance_service_test_() ->
  ?setup(fun setup_405/0, fun consulerl_eunit:stop/1, fun(_) -> {
    inparallel, [
      command(consulerl_agent, maintenance_service, ["google:test"], {error, "Method Not Allowed"}),
      command(consulerl_agent, maintenance_service, ["google:test", true], {error, "Method Not Allowed"}),
      command(consulerl_agent, maintenance_service, ["google:test", false], {error, "Method Not Allowed"}),
      command(consulerl_agent, maintenance_service, ["google:test", true, none], {error, "Method Not Allowed"}),
      command(consulerl_agent, maintenance_service, ["google:test", true, "reason"], {error, "Method Not Allowed"})
    ]
  } end).