-module(consulerl_agent).

-include("consulerl.hrl").

%% API
-export([
  checks/0,
  services/0,
  members/0,
  self/0,
  maintenance/1,
  maintenance/2,
  join/1,
  join/2,
  force_leave/1
]).

-export([
  register_check/12,
  register_script_check/8,
  register_script_check/7,
  register_script_check/6,
  register_script_check/5,
  register_script_check/4,
  register_script_check/3,
  register_docker_check/10,
  register_docker_check/9,
  register_docker_check/8,
  register_docker_check/7,
  register_docker_check/6,
  register_docker_check/5,
  register_http_check/8,
  register_http_check/7,
  register_http_check/6,
  register_http_check/5,
  register_http_check/4,
  register_http_check/3,
  register_tcp_check/8,
  register_tcp_check/7,
  register_tcp_check/6,
  register_tcp_check/5,
  register_tcp_check/4,
  register_tcp_check/3
]).

-export([
  deregister_check/1
]).

-export([
  pass/1,
  pass/2,
  warn/1,
  warn/2,
  fail/1,
  fail/2,
  update/3
]).

-export([
  register_service/6,
  register_service/5,
  register_service/4,
  register_service/3,
  register_service/2,
  register_service/1
]).

-export([
  script_check/3,
  script_check/2,
  http_check/3,
  http_check/2,
  tcp_check/3,
  tcp_check/2
]).

-export([
  deregister_service/1
]).

-export([
  maintenance_service/3,
  maintenance_service/2,
  maintenance_service/1
]).

-define(GET(PATH), consulerl_api:get([agent, PATH])).

-spec checks() -> return().
checks() ->
  ?GET(checks).

-spec services() -> return().
services() ->
  ?GET(services).

-spec members() -> return().
members() ->
  ?GET(members).

-spec self() -> return().
self() ->
  ?GET(self).

-spec maintenance(boolean()) -> return().
maintenance(Enable) ->
  maintenance(Enable, none).

-spec maintenance(boolean(), none | string()) -> return().
maintenance(Enable, none) ->
  consulerl_api:get([agent, maintenance], [{enable, Enable}]);

maintenance(Enable, Reason) ->
  consulerl_api:get([agent, maintenance], [{enable, Enable}, {reason, Reason}]).

-spec join(string()) -> return().
join(Address) ->
  join(Address, none).

-spec join(string(), 1 | none | term()) -> return().
join(Address, 1) ->
  consulerl_api:get([agent, join, Address], [{wan, 1}]);

join(Address, _) ->
  consulerl_api:get([agent, join, Address]).

-spec force_leave(string()) -> return().
force_leave(Node) ->
  consulerl_api:get([agent, "force-leave", Node]).

-spec register_script_check(string() | none, string(), string() | none, string() | none, pos_integer() | none, pos_integer() | none, string() | none, string() | none) -> return().
register_script_check(Id, Name, Notes, Script, Interval, TTL, ServiceId, Status) ->
  register_check(Id, Name, Notes, Script, none, none, none, none, Interval, TTL, ServiceId, Status).

-spec register_script_check(string(), string(), string(), string(), pos_integer(), pos_integer(), string()) -> return().
register_script_check(Id, Name, Notes, Script, Interval, TTL, ServiceId) ->
  register_script_check(Id, Name, Notes, Script, Interval, TTL, ServiceId, none).

-spec register_script_check(string(), string(), string(), string(), pos_integer(), pos_integer()) -> return().
register_script_check(Id, Name, Notes, Script, Interval, TTL) ->
  register_script_check(Id, Name, Notes, Script, Interval, TTL, none, none).

-spec register_script_check(string(), string(), string(), pos_integer(), pos_integer()) -> return().
register_script_check(Id, Name, Script, Interval, TTL) ->
  register_script_check(Id, Name, none, Script, Interval, TTL, none, none).

-spec register_script_check(string(), string(), string(), pos_integer()) -> return().
register_script_check(Id, Name, Script, Interval) ->
  register_script_check(Id, Name, none, Script, Interval, none, none, none).

-spec register_script_check(string(), string(), pos_integer()) -> return().
register_script_check(Name, Script, Interval) ->
  register_script_check(Name, Name, none, Script, Interval, none, none, none).

-spec register_docker_check(string() | none, string(), string() | none, string() | none, string() | none, string() | none, pos_integer() | none, pos_integer() | none, string() | none, string() | none) -> return().
register_docker_check(Id, Name, Notes, DockerContainerId, Script, Shell, Interval, TTL, ServiceId, Status) ->
  register_check(Id, Name, Notes, Script, DockerContainerId, Shell, none, none, Interval, TTL, ServiceId, Status).

-spec register_docker_check(string(), string(), string(), string(), string(), string(), pos_integer(), pos_integer(), string()) -> return().
register_docker_check(Id, Name, Notes, DockerContainerId, Script, Shell, Interval, TTL, ServiceId) ->
  register_docker_check(Id, Name, Notes, DockerContainerId, Script, Shell, Interval, TTL, ServiceId, none).

-spec register_docker_check(string(), string(), string(), string(), string(), string(), pos_integer(), pos_integer()) -> return().
register_docker_check(Id, Name, Notes, DockerContainerId, Script, Shell, Interval, TTL) ->
  register_docker_check(Id, Name, Notes, DockerContainerId, Script, Shell, Interval, TTL, none, none).

-spec register_docker_check(string(), string(), string(), string(), string(), pos_integer(), pos_integer()) -> return().
register_docker_check(Id, Name, DockerContainerId, Script, Shell, Interval, TTL) ->
  register_docker_check(Id, Name, none, DockerContainerId, Script, Shell, Interval, TTL, none, none).

-spec register_docker_check(string(), string(), string(), string(), string(), pos_integer()) -> return().
register_docker_check(Id, Name, DockerContainerId, Script, Shell, Interval) ->
  register_docker_check(Id, Name, none, DockerContainerId, Script, Shell, Interval, none, none, none).

-spec register_docker_check(string(), string(), string(), string(), pos_integer()) -> return().
register_docker_check(Name, DockerContainerId, Script, Shell, Interval) ->
  register_docker_check(Name, Name, none, DockerContainerId, Script, Shell, Interval, none, none, none).

-spec register_http_check(string() | none, string(), string() | none, string() | none, pos_integer() | none, pos_integer() | none, string() | none, string() | none) -> return().
register_http_check(Id, Name, Notes, HTTP, Interval, TTL, ServiceId, Status) ->
  register_check(Id, Name, Notes, none, none, none, HTTP, none, Interval, TTL, ServiceId, Status).

-spec register_http_check(string(), string(), string(), string(), pos_integer(), pos_integer(), string()) -> return().
register_http_check(Id, Name, Notes, HTTP, Interval, TTL, ServiceId) ->
  register_http_check(Id, Name, Notes, HTTP, Interval, TTL, ServiceId, none).

-spec register_http_check(string(), string(), string(), string(), pos_integer(), pos_integer()) -> return().
register_http_check(Id, Name, Notes, HTTP, Interval, TTL) ->
  register_http_check(Id, Name, Notes, HTTP, Interval, TTL, none, none).

-spec register_http_check(string(), string(), string(), pos_integer(), pos_integer()) -> return().
register_http_check(Id, Name, HTTP, Interval, TTL) ->
  register_http_check(Id, Name, none, HTTP, Interval, TTL, none, none).

-spec register_http_check(string(), string(), string(), pos_integer()) -> return().
register_http_check(Id, Name, HTTP, Interval) ->
  register_http_check(Id, Name, none, HTTP, Interval, none, none, none).

-spec register_http_check(string(), string(), pos_integer()) -> return().
register_http_check(Name, HTTP, Interval) ->
  register_http_check(Name, Name, none, HTTP, Interval, none, none, none).

-spec register_tcp_check(string(), string(), string() | none, string() | none, pos_integer() | none, pos_integer() | none, string() | none, string() | none) -> return().
register_tcp_check(Id, Name, Notes, TCP, Interval, TTL, ServiceId, Status) ->
  register_check(Id, Name, Notes, none, none, none, none, TCP, Interval, TTL, ServiceId, Status).

-spec register_tcp_check(string(), string(), string(), string(), pos_integer(), pos_integer(), string()) -> return().
register_tcp_check(Id, Name, Notes, TCP, Interval, TTL, ServiceId) ->
  register_tcp_check(Id, Name, Notes, TCP, Interval, TTL, ServiceId, none).

-spec register_tcp_check(string(), string(), string(), string(), pos_integer(), pos_integer()) -> return().
register_tcp_check(Id, Name, Notes, TCP, Interval, TTL) ->
  register_tcp_check(Id, Name, Notes, TCP, Interval, TTL, none, none).

-spec register_tcp_check(string(), string(), string(), pos_integer(), pos_integer()) -> return().
register_tcp_check(Id, Name, TCP, Interval, TTL) ->
  register_tcp_check(Id, Name, none, TCP, Interval, TTL, none, none).

-spec register_tcp_check(string(), string(), string(), pos_integer()) -> return().
register_tcp_check(Id, Name, TCP, Interval) ->
  register_tcp_check(Id, Name, none, TCP, Interval, none, none, none).

-spec register_tcp_check(string(), string(), pos_integer()) -> return().
register_tcp_check(Name, TCP, Interval) ->
  register_tcp_check(Name, Name, none, TCP, Interval, none, none, none).

-spec register_check(string(), string(), string() | none, string() | none, string() | none, string() | none, string() | none, string() | none, pos_integer() | none, pos_integer() | none, string() | none, string() | none) -> return().
register_check(Id, Name, Notes, Script, DockerContainerId, Shell, HTTP, TCP, Interval, TTL, ServiceId, Status) ->
  Value = remove_none(#{
    <<"ID">> => consulerl_util:to_binary(Id),
    <<"Name">> => consulerl_util:to_binary(Name),
    <<"Notes">> => consulerl_util:to_binary(Notes),
    <<"Script">> => consulerl_util:to_binary(Script),
    <<"DockerContainerID">> => consulerl_util:to_binary(DockerContainerId),
    <<"Shell">> => consulerl_util:to_binary(Shell),
    <<"HTTP">> => consulerl_util:to_binary(HTTP),
    <<"TCP">> => consulerl_util:to_binary(TCP),
    <<"Interval">> => consulerl_util:to_interval(Interval),
    <<"TTL">> => consulerl_util:to_interval(TTL),
    <<"ServiceID">> => consulerl_util:to_binary(ServiceId),
    <<"Status">> => consulerl_util:to_binary(Status)
  }),
  consulerl_api:put([agent, check, register], consulerl_json:encode(?JSON, Value)).

-spec deregister_check(string()) -> return().
deregister_check(Id) ->
  consulerl_api:get([agent, check, deregister, Id]).

-spec pass(string()) -> return().
pass(Id) ->
  pass(Id, none).

-spec pass(string(), string() | none) -> return().
pass(Id, none) ->
  consulerl_api:get([agent, check, pass, Id]);

pass(Id, Note) ->
  consulerl_api:get([agent, check, pass, Id], [{note, Note}]).

-spec warn(string()) -> return().
warn(Id) ->
  warn(Id, none).

-spec warn(string(), string() | none) -> return().
warn(Id, none) ->
  consulerl_api:get([agent, check, warn, Id]);

warn(Id, Note) ->
  consulerl_api:get([agent, check, warn, Id], [{note, Note}]).

-spec fail(string()) -> return().
fail(Id) ->
  fail(Id, none).

-spec fail(string(), string() | none) -> return().
fail(Id, none) ->
  consulerl_api:get([agent, check, fail, Id]);

fail(Id, Note) ->
  consulerl_api:get([agent, check, fail, Id], [{note, Note}]).

-spec update(string(), passing | warning | critical, string()) -> return().
update(Id, Status, Output) when Status =:= passing orelse Status =:= warning orelse Status =:= critical ->
  consulerl_api:put([agent, check, update, Id], consulerl_json:encode(?JSON, remove_none(#{
    <<"Status">> => Status,
    <<"Output">> => consulerl_util:to_binary(Output)
  }))).

-spec register_service(string(), string(), list() | none, string() | none, pos_integer() | none, map() | none) -> return().
register_service(Id, Name, Tags, Address, Port, Check) ->
  consulerl_api:put([agent, service, register], consulerl_json:encode(?JSON, remove_none(#{
    <<"ID">> => consulerl_util:to_binary(Id),
    <<"Name">> => consulerl_util:to_binary(Name),
    <<"Tags">> => consulerl_util:list_of_strings_to_binary(Tags),
    <<"Address">> => consulerl_util:to_binary(Address),
    <<"Port">> => Port,
    <<"Check">> => Check
  }))).

-spec register_service(string(), string(), string() | list(), pos_integer() | string(), map() | pos_integer()) -> return().
register_service(Id, Name, Address, Port, Check) when is_map(Check) ->
  register_service(Id, Name, none, Address, Port, Check);
register_service(Id, Name, Tags, Address, Port) ->
  register_service(Id, Name, Tags, Address, Port, none).

-spec register_service(string(), string(), list(), string()) -> return().
register_service(Id, Name, Tags, Address) ->
  register_service(Id, Name, Tags, Address, none, none).

-spec register_service(string(), string(), list()) -> return().
register_service(Id, Name, Tags) ->
  register_service(Id, Name, Tags, none, none, none).

-spec register_service(string(), string()) -> return().
register_service(Id, Name) ->
  register_service(Id, Name, none, none, none, none).

-spec register_service(string()) -> return().
register_service(Name) ->
  register_service(Name, Name, none, none, none, none).

-spec script_check(string(), pos_integer(), pos_integer()) -> map().
script_check(Script, Interval, TTL) -> #{
  <<"Script">> => consulerl_util:to_binary(Script),
  <<"Interval">> => consulerl_util:to_interval(Interval),
  <<"TTL">> => consulerl_util:to_interval(TTL)
}.

-spec script_check(string(), pos_integer()) -> map().
script_check(Script, Interval) -> #{
  <<"Script">> => consulerl_util:to_binary(Script),
  <<"Interval">> => consulerl_util:to_interval(Interval)
}.

-spec http_check(string(), pos_integer(), pos_integer()) -> map().
http_check(HTTP, Interval, TTL) -> #{
  <<"HTTP">> => consulerl_util:to_binary(HTTP),
  <<"Interval">> => consulerl_util:to_interval(Interval),
  <<"TTL">> => consulerl_util:to_interval(TTL)
}.

-spec http_check(string(), pos_integer()) -> map().
http_check(HTTP, Interval) -> #{
  <<"HTTP">> => consulerl_util:to_binary(HTTP),
  <<"Interval">> => consulerl_util:to_interval(Interval)
}.

-spec tcp_check(string(), pos_integer(), pos_integer()) -> map().
tcp_check(TCP, Interval, TTL) -> #{
  <<"TCP">> => consulerl_util:to_binary(TCP),
  <<"Interval">> => consulerl_util:to_interval(Interval),
  <<"TTL">> => consulerl_util:to_interval(TTL)
}.

-spec tcp_check(string(), pos_integer()) -> map().
tcp_check(TCP, Interval) -> #{
  <<"TCP">> => consulerl_util:to_binary(TCP),
  <<"Interval">> => consulerl_util:to_interval(Interval)
}.

-spec deregister_service(string()) -> return().
deregister_service(Id) ->
  consulerl_api:get([agent, service, deregister, Id]).

-spec maintenance_service(string()) -> return().
maintenance_service(Id) ->
  maintenance_service(Id, true, none).

-spec maintenance_service(string(), boolean()) -> return().
maintenance_service(Id, Enable) ->
  maintenance_service(Id, Enable, none).

-spec maintenance_service(string(), boolean(), none | string()) -> return().
maintenance_service(Id, Enable, none) ->
  consulerl_api:get([agent, service, maintenance, Id], [{enable, Enable}]);

maintenance_service(Id, Enable, Reason) ->
  consulerl_api:get([agent, service, maintenance, Id], [{enable, Enable}, {reason, Reason}]).

-spec remove_none(map()) -> map().
remove_none(Map) when is_map(Map) ->
  Predicate = fun
    (_, none) -> false;
    (_, <<"none">>) -> false;
    (_, _) -> true
  end,
  maps:from_list([{K, V} || {K, V} <- maps:to_list(Map), Predicate(K, V)]).