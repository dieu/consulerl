-module(consulerl_atom).

%% API
-export([
  keys_to_atom/1,
  to_atom/1
]).

-spec keys_to_atom(term()) -> term().
keys_to_atom(Map) when is_map(Map) ->
  maps:from_list([{to_atom(K), keys_to_atom(V)} || {K, V} <- maps:to_list(Map)]);

keys_to_atom(List) when is_list(List) ->
  lists:map(fun keys_to_atom/1, List);

keys_to_atom(Value) ->
  Value.

-spec to_atom(binary()) -> atom().
to_atom(<<"Index">>) ->
  index;
to_atom(<<"CreateIndex">>) ->
  create_index;
to_atom(<<"ModifyIndex">>) ->
  modify_index;
to_atom(<<"LockIndex">>) ->
  lock_index;
to_atom(<<"Key">>) ->
  key;
to_atom(<<"Value">>) ->
  value;
to_atom(<<"Flags">>) ->
  flags;
to_atom(<<"OpIndex">>) ->
  op_index;
to_atom(<<"What">>) ->
  what;
to_atom(<<"Results">>) ->
  results;
to_atom(<<"Errors">>) ->
  errors;
to_atom(<<"LastContact">>) ->
  last_contact;
to_atom(<<"KnownLeader">>) ->
  known_leader;
to_atom(<<"CheckID">>) ->
  check_id;
to_atom(<<"Name">>) ->
  name;
to_atom(<<"Node">>) ->
  node;
to_atom(<<"Notes">>) ->
  notes;
to_atom(<<"Output">>) ->
  output;
to_atom(<<"ServiceID">>) ->
  service_id;
to_atom(<<"ServiceName">>) ->
  service_name;
to_atom(<<"Status">>) ->
  status;
to_atom(<<"ID">>) ->
  id;
to_atom(<<"Address">>) ->
  address;
to_atom(<<"Port">>) ->
  port;
to_atom(<<"port">>) ->
  port;
to_atom(<<"Tags">>) ->
  tags;
to_atom(<<"EnableTagOverride">>) ->
  enable_tag_override;
to_atom(<<"Service">>) ->
  service;
to_atom(<<"Addr">>) ->
  addr;
to_atom(<<"Addresses">>) ->
  addresses;
to_atom(<<"DelegateCur">>) ->
  delegate_cur;
to_atom(<<"DelegateMax">>) ->
  delegate_max;
to_atom(<<"DelegateMin">>) ->
  delegate_min;
to_atom(<<"ProtocolCur">>) ->
  protocol_cur;
to_atom(<<"ProtocolMax">>) ->
  protocol_max;
to_atom(<<"ProtocolMin">>) ->
  protocol_min;
to_atom(<<"build">>) ->
  build;
to_atom(<<"dc">>) ->
  dc;
to_atom(<<"role">>) ->
  role;
to_atom(<<"vsn">>) ->
  vsn;
to_atom(<<"vsn_max">>) ->
  vsn_max;
to_atom(<<"vsn_min">>) ->
  vsn_min;
to_atom(<<"expect">>) ->
  expect;
to_atom(<<"Config">>) ->
  config;
to_atom(<<"Telemetry">>) ->
  telemetry;
to_atom(<<"DogStatsdAddr">>) ->
  dog_statsd_addr;
to_atom(<<"DogStatsdTags">>) ->
  dog_statsd_tags;
to_atom(<<"StatsdAddr">>) ->
  statsd_addr;
to_atom(<<"StatsiteAddr">>) ->
  statsite_addr;
to_atom(<<"StatsitePrefix">>) ->
  statsite_prefix;
to_atom(<<"Ports">>) ->
  ports;
to_atom(<<"HTTP">>) ->
  http;
to_atom(<<"HTTPS">>) ->
  https;
to_atom(<<"RPC">>) ->
  rpc;
to_atom(<<"SerfLan">>) ->
  serf_lan;
to_atom(<<"SerfWan">>) ->
  serf_wan;
to_atom(<<"SerfLanRaw">>) ->
  serf_lan_raw;
to_atom(<<"Server">>) ->
  server;
to_atom(<<"UnixSockets">>) ->
  unix_sockets;
to_atom(<<"ACLDatacenter">>) ->
  acl_datacenter;
to_atom(<<"Watches">>) ->
  watches;
to_atom(<<"AtlasEndpoint">>) ->
  atlas_endpoint;
to_atom(<<"CertFile">>) ->
  cert_file;
to_atom(<<"SyslogFacility">>) ->
  syslog_facility;
to_atom(<<"RetryJoin">>) ->
  retry_join;
to_atom(<<"RetryMaxAttempts">>) ->
  retry_max_attempts;
to_atom(<<"RetryIntervalRaw">>) ->
  retry_interval_raw;
to_atom(<<"DNSRecursors">>) ->
  dns_recursors;
to_atom(<<"DisableRemoteExec">>) ->
  disable_remote_exec;
to_atom(<<"DevMode">>) ->
  dev_mode;
to_atom(<<"TaggedAddresses">>) ->
  tagged_addresses;
to_atom(<<"KeyFile">>) ->
  key_file;
to_atom(<<"AtlasJoin">>) ->
  atlas_join;
to_atom(<<"RetryIntervalWanRaw">>) ->
  retry_interval_wan_raw;
to_atom(<<"VersionPrerelease">>) ->
  versionp_rerelease;
to_atom(<<"CAFile">>) ->
  ca_file;
to_atom(<<"RetryJoinWan">>) ->
  retry_join_wan;
to_atom(<<"AdvertiseAddr">>) ->
  advertise_addr;
to_atom(<<"SessionTTLMin">>) ->
  session_ttl_min;
to_atom(<<"Version">>) ->
  version;
to_atom(<<"RetryMaxAttemptsWan">>) ->
  retry_max_attempts_wan;
to_atom(<<"AdvertiseAddrs">>) ->
  advertisea_ddrs;
to_atom(<<"RPCRaw">>) ->
  rpc_raw;
to_atom(<<"SerfWanRaw">>) ->
  serf_wan_raw;
to_atom(<<"DisableCoordinates">>) ->
  disable_coordinates;
to_atom(<<"Bootstrap">>) ->
  bootstrap;
to_atom(<<"Reap">>) ->
  reap;
to_atom(<<"DisableAnonymousSignature">>) ->
  disable_anonymous_signature;
to_atom(<<"BindAddr">>) ->
  bind_addr;
to_atom(<<"TranslateWanAddrs">>) ->
  translate_wan_addrs;
to_atom(<<"RejoinAfterLeave">>) ->
  rejoin_after_leave;
to_atom(<<"StartJoinWan">>) ->
  start_join_wan;
to_atom(<<"VerifyServerHostname">>) ->
  verify_server_hostname;
to_atom(<<"LogLevel">>) ->
  log_level;
to_atom(<<"VerifyIncoming">>) ->
  verify_incoming;
to_atom(<<"SkipLeaveOnInt">>) ->
  skip_leave_on_int;
to_atom(<<"ACLDownPolicy">>) ->
  acl_down_policy;
to_atom(<<"ClientAddr">>) ->
  client_addr;
to_atom(<<"AdvertiseAddrWan">>) ->
  advertise_addr_wan;
to_atom(<<"HTTPAPIResponseHeaders">>) ->
  http_api_response_headers;
to_atom(<<"Protocol">>) ->
  protocol;
to_atom(<<"DisableUpdateCheck">>) ->
  disable_update_check;
to_atom(<<"StartJoin">>) ->
  start_join;
to_atom(<<"AtlasInfrastructure">>) ->
  atlas_infrastructure;
to_atom(<<"VerifyOutgoing">>) ->
  verify_outgoing;
to_atom(<<"DNSRecursor">>) ->
  dns_recursor;
to_atom(<<"CheckUpdateInterval">>) ->
  check_update_interval;
to_atom(<<"BootstrapExpect">>) ->
  bootstrap_expect;
to_atom(<<"Datacenter">>) ->
  datacenter;
to_atom(<<"EnableSyslog">>) ->
  enable_syslog;
to_atom(<<"DNSConfig">>) ->
  dns_config;
to_atom(<<"DataDir">>) ->
  data_dir;
to_atom(<<"LeaveOnTerm">>) ->
  leave_on_term;
to_atom(<<"ACLTTLRaw">>) ->
  acl_ttl_raw;
to_atom(<<"SessionTTLMinRaw">>) ->
  session_ttl_min_raw;
to_atom(<<"UiDir">>) ->
  ui_dir;
to_atom(<<"EnableUi">>) ->
  enable_ui;
to_atom(<<"ACLDefaultPolicy">>) ->
  acl_default_policy;
to_atom(<<"ACLTTL">>) ->
  acl_ttl;
to_atom(<<"ServerName">>) ->
  server_name;
to_atom(<<"EnableDebug">>) ->
  enable_debug;
to_atom(<<"PidFile">>) ->
  pid_file;
to_atom(<<"Revision">>) ->
  revision;
to_atom(<<"NodeName">>) ->
  node_name;
to_atom(<<"Domain">>) ->
  domain;
to_atom(<<"Coord">>) ->
  coord;
to_atom(<<"Error">>) ->
  error;
to_atom(<<"Height">>) ->
  height;
to_atom(<<"Vec">>) ->
  vec;
to_atom(<<"Member">>) ->
  member;
to_atom(<<"DisableHostname">>) ->
  disable_hostname;
to_atom(<<"DNS">>) ->
  dns;
to_atom(<<"Grp">>) ->
  grp;
to_atom(<<"Perms">>) ->
  perms;
to_atom(<<"Usr">>) ->
  us;
to_atom(<<"wan">>) ->
  wan;
to_atom(<<"AllowStale">>) ->
  allow_stale;
to_atom(<<"EnableTruncate">>) ->
  enable_truncate;
to_atom(<<"MaxStale">>) ->
  max_stale;
to_atom(<<"NodeTTL">>) ->
  node_ttl;
to_atom(<<"OnlyPassing">>) ->
  only_passing;
to_atom(<<"ServiceTTL">>) ->
  service_ttl;
to_atom(<<"Adjustment">>) ->
  adjustment;
to_atom(<<"KV">>) ->
  kv;
to_atom(<<"LTime">>) ->
  ltime;
to_atom(<<"NodeFilter">>) ->
  node_filter;
to_atom(<<"Payload">>) ->
  payload;
to_atom(<<"ServiceFilter">>) ->
  service_filter;
to_atom(<<"TagFilter">>) ->
  tag_filter;

to_atom(Binary) ->
  Binary.

%%to_atom(Binary) when is_binary(Binary) ->
%%  list_to_atom(normalize(binary_to_list(Binary))).

%%-spec normalize(list()) -> list().
%%normalize(String) when is_list(String) ->
%%  {NewString, _} = lists:foldl(fun(Char, {Acc, Last}) ->
%%    case {is_capital(Last), is_capital(Char)} of
%%      {_, space} ->
%%        {Acc, Last};
%%      {none, true} ->
%%        {[Char + 32 | Acc], Char};
%%      {true, true} ->
%%        {[Char + 32 | Acc], Char};
%%      {false, true} ->
%%        {[Char + 32, $_ | Acc], Char};
%%      {_, false} ->
%%        {[Char | Acc], Char}
%%    end
%%  end, {[], none}, String),
%%
%%  lists:reverse(NewString).
%%
%%-spec is_capital(integer()) -> boolean() | space | none .
%%is_capital(C) when is_integer(C), $A =< C, C =< $Z ->
%%  true;
%%is_capital(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 ->
%%  true;
%%is_capital(C) when is_integer(C), 16#D8 =< C, C =< 16#DE ->
%%  true;
%%is_capital(C) when is_integer(C), $\ =:= C ->
%%  space;
%%is_capital(none) ->
%%  none;
%%is_capital(_) ->
%%  false.
