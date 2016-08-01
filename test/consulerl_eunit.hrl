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
-define(CHECKS_RESPONSE, <<"{}">>).
-define(CHECKS_RESPONSE_MAP, #{}).
-define(CHECKS_RESPONSE_FULL, <<"{\"google:test\":{\"Node\":\"db8ae3e522df\",\"CheckID\":\"google:test\",\"Name\":\"test\",\"Status\":\"passing\",\"Notes\":\"\",\"Output\":\"\",\"ServiceID\":\"\",\"ServiceName\":\"\",\"CreateIndex\":0,\"ModifyIndex\":0}}">>).
-define(CHECKS_RESPONSE_MAP_FULL, #{
  <<"google:test">> => #{
    check_id => <<"google:test">>,
    create_index => 0,
    modify_index => 0,
    name => <<"test">>,
    node => <<"db8ae3e522df">>,
    notes => <<>>,
    output => <<>>,
    service_id => <<>>,
    service_name => <<>>,
    status => <<"passing">>
  }
}).
-define(SERVICES_RESPONSE, <<"{\"consul\":{\"ID\":\"consul\",\"Service\":\"consul\",\"Tags\":[],\"Address\":\"\",\"Port\":8300,\"EnableTagOverride\":false,\"CreateIndex\":0,\"ModifyIndex\":0}}">>).
-define(SERVICES_RESPONSE_MAP, #{
  <<"consul">> => #{address => <<>>,
    create_index => 0,
    enable_tag_override => false,
    id => <<"consul">>,
    modify_index => 0,
    port => 8300,
    service => <<"consul">>,
    tags => []
  }
}).
-define(MEMBERS_RESPONSE, <<"[{\"Name\":\"db8ae3e522df\",\"Addr\":\"172.17.0.2\",\"Port\":8301,\"Tags\":{\"build\":\"0.6.4:26a0ef8c\",\"dc\":\"dc1\",\"port\":\"8300\",\"role\":\"consul\",\"vsn\":\"2\",\"vsn_max\":\"3\",\"vsn_min\":\"1\"},\"Status\":1,\"ProtocolMin\":1,\"ProtocolMax\":3,\"ProtocolCur\":2,\"DelegateMin\":2,\"DelegateMax\":4,\"DelegateCur\":4}]">>).
-define(MEMBERS_RESPONSE_MAP, #{
  addr => <<"172.17.0.2">>,
  delegate_cur => 4,
  delegate_max => 4,
  delegate_min => 2,
  name => <<"db8ae3e522df">>,
  port => 8301,
  protocol_cur => 2,
  protocol_max => 3,
  protocol_min => 1,
  status => 1,
  tags => #{
    build => <<"0.6.4:26a0ef8c">>,
    dc => <<"dc1">>,
    port => <<"8300">>,
    role => <<"consul">>,
    vsn => <<"2">>,
    vsn_max => <<"3">>,
    vsn_min => <<"1">>
  }
}).
-define(SELF_RESPONSE, <<"{\"Config\":{\"DevMode\":true,\"Bootstrap\":false,\"BootstrapExpect\":0,\"Server\":true,\"Datacenter\":\"dc1\",\"DataDir\":\"/consul/data\",\"DNSRecursor\":\"\",\"DNSRecursors\":[],\"DNSConfig\":{\"NodeTTL\":0,\"ServiceTTL\":null,\"AllowStale\":false,\"EnableTruncate\":false,\"MaxStale\":5000000000,\"OnlyPassing\":false},\"Domain\":\"consul.\",\"LogLevel\":\"DEBUG\",\"NodeName\":\"db8ae3e522df\",\"ClientAddr\":\"0.0.0.0\",\"BindAddr\":\"0.0.0.0\",\"AdvertiseAddr\":\"172.17.0.2\",\"AdvertiseAddrs\":{\"SerfLan\":null,\"SerfLanRaw\":\"\",\"SerfWan\":null,\"SerfWanRaw\":\"\",\"RPC\":null,\"RPCRaw\":\"\"},\"AdvertiseAddrWan\":\"172.17.0.2\",\"TranslateWanAddrs\":false,\"Ports\":{\"DNS\":8600,\"HTTP\":8500,\"HTTPS\":-1,\"RPC\":8400,\"SerfLan\":8301,\"SerfWan\":8302,\"Server\":8300},\"Addresses\":{\"DNS\":\"\",\"HTTP\":\"\",\"HTTPS\":\"\",\"RPC\":\"\"},\"TaggedAddresses\":{\"wan\":\"172.17.0.2\"},\"LeaveOnTerm\":false,\"SkipLeaveOnInt\":false,\"Telemetry\":{\"StatsiteAddr\":\"\",\"StatsdAddr\":\"\",\"StatsitePrefix\":\"consul\",\"DisableHostname\":false,\"DogStatsdAddr\":\"\",\"DogStatsdTags\":null},\"Protocol\":2,\"EnableDebug\":true,\"VerifyIncoming\":false,\"VerifyOutgoing\":false,\"VerifyServerHostname\":false,\"CAFile\":\"\",\"CertFile\":\"\",\"KeyFile\":\"\",\"ServerName\":\"\",\"StartJoin\":[],\"StartJoinWan\":[],\"RetryJoin\":[],\"RetryMaxAttempts\":0,\"RetryIntervalRaw\":\"\",\"RetryJoinWan\":[],\"RetryMaxAttemptsWan\":0,\"RetryIntervalWanRaw\":\"\",\"EnableUi\":true,\"UiDir\":\"\",\"PidFile\":\"\",\"EnableSyslog\":false,\"SyslogFacility\":\"LOCAL0\",\"RejoinAfterLeave\":false,\"CheckUpdateInterval\":300000000000,\"ACLDatacenter\":\"\",\"ACLTTL\":30000000000,\"ACLTTLRaw\":\"\",\"ACLDefaultPolicy\":\"allow\",\"ACLDownPolicy\":\"extend-cache\",\"Watches\":null,\"DisableRemoteExec\":false,\"DisableUpdateCheck\":false,\"DisableAnonymousSignature\":true,\"HTTPAPIResponseHeaders\":null,\"AtlasInfrastructure\":\"\",\"AtlasJoin\":false,\"AtlasEndpoint\":\"\",\"DisableCoordinates\":false,\"Revision\":\"26a0ef8c41aa2252ab4cf0844fc6470c8e1d8256\",\"Version\":\"0.6.4\",\"VersionPrerelease\":\"\",\"UnixSockets\":{\"Usr\":\"\",\"Grp\":\"\",\"Perms\":\"\"},\"SessionTTLMin\":0,\"SessionTTLMinRaw\":\"\",\"Reap\":null},\"Coord\":{\"Vec\":[0,0,0,0,0,0,0,0],\"Error\":1.5,\"Adjustment\":0,\"Height\":1e-05},\"Member\":{\"Name\":\"db8ae3e522df\",\"Addr\":\"172.17.0.2\",\"Port\":8301,\"Tags\":{\"build\":\"0.6.4:26a0ef8c\",\"dc\":\"dc1\",\"port\":\"8300\",\"role\":\"consul\",\"vsn\":\"2\",\"vsn_max\":\"3\",\"vsn_min\":\"1\"},\"Status\":1,\"ProtocolMin\":1,\"ProtocolMax\":3,\"ProtocolCur\":2,\"DelegateMin\":2,\"DelegateMax\":4,\"DelegateCur\":4}}">>).
-define(SELF_RESPONSE_MAP, #{
  config => #{
    domain => <<"consul.">>,
    start_join_wan => [],
    advertise_addr => <<"172.17.0.2">>,
    acl_ttl => 30000000000,
    retry_join => [],
    key_file => <<>>,
    atlas_join => false,
    server_name => <<>>,
    versionp_rerelease => <<>>,
    protocol => 2,
    leave_on_term => false,
    enable_ui => true,
    reap => null,
    retry_join_wan => [],
    verify_server_hostname => false,
    log_level => <<"DEBUG">>,
    acl_ttl_raw => <<>>,
    version => <<"0.6.4">>,
    retry_max_attempts_wan => 0,
    session_ttl_min => 0,
    translate_wan_addrs => false,
    revision => <<"26a0ef8c41aa2252ab4cf0844fc6470c8e1d8256">>,
    dns_recursors => [],
    disable_remote_exec => false,
    enable_debug => true,
    syslog_facility => <<"LOCAL0">>,
    atlas_infrastructure => <<>>,
    node_name => <<"db8ae3e522df">>,
    start_join => [],
    ca_file => <<>>,
    session_ttl_min_raw => <<>>,
    pid_file => <<>>,
    enable_syslog => false,
    dns_config => #{
      allow_stale => false,
      enable_truncate => false,
      max_stale => 5000000000,
      node_ttl => 0,
      only_passing => false,
      service_ttl => null
    },
    retry_max_attempts => 0,
    unix_sockets => #{
      grp => <<>>,
      perms => <<>>,
      us => <<>>
    },
    retry_interval_raw => <<>>,
    server => true,
    disable_coordinates => false,
    ui_dir => <<>>,
    acl_down_policy => <<"extend-cache">>,
    datacenter => <<"dc1">>,
    verify_incoming => false,
    advertisea_ddrs => #{
      rpc => null,
      rpc_raw => <<>>,
      serf_lan => null,
      serf_lan_raw => <<>>,
      serf_wan => null,
      serf_wan_raw => <<>>
    },
    telemetry => #{
      disable_hostname => false,
      dog_statsd_addr => <<>>,
      dog_statsd_tags => null,
      statsd_addr => <<>>,
      statsite_addr => <<>>,
      statsite_prefix => <<"consul">>
    },
    rejoin_after_leave => false,
    check_update_interval => 300000000000,
    watches => null,
    tagged_addresses => #{
      wan => <<"172.17.0.2">>
    },
    atlas_endpoint => <<>>,
    cert_file => <<>>,
    client_addr => <<"0.0.0.0">>,
    bind_addr => <<"0.0.0.0">>,
    acl_datacenter => <<>>,
    skip_leave_on_int => false,
    ports => #{
      dns => 8600,
      http => 8500,
      https => -1,
      rpc => 8400,
      serf_lan => 8301,
      serf_wan => 8302,
      server => 8300
    },
    bootstrap_expect => 0,
    addresses => #{
      dns => <<>>,
      http => <<>>,
      https => <<>>,
      rpc => <<>>
    },
    http_api_response_headers => null,
    verify_outgoing => false,
    disable_update_check => false,
    bootstrap => false,
    dev_mode => true,
    retry_interval_wan_raw => <<>>,
    disable_anonymous_signature => true,
    acl_default_policy => <<"allow">>,
    data_dir => <<"/consul/data">>,
    dns_recursor => <<>>,
    advertise_addr_wan => <<"172.17.0.2">>
  },
  coord => #{
    adjustment => 0,
    error => 1.5,
    height => 1.0e-5,
    vec => [0, 0, 0, 0, 0, 0, 0, 0]
  },
  member => #{
    addr => <<"172.17.0.2">>,
    delegate_cur => 4,
    delegate_max => 4,
    delegate_min => 2,
    name => <<"db8ae3e522df">>,
    port => 8301,
    protocol_cur => 2,
    protocol_max => 3,
    protocol_min => 1,
    status => 1,
    tags => #{
      build => <<"0.6.4:26a0ef8c">>,
      dc => <<"dc1">>,
      port => <<"8300">>,
      role => <<"consul">>,
      vsn => <<"2">>,
      vsn_max => <<"3">>,
      vsn_min => <<"1">>
    }
  }
}).
-define(JOIN_RESPONSE, <<>>).
-define(JOIN_RESPONSE_MAP, <<>>).
-define(FORCE_LEAVE_RESPONSE, <<>>).
-define(FORCE_LEAVE_RESPONSE_MAP, <<>>).
-define(REGISTER_CHECK_RESPONSE, <<>>).
-define(REGISTER_CHECK_RESPONSE_MAP, <<>>).
-define(REGISTER_SCRIPT_CHECK_RESPONSE, <<>>).
-define(REGISTER_SCRIPT_CHECK_RESPONSE_MAP, <<>>).
-define(REGISTER_DOCKER_CHECK_RESPONSE, <<>>).
-define(REGISTER_DOCKER_CHECK_RESPONSE_MAP, <<>>).
-define(REGISTER_HTTP_CHECK_RESPONSE, <<>>).
-define(REGISTER_HTTP_CHECK_RESPONSE_MAP, <<>>).
-define(REGISTER_TCP_CHECK_RESPONSE, <<>>).
-define(REGISTER_TCP_CHECK_RESPONSE_MAP, <<>>).
-define(DEREGISTER_CHECK_RESPONSE, <<>>).
-define(DEREGISTER_CHECK_RESPONSE_MAP, <<>>).
-define(PASS_RESPONSE, <<>>).
-define(PASS_RESPONSE_MAP, <<>>).
-define(WARN_RESPONSE, <<>>).
-define(WARN_RESPONSE_MAP, <<>>).
-define(FAIL_RESPONSE, <<>>).
-define(FAIL_RESPONSE_MAP, <<>>).
-define(UPDATE_RESPONSE, <<>>).
-define(UPDATE_RESPONSE_MAP, <<>>).
-define(REGISTER_SERVICE_RESPONSE, <<>>).
-define(REGISTER_SERVICE_RESPONSE_MAP, <<>>).
-define(DEREGISTER_SERVICE_RESPONSE, <<>>).
-define(DEREGISTER_SERVICE_RESPONSE_MAP, <<>>).
-define(LEADER_RESPONSE, <<"\"172.17.0.2:8300\"">>).
-define(LEADER_RESPONSE_MAP, <<"172.17.0.2:8300">>).
-define(PEERS_RESPONSE, <<"[\"172.17.0.2:8300\"]">>).
-define(PEERS_RESPONSE_MAP, <<"172.17.0.2:8300">>).