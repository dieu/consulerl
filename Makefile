PROJECT = consulerl
PROJECT_VERSION = 0.1.0
PROJECT_DESCRIPTION = Consul client

LOCAL_DEPS = inets
DEPS = jsx erlang_uuid lager

dep_jsx = git  https://github.com/talentdeficit/jsx.git    "v2.5.2"
dep_erlang_uuid = git  https://github.com/travis/erlang-uuid.git   master
dep_lager = git https://github.com/basho/lager.git  "2.2.3"

TEST_DEPS = meck

dep_meck = git https://github.com/eproxus/meck.git  0.8.4

SHELL_DEPS = sync

dep_sync = git  git://github.com/rustyio/sync.git   master

include erlang.mk

UNIVERSAL_ERLC_OPTS += -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

ERLC_OPTS += $(UNIVERSAL_ERLC_OPTS)
ERLC_OPTS += +'{parse_transform, lager_transform}'

SHELL_OPTS += -config rel/shell.config -run consulerl -run sync go

EUNIT_ERL_OPTS += -config rel/shell.config
#EUNIT_OPTS = verbose