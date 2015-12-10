REBAR = ./rebar3

DEPS_PATH = ./_build/default/lib/*/ebin

CT_OPTS = -cover test/cover.spec -erl_args -config test/test.config
CT_SUITES = local_SUITE

CT_OPTS_DIST = -cover test/cover.spec -erl_args -config test/test2.config
CT_SUITES_DIST = dist_SUITE

.PHONY: all compile clean distclean dialyze tests

all: compile

compile:
	$(REBAR) compile

clean:
	rm -rf ebin/* test/*.beam logs log
	$(REBAR) clean

distclean: clean
	rm -rf _build priv/*.so logs log

dialyze:
	$(REBAR) dialyzer

local_tests: compile
	mkdir -p logs
	ct_run -dir test -suite $(CT_SUITES) -pa $(DEPS_PATH) -logdir logs $(CT_OPTS)
	rm -rf test/*.beam

dist_tests: compile
	mkdir -p logs
	ct_run -dir test -suite $(CT_SUITES_DIST) -pa $(DEPS_PATH) -logdir logs $(CT_OPTS_DIST)
	rm -rf test/*.beam

tests: local_tests dist_tests
