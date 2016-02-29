PROJECT = example
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEPS = bucs cowboy jsx
dep_bucs = git https://github.com/botsunit/bucs.git master
dep_jsx = hex 2.8.0


ELIXIR_BINDINGS = example sub sub_again
ELIXIR_BINDINGS_PREFIX = erlang

include erlang.mk
include ../plugins.mk

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include

