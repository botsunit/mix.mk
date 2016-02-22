PROJECT = example
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEPS = bucs cowboy
dep_bucs = git https://github.com/botsunit/bucs.git master

ELIXIR_BINDINGS = example sub
ELIXIR_BINDINGS_PREFIX = erlang

include erlang.mk
include ../plugins.mk

app:: mix.exs mix.bind

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include

