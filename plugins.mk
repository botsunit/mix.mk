.PHONY: mix.exs

ELIXIR_VERSION ?= ~> 1.2
ELIXIR_BINDINGS ?= 
ELIXIR_BINDINGS_SRC ?= src/
ELIXIR_BINDINGS_DEST ?= lib
ELIXIR_BINDINGS_PREFIX ?= 

MIX_PROJECT = $(shell echo "${PROJECT}" | sed -r 's/(.)(.*)/\U\1\E\2/' | sed -r 's/_(.)/\U\1\E/g')
MIX_PROJECT_VERSION = ${PROJECT_VERSION}
MIX_DEPS = []
MIX_APPLICATION = []

mix_verbose_0 = @echo " GEN   " 
mix_verbose = $(mix_verbose_$(V))

## mix.exs

APP_SRC = $(wildcard src/*.app.src)
ifeq ($(APP_SRC),)
APP_SRC = $(wildcard ebin/*.app)
endif

define get_app_version.erl
  case file:consult("$(APP_SRC)") of
		{ok, [{application, _, Terms}]} ->
		  case lists:keyfind(vsn, 1, Terms) of
			  {vsn, Data} ->
				  io:format("~s", [Data]);
				false -> 
				  io:format("0.0.1")
		  end;
	  {error, _} -> io:format("0.0.1")
	end,
	halt(0).
endef

define get_app_applications.erl
  case file:consult("$(APP_SRC)") of
		{ok, [{application, _, Terms}]} ->
		  case lists:keyfind(applications, 1, Terms) of
			  {applications, Data} ->
				  Deps = lists:foldl(fun
					                     (kernel, Acc) -> Acc;
					                     (stdlib, Acc) -> Acc;
					                     (E, Acc) -> [":" ++ atom_to_list(E)|Acc]
					       end, [], Data),
					io:format("~s", [string:join(lists:reverse(Deps), ",")]);
				false -> 
				  io:format("")
		  end;
	  {error, _} -> io:format("")
	end,
	halt(0).
endef

define get_app_mod.erl
  case file:consult("$(APP_SRC)") of
		{ok, [{application, _, Terms}]} ->
		  case lists:keyfind(mod, 1, Terms) of
			  {mod, {Mod, Args}} ->
				io:format(", mod: {:~p, ~p}", [Mod, Args]);
				false -> 
				  io:format("")
		  end;
	  {error, _} -> io:format("")
	end,
	halt(0).
endef

ifneq ($(APP_SRC),)
MIX_PROJECT_VERSION = $(shell $(call erlang,$(call get_app_version.erl)))
MIX_APPLICATION = [applications: [$(shell $(call erlang,$(call get_app_applications.erl)))]$(shell $(call erlang,$(call get_app_mod.erl)))]
endif

define compat_mix_exs
defmodule ${MIX_PROJECT}.Mixfile do
  use Mix.Project

  def project do
    [app: :${PROJECT},
     version: "${MIX_PROJECT_VERSION}",
     elixir: "${ELIXIR_VERSION}",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    ${MIX_APPLICATION}
  end

  defp deps do
    [$(foreach d,$(DEPS),\
\n      {:$(call dep_name,$d), ~r/.*/, git: "$(call dep_repo,$d)", branch: "$(call dep_commit,$d)"},)
    ]
  end
end
endef

$(eval _compat_mix_exs = $$(compat_mix_exs))
$(eval export _compat_mix_exs)

mix.exs: app
	$(gen_verbose) echo "$${_compat_mix_exs}" > mix.exs

## Bindings

rwildcard=$(wildcard $(addsuffix $2, $1)) $(foreach d,$(wildcard $(addsuffix *, $1)),$(call rwildcard,$d/,$2))

ifeq ($(ELIXIR_BINDINGS),)
ERLANG_BINDINGS_SRC = $(call rwildcard,$(ELIXIR_BINDINGS_SRC),*.erl)
else
ERLANG_BINDINGS_SRC = $(foreach e,$(ELIXIR_BINDINGS),$(call rwildcard,$(ELIXIR_BINDINGS_SRC),$(e).erl))
endif
ifneq ($(ELIXIR_BINDINGS_PREFIX),)
ELIXIR_BINDINGS_PREFIX_F = $(shell echo "$(ELIXIR_BINDINGS_PREFIX)" | \
												 sed -r 's/(.)(.*)/\U\1\E\2/' | \
												 sed -r 's/_(.)/\U\1\E/g').
endif

define write_ex.erl	
	case code:load_abs("ebin/$(1)") of
	  {error, _} -> halt(1);
		{module, M} -> 
		  case erlang:apply(M, module_info, [exports]) of
			  MI when is_list(MI) ->
				  {ok, IO} = file:open("$(4)", [write]),
					io:format(IO, "# File: $(4)\n", []),
		      io:format(IO, "# This file was generated from $(2)\n", []),
					io:format(IO, "# Using mix.mk (https://github.com/botsunit/mix.mk)\n", []),
					io:format(IO, "# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!\n", []),
          io:format(IO, "defmodule $(3) do\n", []),
					lists:foreach(fun
					                ({module_info, _}) -> ok;
					                ({N, A}) ->
					                  Args = string:join(
													           lists:map(fun(E) -> 
													  				             "arg" ++ integer_to_list(E) 
													  									 end, lists:seq(1,A)), ", "),
													  io:format(IO, "  def unquote(:~p)(~s) do\n", [atom_to_list(N), Args]),
														io:format(IO, "    :erlang.apply(:~p, :~p, [~s])\n", [atom_to_list(M), atom_to_list(N), Args]),
														io:format(IO, "  end\n", [])
					              end, MI),
          io:format(IO, "end\n", []),
					ok = file:close(IO);
			  _-> halt(1)
		  end
	end,
	halt(0).
endef

define elixir_binding_target
$(eval n := $(notdir $(basename $1)))
$(eval s := $(basename $1))
$(eval m := $(subst $(ELIXIR_BINDINGS_SRC),,$s))
$(eval d := $(ELIXIR_BINDINGS_DEST)/$(ELIXIR_BINDINGS_PREFIX_F)$(shell echo "$m" | sed -r 's/(.)(.*)/\U\1\E\2/' | sed -r 's/\/(.)/.\U\1\E/g' | sed -r 's/_(.)/.\U\1\E/g').ex)
$(eval e := $(notdir $(basename $d)))
$d: $1 $(ELIXIR_BINDINGS_DEST)
	$(mix_verbose) $d
	$(shell $(call erlang,$(call write_ex.erl,$n,$1,$e,$d)))
endef
$(foreach src,$(ERLANG_BINDINGS_SRC),$(eval $(call elixir_binding_target,$(src))))

ALL_ELIXIR_MODULES_SRC0 = $(foreach mod,$(ERLANG_BINDINGS_SRC),$(basename $(mod)))
ALL_ELIXIR_MODULES_SRC1 = $(foreach mod,$(ALL_ELIXIR_MODULES_SRC0),$(subst $(ELIXIR_BINDINGS_SRC),,$(mod)))
ALL_ELIXIR_MODULES_SRC = $(foreach mod,$(ALL_ELIXIR_MODULES_SRC1),$(ELIXIR_BINDINGS_DEST)/$(ELIXIR_BINDINGS_PREFIX_F)$(shell echo "$(mod)" | sed -r 's/(.)(.*)/\U\1\E\2/' | sed -r 's/\/(.)/.\U\1\E/g' | sed -r 's/_(.)/.\U\1\E/g').ex)

$(ELIXIR_BINDINGS_DEST):
	@mkdir -p $(ELIXIR_BINDINGS_DEST)

.PHONY: $(ALL_ELIXIR_MODULES_SRC)

mix.bind: app $(ALL_ELIXIR_MODULES_SRC)

## All

mix.all: mix.exs mix.bind

## Help

help::
	$(verbose) printf "%s\n" "" \
		"Mix.exs targets:" \
		"  mix.exs              Create a mix.exs file" \
		"  mix.bind             Create bindings for Elixir" \
		"  mix.all              Call mix.exs and mix.bind"

