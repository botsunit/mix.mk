.PHONY: mix.exs

ELIXIR_VERSION = ~> 1.2

MIX_PROJECT = $(shell echo "${PROJECT}" | sed -r 's/(.)(.*)/\U\1\E\2/' | sed -r 's/_(.)/\U\1\E/g')
MIX_PROJECT_VERSION = ${PROJECT_VERSION}
MIX_DEPS = []
MIX_APPLICATION = []

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

mix.exs:
	$(gen_verbose) echo "$${_compat_mix_exs}" > mix.exs

help::
	$(verbose) printf "%s\n" "" \
		"Mix.exs targets:" \
		"  mix.exs              Create a mix.exs file"

