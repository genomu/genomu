genomu_src := $(wildcard apps/genomu/lib/*.ex) \
              $(wildcard apps/genomu/lib/**/*.ex) apps/genomu/mix.exs

.PHONY: all test

all: genomu

genomu: apps/genomu/ebin

apps/genomu/ebin: $(genomu_src) mix.lock
	@cd apps/genomu && $(genomu_path) mix do deps.get, compile

iex:
	@ERL_LIBS=deps:apps iex --sname genomu_console

sys.config: config.exs apps/genomu/lib/config.ex
	@ERL_LIBS=apps:deps elixir -e "config = Genomu.Config.file!(%b{config.exs}); config.sys_config!(%b{sys.config})"

start: all sys.config
	@ulimit -n 4096 && ERL_LIBS=apps:deps elixir --sname genomu --erl "-config sys -s Elixir-Genomu" --no-halt

remsh:
	ERL_LIBS=apps:deps iex --sname genomu_remsh --remsh genomu@$(shell hostname | awk -F. '{print $$1;}')

clean:
	@cd apps/genomu && $(genomu_path) mix clean

test: apps/genomu/ebin
	@cd apps/genomu && $(genomu_path) MIX_ENV=test mix test --no-start