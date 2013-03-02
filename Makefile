genomu_src := $(wildcard apps/genomu/lib/*.ex) \
              $(wildcard apps/genomu/lib/**/*.ex) apps/genomu/mix.exs

.PHONY: all test

all: genomu

genomu: apps/genomu/ebin

apps/genomu/ebin: $(genomu_src) mix.lock
	@cd apps/genomu && $(genomu_path) mix do deps.get, compile

iex:
	@ERL_LIBS=deps:apps iex --sname genomu_console

start: all
	@ERL_LIBS=apps:deps elixir -e "config = Genomu.Config.file!(%b{test/config.1.exs}); config.sys_config!(%b{test/sys.1.config})"
	@ERL_LIBS=apps:deps elixir -e "config = Genomu.Config.file!(%b{test/config.2.exs}); config.sys_config!(%b{test/sys.2.config})"
	@ERL_LIBS=apps:deps elixir -e "config = Genomu.Config.file!(%b{test/config.3.exs}); config.sys_config!(%b{test/sys.3.config})"
	@ulimit -n 4096 && ERL_LIBS=apps:deps elixir --name genomu1@127.0.0.1 --erl "-config test/sys.1 -s Elixir-Genomu" --no-halt &
	@ulimit -n 4096 && ERL_LIBS=apps:deps elixir --name genomu2@127.0.0.1 --erl "-config test/sys.2 -s Elixir-Genomu" --no-halt &
	@ulimit -n 4096 && ERL_LIBS=apps:deps elixir --name genomu3@127.0.0.1 --erl "-config test/sys.3 -s Elixir-Genomu" --no-halt &

stop:
	@kill -9 `cat data/$(shell hostname | awk -F. '{print $$1;}')1/genomu.pid` `cat data/$(shell hostname | awk -F. '{print $$1;}')2/genomu.pid` `cat data/$(shell hostname | awk -F. '{print $$1;}')3/genomu.pid`
	@rm -f data/$(shell hostname | awk -F. '{print $$1;}')*/genomu.pid

remsh1:
	ERL_LIBS=apps:deps iex --name genomu_remsh1@127.0.0.1 --remsh genomu1@127.0.0.1

remsh2:
	ERL_LIBS=apps:deps iex --name genomu_remsh2@127.0.0.1 --remsh genomu2@127.0.0.1

remsh3:
	ERL_LIBS=apps:deps iex --name genomu_remsh3@127.0.0.1 --remsh genomu3@127.0.0.1

clean:
	@cd apps/genomu && $(genomu_path) mix clean

test: apps/genomu/ebin
	@cd apps/genomu && $(genomu_path) MIX_ENV=test mix test --no-start