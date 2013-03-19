genomu_src := $(wildcard apps/genomu/lib/*.ex) $(wildcard apps/genomu/lib/**/*.ex) \
              $(wildcard apps/genomu/lib/**/**/*.eex) \
              apps/genomu/mix.exs
instance := $(shell whoami)


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
	@ulimit -n 4096 && ERL_LIBS=apps:deps elixir --name "genomu-$(instance)1@127.0.0.1" --erl "-config test/sys.1 -s Elixir-Genomu" --no-halt &
	@ulimit -n 4096 && ERL_LIBS=apps:deps elixir --name "genomu-$(instance)2@127.0.0.1" --erl "-config test/sys.2 -s Elixir-Genomu" --no-halt &
	@ulimit -n 4096 && ERL_LIBS=apps:deps elixir --name "genomu-$(instance)3@127.0.0.1" --erl "-config test/sys.3 -s Elixir-Genomu" --no-halt &

stop:
	@kill -9 `cat data/$(shell hostname | awk -F. '{print $$1;}')$(instance)1/genomu.pid` \
                 `cat data/$(shell hostname | awk -F. '{print $$1;}')$(instance)2/genomu.pid` \
                 `cat data/$(shell hostname | awk -F. '{print $$1;}')$(instance)3/genomu.pid`
	@rm -f data/$(shell hostname | awk -F. '{print $$1;}')*/genomu.pid

remsh1:
	ERL_LIBS=apps:deps iex --name "genomu_remsh-$(instance)1@127.0.0.1" --remsh "genomu-$(instance)1@127.0.0.1"

remsh2:
	ERL_LIBS=apps:deps iex --name "genomu_remsh-$(instance)2@127.0.0.1" --remsh "genomu-$(instance)2@127.0.0.1"

remsh3:
	ERL_LIBS=apps:deps iex --name "genomu_remsh-$(instance)3@127.0.0.1" --remsh "genomu-$(instance)3@127.0.0.1"

clean:
	@cd apps/genomu && $(genomu_path) mix clean

test:
	@cd apps/genomu && $(genomu_path) GENOMU_PATH=`pwd` MIX_ENV=test mix deps.get
	@cd apps/genomu && $(genomu_path) MIX_ENV=test mix test --no-start