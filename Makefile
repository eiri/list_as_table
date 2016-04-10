.PHONY: all compile eunit test clean example run

ERL := $(shell which erl)
HOSTNAME := $(shell hostname)
BUILD_DIR = _build/default/lib

all: compile eunit

compile:
	@rebar3 compile

eunit:
	@rebar3 eunit

test:
	@rebar3 eunit -v

clean:
	@rebar3 clean --all

example: compile
	@make -C $(PWD)/example

run: example
	@exec ${ERL} -pa $(PWD)/$(BUILD_DIR)/*/ebin \
		$(PWD)/example/$(BUILD_DIR)/*/ebin -name tabula@$(HOSTNAME)
