PROJECT = list_as_table

ERL := $(shell which erl)
HOSTNAME := $(shell hostname)

.PHONY: example run
include erlang.mk

example: app
	make -C $(PWD)/example

run: app example
	exec ${ERL} -pa $(PWD)/ebin $(PWD)/example/ebin  -name tabula@$(HOSTNAME)
