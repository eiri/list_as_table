PROJECT = list_as_table

ERL := $(shell which erl)
HOSTNAME := $(shell hostname)

.PHONY: run
include erlang.mk

run: app
	exec ${ERL} -pa ${PWD}/ebin -name ${PROJECT}@${HOSTNAME}
