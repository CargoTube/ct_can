REBAR = $(shell pwd)/rebar3
APP=sb_core

.PHONY: all ct test clean elvis compile

all: compile

clean:
	$(REBAR) clean

eunit:
	$(REBAR) eunit
	$(REBAR) cover

ct:
	$(REBAR) ct
	$(REBAR) cover

elvis:
	$(REBAR) lint

compile:
	$(REBAR) compile
