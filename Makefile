.PHONY: all test eunit check

all: test

test: check unit

unit:
	./rebar3 eunit -c true -v

check:
	./rebar3 dialyzer
