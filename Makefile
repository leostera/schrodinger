all: test

test: check unit

unit:
	./rebar3 eunit

check:
	./rebar3 dialyzer
