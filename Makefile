all: test

test:
	./rebar3 dialyzer
