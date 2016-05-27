.PHONY: all test eunit check ct compile

all:
	make compile
	make -j test

test: check unit ct

compile:
	./rebar3 compile

ct:
	./rebar3 ct --config .ct_spec -c true -v

unit:
	./rebar3 eunit -c true -v

check:
	./rebar3 dialyzer
