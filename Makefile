.PHONY: ps erl all test clean distclean

.DEFAULT_GOAL := ps

all: test

ps:
	@spago build

clean:
	rm -rf output

distclean: clean
	rm -rf .spago

test: 
	rebar3 compile
	@ERL_LIBS=_build/default/lib spago test
