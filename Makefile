.PHONY: ps erl all test test-erl

all: ps erl

ps:
	spago -x test.dhall build

erl: ps
	erlc -o ebin output/*/*.erl

test: erl
	erl -pa ebin jsx/_build/default/lib/jsx/ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'

jsx:
	git clone git@github.com:talentdeficit/jsx.git
	cd jsx && rebar3 compile

