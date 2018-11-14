.PHONY: ps erl all test test-erl

all: ps erl

ps:
	psc-package sources | xargs purs compile 'test/**/*.purs' 'src/**/*.purs'

test: ps erl
	erl -pa ebin jsx/_build/default/lib/jsx/ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'

test-erl: erl
	erl -pa ebin jsx/_build/default/lib/jsx/ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'

install: jsx
	psc-package install

jsx:
	git clone git@github.com:talentdeficit/jsx.git
	cd jsx && rebar3 compile

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl
