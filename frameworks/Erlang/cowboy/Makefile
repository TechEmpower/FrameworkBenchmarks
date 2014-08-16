all:
	./rebar get-deps
	./rebar compile
	erl -pa ebin deps/*/ebin -s hello_world -noshell -detached
