FROM tfb/erlang:latest

RUN rebar get-deps
RUN rebar compile

CMD erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s hello_world -noshell
