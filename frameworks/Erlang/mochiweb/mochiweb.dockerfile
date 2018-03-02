FROM tfb/erlang:latest

COPY ./ ./

RUN rebar get-deps
RUN rebar compile

CMD erl +K true +sbwt very_long +swt very_low -pa ebin deps/*/ebin -boot start_sasl -config priv/app.config -s mochiweb_bench_app -noshell
