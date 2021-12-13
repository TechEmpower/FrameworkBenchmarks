FROM erlang:18.3.4.8

ADD ./ /mochiweb
WORKDIR /mochiweb

RUN rebar get-deps
RUN rebar compile

EXPOSE 8080

CMD erl +K true +sbwt very_long +swt very_low -pa ebin deps/*/ebin -boot start_sasl -config priv/app.config -s mochiweb_bench_app -noshell
