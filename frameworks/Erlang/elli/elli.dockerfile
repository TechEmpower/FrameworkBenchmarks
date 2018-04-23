FROM erlang:18.3.4.8

ADD ./ /elli
WORKDIR /elli

RUN rebar get-deps
RUN rebar compile

CMD erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s elli_bench -noshell
