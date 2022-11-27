FROM erlang:25.1

WORKDIR /elli
COPY src src
COPY rebar.config rebar.config

RUN rebar get-deps
RUN rebar compile

EXPOSE 8080

CMD erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s elli_bench -noshell
