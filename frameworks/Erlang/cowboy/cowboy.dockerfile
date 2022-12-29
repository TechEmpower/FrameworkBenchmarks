FROM erlang:25.1

ADD ./ /cowboy
WORKDIR /cowboy

RUN rebar get-deps
RUN rebar compile

EXPOSE 8080

CMD erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s hello_world -noshell
