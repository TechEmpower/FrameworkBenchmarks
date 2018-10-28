FROM erlang:20.3.8.4

ADD ./ /cowboy
WORKDIR /cowboy

RUN rebar get-deps
RUN rebar compile

CMD erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s hello_world -noshell
