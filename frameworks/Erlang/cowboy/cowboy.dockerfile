FROM erlang:18.3.4.8

ADD ./ /cowboy
WORKDIR /cowboy

RUN rebar get-deps
RUN rebar compile

CMD erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s hello_world -noshell
