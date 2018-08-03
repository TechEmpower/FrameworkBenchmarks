FROM erlang:18.3.4.8

ADD ./ /chicagoboss
WORKDIR /chicagoboss

RUN rebar get-deps
RUN rebar compile

CMD erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -config boss -s boss -sname chicagoboss -noshell
