FROM erlang:25.1-alpine

ADD ./ /chicagoboss
WORKDIR /chicagoboss

RUN rebar get-deps
RUN rebar compile

EXPOSE 8080

CMD erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -config boss -s boss -sname chicagoboss -noshell
