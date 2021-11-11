%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(plaintext_handler).

-export([init/2]).

init(Req, State) ->
  Req2 = cowboy_req:reply(200, #{<<"Content-Type">> => <<"text/plain">>}, <<"Hello, World!">>, Req),
  {ok, Req2, State}.
