%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(query_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
        random:seed(erlang:now()),
    {N, Req1} = cowboy_req:qs_val(<<"queries">>, Req, <<"1">>),

      I = try binary_to_integer(N) of
            X when X > 500 -> 500;
            X when X < 1 -> 1;
            X -> X
          catch error:badarg -> 1 end,

      JSON = [ {[{<<"id">>, ID}, {<<"randomNumber">>, Rand}]} ||
              {result_packet, _, _, [[ID, Rand]], _} <- [emysql:execute(test_pool, db_stmt, [random:uniform(10000)]) || _ <- lists:seq(1, I) ]],

  {ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], jiffy:encode(JSON), Req1),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
