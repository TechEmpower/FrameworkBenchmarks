%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(query_handler).

-export([init/2]).

init(Req, State) ->
    QsVals = cowboy_req:parse_qs(Req),
    N = case lists:keyfind(<<"queries">>, 1, QsVals) of
            false ->
                <<"1">>;
            {_, BinaryN} ->
                BinaryN
        end,

      I = try binary_to_integer(N) of
            X when X > 500 -> 500;
            X when X < 1 -> 1;
            X -> X
          catch error:badarg -> 1 end,

      JSON = [ {[{<<"id">>, ID}, {<<"randomNumber">>, Rand}]} ||
              {result_packet, _, _, [[ID, Rand]], _} <- [emysql:execute(test_pool, db_stmt, [rand:uniform(10000)]) || _ <- lists:seq(1, I) ]],

    Req2 = cowboy_req:reply(200, #{<<"Content-Type">> => <<"application/json">>}, jiffy:encode(JSON), Req),
  {ok, Req2, State}.
