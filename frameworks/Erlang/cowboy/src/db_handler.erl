%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(db_handler).

-export([init/2]).


init(Req, State) ->
    QsVals = cowboy_req:parse_qs(Req),
    JSON = case lists:keyfind(<<"queries">>, 1, QsVals) of
               false ->
                   {result_packet, _, _, [[ID, Rand]], _} = emysql:execute(test_pool, db_stmt, [rand:uniform(10000)]),
                   [{[{<<"id">>, ID}, {<<"randomNumber">>, Rand}]}];
               {_, N} ->
                   I = binary_to_integer(N),
                   [ {[{<<"id">>, ID}, {<<"randomNumber">>, Rand}]} ||
                       {result_packet, _, _, [[ID, Rand]], _} <- [emysql:execute(test_pool, db_stmt, [rand:uniform(10000)]) || _ <- lists:seq(1, I) ]]
           end,
    Req3 = cowboy_req:reply(200, #{<<"Content-Type">> => <<"application/json">>}, jiffy:encode(lists:nth(1,JSON)), Req),
	{ok, Req3, State}.
