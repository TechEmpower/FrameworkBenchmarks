%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(db_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
        random:seed(erlang:now()),
        {JSON, Req2} = case cowboy_req:qs_val(<<"queries">>, Req) of
		{undefined, Req1} ->
			{result_packet, _, _, [Res], _} = emysql:execute(test_pool, db_stmt, [random:uniform(10000)]),
			{Res, Req1};
		{N, Req1} ->
			I = list_to_integer(binary_to_list(N)),
			Res = [ Res || 
			        {result_packet, _, _, [Res], _} <- [emysql:execute(test_pool, db_stmt, [random:uniform(10000)]) || _ <- lists:seq(1, I) ]],
			{Res, Req1}
		end,
	{ok, Req3} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(JSON), Req2),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.
