-module(elli_bench_cb).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"json">>], _Req) ->
    %% Reply with a normal response. 'ok' can be used instead of '200'
    %% to signal success.
    {ok, [{<<"Content-Type">>, <<"application/json">>}], jiffy:encode({[{<<"message">>, <<"Hello, World!">>}]})};

handle('GET',[<<"db">>], Req) ->
        random:seed(erlang:now()),
        JSON = case elli_request:get_arg(<<"queries">>, Req) of
		undefined ->
			{result_packet, _, _, [[ID, Rand]], _} = emysql:execute(test_pool, db_stmt, [random:uniform(10000)]),
			{[{<<"id">>, ID}, {<<"randomNumber">>, Rand}]};
		N ->
			I = list_to_integer(binary_to_list(N)),
			Res = [ {[{<<"id">>, ID}, {<<"randomNumber">>, Rand}]} || 
			        {result_packet, _, _, [[ID, Rand]], _} <- [emysql:execute(test_pool, db_stmt, [random:uniform(10000)]) || _ <- lists:seq(1, I) ]],
			Res
		end,
    {ok, [{<<"Content-Type">>, <<"application/json">>}], jiffy:encode(JSON)};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
