%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(json_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], jiffy:encode({[{<<"message">>, <<"Hello, World!">>}]}), Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
