%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
        application:start(jiffy),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/json", toppage_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 200, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	hello_world_sup:start_link().

stop(_State) ->
	ok.
