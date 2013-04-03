%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
        crypto:start(),
        application:start(emysql),
        application:start(jiffy),
        emysql:add_pool(test_pool, 32,
          "benchmarkdbuser", "benchmarkdbpass", "localhost", 3306,
          "hello_world", utf8),
	emysql:prepare(db_stmt, <<"SELECT * FROM World where id = ?">>),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/json", json_handler, []},
			{"/db", db_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	hello_world_sup:start_link().

stop(_State) ->
	ok.
