%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

%% NOTE: 
%%   If size of db testpool is too big (e.g: 5000), 
%%   it will fail travis ci test. So I shrink this to 256.
%%   blee@techempower.com


start(_Type, _Args) ->
        crypto:start(),
        application:start(emysql),
        emysql:add_pool(test_pool, 256,
          "benchmarkdbuser", "benchmarkdbpass", "TFB-database", 3306,
          "hello_world", utf8),
	emysql:prepare(db_stmt, <<"SELECT * FROM World where id = ?">>),
	Dispatch = cowboy_router:compile([
		{'_', [
      {"/plaintext", plaintext_handler, []},
			{"/json", json_handler, []},
			{"/db", db_handler, []},
      {"/query", query_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 256, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	hello_world_sup:start_link().

stop(_State) ->
	ok.
