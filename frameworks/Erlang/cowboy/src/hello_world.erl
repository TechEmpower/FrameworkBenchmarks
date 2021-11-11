%% Feel free to use, reuse and abuse the code in this file.

-module(hello_world).

%% API.
-export([start/0]).

%% API.

start() ->
        ssl:start(),	
	ok = application:start(ranch),
	ok = application:start(cowlib),
	ok = application:start(cowboy),
	ok = application:start(hello_world).
