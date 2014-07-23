-module(yaws_bench_app).
-behaviour(application).
-export([start/0]).
-export([start/2, stop/1]).

start() ->
    application:ensure_all_started(yaws_bench).

start(_Type, _StartArgs) ->
    {ok, Port} = application:get_env(yaws_bench, http_port),
    yaws_bench_sup:start_link([{port, Port}]).

stop(_State) ->
    ok.
