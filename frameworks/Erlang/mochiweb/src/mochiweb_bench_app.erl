-module(mochiweb_bench_app).
-behaviour(application).
-export([start/0]).
-export([start/2, stop/1]).

start() ->
    application:ensure_all_started(mochiweb_bench).

start(_Type, _StartArgs) ->
    {ok, Port} = application:get_env(mochiweb_bench, http_port),
    mochiweb_bench_sup:start_link([{port, Port}]).

stop(_State) ->
    ok.
