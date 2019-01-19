-module(mochiweb_bench_app).
-behaviour(application).
-export([start/0]).
-export([start/2, stop/1]).

start() ->
    application:ensure_all_started(mochiweb_bench).

start(_Type, _StartArgs) ->
    store:init(),
    Port = 8080,
    mochiweb_bench_sup:start_link([{port, Port}]).

stop(_State) ->
    ok.
