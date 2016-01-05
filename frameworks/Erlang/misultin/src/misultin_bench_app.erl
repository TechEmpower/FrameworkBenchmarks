-module(misultin_bench_app).
-behaviour(application).
-export([start/0]).
-export([start/2, stop/1]).

start() ->
    application:ensure_all_started(misultin_bench).

start(_Type, _StartArgs) ->
    {ok, Port} = application:get_env(misultin_bench, http_port),
    Options = [{loop, fun(Req) -> web_handler:dispatch(Req) end},
               {port, Port}],
    misultin_bench_sup:start_link(Options).

stop(_State) ->
    ok.
